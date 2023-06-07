{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Rules.Verifier  where
import TRS.Bind
import TRS.TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP, execX, ltExpr)
import Control.Monad (guard)

trivVerifier :: TRSystem Expr
trivVerifier = icfpVerifier
  { sname = "Verifier rules based on ICFP (trivial)",
    rules  = (rules icfpVerifier -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL")
               <> generalizedIcfpRules
               <> contextFreeRules
               <> contextSensitiveRules
--    rules2 = contextSensitiveRules
  }

icfpVerifier :: TRSystem Expr
icfpVerifier = base'{ sname = "Verifier rules based on ICFP" }
  where
    base     = head allSystemsICFP
    base'    = base { rules = rules base }

--------------------------------------------------------------------------------------
-- | The "Context" in which a subsumption must hold; Tim's "G" -- set of "known facts"
--------------------------------------------------------------------------------------

data QContext
  = QDef Ident QContext   -- ^ x; G
  | QAsm Expr  QContext   -- ^ e; G
  | QEmp                  -- ^ EMPTY
  deriving (Show)

--------------------------------------------------------------------------------
-- | Abstract Rules
--------------------------------------------------------------------------------
type VRule = Rule Expr

-- | ICFP rules generalized to remove the trailing `e :>: ...` pattern


generalizedIcfpRules :: VRule
generalizedIcfpRules env lhs =
  "EQN-FLOAT-GEN" `name`
  do Val v :=: (eq :>: e1) <- [lhs]
     pure (eq :>: (Val v :=: e1))
  ++
  "U-LIT-GEN" `name`
  do (Int k1 :=: Int k2) <- [lhs]
     guard (k1 == k2)
     pure (Int k1)
  ++
  "U-FAIL-GEN" `name`
  do HNF e1 :=: HNF e2 <- [lhs]
     -- Avoid the cases handled above
     guard (case (e1,e2) of (Int k1,Int k2) -> k1 /= k2
                            (Ref k1,Ref k2) -> k1 /= k2
                            (Arr a1,Arr a2) -> length a1 /= length a2
                            _               -> True)
     pure Fail
  ++
  "SUBST-GEN" `name`
  do (ctx, Var x :=: Val v) <- execX lhs
     let freeX = free ctx
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (subst sub (ctx (Var x0 :=: Val v)))

-- | Rules to "push" `Assume` and `Assert` into sub-terms -----------------------


-- assume {x = 2}; assert { x = y ; y = 2; 0}


contextFreeRules :: VRule
contextFreeRules _ lhs =
  -- Assume {e1; e2} ---> Assume e1; Assume e2
  "Assume-Seq" `name`
  do Assume (e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  -- Assert {v} ----> v
  "Assert-Val" `name`
  do Assert (Val v) <- [lhs]
     pure (Val v)
  ++
  -- Assume {v} ----> v
  "Assume-Val" `name`
  do Assume (Val v) <- [lhs]
     pure (Val v)
  ++
  -- Assume {Assume{e}} ----> Assume{e}
  "Assume-Assume" `name`
  do Assume (Assume e) <- [lhs]
     pure e
  ++
  -- Assert {Assert {e}} ----> Assert {e}
  "Assert-Assert" `name`
  do Assert (Assert e) <- [lhs]
     pure (Assert e)
  ++
  -- Assert { Assume {e1}; e2 } ----> Assume {e1} ; Assert {e2}
  "Assert-Assume" `name`
  do Assert (Assume e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assert e2)
  ++
  -- Assume { Assert {e} } ----> Assume {e}
  "Assume-Assert" `name`
  do Assume (Assert e) <- [lhs]
     pure (Assume e)
  ++
  -- Assume { e1 | e2 } ----> Assume {e1} | Assume {e2}
  "Assume-Choice" `name`
  do Assume (e1 :|: e2) <- [lhs]
     pure (Assume e1 :|: Assume e2)
  ++
  -- Assert { e } ----> e   if   e is crash-free
  "Assert-CrashFree" `name`
  do Assert e <- [lhs]
     guard (crashFree e)
     pure e
  ++
{- -- these rules seem wrong? --Koen
  -- Assert { e1 | e2 } ----> Assert {e1} | Assert {e2}
  "Assert-Choice" `name`      -- seems TOO strong?
  do Assert (e1 :|: e2) <- [lhs]
     pure (Assert e1 :|: Assert e2)
  ++
  -- Assert { Fail } ---> Fail
  "Assert-Fail" `name`
  do Assert Fail <- [lhs]
     pure Fail
  ++
-}
  "Verify" `name`
  do Verify e <- [lhs]
     let verified (Assert _) = False
         verified _          = True 
     guard (collect verified (&&) e)
     pure e
  ++
  "Assume-Verify" `name`
  do Assume (Verify e) <- [lhs]
     pure (Assume e)

crashFree :: Expr -> Bool
crashFree (Val _) = True
crashFree (Assume _ :>: e) = crashFree e
crashFree (e1 :|: e2) = crashFree e1 && crashFree e2
crashFree _ = False

-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
contextSensitiveRules :: VRule
contextSensitiveRules _env lhs =
   "Prove" `name`
   -- | E[Assert (e; e')] ---> E[e; Assert{e'}]    IF ctx(E) |- e
   do (ctx, g, e) <- execEX lhs
      guard (g `proves` e)
      pure (ctx (Assume e))
   ++
   "Assume-Exi" `name`
   do (ctx, g, Assume (EXI x e)) <- execEX lhs
      let x' = fresh g e
      pure (ctx (Assume (subst [(x, Var x')] e)))

--------------------------------------------------------------------------------
-- | A simple "decision procedure"
--------------------------------------------------------------------------------
proves :: QContext -> Expr -> Bool
proves QEmp _       = False
proves (QDef _ g) e = proves g e
proves (QAsm p g) e = proves1 p e || proves g e

proves1 :: Expr -> Expr -> Bool
proves1 (INT (Var x)) (Var y :=: Var z)
  | x == y && x == z = True
proves1 p e          = p == e

pattern INT :: Expr -> Expr
pattern INT e = Op IsInt :@: e

----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

-- (forall x. assume {x=3}) ;  assert {x=3}

-- scope contexts
-- E ::= v = HOLE | HOLE; e
execEX, execEX1 :: Expr -> [(EContext, QContext, Expr)]
-- E context
execEX lhs = execEX1 lhs ++ [(id, QEmp, lhs)]
-- X context, X /= hole
execEX1 lhs =
  do v :=: x     <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\ a -> v :=: ctx a, g, hole)
 ++
   -- HOLE; e
  do x :>: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:>: e) . ctx, qAsm e g, hole)
 ++
   -- e; HOLE
  do e :>: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :>:) . ctx, qAsm e g, hole)
 ++
   -- Exi y HOLE
  do EXI y x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (EXI y . ctx, QDef y g, hole)
 ++
   -- HOLE e
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
   -- ONE HOLE
  do One x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (One . ctx, g, hole)
 ++
  do All x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (All . ctx, g, hole)
 ++
  do x :|: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:|: e) . ctx, g, hole)
 ++
  do e :|: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :|:) . ctx, g, hole)
 ++
  do Lam (Bind y x) <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Lam . Bind y . ctx, QDef y g, hole)
 ++
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
  do e :@: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :@:) . ctx, g, hole)
 ++
  do Assert x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Assert . ctx, g, hole)


qAsm :: Expr -> QContext -> QContext
qAsm (Assume e)  g = QAsm e g
qAsm (e1 :>: e2) g = qAsm e1 (qAsm e2 g)
qAsm (EXI x e)   g = QDef x  (qAsm e  g)
--  qAsm (LAM x e)   g = QDef x  (qAsm e  g)
qAsm _           g = g


fresh :: QContext -> Expr -> Ident
fresh g e = identNotIn (free e ++ bound g)

bound :: QContext -> [Ident]
bound QEmp       = []
bound (QDef x g) = x : bound g
bound (QAsm _ g) =     bound g
