{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}

module Rules.Verifier  where
import TRS.Bind
import TRS.TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP, execX, ltExpr)
import Control.Monad (guard)
import Data.List( intersect )

-- | Top-level "Verifier" rewrite system based on ICFP rules -------------------------

icfpVerifier :: TRSystem Expr
icfpVerifier = icfpActual
  { sname = "ICFP + extra verifier rules"
  , rules = (rules icfpActual -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL")
              <> generalizedIcfpRules
              <> assumeAssertRules
              <> verifierRules
  }

icfpActual :: TRSystem Expr
icfpActual = head allSystemsICFP
    
--------------------------------------------------------------------------------------
-- | The "Context" in which a subsumption must hold; Tim's "G" -- set of "known facts"
--------------------------------------------------------------------------------------

type QContext = Expr

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

-- | Rules for `Assume` and `Assert` -------------------------------------------

assumeAssertRules :: VRule
assumeAssertRules _ lhs =
  -- ASSUME --  

  -- Assume {v} ----> v
  --"Assume-Val" `name`
  --do Assume (Val v) <- [lhs]
  --   pure (Val v)
  -- ++
  -- Assume {e1; e2} ---> Assume e1; Assume e2
  "Assume-Seq" `name`
  do Assume (e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  -- Assume { e1 | e2 } ----> Assume {e1} | Assume {e2}
  "Assume-Choice" `name`
  do Assume (e1 :|: e2) <- [lhs]
     pure (Assume e1 :|: Assume e2)
  ++
  -- Assume { exi x . e } ----> exi x . Assume {e}
  "Assume-Exi" `name`
  do Assume (Exi (Bind x e)) <- [lhs]
     pure (Exi (Bind x (Assume e)))
  ++
  -- Assume {Assume{e}} ----> Assume{e}
  "Assume-Assume" `name`
  do Assume (Assume e) <- [lhs]
     pure (Assume e)
  ++
  -- Assume { Assert {e} } ----> Assume {e}
  "Assume-Assert" `name`
  do Assume (Assert e) <- [lhs]
     pure (Assume e)
  ++

  -- ASSERT --  

  -- Assert { e } ----> e   if   e must succeed
  "Assert-mustSucceed" `name`
  do Assert e <- [lhs]
     guard (mustSucceed e)
     pure e
  ++
{- (these are all subsumed by the above rule)
  -- Assert {v} ----> v
  "Assert-Val" `name`
  do Assert (Val v) <- [lhs]
     pure (Val v)
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
-}
  
  -- VERIFY --
  
  "Verify" `name`
  do Verify e <- [lhs]
     let verified (Assert _) = False
         verified _          = True
     guard (collect verified (&&) e)
     pure (Val (Arr []))
  ++
  "Assume-Verify" `name`
  do Assume (Verify _) <- [lhs]
     pure (Val (Arr []))

mustSucceed :: Expr -> Bool
mustSucceed (Int _)          = True
mustSucceed (Arr as)         = all mustSucceed as
mustSucceed (Lam _)          = True
mustSucceed (Assume _)       = True
mustSucceed (Assert _)       = True
mustSucceed (One e)          = mustSucceed e
mustSucceed (e1 :>: e2)      = mustSucceed e1 && mustSucceed e2
mustSucceed (e1 :|: e2)      = mustSucceed e1 || mustSucceed e2
mustSucceed (Exi (Bind _ e)) = mustSucceed e
mustSucceed _                = False

-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
verifierRules :: VRule
verifierRules _env lhs =
   -- CTX[e] ---> CTX[assume{e}]    if    CTX |- e
   "Prove" `name`
   do (ctx, g, e) <- execEX lhs
      guard (case e of Assume _ -> False; _ -> True)
      guard (g `proves` e)
      pure (ctx (Assume e))

--------------------------------------------------------------------------------
-- | A simple "decision procedure"
--------------------------------------------------------------------------------

proves :: QContext -> Expr -> Bool
g `proves` e = unAssume e `elem` facts g
 where
  unAssume (e1 :>: e2) = unAssume e1 :>: unAssume e2
  unAssume (e1 :|: e2) = unAssume e1 :|: unAssume e2
  unAssume (f :@: x)   = unAssume f :@: unAssume x
  unAssume (Assume a)  = unAssume a
  unAssume (e1 :=: e2) = unAssume e1 :=: unAssume e2
  unAssume a           = a
  
  vs = free e
 
  facts (g1 :>: g2) = facts g1 ++ facts g2
  facts (g1 :|: g2) = facts g1 `intersect` facts g2
  facts (Exi bnd)   = facts g' where Bind _ g' = alphaRename vs bnd
  facts (Assume a)  = assumes a
  facts _           = []
  
  assumes a = [ a ] ++ derives a
  
  -- special rules
  derives (Op IsInt :@: a) = [ a :=: a ] ++ assumes a 
  derives _                = []

----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

-- (forall x. assume {x=3}) ;  assert {x=3}

-- scope contexts
-- E ::= v = HOLE | HOLE; e
execEX, execEX1 :: Expr -> [(EContext, QContext, Expr)]
-- E context
execEX lhs = execEX1 lhs ++ [(id, Arr [], lhs)]
-- X context, X /= hole
execEX1 lhs =
  do v :=: x     <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\ a -> v :=: ctx a, g, hole)
 ++
   -- HOLE; e
  do x :>: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:>: e) . ctx, g :>: e, hole)
 ++
   -- e; HOLE
  do e :>: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :>:) . ctx, e :>: g, hole)
 ++
   -- Exi y HOLE
  do EXI y x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (EXI y . ctx, g, hole)   -- y should be visible to e in g |- e
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
     pure (Lam . Bind y . ctx, g, hole)  -- y should be visible to e in g |- e
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

