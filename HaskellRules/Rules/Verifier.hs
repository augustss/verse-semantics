{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Rules.Verifier(
  allSystemsVerify,
  icfpVerifier,
  verify,
  ) where
import TRS.Bind
import TRS.TRS
import TRS.Traced
import TRS.Tarjan
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP, execX, ltExpr)
import Control.Monad (guard)
import Data.List( intersect )

-- | Run verification rules.

verify :: TRSystem Expr -> Expr -> (Bool, Traced Expr)
verify sys e = res
 where
   res =
     case tarjan1 (-1) arrow (e :<-- []) of -- (preProcess sys (ruleEnv sys) e :<-- [])
       Just (tr@(x :<-- _):_) -> (isDone x, tr)
       _ -> undefined
   arrow (a :<-- t)       = [ b :<-- ((r,a):t) | (r,b) <- stepS sys a ]

  --norms           = normalFormsFuelTracePlain sys (-1) e
  --tr@(x :<-- _):_ = nrDone norms ++ nrLeft norms

isDone :: Expr -> Bool
isDone = collect done (&&)
 where
  done (Assert _) = False
  done _          = True

-- | Top-level "Verifier" rewrite system based on ICFP rules -------------------------

icfpVerifier :: TRSystem Expr
icfpVerifier = icfpActual
  { sname = "ICFP-verify"
  , description = "ICFP + extra verifier rules"
  , rules = (rules icfpActual -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL")
              <> generalizedIcfpRules
              <> assumeAssertRules
              <> verifierRules
  }

icfpActual :: TRSystem Expr
icfpActual = head allSystemsICFP

allSystemsVerify :: [TRSystem Expr]
allSystemsVerify = [icfpVerifier]

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
assumeAssertRules _foo lhs =
  -- ASSUME --

  -- Assume {v} ----> v
  --"Assume-Val" `name`
  --do Assume (Val v) <- [lhs]
  --   pure (Val v)
  -- ++
  "Assume-HNF" `name`
  do Assume (HNF v) <- [lhs]
     pure v
  ++
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

mustDecide :: [BndVar] -> Expr -> Bool
mustDecide bs = go
  where
    lamBinds       = [x | BLam x <- bs]
    go (Arr as)    = all go as
    go (One e)     = go e
    go (e1 :=: e2) = go e1 && go e2
    go (e1 :>: e2) = go e1 && go e2
    go (e1 :@: e2) = go e1 && go e2 && isDecideOp e1
    go (Assume _)  = True
    go (Int _)     = True
    go (Op _)      = True
    go (Var x)     = x `elem` lamBinds
    go _           = False

-- mustDecide (Arr as)    = all mustDecide as
-- mustDecide (One e)     = mustDecide e
-- mustDecide (e1 :=: e2) = mustDecide e1 && mustDecide e2
-- mustDecide (e1 :>: e2) = mustDecide e1 && mustDecide e2
-- mustDecide (e1 :@: e2) = mustDecide e1 && mustDecide e2 && isDecideOp e1
-- mustDecide (Assume _)  = True
-- mustDecide (Int _)     = True
-- mustDecide (Op _)      = True
-- mustDecide _           = False

isDecideOp :: Expr -> Bool
isDecideOp (Op Le)    = True
isDecideOp (Op Lt)    = True
isDecideOp (Op Ge)    = True
isDecideOp (Op Gt)    = True
isDecideOp (Op Ne)    = True
isDecideOp (Op Div)   = True
isDecideOp (Op IsInt) = True
isDecideOp (Op DotDot)= True
isDecideOp (Op Append)= True
isDecideOp _          = False

-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
verifierRules :: VRule
verifierRules env lhs =
   -- CTX[e] ---> CTX[assume{e}]    if    CTX |- e
   "Prove" `name`
   do (ctx, g, e) <- eX lhs
      guard (case e of Assume _ -> False; _ -> True)
      guard (g `proves` e)
      pure (ctx (Assume e))
   ++
   -- CTX[if e1 e2 e3] ---> CTX[(assume{e1} ; e2) | (assume-fail{e1}; e3)] IF CTX `mustDecide` e1
   "Unfold-If" `name`
   do (ctx, _, If e1 e2 e3) <- eX lhs
      let bs = bndVars env
      guard (mustDecide bs e1)
      pure (ctx (unfoldIte e1 e2 e3))

unfoldIte :: Expr -> Expr -> Expr -> Expr
unfoldIte e1 e2 e3 = (Assume e1 :>: e2) :|: {- assume-fail{e1} :>: -} e3

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
  facts (Assume a)  = assumes (unAssume a)
  facts _           = []

  assumes a = a : derives a

  -- special rules
  derives (Op IsInt :@: a) = ( a :=: a ) : assumes a
  derives _                = []

----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

eX :: Expr -> [(EContext, QContext, Expr)]
eX = execEX

execEX :: Expr -> [(EContext, QContext, Expr)]
execEX lhs = execEX1 lhs ++ [(id, Arr [], lhs)]

execEX1 :: Expr -> [(EContext, QContext, Expr)]
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
     pure (Lam . Bind y . ctx, Assume (Var y) :>: g, hole)  -- y should be visible to e in g |- e
 ++
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
  do e :@: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :@:) . ctx, g, hole)
 ++
  do If x e1 e2 <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\a -> If (ctx a) e1 e2, g, hole)
 ++
  do Assert x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Assert . ctx, g, hole)
 ++
  do Verify x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (Verify . ctx, g, hole)
