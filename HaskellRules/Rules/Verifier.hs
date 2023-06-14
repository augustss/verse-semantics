{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

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

mustDecide :: Binders -> Expr -> Bool
mustDecide bs = go
  where
    go (Arr as)    = all go as
    go (One e)     = go e
    go (e1 :=: e2) = go e1 && go e2
    go (e1 :>: e2) = go e1 && go e2
    go (e1 :@: e2) = go e1 && go e2
    go (Assume _)  = True
    go (Int _)     = True
    go (Op _)      = True
    go (Var x)     = x `elem` lamBinds bs
    go _           = False





{-

if (e1)
  { e2 }
else
  { e3 }

-}

-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
verifierRules :: VRule
verifierRules _env lhs =
   -- CTX[e] ---> CTX[assume{e}]    if    CTX |- e
   "Prove" `name`
   do (ctx, g, _, e) <- eX lhs
      guard (case e of Assume _ -> False; _ -> True)
      guard (g `proves` e)
      pure (ctx (Assume e))
   ++
   -- CTX[if e1 e2 e3] ---> CTX[(assume{e1} ; e2) | (assume-fail{e1}; e3)] IF CTX `mustDecide` e1
   "Unfold-If" `name`
   do (ctx, _, bs, If e1 e2 e3) <- eX lhs
      guard (mustDecide mempty e1)
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

-----------------------------------------------------------------------
-- | Binders of an Execution Context
-----------------------------------------------------------------------

data Binders = MkBinders { lamBinds :: [Ident], exiBinds :: [Ident] }
   deriving (Eq, Ord, Show)

instance Semigroup Binders where
  (<>) :: Binders -> Binders -> Binders
  (<>) b1 b2 = MkBinders { lamBinds = lamBinds b1 ++ lamBinds b2
                         , exiBinds = exiBinds b1 ++ exiBinds b2 }

instance Monoid Binders where
   mempty :: Binders
   mempty = MkBinders { lamBinds = [], exiBinds = [] }

pushLam :: Ident -> Binders -> Binders
pushLam x bs = bs { lamBinds = x : lamBinds bs }

pushExi :: Ident -> Binders -> Binders
pushExi x bs = bs { exiBinds = x : exiBinds bs }
----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

eX :: Expr -> [(EContext, QContext, Binders, Expr)]
eX = execEX mempty

execEX :: Binders -> Expr -> [(EContext, QContext, Binders, Expr)]
execEX bs lhs = execEX1 bs lhs ++ [(id, Arr [], bs, lhs)]

execEX1 :: Binders -> Expr -> [(EContext, QContext, Binders, Expr)]
execEX1 bs lhs =
  do v :=: x     <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (\ a -> v :=: ctx a, g, bs', hole)
 ++
   -- HOLE; e
  do x :>: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:>: e) . ctx, g :>: e, bs', hole)
 ++
   -- e; HOLE
  do e :>: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :>:) . ctx, e :>: g, bs', hole)
 ++
   -- Exi y HOLE
  do EXI y x <- [lhs]
     (ctx, g, bs', hole) <- execEX (pushExi y bs) x
     pure (EXI y . ctx, g, bs', hole)   -- y should be visible to e in g |- e
 ++
   -- HOLE e
  do x :@: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:@: e) . ctx, g, bs', hole)
 ++
   -- ONE HOLE
  do One x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (One . ctx, g, bs', hole)
 ++
  do All x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (All . ctx, g, bs', hole)
 ++
  do x :|: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:|: e) . ctx, g, bs', hole)
 ++
  do e :|: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :|:) . ctx, g, bs', hole)
 ++
  do Lam (Bind y x) <- [lhs]
     (ctx, g, bs', hole) <- execEX (pushLam y bs) x
     pure (Lam . Bind y . ctx, Assume (Var y) :>: g, bs', hole)  -- y should be visible to e in g |- e
 ++
  do x :@: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:@: e) . ctx, g, bs', hole)
 ++
  do e :@: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :@:) . ctx, g, bs', hole)
 ++
  do If x e1 e2 <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (\a -> If (ctx a) e1 e2, g, bs', hole)
 ++
  do Assert x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (Assert . ctx, g, bs', hole)
 ++
  do Verify x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (Verify . ctx, g, bs', hole)     
