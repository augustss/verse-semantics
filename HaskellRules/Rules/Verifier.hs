{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Rules.Verifier  where
import TRS.Bind
import TRS.TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP)
import Control.Monad (guard)
-- import Epic.Print
-- import qualified Verifier.Verify as V
-- import Control.Monad (guard, filterM)
-- import Data.List (intersect)
-- import qualified Rules.ICFP as ICFP
-- import qualified Verifier.FOL as FOL
-- import Data.Maybe (maybeToList)

trivVerifier :: TRSystem Expr
trivVerifier = icfpVerifier
  {
    rules  = rules icfpVerifier Prelude.<> contextFreeRules,
    rules2 = contextSensitiveRules
  }

icfpVerifier :: TRSystem Expr
icfpVerifier = base'
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

contextFreeRules :: VRule
contextFreeRules _ lhs =
--   "Assert-Exi" `name`
--   do Assert (EXI x e) <- [lhs]
--      pure (EXI x (Assert e))
--   ++
--   "Assume-Exi" `name`
--   do Assert (EXI x e) <- [lhs]
--      pure (EXI x (Assert e))
--   ++
--   "Assert-Seq" `name`
--   do Assert (e1 :>: e2) <- [lhs]
--      pure (Assert e1 :>: Assert e2)
--  ++
  "Assume-Seq" `name`
  do Assume (e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  "Assert-Val" `name`
  do Assert (Val v) <- [lhs]
     pure (Val v)
  ++
  "Assert-Assert" `name`
  do Assert (Assert e) <- [lhs]
     pure (Assert e)
  ++
  "Assert-Assume" `name`
  do Assert (Assume e) <- [lhs]
     pure (Assume e)
  ++
  "Assume-Assert" `name`
  do Assume (Assert e) <- [lhs]
     pure (Assume e)

contextSensitiveRules :: VRule
contextSensitiveRules _env lhs =
   "Prove" `name`
   do (ctx, g, Assert (e :>: e')) <- execEX lhs
      guard (g `proves` e)
      pure (ctx (e :>: Assert e'))


proves :: QContext -> Expr -> Bool
proves QEmp _       = False
proves (QDef _ g) e = proves g e
proves (QAsm p g) e = p == e || proves g e

{-
 "A-APP" `TRSS.name`
  do (ctx, g, (AFUN x t1 p t2) :@: AVAL t) <- execEX lhs
     let freeT = free t
     if x `notElem` freeT then
        -- no name clash
        pure (ctx (absBeta x t p t2), subsumes g t t1)
     else do
        -- The x has to be renamed to avoid capture
        let freeE = free t1 ++ free p ++ free t2
            x' = identNotIn (freeT ++ freeE)
            (t1', p', t2') = substI x x' (t1, p, t2)
        pure (ctx (absBeta x' t p' t2'), subsumes g t t1')
  ++
  "A-CONSEQ-R" `TRSS.name`
  do (ctx, g, (Vval t) `Vis` t') <- execEX lhs
     pure (ctx (aval t'), subsumes g (ABase t) t')
-}

{-
absBeta :: Ident -> AVal -> Form -> AVal -> Expr
absBeta x t p t2 = EXI x ((Var x :=: aval t) :>: (Vasm p :>: aval t2))

join :: (Form, AVal) -> (Form, AVal) -> Expr
join (p1, t1)   (p2, t2)   = Vasm (joinForm p1 p2) :>: aval (joinAVal t1 t2)

joinForm :: Form -> Form -> Form
joinForm TRUE  _     = TRUE
joinForm _     TRUE  = TRUE
joinForm FALSE p     = p
joinForm p     FALSE = p
joinForm p1    p2    = if p1 == p2 then p1 else p1 :||: p2

mergeForm :: Form -> Form -> Form
mergeForm TRUE  p     = p
mergeForm p     TRUE  = p
mergeForm FALSE _     = FALSE
mergeForm _     FALSE = FALSE
mergeForm p1    p2    = p1 :||: p2

joinAVal :: AVal -> AVal -> AVal
joinAVal (ABase (Base (Bind a1 p1) ixs1)) (ABase (Base (Bind a2 p2) ixs2))
  | a1 == a2 = aBase (Bind a1 (joinForm p1 p2)) (ixs1 `intersect` ixs2) -- TODO: name shift shenanigans
joinAVal (AFun (Bind a1 (s1, p1, t1))) (AFun (Bind a2 (s2, p2, t2)))
  | a1 == a2 = AFun (Bind a1 (joinAVal s1 s2, mergeForm p1 p2, joinAVal t1 t2))
joinAVal  _ _ = error "todo: joinAVal"

tLam :: Ident -> Expr -> Form -> Expr -> Expr
tLam x e1 TRUE e2 = TLam (Bind x (e1, e2))
tLam x e1 p    e2 = TLam (Bind x (e1, Vasm p :>: e2))

-}


----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------

-- scope contexts

execEX, execEX1 :: Expr -> [(EContext, QContext, Expr)]
-- E context
execEX lhs = execEX1 lhs ++ [(id, QEmp, lhs)]
-- X context, X /= hole
execEX1 lhs =
  do v :=: x     <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (\ a -> v :=: ctx a, g, hole)
 ++
  do x :>: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:>: e) . ctx, qAsm e g, hole)
 ++
  do e :>: x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((e :>:) . ctx, qAsm e g, hole)
 ++
  do EXI y x <- [lhs]
     (ctx, g, hole) <- execEX x
     pure (EXI y . ctx, QDef y g, hole)
 ++
--   do Vif p x <- [lhs]
--      (ctx, g, hole) <- execEX x
--      pure (Vif p . ctx, g, hole)
--  ++
--   do Vis x t <- [lhs]
--      (ctx, g, hole) <- execEX x
--      pure ((`Vis` t) . ctx, g, hole)
--  ++
  do x :@: e <- [lhs]
     (ctx, g, hole) <- execEX x
     pure ((:@: e) . ctx, g, hole)
 ++
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

qAsm :: Expr -> QContext -> QContext
qAsm (Assume e)  g = QAsm e g
qAsm (e1 :>: e2) g = qAsm e1 (qAsm e2 g)
qAsm _           g = g
