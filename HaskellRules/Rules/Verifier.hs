{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}

module Rules.Verifier  where
import TRS.Bind
import TRS.TRS
import Rules.Core hiding (Wrong)
import Rules.ICFP (allSystemsICFP)
import Control.Monad (guard)

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
  "Assume-Val" `name`
  do Assume (Val v) <- [lhs]
     pure (Val v)
  ++
  "Assert-Assert" `name`
  do Assert (Assert e) <- [lhs]
     pure (Assert e)
--   ++
--   "Assert-Assume" `name`
--   do Assert (Assume e) <- [lhs]
--      pure (Assume e)
  ++
  "Assert-Assume" `name`
  do Assert (Assume e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assert e2)
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
   ++
   "Assume-Exi" `name`
   do (ctx, g, Assume (EXI x e)) <- execEX lhs
      let x' = fresh g e
      pure (ctx (Assume (subst [(x, Var x')] e)))


proves :: QContext -> Expr -> Bool
proves QEmp _       = False
proves (QDef _ g) e = proves g e
proves (QAsm p g) e = p == e || proves g e

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
qAsm (EXI x e)   g = QDef x  (qAsm e  g)
qAsm (LAM x e)   g = QDef x  (qAsm e  g)
qAsm _           g = g

fresh :: QContext -> Expr -> Ident
fresh g e = identNotIn (free e ++ bound g)

bound :: QContext -> [Ident]
bound QEmp       = []
bound (QDef x g) = x : bound g
bound (QAsm _ g) =     bound g