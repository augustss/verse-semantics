module Rules.Equiv(equiv, norm, normalForm) where
import Data.Maybe
import GHC.Stack
import Rules.Core
import TRS.TRS( step )
import TRS.Tarjan
import TRS.Traced
import TRS.System

-- Test if two expressions are equivalent.
-- Reduce both and normalize.
equiv :: (HasCallStack) => TRSystem Expr -> Expr -> Expr -> Bool
equiv sys e1 e2 = normalForm sys e1 == normalForm sys e2

normalForm :: (HasCallStack) => TRSystem Expr -> Expr -> Expr
normalForm sys e = term $ fromMaybe (error $ "equiv: tarjan timed out (steps=" ++ show (tfNormSteps (ruleEnv sys)) ++ "): " ++ show e) $
                          norm sys $ start e

-- Normalize an expression.  Return Nothing if the normalization times out.
norm :: TRSystem Expr -> Traced Expr -> Maybe (Traced Expr)
norm sys tre = minimum . head <$> tarjan (tfNormSteps (ruleEnv sys)) tstep tre
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluenceRules sys) defaultTRSFlags t
    ]
