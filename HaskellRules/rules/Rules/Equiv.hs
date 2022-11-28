module Rules.Equiv(equiv) where
import Rules.Core
import TRS.TRS( step, normalFormsFuelTrace )
import TRS.TRSGraph( normalFormsFuelTraceWithGraph )
import TRS.Tarjan
import TRS.Traced
import TRS.System

-- Test if two expressions are equivalent.
-- Reduce both and normalize.
equiv :: TRSystem Expr -> Expr -> Expr -> Bool
equiv sys e1 e2 = normalForm sys e1 == normalForm sys e2

normalForms :: TRSystem Expr -> Expr -> [Traced Expr]
normalForms sys
  | rulesHaveStructural sys = normalFormsFuelTraceWithGraph defaultTRSFlags 99 (rules sys)
  | otherwise               = normalFormsFuelTrace          defaultTRSFlags 99 (rules sys)

normalForm :: TRSystem Expr -> Expr -> Expr
normalForm sys e =
  case normalForms sys e of
    [] -> error "normalForms returned []"
    [x] -> term (norm sys x)
    _ -> error "normalForms returned many"

norm :: TRSystem Expr -> Traced Expr -> Traced Expr
norm sys = minimum . head . tarjan tstep
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluenceRules sys) defaultTRSFlags t
    ]
