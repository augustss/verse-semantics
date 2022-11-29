module Rules.Equiv(equiv) where
import Rules.Core
import TRS.TRS( step )
import TRS.Tarjan
import TRS.Traced
import TRS.System

-- Test if two expressions are equivalent.
-- Reduce both and normalize.
equiv :: TRSystem Expr -> Expr -> Expr -> Bool
equiv sys e1 e2 = normalForm sys e1 == normalForm sys e2

normalForm :: TRSystem Expr -> Expr -> Expr
normalForm sys e = term $ norm sys $ start e

norm :: TRSystem Expr -> Traced Expr -> Traced Expr
norm sys = minimum . head . tarjan tstep
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluenceRules sys) defaultTRSFlags t
    ]
