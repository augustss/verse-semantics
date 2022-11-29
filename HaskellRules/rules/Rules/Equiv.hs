module Rules.Equiv(equiv) where
import Data.List(nub)
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
  case nub $ map (norm sys) $ normalForms sys e of
    [] -> error "normalForm: []"
    [x] -> term x
    xs -> error $ "normalForm: many\n" ++ show e ++ "\n" ++ show (map term xs)

norm :: TRSystem Expr -> Traced Expr -> Traced Expr
norm sys = minimum . head . tarjan tstep
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluenceRules sys) defaultTRSFlags t
    ]
