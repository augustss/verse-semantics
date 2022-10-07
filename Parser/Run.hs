module Run(
  run,
  Flags(..),
  defaultFlags,
  ) where
import Core(Core)
import CoreSimp(simpCore)
import Eval(eval, replacePrelude, EFlags(..))
import Flags
import TRSAdapter(rewrite)

run :: Flags -> Core -> Core
run f e | fRewrite f = one $ rewrite (fRewriteSteps f) e'
        | otherwise = eval flg e'
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f, steps = fEvalSteps f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
        one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one _ = error "run: multiple results from rewrite"
