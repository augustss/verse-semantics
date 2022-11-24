module FrontEnd.Run(
  run,
  Flags(..),
  defaultFlags,
  ) where
import FrontEnd.Core(Core)
import FrontEnd.CoreSimp(simpCore)
import FrontEnd.Eval(eval, replacePrelude, EFlags(..))
import FrontEnd.Flags
import FrontEnd.TRSAdapter(rewrite)
--import DenSem.DenSem(denSem)

run :: Flags -> Core -> Core
run f e | fRewrite f = one $ rewrite f e'
        | fDenSem f = undefined -- denSem e'
        | otherwise = eval flg e'
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f, steps = fEvalSteps f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
        one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one _ = error "run: multiple results from rewrite"
