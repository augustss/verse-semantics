module FrontEnd.Run(
  run, runM,
  Flags(..),
  defaultFlags,
  ) where
import FrontEnd.Core(Core)
import FrontEnd.CoreSimp(simpCore)
import FrontEnd.Eval(eval, replacePrelude, EFlags(..))
import FrontEnd.Flags
import FrontEnd.TRSAdapter(rewrite)
import Rules.Systems(ESystem)
--import DenSem.DenSem(denSem)

run :: Flags -> ESystem -> Core -> Core
run f s = one . runM f s
 where  one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one _ = error "run: multiple results from rewrite"

runM :: Flags -> ESystem -> Core -> [Core]
runM f s e | fRewrite f = rewrite f s e'
--x        | fDenSem f = undefined -- denSem e'
           | otherwise = [eval flg e']
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f, steps = fEvalSteps f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
