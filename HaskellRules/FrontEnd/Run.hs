module Parser.Run(
  run,
  Flags(..),
  defaultFlags,
  ) where
import Parser.Core(Core)
import Parser.CoreSimp(simpCore)
import Parser.Eval(eval, replacePrelude, EFlags(..))
import Parser.Flags
import Parser.TRSAdapter(rewrite)
import Parser.DenSem(denSem)

run :: Flags -> Core -> Core
run f e | fRewrite f = one $ rewrite f e'
        | fDenSem f = denSem e'
        | otherwise = eval flg e'
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f, steps = fEvalSteps f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
        one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one _ = error "run: multiple results from rewrite"
