module FrontEnd.Run(
  run, runM,
  Flags(..),
  defaultFlags,
  blockSystem,
  findSystem,
  everySystem,
  ) where
import Data.List
import Epic.Print
import FrontEnd.Desugar(simpCore)
import FrontEnd.EvalBlock(runBlock)
import FrontEnd.Expr(Core)
import FrontEnd.Flags
--import FrontEnd.RefImpl(evalRI)
import FrontEnd.TRSAdapter(rewrite)
import Rules.Systems(TRSystem(..), ESystem, lookupSystemEx, allSystems)
import Rules.Core(defaultTRSFlags)
--import DenSem.DenSem(denSem)
--import Debug.Trace

run :: Flags -> ESystem -> Core -> Core
run f s = one . runM f s
 where  one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one rs = error $ "run: multiple results from rewrite:\n" ++ intercalate "\n-----------------\n" (map prettyShow rs)

runM :: Flags -> ESystem -> Core -> [Core]
runM f s e = rewrite f s e'
  where 
        e' = if fSimplify f then simpCore e else e

--------------------

-- These two are nor proper rewrite systems.
-- They are just evaluators shoe-horned into the TRSystem
-- so it's easier to reuse that framework.
-- There are no rewrite rules, instead everything happens in the preprocessing stage.

{-
evalSystem :: ESystem
evalSystem = TRSystem { sname = "eval", description = "single path shortcut POPL rules",
  ruleEnv = defaultTRSFlags,
  preProcess = evaluate, postProcess = const id, rules = noRules, rules2 = noRules, rulesHaveStructural = False,
  confluenceRules = noRules, validExpr = \ _ _ -> True }
  where
    noRules _ _ = []
    evaluate tflg = coreToTrs . eval flg . trsToCore
      where flg = EFlags { underLambda = tfUnderLambda tflg, traceEval = tfTrace tflg, steps = tfRewriteSteps tflg }
-}

blockSystem :: ESystem
blockSystem = TRSystem { sname = "iblock", description = "left-to-right ICFP rules",
  ruleEnv = defaultTRSFlags,
  preProcess = evaluate, postProcess = const id, rules = noRules, rules2 = noRules, rulesHaveStructural = False,
  confluenceRules = noRules, validExpr = \ _ _ -> True }
  where
    noRules _ _ = []
    evaluate = runBlock

{-
refiSystem :: ESystem
refiSystem = TRSystem { sname = "refimpl", description = "Andy's reference implementation",
  ruleEnv = defaultTRSFlags,
  preProcess = eval', postProcess = const id, rules = noRules, rulesHaveStructural = False,
  confluenceRules = noRules, validExpr = \ _ _ -> True }
  where
    noRules _ _ = []
    eval' _ = coreToTrs . evalRI . trsToCore
-}

everySystem :: [ESystem]
everySystem = allSystems ++ [blockSystem] -- , refiSystem]

findSystem :: String -> Either String ESystem
findSystem = lookupSystemEx everySystem
