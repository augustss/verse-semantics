module Run(
  run,
  Flags(..),
  defaultFlags,
  ) where
import Core(Core)
import CoreSimp(simpCore)
import Eval(eval, replacePrelude, EFlags(..))
import TRSAdapter(rewrite)

data Flags = Flags
  { fTrace        :: !Bool
  , fSplit        :: !Bool
  , fSimplify     :: !Bool
  , fUnderLambda  :: !Bool
  , fRewrite      :: !Bool
  , fRewriteSteps :: !Int
  , fEvalSteps    :: !Int
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags
  { fTrace        = False
  , fSplit        = True
  , fSimplify     = False
  , fUnderLambda  = False
  , fRewrite      = False
  , fRewriteSteps = 10000
  , fEvalSteps    = 1000
  }

run :: Flags -> Core -> Core
run f e | fRewrite f = one $ rewrite (fRewriteSteps f) e'
        | otherwise = eval flg e'
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f, steps = fEvalSteps f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
        one [r] = r
        one [] = error "run: rewrite ran out of fuel"
        one _ = error "run: multiple results from rewrite"
