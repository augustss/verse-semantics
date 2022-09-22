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
  { fTrace       :: !Bool
  , fSplit       :: !Bool
  , fSimplify    :: !Bool
  , fUnderLambda :: !Bool
  , fRewrite     :: !Bool
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags
  { fTrace       = False
  , fSplit       = True
  , fSimplify    = False
  , fUnderLambda = False
  , fRewrite     = False
  }

run :: Flags -> Core -> Core
run f e | fRewrite f = one $ rewrite 1000 e'
        | otherwise = eval flg e'
  where flg = EFlags { underLambda = fUnderLambda f, traceEval = fTrace f }
        e' = (if fSimplify f then simpCore else id) . replacePrelude . (if fSimplify f then simpCore else id) $ e
        one [r] = r
        one [] = error "run: no results from rewrite"
        one _ = error "run: multiple results from rewrite"
