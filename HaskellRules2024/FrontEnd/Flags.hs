module FrontEnd.Flags(
   Flags(..),
   Desugar(..),
   defaultFlags, showFlags
 ) where

import FrontEnd.Expr
import FrontEnd.Prelude( PreludeName, findPrelude, defaultPrelude )

-- Different desugaring styles.
-- The names refer to the figures in the desugaring paper.
data Desugar = DS12
  deriving (Show, Read)

data Flags = Flags
  { fTrace        :: !Bool
  , fSplit        :: !Bool
  , fSimplify     :: !Bool
  , fUnderLambda  :: !Bool
--  , fDenSem       :: !Bool
  , fDfs          :: !Bool
  , fLatex        :: !Bool
  , fPostProcess  :: !Bool
  , fRewriteSteps :: !Int
  , fEvalSteps    :: !Int
  , fNoFuelStop   :: !Bool
  , fNoLambdaIf   :: !Bool
  , fVerify       :: !Bool   -- desugar for verification
  , fAssumeVerified :: !Bool
  , fTraceDesugar :: !Bool
  , fTraceVerify  :: !Bool
  , fTraceEval    :: !Bool
  , fPrelude      :: !(PreludeName, SrcExpr)
  , fNoWarn       :: !Bool
  , fDesugar      :: !Desugar
  , fKeepIf       :: !Bool
  , fAllAsIter    :: !Bool
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags
  { fTrace        = False
  , fSplit        = False
  , fSimplify     = False
  , fUnderLambda  = True
--  , fDenSem       = False
  , fDfs          = False
  , fLatex        = False
  , fPostProcess  = True
  , fRewriteSteps = 25000
  , fEvalSteps    = 1000
  , fNoFuelStop   = False
  , fNoLambdaIf   = False
  , fVerify       = False
  , fAssumeVerified = False
  , fTraceDesugar = False
  , fTraceVerify  = False
  , fTraceEval    = True
  , fPrelude      = either error id $ findPrelude defaultPrelude
  , fNoWarn       = False
  , fDesugar      = DS12
  , fKeepIf       = False
  , fAllAsIter    = False
  }

showFlags :: Flags -> String
showFlags f = unwords
  [ if fDfs f then "one-path" else "many-paths"
  , if fSplit f then "split" else "no-split"
  , if fSimplify f then "simplify" else "no-simplify"
  , if fUnderLambda f then "under-lambda" else "no-under-lambda"
  , if fPostProcess f then "post-process" else ""
  , "max-steps=" ++ show (fRewriteSteps f)
  , "prelude=" ++ show (fst (fPrelude f))
  , "desugar=" ++ show (fDesugar f)
  ]
