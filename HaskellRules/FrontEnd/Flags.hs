module FrontEnd.Flags(Flags(..), defaultFlags, showFlags) where

data Flags = Flags
  { fTrace        :: !Bool
  , fSplit        :: !Bool
  , fSimplify     :: !Bool
  , fUnderLambda  :: !Bool
--  , fDenSem       :: !Bool
  , fDfs          :: !Bool
  , fLatex        :: !Bool
  , fFinalInline  :: !Bool
  , fRewriteSteps :: !Int
  , fEvalSteps    :: !Int
  , fNoFuelStop   :: !Bool
  , fNoLambdaIf   :: !Bool
  , fVerify       :: !Bool
  , fTraceDesugar :: !Bool
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
  , fFinalInline  = True
  , fRewriteSteps = 25000
  , fEvalSteps    = 1000
  , fNoFuelStop   = False
  , fNoLambdaIf   = False
  , fVerify       = False
  , fTraceDesugar = False
  }

showFlags :: Flags -> String
showFlags f = unwords
  [ if fDfs f then "one-path" else "many-paths"
  , if fSplit f then "split" else "no-split"
  , if fSimplify f then "simplify" else "no-simplify"
  , if fUnderLambda f then "under-lambda" else "no-under-lambda"
  , if fFinalInline f then "final-inlines" else ""
  , "max-steps=" ++ show (fRewriteSteps f)
  ]
