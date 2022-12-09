module FrontEnd.Flags(Flags(..), defaultFlags, showFlags) where

data Flags = Flags
  { fTrace        :: !Bool
  , fSplit        :: !Bool
  , fSimplify     :: !Bool
  , fUnderLambda  :: !Bool
  , fTimLambda    :: !Bool
--  , fDenSem       :: !Bool
  , fDfs          :: !Bool
  , fLatex        :: !Bool
  , fFinalInline  :: !Bool
  , fRewriteSteps :: !Int
  , fEvalSteps    :: !Int
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags
  { fTrace        = False
  , fSplit        = True
  , fSimplify     = False
  , fUnderLambda  = True
  , fTimLambda    = False
--  , fDenSem       = False
  , fDfs          = False
  , fLatex        = False
  , fFinalInline  = True
  , fRewriteSteps = 25000
  , fEvalSteps    = 1000
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
