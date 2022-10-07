module Flags(Flags(..), defaultFlags) where

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
