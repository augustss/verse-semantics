module Flags(Flags(..), defaultFlags) where

data Flags = Flags
  { fTrace        :: !Bool
  , fSplit        :: !Bool
  , fSimplify     :: !Bool
  , fUnderLambda  :: !Bool
  , fRewrite      :: !Bool
  , fFresh        :: !Bool
  , fTimLambda    :: !Bool
  , fDenSem       :: !Bool
  , fDfs          :: !Bool
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
  , fFresh        = False
  , fTimLambda    = False
  , fDenSem       = False
  , fDfs          = False
  , fRewriteSteps = 25000
  , fEvalSteps    = 1000
  }
