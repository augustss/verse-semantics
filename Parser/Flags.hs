module Parser.Flags(Flags(..), defaultFlags, showFlags) where

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
  , fLatex        :: !Bool
  , fFinalInline  :: !Bool
  , fAlias        :: !Bool
  , fUnifyEq      :: !Bool
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
  , fFresh        = True
  , fTimLambda    = False
  , fDenSem       = False
  , fDfs          = False
  , fLatex        = False
  , fFinalInline  = True
  , fAlias        = False
  , fUnifyEq      = False
  , fRewriteSteps = 25000
  , fEvalSteps    = 1000
  }

showFlags :: Flags -> String
showFlags f = unwords
  [ if fDenSem f then "densem" else
      if fRewrite f then (if fFresh f then "PLDI" else "POPL") else "eval"
  , if fDfs f then "one-path" else "many-paths"
  , if fSplit f then "split" else "no-split"
  , if fSimplify f then "simplify" else "no-simplify"
  , if fAlias f then "elim-alias" else "no-elim-alias"
  , if fUnifyEq f then "unify-equal" else "no-unify-equal"
  ]
