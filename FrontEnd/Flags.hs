module FrontEnd.Flags(
   Flags(..),
   ReportError(..),
   defaultFlags, showFlags,
 ) where

--import FrontEnd.Prelude( PreludeName, findPrelude, defaultPrelude )
import Core.Traced     ( Verbosity )

data Flags = Flags
  { fTrace          :: !Bool
  , fSplit          :: !Bool
  , fSimplify       :: !Bool
  , fUnderLambda    :: !Bool
  , fDfs            :: !Bool
  , fLatex          :: !Bool
  , fRewriteSteps   :: !Int
  , fNoFuelStop     :: !Bool
  , fNoLambdaIf     :: !Bool
  , fVerify         :: !Bool   -- Desugar for verification
  , fTraceDesugar   :: !Bool   -- Used for 'tester', not 'repl'
  , fTraceEval      :: !Bool
  , fTraceVerbosity :: !Verbosity
--  , fPrelude        :: !(PreludeName, SrcExpr)
  , fReportError    :: !ReportError
  , fKeepIf         :: !Bool
  , fAllAsIter      :: !Bool
  , fDsUniform      :: !Bool   -- Desugar Essential->Mini in a "uniform" way
                               -- i.e. without using "\bullet"
  , fUseLibParser   :: !Bool
  , fQuiet          :: !Bool   -- be less verbose
  }
  deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags
  { fTrace          = False
  , fSplit          = False
  , fSimplify       = False
  , fUnderLambda    = True
  , fDfs            = False
  , fLatex          = False
  , fRewriteSteps   = 10000
  , fNoFuelStop     = False
  , fNoLambdaIf     = False
  , fVerify         = False
  , fTraceDesugar   = True
  , fTraceEval      = True
  , fTraceVerbosity = 2
--  , fPrelude        = either error id $ findPrelude defaultPrelude
  , fReportError    = ErrWarning
  , fKeepIf         = False
  , fAllAsIter      = False
  , fDsUniform      = False
  , fUseLibParser   = False
  , fQuiet          = False
  }

data ReportError = ErrError | ErrWarning | ErrNone
  deriving (Eq, Show)

showFlags :: Flags -> String
showFlags f = unwords
  [ -- if fDfs f then "one-path" else "many-paths"
    -- if fSplit f then "split" else "no-split"
    -- if fSimplify f then "simplify" else "no-simplify"
    -- if fUnderLambda f then "under-lambda" else "no-under-lambda"
    "max-steps=" ++ show (fRewriteSteps f)
--  , "prelude=" ++ show (fst (fPrelude f))
  ]
