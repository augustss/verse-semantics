module Main where

import Rules.Core
import Rules.Systems
import TRS.TRS( step, normalFormsFuelTrace, nub )
import TRS.TRSGraph( normalFormsFuelTraceWithGraph )
import TRS.Tarjan
import TRS.Traced
import TRS.System
import Test.QuickCheck
import Options.Applicative

--------------------------------------------------------------------------------

main :: IO ()
main = do
  flags <- testArgs
  let sys =
        case lookupSystem (rulesys flags) of
          Left msg -> error msg
          Right s -> s
      qcargs = stdArgs{ maxSuccess = numtests flags }
  putStrLn $ "Running " ++ show (numtests flags) ++ " tests of " ++ description sys
  quickCheckWith qcargs (prop_Confluence sys)

prop_Confluence :: TRSystem Expr -> Expr -> Property
prop_Confluence sys p =
  case nub . map (norm sys) . normalForms sys $ p of
    trs@(_:_:_) ->
      whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (ttr,i) <- trs `zip` [1::Int ..]
                  ]) False
    
    _ -> property True

---
  
normalForms :: TRSystem Expr -> Expr -> [Traced Expr]
normalForms sys
  | rulesHaveStructural sys = normalFormsFuelTraceWithGraph defaultTRSFlags 99 (rules sys)
  | otherwise               = normalFormsFuelTrace          defaultTRSFlags 99 (rules sys)

norm :: TRSystem Expr -> Traced Expr -> Traced Expr
norm sys = minimum . head . tarjan tstep
 where
  tstep (t :<-- tr) =
    [ t' :<-- ((n,t):tr)
    | (n, t') <- step (confluenceRules sys) defaultTRSFlags t
    ]

--------------------------------------------------------------------------------

data TestFlags = TestFlags
  { rulesys  :: !String
  , numtests :: !Int
  }

testFlags :: Parser TestFlags
testFlags = TestFlags
  <$> strOption
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> help "Use rule system NAME" )
  <*> option auto
         ( long "numtests"
        <> short 'n'
        <> metavar "N"
        <> value (maxSuccess stdArgs)
        <> help "Maximum of N successful tests" )

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
