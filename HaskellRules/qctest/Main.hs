module Main where

import Epic.List( nub )
import Rules.Core
import Rules.Systems
import TRS.TRS( step )
import TRS.NormalForm( normalFormsFuelTrace )
import TRS.Tarjan
import TRS.Traced
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
  quickCheckWith qcargs (prop_Confluence flags sys)

prop_Confluence :: TestFlags -> TRSystem Expr -> Expr -> Property
prop_Confluence flags sys p =
  let p' = if wrapOne flags then One p else p in
  case nub . map (norm sys) . normalFormsFuelTrace sys (maxSteps flags) . preProcess sys $ p' of
    trs@(_:_:_) ->
      whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (ttr,i) <- trs `zip` [1::Int ..]
                  ]) False
    
    _ -> property True

---
  
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
  , wrapOne  :: !Bool
  , maxSteps :: !Int
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
        <> metavar "NUM"
        <> value (maxSuccess stdArgs)
        <> help "Maximum of NUM successful tests" )
  <*> switch
      (  long "wrap-one"
      <> help "Wrap tested expression in one{}"
      )
  <*> option auto
         ( long "max-steps"
        <> short 'm'
        <> metavar "NUM"
        <> value 100
        <> help "Maximum number of rewrite steps" )

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
