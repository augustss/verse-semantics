module Main where

import Rules.Core
import Rules.Systems
import TRS.TRS( step, nub )
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
  quickCheckWith qcargs (prop_Confluence (wrapOne flags) sys)

prop_Confluence :: Bool -> TRSystem Expr -> Expr -> Property
prop_Confluence wrap sys p =
  let p' = if wrap then One p else p in
  case nub . map (norm sys) . normalForms sys . preProcess sys $ p' of
    trs@(_:_:_) ->
      whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (ttr,i) <- trs `zip` [1::Int ..]
                  ]) False
    
    _ -> property True

---
  
normalForms :: TRSystem Expr -> Expr -> [Traced Expr]
normalForms sys = normalFormsFuelTrace sys 99

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
  <*> switch
      (  long "wrap-one"
      <> help "Wrap tested expression in one{}"
      )

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
