module Main where
import Data.Maybe
import Epic.List( nub )
import Rules.Core
import Rules.Equiv(norm)
import Rules.Systems
--import TRS.TRS( step )
import TRS.NormalForm( normalFormsFuelTrace, NormResult(..) )
--import TRS.Tarjan
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
  case normalFormsFuelTrace sys (maxSteps flags) $ preProcess sys (ruleEnv sys) p' of
    NormResult { nrDone = done, nrLeft = left } ->
      -- First, check if all the stuck terms actually have the same normal form
      case nub $ map (norm sys) done of
        trs | any isNothing trs ->      -- normalization timed out
          discard
        trs@(_:_:_) ->                  -- multiple normal form
          whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (Just ttr,i) <- trs `zip` [1::Int ..]
                  ]) False
        _ | null left || ignoreFuelStop flags ->
            property True  -- no time-outs
          | otherwise ->
--            Debug.Trace.trace ("TO " ++ show p')
            discard                     -- reduction timed out

--------------------------------------------------------------------------------

data TestFlags = TestFlags
  { rulesys        :: !String
  , numtests       :: !Int
  , wrapOne        :: !Bool
  , maxSteps       :: !Int
  , ignoreFuelStop :: !Bool
  }

testFlags :: Parser TestFlags
testFlags = TestFlags
  <$> strOption
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> help "Use rule system NAME" )
  <*> option auto
         ( long "max-success"
        <> short 'n'
        <> metavar "NUM"
        <> value (maxSuccess stdArgs)
        <> help "Maximum of NUM successful tests" )
  <*> switch
         ( long "wrap-one"
        <> help "Wrap tested expression in one{}" )
  <*> option auto
         ( long "max-steps"
        <> short 'm'
        <> metavar "NUM"
        <> value 1000
        <> help "Maximum number of rewrite steps" )
  <*> switch
         ( long "ignore-fuel-stop"
        <> short 'i'
        <> help "Do not discard out-of-fuel tests" )

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
