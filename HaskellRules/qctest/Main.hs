module Main where
import Data.Maybe
import Data.List( nubBy )
import Data.Function( on )
import qualified Data.Set as S
import Epic.List( nub )
import Rules.Core
import Rules.Equiv(norm)
import Rules.Systems
import TRS.TRS( step )
import TRS.NormalForm( normalFormsFuelTrace, NormResult(..) )
--import TRS.Tarjan
import TRS.Traced
import Test.QuickCheck as QC
import Options.Applicative

--------------------------------------------------------------------------------

main :: IO ()
main = do
  flags <- testArgs
  let sys =
        case lookupSystem (rulesys flags) of
          Left msg -> error msg
          Right s -> s
      qcargs = stdArgs{ maxSuccess = numtests flags, replay = read <$> replayStr flags }
  putStrLn $ "Running " ++ show (numtests flags) ++ " tests of " ++ description sys
  quickCheckWith qcargs (prop_Confluence flags sys)

prop_Confluence :: TestFlags -> TRSystem Expr -> Property
prop_Confluence flags | koen flags = prop_Confluence2 flags
                      | otherwise  = prop_Confluence1 flags

prop_Confluence1 :: TestFlags -> TRSystem Expr -> Property
prop_Confluence1 flags sys =
  forAllShrink arbExpr shrinkExpr $ \p ->
  let p' = if wrapOne flags then One p else p in
  case normalFormsFuelTrace sys (maxSteps flags) p' of
    NormResult { nrDone = done, nrLeft = left } ->
      -- First, check if all the stuck terms actually have the same normal form
      case nub $ map (norm sys) done of
        [] ->                           -- no "stuck" results
          discard
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
 where
  arbExpr =
    do p <- arbitrary
       return (preProcess sys (ruleEnv sys) p)

  shrinkExpr p =
    [ p' | p' <- shrink p, validExpr sys (ruleEnv sys) p' ]

prop_Confluence2 :: TestFlags -> TRSystem Expr -> Property
prop_Confluence2 flags sys =
  forAllShrink arbExpr shrinkExpr $ \p0 ->
    let p = if wrapOne flags then One p0 else p0 in
      forAllBlind (arbTrace sys p) $ \m1 ->
        forAllBlind (arbTrace sys p) $ \m2 ->
          case (m1, m2) of
            (Just w1@(r1 :<-- t1), Just w2@(r2 :<-- t2)) ->
              whenFail (do putStrLn "==trace:1=="
                           putStr (unlines (showTrace w1))
                           print (step (rules sys) (ruleEnv sys) r1)
                           putStrLn "==trace:2=="
                           putStr (unlines (showTrace w2))
                           print (step (rules sys) (ruleEnv sys) r2)) $
                norm sys w1 == norm sys w2
            
            _ -> discard
 where
  arbExpr =
    do p <- arbitrary
       return (preProcess sys (ruleEnv sys) p)

  shrinkExpr p =
    [ p' | p' <- shrink p, validExpr sys (ruleEnv sys) p' ]

arbTrace :: TRSystem Expr -> Expr -> Gen (Maybe (Traced Expr))
arbTrace sys p = go (0 :: Int) [] p
 where
  go k _t _p | k > 100 = return Nothing
  go k t p' =
    case step (rules sys) (ruleEnv sys) p' of
      []  -> do return (Just (p' :<-- t))
      nqs -> do (n,q) <- elements nqs
                go (k+1) ((n,p'):t) q

--------------------------------------------------------------------------------

data TestFlags = TestFlags
  { rulesys        :: !String
  , numtests       :: !Int
  , wrapOne        :: !Bool
  , maxSteps       :: !Int
  , replayStr      :: !(Maybe String)
  , ignoreFuelStop :: !Bool
  , koen           :: !Bool
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
        <> value nDef
        <> help ("Maximum of NUM successful tests (default " ++ show nDef ++ ")") )
  <*> switch
         ( long "wrap-one"
        <> help "Wrap tested expression in one{}" )
  <*> option auto
         ( long "max-steps"
        <> short 'm'
        <> metavar "NUM"
        <> value mDef
        <> help ("Maximum number of rewrite steps (default " ++ show mDef ++ ")") )
  <*> optional (option str
         ( long "replay"
        <> metavar "REPLAY"
        <> help "Random replay setting") )
  <*> switch
         ( long "ignore-fuel-stop"
        <> short 'i'
        <> help "Do not discard out-of-fuel tests" )
  <*> switch
         ( long "koen"
        <> help "Use Koen's prop_Confluence2" )
 where nDef = maxSuccess stdArgs
       mDef = 1000

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
