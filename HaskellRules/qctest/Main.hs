{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
-- import Data.Maybe
-- import Control.Monad( guard )
import Epic.List( nub )
import Rules.Core
import Rules.Equiv(norm)
import Rules.Systems
import TRS.TRS( step )
import TRS.NormalForm( normalFormsFuelTrace, NormResult(..) )
import TRS.Tarjan
import TRS.Traced
--import TRS.Bind( Bind(..), ident )
import Test.QuickCheck as QC
import Options.Applicative
import qualified Data.Set as S
import System.Exit
import GitHash

gitHash :: String
gitHash = giHash gitInfo

gitDirty :: Bool
gitDirty = giDirty gitInfo

gitInfo :: GitInfo
gitInfo = $$tGitInfoCwd

--------------------------------------------------------------------------------

main :: IO ()
main = do
  flags <- testArgs
  let sys =
        case lookupSystem (rulesys flags) of
          Left msg -> error msg
          Right s -> s
      qcargs = stdArgs{ maxSuccess = numtests flags
                      , replay = read <$> replayStr flags
                      , maxShrinks = maxShrink flags }
  putStrLn $ "Running " ++ show (numtests flags) ++ " tests of " ++ description sys
  putStrLn $ "This source code has git hash " ++ gitHash ++ if gitDirty then " (with uncommited files)" else ""
  res <- quickCheckWithResult qcargs (prop_Confluence flags sys)
  case res of
    QC.Failure{usedSeed = seed, usedSize = size} ->
      putStrLn $ "To replay use --replay '" ++ show (seed, size) ++ "'"
    _ ->
      pure ()
  case res of
    QC.Success{} -> exitWith ExitSuccess
    _            -> exitWith (ExitFailure 1)

prop_Confluence :: TestFlags -> TRSystem Expr -> Property
prop_Confluence flags
  | loopy flags = prop_Confluence3 flags
  | koen  flags = prop_Confluence2 flags
  | otherwise   = prop_Confluence1 flags

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
        trs | any isTimeout trs ->      -- normalization timed out
          discard
        trs@(_:_:_)
          | ignoreRecursive flags && any (finish False (isRecursive . term)) trs ->
            discard
          | otherwise ->                  -- multiple normal form
          whenFail (sequence_
                  [ do putStrLn ("==trace:" ++ show i ++ "==")
                       putStr $ unlines $ showTrace ttr
                  | (Finish ttr,i) <- trs `zip` [1::Int ..]
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
    [ p''
    | p' <- shrink p ++ map snd (step (rules sys) (ruleEnv sys) p)
    , let p'' = postProcess sys (ruleEnv sys) p'
    , validExpr sys (ruleEnv sys) p''
    ]

prop_Confluence2 :: TestFlags -> TRSystem Expr -> Property
prop_Confluence2 flags sys =
  forAllShrink arbExpr shrinkExpr $ \p0 ->
    let p = if wrapOne flags then One p0 else p0 in
      forAllBlind (arbTrace flags sys p) $ \m1 ->
        forAllBlind (arbTrace flags sys p) $ \m2 ->
          case (m1, m2) of
            (Just w1, Just w2) ->
              whenFail (do putStrLn "==trace:1=="
                           putStr (unlines (showTrace w1))
                           putStrLn "==trace:2=="
                           putStr (unlines (showTrace w2))) $
                norm sys w1 == norm sys w2

            _ -> discard
 where
  arbExpr =
    do p <- arbitrary
       return (preProcess sys (ruleEnv sys) p)

  shrinkExpr p =
    [ p'
    | p' <- shrink p ++ map snd (step (rules sys) (ruleEnv sys) p)
    , validExpr sys (ruleEnv sys) p'
    ]

prop_Confluence3 :: TestFlags -> TRSystem Expr -> Property
prop_Confluence3 flags sys =
  forAllBlind (liftArbitrary arbPermutation) $ \permf ->
    forAllShrink arbExpr shrinkExpr $ \p0 ->
      let p = if wrapOne flags then One p0 else p0 in
        case (normTrace sys (const id) p, normTrace sys permf p) of
          (Just t1, Just t2) ->
            whenFail (do putStrLn "==trace:1=="
                         putStr (unlines (showTrace t1))
                         putStrLn "==trace:2=="
                         putStr (unlines (showTrace t2))) $
              norm sys t1 == norm sys t2

          _ -> discard
 where
  arbExpr =
    do p <- arbitrary
       return (preProcess sys (ruleEnv sys) p)

  shrinkExpr p =
    [ p'
    | p' <- shrink p ++ map snd (step (rules sys) (ruleEnv sys) p)
    , validExpr sys (ruleEnv sys) p'
    ]

  normTrace rsys permf p =
    if ignoreRecursive flags && isRecursive p then
      Nothing
    else
      case tarjan1 100 next (start p) of
        Timeout _ -> Nothing
        Finish ps -> let p' = minimum ps in
                     if (not (ignoreRecursive flags && isRecursive (term p')))
                       then Just p'
                       else Nothing

   where
    next (t :<-- tr) =
      [ q :<-- ((n,t):tr)
      | (n,q) <- permf t (step (rules rsys) (ruleEnv rsys) t)
      ]

arbPermutation :: Gen ([a] -> [a])
arbPermutation =
  do is <- infiniteListOf (choose (0,maxBound::Int))
     return (\xs -> perm is (length xs) xs)
 where
  perm _is     0 _xs = []
  perm ~(i:is) n  xs = (xs!!j) : perm is (n-1) (take j xs ++ drop (j+1) xs)
   where
    j = i `mod` n

arbTrace :: TestFlags -> TRSystem Expr -> Expr -> Gen (Maybe (Traced Expr))
arbTrace flags sys p = go (5 :: Int) (15 :: Int) [] p
 where
  go _k0 k1 _t p' | k1 <= 0 || (ignoreRecursive flags && isRecursive p') =
    return Nothing

  go k0 k1 t p' | k0 > 0 =
    frequency
    [ (1, go 0 k1 t p')
    , (4, case step (confluenceRules sys) (ruleEnv sys) p' of
            []  -> do go 0 k1 t p'
            nqs -> do (n,q) <- elements nqs
                      go (k0-1) k1 ((n,p'):t) q)
    ]

  go k0 k1 t p' =
    case step (rules sys) (ruleEnv sys) p' of
      []  -> do return (Just (p' :<-- t))
      nqs -> do (n,q) <- elements nqs
                go k0 (k1-1) ((n,p'):t) q

prop_Terminates :: TestFlags -> TRSystem Expr -> Property
prop_Terminates flags sys =
  forAllShrinkBlind arbExpr shrinkExpr $ \p ->
  let p' = if wrapOne flags then One p else p in
  case diverges (999::Int) S.empty [(299::Int,S.empty,p' :<-- [])] of
    Nothing ->
      property True

    Just trp ->
      whenFail (putStr (unlines (showTrace trp))) $
        False
 where
  arbExpr =
    do p <- arbitrary
       return (preProcess sys (ruleEnv sys) p)

  shrinkExpr p =
    [ p' | p' <- shrink p, validExpr sys (ruleEnv sys) p' ]

  diverges n _ ps | n <= 0 || null ps =
    Nothing

  diverges n seen ((fuel,pars,trp@(p :<-- tr)):ps)
    | fuel <= 0 || p `S.member` pars =
      Just trp

    | isRecursive p || p `S.member` seen =
      diverges n seen ps

    | otherwise =
      diverges (n-1) (S.insert p seen) $
        [ (fuel-1, S.insert p pars, q :<-- ((rule,p):tr))
        | (rule,q) <- step (rules sys) (ruleEnv sys) p
        ]
       ++ ps

  diverges _ _ _ = undefined

--------------------------------------------------------------------------------

data TestFlags = TestFlags
  { rulesys        :: !String
  , numtests       :: !Int
  , wrapOne        :: !Bool
  , maxSteps       :: !Int
  , replayStr      :: !(Maybe String)
  , ignoreFuelStop :: !Bool
  , koen           :: !Bool
  , loopy          :: !Bool
  , ignoreRecursive :: !Bool
  , maxShrink      :: !Int
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
  <*> switch
         ( long "loopy"
        <> help "Use Koen's new prop_Confluence3 (for loopy systems)" )
  <*> switch
         ( long "ignore-recursive"
        <> short 'r'
        <> help "Discard failures involving recursion" )
  <*> option auto
         ( long "max-shrinks"
        <> metavar "NUM"
        <> value mShrink
        <> help ("Maximum number of shrink steps (default " ++ show mShrink ++ ")") )
 where nDef = maxSuccess stdArgs
       mDef = 1000
       mShrink = 10000

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "QuickCheck Verse rules"
            <> header "qctest - QuickCheck testing of Verse rules"
             )
