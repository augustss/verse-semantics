{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" "Use camelCase" #-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main(main) where

import FrontEnd.CopyHook
import FrontEnd.Desugar as FE
import FrontEnd.ToCore  as FE ( convertToPrepdCore )
import FrontEnd.Flags   as FE
import FrontEnd.Expr    as Src
import FrontEnd.Prelude( findPrelude )

import Red as EV    -- "EV" for "Essential verse evaluator"

import qualified Parser.Verse               as V
import qualified Parser.Compat              as PC


import Core.Expr as Core
import Core.Traced
import Core.Verifier( verificationRules )
import Core.Rules ( runtimeRules )
import Core.Rule( everywhere, normalizeExpr )

{- Imports for denotational semantics

-- verse-densem
import SExp

-- plancc densem
import PlanCC(edenSem, edenSemDS)
import SExpC(srcExprToExp)

-- Tim densem
import qualified TimE   (den)
import qualified Pom    (den)
import qualified PomPom (denS, defaultConfig)
import qualified SemClass (den)

import Control.Monad( (>=>))
-}

import Epic.Print hiding ( (<>) )
import Epic.List( orElse )
import Data.Generics.Uniplate.Data( universeBi )

import Text.Megaparsec( unPos )
import Text.Megaparsec.Pos (SourcePos(..))

import GHC.Stack( HasCallStack )

import Data.List( isPrefixOf )
import Data.Maybe
import Control.Monad( unless, when, guard )
import System.Directory( doesFileExist, removeFile )
import System.Exit( exitWith, ExitCode(..) )
import Text.Printf
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Exception( catch, SomeException )
import qualified Options.Applicative as OA

import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString    as B
import qualified Data.ByteString.Char8 as BC


-----------------------------------------------
--
--    Main progam
--
-----------------------------------------------

main :: IO ()
main = do
  copyHook
  do { tflg <- testArgs
     ; runTests tflg }

runTests :: TestFlags -> IO ()
runTests test_flags
  -- Parsing only
  | parseOnly test_flags
  = mapM_ parseSourceOnly (fileNames test_flags)

  -- Run the tester on this input expression
  | Just expr_string <- to_bs <$> testExpr test_flags
  , let fn = "<command-line>"
  = runTestFile test_flags (fn, V.parseDie (pTestFile fn) fn expr_string)

  -- Display the test in Tim's syntax
  | timOutput test_flags
  = mapM_ read_and_display (fileNames test_flags)

  -- TODO: Jeff: Output the status of Tim's test?
  | timCSV test_flags
  = mapM_ read_and_csv (fileNames test_flags)

  -- Run a file-full of tests
  | otherwise
  = mapM_ read_and_run (fileNames test_flags)
  where
    read_and_run :: FilePath -> IO ()
    read_and_run fn
      = do { tests <- readTests fn
           ; runTestFile test_flags (fn, tests) }

    read_and_display :: FilePath -> IO ()
    read_and_display fn = do { tests <- readTests fn
                             ; displayTestFile test_flags (fn, tests) }

    read_and_csv :: FilePath -> IO ()
    read_and_csv fn = do { tests <- readTests fn
                         ; displayCSV test_flags (fn, tests) }

    to_bs :: String -> B.ByteString
    to_bs = BC.pack

-----------------------------------------------
--
--    Data types
--
--    Test, TestInfo, TestRes
--
-----------------------------------------------

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq TestInfo SrcExpr SrcExpr  -- testeq( name ) {code} { value }
  | TestVerify TestInfo SrcExpr          -- verify( name, pass/fail ){ code }
  | TestDenSem TestInfo SrcExpr SrcExpr  -- testds( name, densem-fn ) {code} {densem as str lit}
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
testInfo (TestVerify ti _)   = ti
testInfo (TestDenSem ti _ _) = ti

testSrc :: Test -> SrcExpr
testSrc (TestEvalEq _ e1 _) = e1
testSrc (TestVerify _ e)    = e
testSrc (TestDenSem _ e1 _) = e1

data TestInfo =  -- Per-test info e.g.  verify(pass, ICFPEverify=skip){ ...code... }
                 -- The stuff in the parens is the TestInfo
  TestInfo
    { testMName    :: !(Maybe String)
    , testLocStart :: !Loc
    , testLocEnd   :: !Loc
    , testType     :: !TestType          -- Default test type
    , testRunner   :: !(Maybe Evaluator) -- The function the test data is passed to,
                                         -- Nothing is evaluation/verification
    , testStatus   :: !TestStatus
    , testTimSkip  :: !TimSkip           -- Skip (or error code) when converting to Tim's format
    }
    deriving (Show)

data Evaluator
  = EvalCore           -- Desguar to ICFP Core and rewrites
  | EvalEssential      -- Rewrite Essential Verse directly
  | EvalDenSem DenSem  -- Use (this variant of) denotational semantics

data DenSem = Tim_DS | DLS_DS | SLS_DS | ELS_DS | POM_DS | PPM_DS | MON_DS   -- denotational semantic functions

instance Show Evaluator where
  show EvalCore        = "core"
  show EvalEssential   = "essential"
  show (EvalDenSem ds) = show ds

instance Show DenSem where
  -- INFO: Ideally these should correspond to their respective commands in the
  -- repl just without the ':' prefix, i.e., dls-densem here is :dls-densem in
  -- the repl. But this would require changing the frontend parser to handle the
  -- '-' which would no longer be valid verse. See Note [Testing densem in the
  -- tester]
  show Tim_DS = "tim"
  show DLS_DS = "dls"
  show SLS_DS = "sls"
  show ELS_DS = "els"
  show POM_DS = "pom"
  show PPM_DS = "ppo"
  show MON_DS = "mon"

instance Read Evaluator where
  readsPrec _ s =    [ (EvalDenSem ds, r) | (ds,r) <- reads s ]
                  ++ [ (EvalCore,      r) | ("core",r)      <- lex s ]
                  ++ [ (EvalEssential, r) | ("essential",r) <- lex s ]

instance Read DenSem where
  readsPrec _ s =    [(Tim_DS, r) | ("tim", r) <- lex s]
                  ++ [(DLS_DS, r) | ("dls", r) <- lex s]
                  ++ [(SLS_DS, r) | ("sls", r) <- lex s]
                  ++ [(ELS_DS, r) | ("els", r) <- lex s]
                  ++ [(POM_DS, r) | ("pom", r) <- lex s]
                  ++ [(PPM_DS, r) | ("ppo", r) <- lex s]
                  ++ [(MON_DS, r) | ("ppo", r) <- lex s]

data TestType =
  TestPass | TestFail | TestLoop                -- Expected behaviour
  deriving (Eq)

data TimSkip = TimNone | TimSkip String | TimError String
  deriving (Eq, Show)

instance Show TestType where
  show TestPass = "pass"
  show TestFail = "fail"
  show TestLoop = "loop"

data TestStatus = TS_Normal
                | TS_Broken   -- Test is currently broken (i.e., pass/fail is negated)
                | TS_Skip     -- Test should be skipped, probably because it somehow
                              --   crashes the entire implementation
                deriving( Show, Eq )

testName :: TestInfo -> String
testName ti = fromMaybe ("L" ++ show (unPos (sourceLine (testLocStart ti)))) (testMName ti)

data TestRes = TestRes { tr_info    :: TestInfo
                       , tr_outcome :: TestOutcome
                       , tr_details :: Doc }
  deriving (Show)

type NSteps = Int

data TestOutcome = TO_Equal    NSteps       -- Terminated, results equal
                 | TO_NotEqual NSteps       -- Terminated, results differ
                 | TO_Abnormal NormResult NSteps   -- Could not reach a normal form;
                                                   -- the NormResult is never NormOK
                 | TO_Excn
                 | TO_Skipped               -- We didn't run this test
                 deriving( Eq, Show )

isEqualTO :: TestOutcome -> Bool
isEqualTO (TO_Equal {}) = True
isEqualTO _             = False

isNotEqualTO :: TestOutcome -> Bool
isNotEqualTO (TO_NotEqual {}) = True
isNotEqualTO _                = False

isTimeoutTO ::  TestOutcome -> Bool
isTimeoutTO (TO_Abnormal NormExpired _) = True
isTimeoutTO _                           = False

skipTestRes :: TestRes -> Bool
skipTestRes (TestRes { tr_outcome = TO_Skipped }) = True
skipTestRes _                                     = False

expectedTestRes :: TestRes -> Maybe NSteps
-- Expected results, not skipped; account for broken-ness
expectedTestRes tr@(TestRes { tr_info = info })
  = case testStatus info of
      TS_Normal -> expectedOutcome tr
      TS_Broken -> unexpectedOutcome tr
      TS_Skip   -> Nothing

expectedOutcome :: TestRes -> Maybe NSteps
-- Expected results; ignore broken-ness
expectedOutcome (TestRes { tr_info = info, tr_outcome = outcome })
  = case (testType info, outcome) of
      (TestPass, TO_Equal ns)                -> Just ns
      (TestFail, TO_NotEqual ns)             -> Just ns
      (TestLoop, TO_Abnormal NormExpired ns) -> Just ns
      _ -> Nothing

unexpectedTestRes :: TestRes -> Maybe NSteps
unexpectedTestRes tr@(TestRes { tr_info = info })
  = case testStatus info of
      TS_Normal -> unexpectedOutcome tr
      TS_Broken -> expectedOutcome tr
      TS_Skip   -> Nothing

unexpectedOutcome :: TestRes -> Maybe NSteps
-- Unexpected results, not skipped, not exception, not invalid
unexpectedOutcome (TestRes { tr_info = info, tr_outcome = outcome })
 = case (testType info, outcome) of
      (TestPass, TO_NotEqual ns)                -> Just ns
      (TestPass, TO_Abnormal NormExpired ns)    -> Just ns
      (TestFail, TO_Equal ns)                   -> Just ns
      (TestFail, TO_Abnormal NormExpired ns)    -> Just ns
      (TestLoop, TO_Equal ns)                   -> Just ns
      (TestLoop, TO_NotEqual ns)                -> Just ns
      _ -> Nothing

outcomeIsInvalid :: TestRes -> Bool
outcomeIsInvalid (TestRes { tr_outcome = TO_Abnormal NormInvalid _ }) = True
outcomeIsInvalid _ = False

outcomeIsExcn :: TestRes -> Bool
outcomeIsExcn (TestRes { tr_outcome = TO_Excn }) = True
outcomeIsExcn _ = False

passedButShouldFail :: TestRes -> Bool
passedButShouldFail (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TestFail -> isEqualTO outcome
       _     -> False

failedButShouldPass :: TestRes -> Bool
failedButShouldPass (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TestPass  -> isNotEqualTO outcome
       _      -> False

failedWithLoop :: TestRes -> Bool
failedWithLoop (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TestLoop -> False
       _     -> isTimeoutTO outcome

isBrokenPass :: TestRes -> Bool
isBrokenPass tr@(TestRes { tr_info = info })
  = testStatus info == TS_Broken && isJust (expectedOutcome tr)

isBrokenFail :: TestRes -> Bool
isBrokenFail tr@(TestRes { tr_info = info })
   = testStatus info == TS_Broken && isJust (unexpectedOutcome tr)

-----------------------------------------------
--
--     Run tests
--     runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
--
-----------------------------------------------

runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
runTestFile tflg (fn, ts)
 = do { putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFEFlags tflg)

      ; when (logUnexpected tflg) $ clearUnexpectedFiles fn

      ; let tests_to_run :: [Test]
            tests_to_run = filter (keepOnly tflg) ts

      ; res :: [TestRes] <- mapM (runTest tflg) tests_to_run

      ; let n_tests      = length res
            n_skipped    = count skipTestRes          res
            expected     = filter (isJust . expectedTestRes)      res  -- Excludes skipped
            n_expected   = length expected
            unexpected   = filter (isJust . unexpectedTestRes)    res  -- Excludes skipped, invalid, exn
            n_unexpected = length unexpected
            n_invalid    = count outcomeIsInvalid res
            n_excn       = count outcomeIsExcn    res
      ; putStrLn ""
      ; putStrLn "------------ Overall summary ---------------------------"
      ; putStrLn $ "Number of tests: " ++ show n_tests
      ; printNZ n_excn     "%5d CRASHED: threw an exception"
      ; printNZ n_invalid  "%5d CRASHED: rewrite produced an invalid term"
      ; printNZ n_expected "%5d PASSED with expected results"
      ; printSome isBrokenFail expected "      including %d broken tests"
      ; when (n_unexpected > 0)  $
        do { putStrLn $ printf "%5d FAILED with unexpected results, of which" n_unexpected
           ; printSome failedButShouldPass unexpected "      %d should pass, but actually failed"
           ; printSome passedButShouldFail unexpected "      %d should fail, but actually passed"
           ; printSome failedWithLoop      unexpected "      %d had an unexpected timeout"
           ; printSome isBrokenPass        unexpected "      %d expected broken, but actually passed" }
      ; printNZ n_skipped  "%5d skipped"
      ; putStrLn "---------------------------------------------------------"
      ; unless (n_expected + n_skipped == n_tests) $ exitWith (ExitFailure 1) }
  where
    count :: (TestRes->Bool) -> [TestRes] -> Int
    count p rs = length (filter p rs)


keepOnly :: TestFlags -> Test -> Bool
-- With --only=tst, rum tests for which "tst" matches the test name
-- We support a postfix '*' for wildcard. Thus "foo*" matches anything
-- starting with "foo"
keepOnly tflg
  | Just only <- onlyTest tflg
  = case check_for_star only of
      Just str -> \t -> str  `isPrefixOf` testName (testInfo t)
      Nothing  -> \t -> only ==           testName (testInfo t)
  | otherwise
  = \_ -> True
  where
    check_for_star :: String -> Maybe String
    check_for_star s = case reverse s of
                         ('*':rs) -> Just (reverse rs)
                         _        -> Nothing

printNZ :: Int -> String -> IO ()
printNZ 0 _       = return ()
printNZ n fmt_str = putStrLn $ printf fmt_str n

printSome :: (TestRes -> Bool) -> [TestRes] -> String -> IO ()
printSome pick_me res fmt_str
  | null these = return ()
  | otherwise  = do { putStrLn $ printf fmt_str (length these)
                    ; putStrLn $ (render (text "          namely" <+>
                                          fsep (punctuate comma (map pp these)))) }
  where
    these :: [TestRes]
    these = filter pick_me res

    pp :: TestRes -> Doc
    pp (TestRes { tr_info = TestInfo { testMName = mname, testLocStart = loc } })
      | Just n <- mname = text n
      | otherwise       = char 'L' <> int (unPos (sourceLine loc))

widthTestName :: Int
widthTestName = 15

widthFileName :: Int
widthFileName = 25


-----------------------------------------------
--
--    Testing evaluation
--
-----------------------------------------------


type TimTag = Src.Ident

timTestInfo :: TimTag -> TestInfo
timTestInfo (Ident loc status) = TestInfo
  { testMName    = Nothing
  , testLocStart = loc
  , testLocEnd   = loc
  , testType     = timTestType status
  , testRunner   = Nothing
  , testStatus   = TS_Normal
  , testTimSkip  = timSkip status
  }

timTestType :: String -> TestType
-- Any TimTest starting in "S" should pass, e.g. S00, S01
-- All others should fail.
timTestType ('S' : _) = TestPass
timTestType _         = TestFail

timSkip :: String -> TimSkip
timSkip ('S' : _) = TimNone
timSkip s         = TimError s

----------------------------
runTest :: HasCallStack => TestFlags -> Test -> IO TestRes
runTest tflg test
  =  catch (run_test tflg test)
           (\e -> do { exn_handler e
                     ; pure (TestRes { tr_info = info, tr_outcome = TO_Excn
                                     , tr_details = empty })})
  where
    info        = testInfo test
    test_herald = testHerald test
    exn_handler :: SomeException -> IO ()
    exn_handler e
      = -- unless (noError tflg) $
        do { putStrLn $ test_herald ++ "Exception:" ++ show e
           ; putStrLn "" }

-- | `doTest` does the actual work of parsing, converting to core, and
-- evaluating/verifying; each of which can throw an exception.
run_test :: HasCallStack => TestFlags -> Test -> IO TestRes
run_test tflg test
  | skipThisTest tflg test
  = do { when (noisy tflg) (putStrLn $ testHerald test ++ "Skipped")
       ; pure (TestRes { tr_info = test_info, tr_outcome = TO_Skipped
                       , tr_details = empty }) }

  | otherwise
  = do { let evaluator = useEvaluator tflg
                         `orElse` testRunner test_info
                         `orElse` EvalEssential
       ; test_res <- case evaluator of
                        EvalCore      -> doEvalCoreTest      tflg test
                        EvalEssential -> doEvalEssentialTest tflg test
                        EvalDenSem ds -> doEvalDenSemTest    tflg ds test
       ; showTestResult tflg test test_res
       ; return test_res }
  where
    test_info = testInfo test

skipThisTest :: TestFlags -> Test -> Bool
skipThisTest tflags test
  | TS_Skip <- testStatus (testInfo test)
  = True   -- Test is marked 'skip' in the test file
  | assumeVerified tflags
  , TestVerify {} <- test
  = True   -- Skip verification tests when --assume-verified is set
  | otherwise
  = False

showTestResult :: TestFlags -> Test -> TestRes -> IO ()
showTestResult tflg test test_res
  | Just n_steps <- expectedTestRes test_res -- What to display if all is well
  = when (noisy tflg) $
    putStrLn $ test_herald ++ "Expected " ++ succ_what
                           ++ " in " ++ printf "%5d" n_steps ++ " steps"

  | TS_Broken <- status
  = putStrLn $ test_herald ++ "Broken test now pass"

  | TO_Abnormal NormInvalid _ <- outcome
  = putStrLn $ test_herald ++ "Crash: rewrite yields invalid results"

  | otherwise   -- TS_Normal
  = do { putStrLn $ test_herald ++ "Unexpected " ++ fail_what
       ; when (logUnexpected tflg) $ logUnexpectedToFile test_res
       ; unless (noError tflg || isTimeoutTO outcome) $
         -- Display more info unless
         --    (a) timeout, which often gives vast output
         --    (b) --no-error flag is set
         displayDoc (tr_details test_res) }

  where
    test_herald = testHerald test
    outcome     = tr_outcome test_res
    info        = testInfo test
    test_type   = testType info
    status      = testStatus info

    succ_what = case (status, test_type) of
             (TS_Broken,_) -> "broken "
             (_, TestPass)    -> "success"
             (_, TestFail)    -> "failure"
             (_, TestLoop)    -> "loop   "

    fail_what
      | failedWithLoop test_res = "timeout"
      | otherwise = case test_type of
                      TestPass -> "failure"
                      TestFail -> "success"
                      TestLoop -> "termination"

testExprs :: Test -> (SrcExpr, SrcExpr)
testExprs (TestVerify _ e)     = (e, Array [])
testExprs (TestDenSem _ e1 e2) = (e1,e2)
testExprs (TestEvalEq _ e1 e2) = (e1,e2)

----------------------------------------------------------------
--
--          Use the Core evaluator
--
----------------------------------------------------------------

doEvalCoreTest :: TestFlags -> Test -> IO TestRes
doEvalCoreTest tflg test
  = do { let (src1, src2) = testExprs test
             flags        = mkFEFlags tflg add_verif
             add_verif    = desugarForVerification test

       ; core1 <- srcToCore flags add_verif src1

       -- mb_v2 is Nothing if e2 is "wrong", which
       -- tells us that we expect e1 to get stuck
       ; mb_v2 <- case src2 of
                     Variable (Ident _ "wrong") -> pure Nothing
                     _ -> do { core2 <- srcToCore flags False src2
                             ; let (_,tr2) = evalCoreExpr tflg test core2
                             ; return (Just (Core.Traced.getTerm tr2)) }

       ; let (res1,tr1) = evalCoreExpr tflg test core1
             v1          = Core.Traced.getTerm tr1
             n_steps     = traceLength tr1
             test_passed = equivValue v1 mb_v2

             outcome :: TestOutcome
             outcome = case res1 of
                NormOK | test_passed -> TO_Equal n_steps
                       | otherwise   -> TO_NotEqual n_steps
                _                    -> TO_Abnormal res1 n_steps

             details :: Doc
             -- Show this if the the outcome is unexpected
             details = text "-----------------------------------------------"
                       $$ nest 2 (vcat
                          [ text "Expression" <+> pPrint src1
                          , text "evaluates to" <+>  pPrint v1
                          , text "while" <+> pPrint src2
                          , text "evaluates to" <+> pPrint mb_v2  ])

       -- Display the trace if asked for, regardless of success/failure
       ; when (showTrace tflg) $
         do { putStrLn "Trace is:"; displayTraceV (traceVerbosity tflg) tr1 }

       ; return (TestRes { tr_info = testInfo test
                         , tr_outcome = outcome
                         , tr_details = details }) }

-- | Equivalence on values (or stuck expressions)
-- e2=Nothing <=> e2=WRONG <=> e1 gets stuck without reaching a value
equivValue :: Core.Expr -> Maybe Core.Expr -> Bool
equivValue e1 (Just e2) = Core.norm e1 == Core.norm e2
equivValue e1 Nothing   = not (Core.isVal e1)


srcToCore :: HasCallStack => FE.Flags -> Bool -> SrcExpr -> IO Core.Expr
srcToCore flags add_verification e
  = do { e1 :: SrcCore <- FE.desugar flags add_verification e
       ; FE.convertToPrepdCore flags e1 }

mkFEFlags :: TestFlags -> Bool -> FE.Flags
mkFEFlags tflg add_verification
  = setPreludeFlag add_verification tflg $
    testFlagsToFEFlags tflg


desugarForVerification :: Test -> Bool
desugarForVerification TestEvalEq{}   = False
desugarForVerification TestDenSem{}   = False
desugarForVerification TestVerify{}   = True

evalCoreExpr :: TestFlags -> Test -> Core.Expr -> (NormResult, Traced Core.Expr)
evalCoreExpr flags test e = normalizeExpr rules (maxSteps flags) e
  where
    rules = case test of
        TestEvalEq {} -> everywhere runtimeRules
        TestVerify {} -> everywhere verificationRules
        TestDenSem {} -> error "evalCoreExpr: found a densem test...impossibly"



----------------------------------------------------------------
--
--          Use the Essential Verse evaluator
--
----------------------------------------------------------------

doEvalEssentialTest :: TestFlags -> Test -> IO TestRes
-- doEvalEssentialTest = error "doEvalEssentialTest"

doEvalEssentialTest tflags test
  = do { (cxt, blk, should_be_stuck) <- mkBlkToTest tflags test
       ; let tr1                 = runEssentialEval tflags cxt blk
             (res1, n_steps, v1) = traceSummary tr1
             reached_val = case v1 of
                             EV.Blk _is _hp (EV.Val {}) -> True
                             _                          -> False

             outcome :: TestOutcome
             outcome = case res1 of
                         NormOK | should_be_stuck == not reached_val
                                -> TO_Equal n_steps
                                | otherwise
                                -> TO_NotEqual n_steps
                         _      -> TO_Abnormal res1 n_steps

             details :: Doc
             -- Show this if the the outcome is unexpected
             details = text "-----------------------------------------------"
                       $$ nest 2 (vcat
                          [ text "Expression" <+> pPrint blk
                          , text "evaluates to" <+>  pPrint v1  ])

       -- Display the trace if asked for, regardless of success/failure
       ; when (showTrace tflags) $
         do { putStrLn "Trace is:"; displayTraceV (traceVerbosity tflags) tr1 }

       ; return (TestRes { tr_info = testInfo test
                         , tr_outcome = outcome
                         , tr_details = details }) }

runEssentialEval :: TestFlags -> ReductionContext -> Blk -> Traced Blk
runEssentialEval tflags cxt blk
  | matchFirst tflags = tr1 `appendTrace` tr2
  | otherwise         = EV.runTraced max_steps cxt blk
  where
    max_steps = maxSteps tflags
    tr1  = EV.runTraced max_steps (setJustMatching cxt) blk
    tr2  = EV.runTraced max_steps cxt (getTerm tr1)

mkBlkToTest :: TestFlags -> Test -> IO (EV.ReductionContext, EV.Blk, Bool)
mkBlkToTest tflags (TestEvalEq _ src1 src2)
  | Variable (Ident _ "wrong") <- src2
  = -- testeq(...){e1}{wrong} is a special form that means
    --       see if e1 gets stuck
    -- It's quite different to, say, testeq(...){e1}{3}
    do { term <- mk_term tflags src1
       ; let top_cxt = mkTopCxt term

             mc | assumeVerified tflags = mcAssumeVerified topMatchContext
                | otherwise             = topMatchContext

             main_exp = -- EV.Verify S.empty [] $ mkBlkE $
                        EV.mkCheck Core.Succeeds  $
                        mkBlkE $ EV.matchTop top_cxt mc term

             main_blk = EV.mkBlkE main_exp

       ; return (top_cxt, main_blk, True) }

  | otherwise
  = do { term1 <- mk_term tflags src1
       ; term2 <- mk_term tflags src2
       ; let top_cxt = mkTopCxt (term1 :>% term2)

             mc | assumeVerified tflags = mcAssumeVerified topMatchContext
                | otherwise             = topMatchContext

             (r1,r2) = EV.freshIds2 top_cxt "r"
             rs :: S.Set EV.Ident = S.fromList [r1,r2]

             main_exp = EV.Var r1 EV.:=: mk_all top_cxt mc term1 EV.:>
                        EV.Var r2 EV.:=: mk_all top_cxt mc term2 EV.:>
                        -- EV.Verify S.empty [] $ mkBlkE $
                        EV.mkCheck Core.Succeeds  $
                        mkBlkE $ EV.Var r1 EV.:=: EV.Var r2

             main_blk = EV.Blk rs EV.emptyHeap main_exp

       ; return (top_cxt, main_blk, False) }

mkBlkToTest tflags (TestVerify _ src)
  = do { term <- mk_term tflags src
       ; let top_cxt = mkTopCxt term

             -- check<succeeds>{ exists u. u ~> tm }
             check_exp = EV.mkCheck Core.Succeeds $ EV.mkBlkE $
                         EV.matchTop top_cxt topMatchContext term

             main_blk = EV.Blk S.empty EV.emptyHeap $
                        check_exp :> EV.Arr []

       ; return (top_cxt, main_blk, False) }


mkBlkToTest _tflags (TestDenSem {}) = error "TestDenSem"

mkTopCxt :: Term -> ReductionContext
mkTopCxt term
  = EV.RC { rc_depth  = 0
          , rc_eqns   = EV.thePrelude
          , rc_exis   = prel_bndrs
          , rc_skols  = top_skols
          , rc_vcxt   = NotVerifying
          , rc_mode   = mode }
  where
    top_skols = freeVarsTerm term `S.difference` prel_bndrs

    prel_bndrs :: S.Set EV.Ident
    prel_bndrs = S.fromList (map fst EV.thePrelude)

    mode = RM { rm_just_matching = False }

mk_term :: TestFlags -> SrcExpr -> IO EV.Term
mk_term tflags src = do { ess <- srcToEssential fe_flags src
                        ; return (TBlock $ EV.srcToTerm ess) }
  where
    fe_flags = testFlagsToFEFlags tflags

mk_all :: ReductionContext -> MatchContext -> EV.Term -> EV.Exp
-- Returns all{exists u.  u ~~> block{t} }
mk_all cxt mc t = EV.mkAll $ EV.mkBlkE $
                  EV.matchTop cxt mc $ EV.TBlock t

----------------------------------------------------------------
--
--          Use the denotational semantics
--
----------------------------------------------------------------

{- Note [Testing densem in the tester]
~~~~~~~~~~~~~~~~~~~~~~~~~~
  * To test the semantic functions for different denotational semantics we abuse
    'TestType' in 'TestInfo' to track which semantic function to call. This is
    purely a descision to enable the tester to use the semantic functions as
    fast as possible. In general, the tester should know less about the tests,
    see #77.

  * We try to keep the data pipeline for the tester as similar as possible when
    testing densem or normal tests. Thus we define a parser ('pTestDenSem') to
    parse a "testds" call in a .versetest file. This parser decides the correct
    semantic function to run in 'pTestRunner' which propogates this information
    to the 'testRunner' field of 'pTestInfo'. Each semantic function is built
    into the tester and is one of this set: { "tim", "dls", "sls", "els" }. We
    have precisely chosen this format for these symbols so that the
    'foo.versetest' files are valid verse.

  * A 'TestDenSem info e1 e2' is morally the same as a 'TestEvalEq info e1 e2',
    only instead of evaluating e1, then e2 and checking for equivalence with
    'equivValue', 'TestDenSem' Expects that 'e2' /is only/ a literal string that
    represents the result of the semantic function applied to 'e1'.

    For example:

    -- in tests.versetest
    testds("DS1", tim){ (1,2) }     { "[{{r=<1,2>}}]" }

    becomes 'TestDenSem tinfo e1 e2' where:
      tinfo = TestInfo {..., testType = Tim_DS }
      e1    = (1,2)
      e2    = "[{{r=<1,2>}}]"

  * We diverge from the normal data pipeline in 'checkResults' by calling
    'evalDenSem' and then construct the output exactly like the normal
    pipeline. 'evalDenSem' is responsible for dispatching the TestRunner to the
    semantic functions for each denotational semantics.
-}


doEvalDenSemTest :: TestFlags -> DenSem -> Test -> IO TestRes
doEvalDenSemTest _tflg _test
  = error "doEvalDenSemTest"

{-
-- Eval using a semantic function.
evalDenSem :: TestFlags -> Test -> SrcExpr -> IO (NormResult, Int, Expr)
evalDenSem flags test e = do
  res <- LitStr <$> f e
  return (NormOK, 0, res)
  where
    runner = forceSem flags <|> dfltRunner
    dfltRunner = testRunner $ testInfo test
    flgs = FE.defaultFlags{ fReportError = ErrNone }
    err  = error "runD: exception in evalDenSem"
    go   = runD flgs err . getEssential flgs

    -- the moral equivalent of 'getEssential' in 'repl/Main.hs'
    -- getEssential :: Flags -> SrcExpr -> DsM SrcEssential
    getEssential _ = addPrelude >=> sDesugarExpr

    -- because each semantic function returns a different type we convert to
    -- show to normalize the result
    f :: SrcExpr -> IO String
    f = case runner of
          Nothing     -> error $ "evalExpr: Expected densem type, got Nothing with test: " ++ show test
          Just Tim_DS -> fmap (showASCII . TimE.den . envDesugar) . go
          Just POM_DS -> fmap (showASCII . Pom.den . envDesugar) . go
          Just PPM_DS -> go >=> (fmap (showASCII . fst)
                               . PomPom.denS PomPom.defaultConfig False . envDesugar)
          Just DLS_DS -> go >=> fmap showASCII . edenSem . edenSemDS . srcExprToExp
          Just SLS_DS -> error "SLS densem not implemented yet. Sorry!"
          Just ELS_DS -> go >=> denSemDesugar >=> fmap showASCII . denSem
          Just MON_DS -> fmap (showASCII . SemClass.den . envDesugar) . go

-- Hackily replace some Unicode characters
showASCII :: Show a => a -> String
showASCII = concatMap ascii . show
  where ascii '\8746' = "U"
        ascii '\8800' = "/="
        ascii '\8709' = "EMPTY"
        ascii '\10629' = "{{"
        ascii '\10630' = "}}"
        ascii '\12296' = "<"
        ascii '\12297' = ">"
        ascii '\8801' = "="
        ascii '\n' = ""
        ascii c = [c]
-}


{-
-- | `checkResults` just compares the results of two evaluations and prints out
--   the appropriate message, it does _not_ throw or catch any exceptions.
checkResults :: TestFlags -> Test -> (SrcExpr, Expr) -> (SrcExpr, Maybe Expr) -> IO TestRes
checkResults tflg test@TestDenSem{} (src1, _core1) (src2, mb_v2)
    = do { -- See Note [Testing densem in the tester]
         ; (res1, n_steps, v1) <- evalDenSem tflg test src1
         ; let info = testInfo test
               test_passed  = equivValue v1 mb_v2
               test_herald  = testHerald test
               test_res     = (TestRes { tr_info = info, tr_outcome = outcome })
               status       = testStatus info
               typ          = testType info

               outcome :: TestOutcome
               outcome = case res1 of
                  NormOK | test_passed -> TO_Equal
                         | otherwise   -> TO_NotEqual
                  _                    -> TO_Abnormal res1

               succ_what = case (status, typ) of
                        (TS_Broken,_) -> "broken "
                        (_, TestPass)    -> "success"
                        (_, TestFail)    -> "failure"
                        (_, TestLoop)    -> "loop   "

               fail_what
                 | failedWithLoop test_res = "timeout"
                 | otherwise = case typ of
                                 TestPass -> "failure"
                                 TestFail -> "success"
                                 TestLoop -> "termination"

         ; show_result tflg status outcome test_herald test_res succ_what
           fail_what src1 v1 src2 mb_v2 n_steps
         ; -- Display the trace if asked for, regardless of success/failure
         ; when (showTrace tflg) $ putStrLn "Test is a den-sem test, trace not implemented"
         ; pure test_res }
-}


testHerald :: Test -> String
-- Prints fixed-width herald string
testHerald test = printf "%-*s %-*s" widthTestName test_nm widthFileName loc_str
  where
    test_nm   = fromMaybe "<anon>" (testMName ti)
    loc_str   = filename ++ ":" ++ show (unPos (sourceLine loc))
    loc ::Loc = testLocStart ti
    ti        = testInfo test
    filename  = baseName (sourceName loc)

baseName :: FilePath -> FilePath
baseName = reverse . takeWhile (/= '/') . reverse

noisy :: TestFlags -> Bool
noisy = not . quiet

-----------------------------------------------
--
--     Parse a file of Verse source code
--     parseSourceOnly :: FilePath -> IO ()
--
-----------------------------------------------

parseSourceOnly :: FilePath -> IO ()
parseSourceOnly fn = do
  _ <- V.parseDie V.pFile fn <$> B.readFile fn
  -- if we get here we have succeeded
  putStrLn $ "parsed " ++ fn
  putStrLn "SUCCESS"
  pure ()


--------------------------------------------------------------------------------
--
--     Read the test file, and parse with the verse-parser library many
--     functions in this section are duplicates until verse-parser is at feature
--     parity
--
--------------------------------------------------------------------------------

-- Read the test file, and parse it
readTests :: FilePath -> IO [Test]
readTests fn = do
  tests <- V.parseDie (pTestFile fn) fn <$> B.readFile fn
  skips <- parseSkipped (fn ++ ".skip")
  pure $ tests `skipping` skips

-- Parse a file of tests
pTestFile :: FilePath -> V.Parser [Test]
pTestFile file = V.skip *> V.many (pTest file) <* V.eof

-- Parse a test
pTest :: FilePath -> V.Parser Test
pTest file = V.skip *> (pTestEq file
                         OA.<|> pTestVerify file
                         OA.<|> pTimTest
                         OA.<|> pTestDenSem file) <* V.skip

-- Parse an expression evaluation equality test
pTestEq :: FilePath -> V.Parser Test
pTestEq file =
  V.lexeme (V.pKeyword "testeq") *> do
    let pdExpr = PC.toSrcExpr <$> V.pcExpr
    tId <- V.lexeme $ V.pParens (pTestInfo file)
    TestEvalEq tId <$> (V.lexeme $ V.pcBraces pdExpr) <*> V.lexeme (V.pBraces pdExpr)

-- Parse an expression verification test
pTestVerify :: FilePath -> V.Parser Test
pTestVerify file =
  V.pKeyword "verify" *> do
    tId <- V.lexeme $ V.pParens (pTestInfo file)
    src <- V.lexeme $ V.pcBraces (fmap PC.toSrcExpr V.pcExpr <* V.optionMaybe V.pSemi)
    locEnd <- V.getLoc
    pure $ TestVerify (tId { testLocEnd        = PC.locToSrcLoc file locEnd
                           }) src

pTimTest :: V.Parser Test
pTimTest =
  V.pKeyword "test" *> do
    tag <- V.pParens V.pIdent
    src <- V.pLBrace *> V.pExpr <* V.optionMaybe V.pSemi <* V.pRBrace
    let ti = timTestInfo $ PC.mkSrcIdent tag
    pure $ TestVerify ti $ PC.toSrcExpr src

pTestDenSem :: FilePath -> V.Parser Test
pTestDenSem file =
    V.pKeyword "testds" *> do
    tId <- V.lexeme $ V.pParens (pTestInfo file)
    let pAnyString = (Src.Lit . LStr) <$> do
          _ <- V.match '"'
          V.manyTill (V.anySingleBut '"') (V.match '"')
        ds_expr    = V.lexeme $ V.pcBraces (PC.toSrcExpr <$> V.pcExpr)
    TestDenSem tId <$> ds_expr <*> V.pBraces pAnyString

pTestInfo :: FilePath -> V.Parser TestInfo
pTestInfo file = do
  locB  <- V.getLoc
  let pComma = V.lexeme V.pComma
  mname <- V.optionMaybe (V.pStringLit <* V.lexeme V.pComma)
  rnner <- V.try $ V.lexeme $ (V.optionMaybe pTestRunner)
  typ   <- pTestType <* V.optional pComma
  stat  <- V.try (pTestStatus <* V.optional pComma) OA.<|> pure TS_Normal
  tim   <- V.try (pTimSkip <* V.optional pComma) OA.<|> pure TimNone
  locE  <- V.getLoc
  pure (TestInfo { testMName    = fmap (T.unpack . unLoc) mname
                 , testLocStart = PC.locToSrcLoc file locB
                 , testLocEnd   = PC.locToSrcLoc file locE
                 , testType     = typ
                 , testRunner   = rnner
                 , testStatus   = stat
                 , testTimSkip  = tim
                 })

pTestType :: V.Parser TestType
pTestType = V.lexeme $ do
  i <- V.many V.pAlpha -- cannot use V.pIdent, "fail" is a reserved
  case T.toLower $ TE.decodeLatin1 $ B.pack $ i of
    "pass"    -> pure TestPass
    "fail"    -> pure TestFail
    "loop"    -> pure TestLoop
    _         -> fail "pTestType"

pTestStatus :: V.Parser TestStatus
pTestStatus = V.lexeme $ do
  i <- V.pIdent
  case unLoc $ fmap T.toLower i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pTestStatus"

pTimSkip :: V.Parser TimSkip
pTimSkip = do
  i <- V.pIdent
  guard (unLoc (fmap T.toLower i) == "tim")
  _  <- V.pEqual
  sk <- V.pIdent
  case T.unpack $ unLoc sk of
    's':'k':'i':'p':s -> pure (TimSkip s)
    s                 -> pure (TimError s)
  OA.<|> pure TimNone

pTestRunner :: V.Parser Evaluator
pTestRunner = V.choice
    [ V.pKeyword "tim" *> pure (EvalDenSem Tim_DS)
    , V.pKeyword "sls" *> pure (EvalDenSem SLS_DS)
    , V.pKeyword "dls" *> pure (EvalDenSem DLS_DS)
    , V.pKeyword "els" *> pure (EvalDenSem ELS_DS)
    , V.pKeyword "pom" *> pure (EvalDenSem POM_DS)
    , V.pKeyword "ppo" *> pure (EvalDenSem PPM_DS)
    , V.pKeyword "mon" *> pure (EvalDenSem MON_DS)
    , V.pKeyword "core"     *> pure EvalCore
    , V.pKeyword "essental" *> pure EvalEssential
    ] <* V.pComma


-----------------------------------------------
--
--    TestFlags, and command-line argument parsing
--        testArgs :: IO TestFlags
--
-----------------------------------------------

data TestFlags = TestFlags
  { dfs            :: !Bool                -- just find one normal form
  , split          :: !Bool                -- use split
  , parseOnly      :: !Bool                -- parse only
  , simplify       :: !Bool                -- use simplifier
  , noUnderLam     :: !Bool                -- do not reduce under lambda
  , quiet          :: !Bool                -- Less noisy
  , verbose        :: !Bool                -- More noisy
  , noError        :: !Bool                -- Don't show error message
  , postProc       :: !Bool                -- Post processing
  , summary        :: !Bool                -- Produce a summary
  , showTrace      :: !Bool                -- Show traces
  , traceVerbosity :: !Verbosity           -- Level of verbosity for traces
  , logUnexpected  :: !Bool                -- Log unexpected results
  , onlyTest       :: !(Maybe String)      -- run only this test
  , testExpr       :: !(Maybe String)      -- use this expression as a test, for example:
  , maxSteps       :: !Int                 -- max number of rewrite steps
  , maxNormSteps   :: !Int                 -- max number of normalization steps
  , ignoreFuelStop :: !Bool                -- ignore running out of fuel
  , assumeVerified :: !Bool                -- turn succeeds into a no-op
  , timRun         :: !Bool                -- run Tim's verifier tests
  , timVerify      :: !Bool                -- verify Tim's verifier tests
  , timOutput      :: !Bool                -- just display the test in Tim's syntax
  , timCSV         :: !Bool                -- output status of Tim tests
  , showDesugared  :: !Bool                -- show desugared version just before evaluation
  , preludeEval    :: !String              -- use this prelude in TestEval
  , preludeVerify  :: !String              -- use this prelude in TestVerify
  , allAsIter      :: !Bool                -- encode all as iter
  , dsUniform      :: !Bool
  , fileNames      :: ![FilePath]          -- input files
  , useEvaluator   :: !(Maybe Evaluator)
  , matchFirst     :: !Bool                -- Run matching code first
  }
  deriving (Show)

testArgs :: IO TestFlags
-- Parse the TestFlags from the command line
testArgs = do
  let prf = OA.prefs OA.disambiguate
  t <- OA.customExecParser prf $ OA.info (testFlags OA.<**> OA.helper)
             ( OA.fullDesc
            <> OA.progDesc "Test Verse rules"
            <> OA.header "tests - testing Verse rules"
             )
  let t' = case fileNames t of
             [] -> t{ fileNames = [if parseOnly t then test1 else verseTest] }
             _  -> t
  pure t'

verseTest :: FilePath
verseTest = "tests.versetest"

test1 :: FilePath
test1     = "test1.verse"

testFlags :: OA.Parser TestFlags
testFlags
  = -- Uses ApplicativeDo, because OA.Parser isn't a monad
    -- Order in this do-block is not important
    --
    -- NB: disambiguation is /on/, so you can't have both
    --        --foo   and    --foo-wombat
    -- Because then --foo is ambiguous
    do { dfs <- OA.switch $
                OA.long "dfs" <>
                OA.help "Only find one normal form"

       ; split <- OA.switch $
                  OA.long "split" <>
                  OA.help "Use split"

       ; parseOnly <- OA.switch $
                      OA.long "parse-only" <>
                      OA.help "Just do parsing"

       ; simplify <- OA.switch $
                     OA.long "simplify" <>
                     OA.help "Use simplifier"

       ; noUnderLam <- OA.switch $
                        OA.long "no-under-lambda" <>
                        OA.help "do not reduce under lambda"

       ; quiet <- OA.switch $
                  OA.long "quiet" <>
                  OA.help "Be less noisy"

       ; verbose <- OA.switch $
                    OA.long "verbose" <>
                    OA.help "Be more noisy"

       ; noError <- OA.switch $
                    OA.long "no-error" <>
                    OA.help "Do not show error message on failure"

       ; postProc <- OA.switch $
                     OA.long "post-process" <>
                     OA.help "Do post processing"

       ; summary <- OA.switch $
                    OA.long "summary" <>
                    OA.help "Produce test summary"

       ; showTrace <- OA.switch $
                      OA.long "trace" <>
                      OA.help "Print rewrite traces"

       ; traceVerbosity <- OA.option OA.auto $
                           OA.long "tr-verbosity" <>
                           OA.metavar "NUM" <>
                           OA.value 3 <>
                           OA.help "Verbosity of rewrite trace (0,1,2,3)"

       ; logUnexpected <- OA.switch $
                          OA.long "log-unexpected" <>
                          OA.help "log unexpected results to file"

       ; onlyTest <- OA.optional $ OA.strOption $
                     OA.long "only" <>
                     OA.metavar "TEST" <>
                     OA.help "Run only test named TEST"

       ; testExpr <- OA.optional $ OA.strOption $
                     OA.long "expr" <>
                     OA.metavar "EXPR" <>
                     OA.help "Use EXPR as a test"

       ; maxSteps <- OA.option OA.auto $
                     OA.long "max-steps" <>
                     OA.short 'm' <>
                     OA.metavar "NUM" <>
                     OA.value 2000 <>  -- test M28Jul24-1 takes ages with 1000 steps
                     OA.help "Maximum number of rewrite steps"

       ; maxNormSteps <- OA.option OA.auto $
                         OA.long "max-norm-steps" <>
                         OA.metavar "NUM" <>
                         OA.value 10000 <>
                         OA.help "Maximum number of normalization steps"

       ; ignoreFuelStop <- OA.switch $
                           OA.long "ignore-fuel-stop" <>
                           OA.help "Ignore running out of fuel"

       ; assumeVerified <- OA.switch $
                           OA.long "assume-verified" <>
                           OA.help "discard runtime verifications"

       ; timRun <- OA.switch $
                   OA.long "tim-run" <>
                   OA.help "run a Tim test"
       ; timVerify <- OA.switch $
                      OA.long "tim-verify" <>
                      OA.help "verify Tim test"
       ; timOutput <- OA.switch $
                      OA.long "tim-output" <>
                      OA.help "display as a Tim test"
       ; timCSV <- OA.switch $
                   OA.long "tim-csv" <>
                   OA.help "displkay status of Tim tests"

       ; showDesugared <- OA.switch $
                          OA.long "show-desugared" <>
                          OA.help "show desugared version"

       ; preludeEval <- OA.strOption $
                        OA.long "eval-prelude" <>
                        OA.metavar "NAME" <>
                        OA.value "miniprelude" <>
                        OA.help "use the given prelude for evaluation tests"

       ; preludeVerify <- OA.strOption $
                          OA.long "verify-prelude" <>
                          OA.metavar "NAME" <>
                          OA.value "miniverifyprelude" <>
                          OA.help "use the given prelude for verification tests"

       ; allAsIter <- OA.switch $
                        OA.long "all-as-iter" <>
                        OA.help "encode all with iter"

       ; dsUniform <- OA.switch $
                        OA.long "ds-uniform" <>
                        OA.help "use uniform desugaring"

       ; fileNames <- OA.many $
                      OA.argument OA.str (OA.metavar "FILES...")

       ; useEvaluator <- OA.optional $ OA.option OA.auto $
                         OA.long "evaluator" <>
                         OA.help "use a particular semantics"

       ; matchFirst <- OA.switch $
                        OA.long "match-first" <>
                        OA.help "run matching rules first"

       ; return (TestFlags { .. }) }

testFlagsToFEFlags :: TestFlags -> FE.Flags
testFlagsToFEFlags t =
  let flags = defaultFlags
  in  flags{ fSplit        = split t, fSimplify = simplify t
           , fTrace        = showTrace t
           , fDfs          = dfs t
           , fUnderLambda  = not (noUnderLam t)
           , fRewriteSteps = maxSteps t
           , fNoFuelStop   = ignoreFuelStop t
           , fTraceDesugar = verbose t
           , fAllAsIter    = allAsIter t
           , fDsUniform    = dsUniform t
           }

setPreludeFlag :: Bool    -- True <=> verifying
               -> TestFlags
               -> Flags -> Flags
setPreludeFlag are_verifying test_flags flags
  = flags { fPrelude = case findPrelude prel_name of
                         Right stuff -> stuff
                         Left  err   -> error err }
  where
    prel_name | are_verifying = preludeVerify test_flags
              | otherwise     = preludeEval   test_flags


-----------------------------------------------
--
--    BrokenTests
--      eraseSkipped :: FilePath -> String -> IO String
--
-----------------------------------------------

type Skipped = [SkippedTest]
data SkippedTest = MkSkippedTest
  { skipName   :: Maybe String  -- ^ The name of the test (optional)
  , skipStatus :: TestStatus    -- ^ The type of the test (pass/fail)
  , skipReason :: String        -- ^ The reason why the test is broken (in quotes)
  , skipCode   :: SrcExpr       -- ^ The exact string (single line) corresponding to the test (in quotes)
  }
  deriving (Show)

skipping :: [Test] -> Skipped -> [Test]
skipping tests skips = skip1 <$> tests
  where
    m     = M.fromList [ (skipCode s, skipStatus s) | s <- skips ]
    skip1 test = case M.lookup (testSrc test) m of
                   Just status -> test `testWithStatus` status
                   Nothing     -> test

testWithStatus :: Test -> TestStatus -> Test
testWithStatus (TestVerify ti e)    status = TestVerify (ti { testStatus = status }) e
testWithStatus (TestEvalEq ti e e') status = TestEvalEq (ti { testStatus = status }) e e'
testWithStatus (TestDenSem ti e e') status = TestDenSem (ti { testStatus = status }) e e'

{-
skip("reason"){ test }
broken("reason"){ test }
-}
pSkipped :: V.Parser SkippedTest
pSkipped = do
  status <- pSkipTestStatus
  (mname, reason) <- V.pParens pSkipInfo
  code   <- PC.toSrcExpr <$> V.pExpr <* V.optionMaybe V.pSemi
  pure (MkSkippedTest mname status reason code)

pSkipInfo :: V.Parser (Maybe String, String)
pSkipInfo = mkSkip <$> V.sepBy1 V.pStringLit V.pComma
  where
    mkSkip [a,b] = (Just $ T.unpack $ unLoc a, T.unpack $ unLoc b)
    mkSkip [b]   = (Nothing, T.unpack $ unLoc b)
    mkSkip _     = undefined

pSkipTestStatus :: V.Parser TestStatus
pSkipTestStatus = do
  i <- V.pIdent
  case unLoc $ fmap (T.unpack . T.toLower) i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pSkipType"

pSkippedFile :: V.Parser Skipped
pSkippedFile = V.skip *> V.many pSkipped <* V.eof

parseSkipped :: FilePath -> IO Skipped
parseSkipped fn = do
  exists <- doesFileExist fn
  if exists
    then V.parseDie pSkippedFile fn <$> B.readFile fn
    else pure mempty

-----------------------------------------------
--
--    Log Unexpected Tests
--      logUnexpectedToFile  -- saves an individual test
--      eraseUnexpectedFiles -- removes all unexpected test logs
--
-----------------------------------------------

-- TODO: this is rather egregiously slow... but lets see if it matters on the TimTests...
logUnexpectedToFile :: TestRes -> IO ()
logUnexpectedToFile res = do
    putStrLn "Writing unexpected result to file"
    str <- readTestString info
    appendFile log_fn str
  where
    info   = tr_info res
    fn     = sourceName (testLocStart info)
    log_fn = fn ++ "." ++ show (testType info)

clearUnexpectedFiles :: FilePath -> IO ()
clearUnexpectedFiles fn = do
  let pass_fn = fn ++ ".pass"
  exists_pass <- doesFileExist pass_fn
  when exists_pass $ removeFile pass_fn
  let fail_fn = fn ++ ".fail"
  exists_fail <- doesFileExist fail_fn
  when exists_fail $ removeFile fail_fn


readTestString :: TestInfo -> IO String
readTestString info = do
  let loc  = testLocStart info
  let loc' = testLocEnd info
  let fn = sourceName loc
  grabLines (unPos (sourceLine loc))  (unPos (sourceLine loc')) <$> readFile fn

grabLines :: Int -> Int -> String -> String
grabLines from to = unlines . take (to - from). drop (from - 1). lines

-- >>> grabLines 4 8 (unlines ["1", "2","3","4","5","6","7","8","9","10"])
-- "4\n5\n6\n7\n"

-----------------------------------------------
--
--     Display status of Tim conversion as a CSV
--     displayCSV :: TestFlags -> (FilePath, [Test]) -> IO ()
--
-----------------------------------------------

displayCSV :: TestFlags -> (FilePath, [Test]) -> IO ()
displayCSV _tflg (_fn, ts) = mapM_ displayCSVTest ts

displayCSVTest :: Test -> IO ()
displayCSVTest test | TimSkip s <- testTimSkip (testInfo test)  = skipCSV (skipMsg s)
                    | hasUnimpPrimOp $ testSrc test             = skipCSV "unimplemented"
                    | testStatus (testInfo test) == TS_Skip     = skipCSV "marked skip"
                    | testStatus (testInfo test) == TS_Broken   = skipCSV "marked broken"
                    | otherwise                                 = skipCSV "OK"
  where
    skipCSV :: String -> IO () -- <-- just silencing defaulting warnings
    skipCSV msg = putStrLn $ show (testName (testInfo test)) ++ "," ++ show msg
    skipMsg ('_':s) = skipMsg s
    skipMsg ""      = "skip"
    skipMsg "acc"   = "accepted"
    skipMsg "rej"   = "rejected"
    skipMsg "crash" = "crash"
    skipMsg "any"   = ":any behaviour"
    skipMsg "loop"  = "loops"
    skipMsg "unimpl"= "unimplemented"
    skipMsg s       = s

-----------------------------------------------
--
--     Display tests in Tim's format
--     displayTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
--
-----------------------------------------------

displayTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
displayTestFile _tflg (_fn, ts) = mapM_ displayTest ts

displayTest :: Test -> IO ()
displayTest test | TimSkip _ <- testTimSkip (testInfo test) = return ()
                 | hasUnimpPrimOp $ testSrc test            = return ()
                 | testStatus (testInfo test) /= TS_Normal  = return ()
displayTest test = do
  let retCode :: TestType -> String
      retCode TestFail | Just err <- bad = err
                       | otherwise = "F00"
      retCode TestPass | Just err <- bad = err
                       | otherwise = "S00"
      retCode TestLoop = "S00"  -- What should this be?

      -- Error code, if this is a bad test.
      bad = case testTimSkip (testInfo test) of
              TimError s -> Just s
              _ -> Nothing

      -- How to wrap the computation and the result.
      -- If there are choices we need to turn them into arrays.
      wrap e | maybe False hasChoice res = "for{" ++ timShow True e ++ "}"
             | otherwise                 = timShow False (Parens e)

      -- Result expression, if we need one
      res = case test of
              TestEvalEq _ _ e2 | isNothing bad -> Just e2
              _ -> Nothing
  putStrLn $ "test(" ++ retCode (testType (testInfo test)) ++ "){" ++
             (case res of
                Just e -> wrap (testSrc test) ++ " = " ++ wrap e
                Nothing -> timShow True (testSrc test)
             ) ++ "}   # " ++ testName (testInfo test)

hasChoice :: SrcExpr -> Bool
hasChoice e = not $ null $ filter choicy $ universeBi e
  where choicy (PrefixOp (Ident _ ":") (Variable (Ident _ "false"))) = True
        choicy (InfixOp _ (Ident _ "|") _) = True
        choicy (Variable (Ident _ "fail")) = True
        choicy _ = False

hasUnimpPrimOp :: SrcExpr -> Bool
hasUnimpPrimOp e =
  let is = universeBi e
      notImpIds = map (Ident noLoc) notImp
      -- Comparisons are not implemented at all,
      -- and arithmetic only works on constants.
      notImp = words "< <= > >= <> + - * / intAdd$ []"
  in  any (`elem` notImpIds) is

timShow :: Bool -> SrcExpr -> String
timShow sem = renderStyle s . ppTim sem 0
  where
    s = style{ lineLength = 1000000, ribbonsPerLine = 1.2 }

-------------

ppTim :: Bool -> Rational -> SrcExpr -> Doc
ppTim = pp
  where
    ppr :: (Pretty a) => Rational -> a -> Doc
    ppr = pPrintPrec prettyNormal

    ppOp = ppr 0

    ppEs :: [SrcExpr] -> Doc
    ppEs = fsep . punctuate comma . map (pp False 1)

    ppSeq :: [SrcExpr] -> Doc
    ppSeq es = sep $ punctuate (text ";") $ map (pp False 0) es

    ppBlock :: Rational -> [SrcExpr] -> Doc
    -- We have list of expressions that need to be considered a single expression.
    -- This should have been 'block{es}', but Tim does not implement that.
    -- It could have been 'let(){es}', but Tim has the wrong scope for that.
    -- So we settle on 'array{e1, ..., en}[n]', which is pretty horrible.
    ppBlock prec es = maybeParens (prec > 0) $
                      --text "array" <> braces (ppSeq lvl es) <> brackets (text (show (length es - 1)))
                      text "let()" <> braces (ppSeq es)

    ppArg :: SrcExpr -> Doc
    ppArg (Array es) | length es /= 1 = ppEs es
    ppArg e                           = pp False 0 e

    ppBlk es = braces $ ppSeq es

    ppEffs :: [EffString] -> Doc
    ppEffs rs = mconcat (map (\ r -> text "<" <> text r <> text ">") rs)

    pp :: Bool -> Rational -> SrcExpr -> Doc  -- boolean indicates that ';' is allowed
    pp sem prec expr =
      case expr of
        Src.Lit lit    -> ppr prec lit
        Variable v | Just s <- lookup v timRename
                     -> text s
                   | otherwise
                     -> ppr 0 v
        Seq e1 e2 | sem       -> ppSeq es
                  | otherwise -> ppBlock prec es
                  where
                    es = e1 : grab e2
                    grab (Seq s1 s2) = s1 : grab s2
                    grab s           = [s]
        PrefixOp o e -> maybeParens (prec > q) $ ppOp o <> pp False qr e
          where (q, _, qr) = fixity ("pre" ++ identString o)
        PostfixOp e o -> maybeParens (prec > q) $ pp False ql e <> ppOp o
          where (q, ql, _) = fixity ("post" ++ identString o)
        InfixOp e1 o e2 -> maybeParens (prec > q) $ sep [pp False ql e1 <+> ppOp o, indent $ pp False qr e2]
          where (q, ql, qr) = fixity (identString o)
        Parens (Tuple es) -> parens (ppEs es)
        Parens e -> parens (pp False 0 e)
        Tuple es -> parens (ppEs es)
        Array es   -> text "array" <> braces (ppSeq es)
        ApplyS  f a -> maybeParens (prec > q) $ pp False ql f <> parens (ppArg a)
          where (q, ql, _) = fixity "()"
        ApplyD f a -> maybeParens (prec > q) $ pp False ql f <> brackets (ppArg a)
          where (q, ql, _) = fixity "()"
        Src.Blk es -> ppBlk es
        Macro1 (Ident _ "one")  [] b -> text "first" <> pp False 10 b
        Macro1 (Ident _ "all")  [] b -> text "for"   <> pp False 10 b
        Macro1 (Ident _ "type") [] b -> text "type"  <> pp False 10 b
        Macro1 (Ident _ "check") fs b -> text "check" <> ppEffs fs <> pp False 10 b
        Macro1 (Ident _ "assume") [] b -> text "assume"  <> pp False 10 b
        For1   b -> text "for" <>                         pp False 10 b
        For2 e b -> text "for" <> parens (pp True 0 e) <> pp False 10 b
        Function {} -> maybeParens (prec > 0) $
                         cat [ text "function" <> hcat (map ppArs args)
                             , indent (pp False 10 body) ]
                where
                  (args,body) = split_args [] expr
                  split_args acc (Function q a fxs b) = split_args ((q,a,fxs):acc) b
                  split_args acc b                    = (reverse acc, b)

                  ppArs (q, e, rs) = parens (ppArg e) <> pPrint q <> pPrint rs

        If3 e1 e2 e3 -> maybeParens (prec > 0) $
                        sep [text "if" <+> parens (pp True 0 e1) <+> text "then",
                             indent $ pp False 0 e2,
                             text "else",
                             indent $ pp False 0 e3]
        If2 e1 e2 -> maybeParens (prec > 0) $
                        sep [text "if" <+> parens (pp True 0 e1) <+> text "then",
                             indent $ pp False 0 e2]
        EffAttr f a -> maybeParens (prec > q) $ pp False ql f <> text "<" <> ppr 0 a <> text ">"
          where (q, ql, _) = fixity "()"
        Let e1 e2 -> maybeParens (prec > 0) $
                     sep [text "let" <+> parens (pp True 0 e1),
                          indent $ pp False 10 e2]
        Truth (Array []) -> text "true"
        Truth e -> text "truth" <> braces (pp True 0 e)
        Option me -> text "option" <> braces (maybe empty (pp True 0) me)
        Exists is e -> pp sem prec $ eSeq $ vars ++ unBlk e
          where vars = map (\ i -> InfixOp (Variable i) (Ident noLoc ":") (Variable (Ident noLoc "any"))) is
                unBlk (Src.Blk es) = es
                unBlk ee = [ee]

        _ -> error $ "ppTim: unimplemented " ++ take 100 (show expr)

timRename :: [(Src.Ident, String)]
timRename = [ (Src.Ident noLoc x, y) | (x, y) <-
  [ ("intAdd$", "operator'+'")
  , ("fail", ":false")
  ] ]

-- small util to remove the location from the parser.
unLoc :: V.L a -> a
unLoc (V.L _ a) = a
