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
import FrontEnd.Desugar as FrontEnd ( desugar, runD, addPrelude, sDesugarExpr )
import FrontEnd.ToCore  as FrontEnd ( convertToPrepdCore )
import FrontEnd.Flags   as FrontEnd
import FrontEnd.Expr    as Src
import FrontEnd.Parse( P, parseDie, pFile, pOp, pIdent, pExprSeq, pBraces, pParens
                     , pString, pKeyword, many, optional, skip, eof, lexeme )
import FrontEnd.Prelude( findPrelude )

import qualified Parser.Verse               as V
import qualified Parser.Compat              as PC

import Core.Expr as Core
import Core.Traced
import Core.Verifier( verificationRules )
import Core.Rules ( runtimeRules )
import Core.Rule( everywhere, normalize, NormResult(..) )

-- verse-densem
import SExp

-- plancc densem
import PlanCC(edenSem, edenSemDS)
import SExpC(srcExprToExp)

-- Tim densem
import TimE (den)
import ENVDesugar (envDesugar)

import Epic.Print hiding ( (<>) )
import Data.Generics.Uniplate.Data( universeBi )

import Text.Megaparsec( getSourcePos, unPos, sepBy1, try, anySingleBut, manyTill, single, choice)
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Pos (mkPos, SourcePos(..))
-- import Control.Applicative.Combinators (between)

import GHC.Stack( HasCallStack )

import Data.List( isPrefixOf )
import Data.Char( toLower )
import Data.Maybe
import Control.Monad( unless, when, guard, (>=>))
import System.Directory( doesFileExist, removeFile )
import System.Exit( exitWith, ExitCode(..) )
import Text.Printf
import qualified Data.Map as M

import Control.Exception( catch, SomeException )
import qualified Options.Applicative as OA

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

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
  | parseOnly test_flags && useLibParser test_flags
  = mapM_ parseSourceOnly' (fileNames test_flags)

  | parseOnly test_flags
  = mapM_ parseSourceOnly (fileNames test_flags)

  | Just expr_string <- testExpr test_flags
  , let fn = "<command-line>"
  = runTestFile test_flags (fn, parseDie pTestFile fn expr_string)

  | timOutput test_flags
  = mapM_ read_and_display (fileNames test_flags)

  | timCSV test_flags
  = mapM_ read_and_csv (fileNames test_flags)

  | useLibParser test_flags
  = mapM_ read_and_run' (fileNames test_flags)

  | otherwise
  = mapM_ read_and_run (fileNames test_flags)
  where
    read_and_run :: FilePath -> IO ()
    read_and_run fn = do { tests <- readTests fn
                         ; runTestFile test_flags (fn, tests) }

    -- See Note [Ticks in Tester] -- TODO: Jeff
    read_and_run' :: FilePath -> IO ()
    read_and_run' fn = do { tests <- readTests' fn
                          ; runTestFile test_flags (fn, fmap test'ToTest tests) }

    read_and_display :: FilePath -> IO ()
    read_and_display fn = do { tests <- readTests fn
                             ; displayTestFile test_flags (fn, tests) }

    read_and_csv :: FilePath -> IO ()
    read_and_csv fn = do { tests <- readTests fn
                         ; displayCSV test_flags (fn, tests) }

-----------------------------------------------
--
--    Data types
--
--    Test, TestInfo, TestRes
--
-----------------------------------------------

{- Note [Testing densem in the tester]
~~~~~~~~~~~~~~~~~~~~~~~~~~

  * To test the semantic functions for different denotational semantics we abuse
    'TestType' in 'TestInfo' to track which semantic function to call. This is
    purely a descision to enable the tester to use the semantic functions as
    fast as possible. In general, the tester should know less about the tests,
    see #77.

  * We try to keep the data pipeline for the tester the same as much as
    possible. Thus we define a parser ('pTestDenSem') to parse a "testds" call
    in a .versetest file. This parser decides the correct semantic function to
    run in 'pTestRunner' which propogates this information to the 'testRunner'
    field of 'pTestInfo'. Each semantic function is built into the tester and is
    one of this set: { "tim", "dls", "sls", "els" }. We have precisely chosen
    this format for these symbols so that the 'foo.versetest' files are valid
    verse.

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
    , testType     :: !TestType           -- Default test type
    , testRunner   :: !(Maybe TestRunner) -- the function the test data is passed to, Nothing is evaluation/verification
    , testStatus   :: !TestStatus
    , testTimSkip  :: !TimSkip            -- skip (or error code) when converting to Tim's format
    }
    deriving (Show)

data TestRunner = Tim_DS | DLS_DS | SLS_DS | ELS_DS   -- denotational semantic functions

instance Show TestRunner where
  -- INFO: Ideally these should correspond to their respective commands in the
  -- repl just without the ':' prefix, i.e., dls-densem here is :dls-densem in
  -- the repl. But this would require changing the frontend parser to handle the
  -- '-' which would no longer be valid verse. See Note [Testing densem in the
  -- tester]
  show Tim_DS = "tim"
  show DLS_DS = "dls"
  show SLS_DS = "sls"
  show ELS_DS = "els"


data TestType =
  TPass | TFail | TLoop                -- Expected behaviour
  deriving (Eq)

data TimSkip = TimNone | TimSkip String | TimError String
  deriving (Eq, Show)

instance Show TestType where
  show TPass = "pass"
  show TFail = "fail"
  show TLoop = "loop"

data TestStatus = TS_Normal
                | TS_Broken   -- Test is currently broken (i.e., pass/fail is negated)
                | TS_Skip     -- Test should be skipped, probably because it somehow
                              --   crashes the entire implementation
                deriving( Show, Eq )

testName :: TestInfo -> String
testName ti = fromMaybe ("L" ++ show (unPos (sourceLine (testLocStart ti)))) (testMName ti)

data TestRes = TestRes { tr_info    :: TestInfo
                       , tr_outcome :: TestOutcome }
  deriving (Show)

data TestOutcome = TO_Equal                -- Terminated, results equal
                 | TO_NotEqual             -- Terminated, results differ
                 | TO_Excn
                 | TO_Abnormal NormResult   -- Could not reach a normal form;
                                            -- the NormResult is never NormOK
                 | TO_Skipped               -- We didn't run this test
                 deriving( Eq, Show )

skipTestRes :: TestRes -> Bool
skipTestRes (TestRes { tr_info = info }) = testStatus info == TS_Skip

expectedTestRes :: TestRes -> Bool
-- Expected results, not skipped; account for broken-ness
expectedTestRes tr@(TestRes { tr_info = info })
  = case testStatus info of
      TS_Normal -> expectedOutcome tr
      TS_Broken -> unexpectedOutcome tr
      TS_Skip   -> False

expectedOutcome :: TestRes -> Bool
-- Expected results; ignore broken-ness
expectedOutcome (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
      TPass  -> outcome == TO_Equal
      TFail  -> outcome == TO_NotEqual
      TLoop  -> outcome == TO_Abnormal NormExpired

unexpectedTestRes :: TestRes -> Bool
unexpectedTestRes tr@(TestRes { tr_info = info })
  = case testStatus info of
      TS_Normal -> unexpectedOutcome tr
      TS_Broken -> expectedOutcome tr
      TS_Skip -> False

unexpectedOutcome :: TestRes -> Bool
-- Unexpected results, not skipped, not exception, not invalid
unexpectedOutcome (TestRes { tr_info = info, tr_outcome = outcome })
 = case testType info of
      TPass -> outcome == TO_NotEqual || outcome == TO_Abnormal NormExpired
      TFail -> outcome == TO_Equal    || outcome == TO_Abnormal NormExpired
      TLoop -> outcome == TO_Equal    || outcome == TO_NotEqual

outcomeIs :: TestOutcome -> TestRes -> Bool
outcomeIs oc1 (TestRes { tr_outcome = oc2 }) = oc1 == oc2

passedButShouldFail :: TestRes -> Bool
passedButShouldFail (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TFail -> outcome == TO_Equal
       _     -> False

failedButShouldPass :: TestRes -> Bool
failedButShouldPass (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TPass  -> outcome == TO_NotEqual
       _      -> False

failedWithLoop :: TestRes -> Bool
failedWithLoop (TestRes { tr_info = info, tr_outcome = outcome })
  = case testType info of
       TLoop -> False
       _     -> outcome == TO_Abnormal NormExpired

isBrokenPass :: TestRes -> Bool
isBrokenPass tr@(TestRes { tr_info = info })
  = testStatus info == TS_Broken && expectedOutcome tr

isBrokenFail :: TestRes -> Bool
isBrokenFail tr@(TestRes { tr_info = info })
   = testStatus info == TS_Broken && unexpectedOutcome tr

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
            expected     = filter expectedTestRes      res  -- Excludes skipped
            n_expected   = length expected
            unexpected   = filter unexpectedTestRes    res  -- Excludes skipped, invalid, exn
            n_unexpected = length unexpected
            n_invalid    = count (outcomeIs (TO_Abnormal NormInvalid)) res
            n_excn       = count (outcomeIs TO_Excn)       res
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

srcToCore :: FrontEnd.Flags -> Bool -> SrcExpr -> IO Core.Expr
srcToCore flags add_verification e
  = do { e1 :: SrcCore <- FrontEnd.desugar flags add_verification e
       ; FrontEnd.convertToPrepdCore flags e1 }

evalExpr :: TestFlags -> Test -> Core.Expr -> (NormResult, Int, Traced Core.Expr)
evalExpr flags test e = (r1,length (trace tr),tr)
  where
    (r1,tr) = normalize (maxSteps flags) rules e
    rules   =
      case test of
        TestEvalEq {} -> everywhere runtimeRules
        TestVerify {} -> everywhere verificationRules
        TestDenSem {} -> error "evalExpr: found a densem test...impossibly"

-- Eval using a semantic function.
evalDenSem :: TestFlags -> Test -> SrcExpr -> IO (NormResult, Int, Expr)
evalDenSem _flags test e = do
  res <- LitStr <$> f e
  return (NormOK, 0, res)
  where
    flgs = FrontEnd.defaultFlags
    err  = error "runD: exception in evalDenSem"
    go   = runD flgs err . getEssential flgs

    -- the moral equivalent of 'getEssential' in 'repl/Main.hs'
    -- getEssential :: Flags -> SrcExpr -> DsM SrcEssential
    getEssential _ = addPrelude >=> sDesugarExpr

    -- because each semantic function returns a different type we convert to
    -- show to normalize the result
    f :: SrcExpr -> IO String
    f = case testRunner $ testInfo test of
          Nothing     -> error $ "evalExpr: Expected densem type, got Nothing with test: " ++ show test
          Just Tim_DS -> fmap (showASCII . den . envDesugar) . go
          Just DLS_DS -> go >=> fmap showASCII . edenSem . edenSemDS . srcExprToExp
          Just SLS_DS -> error "SLS densem not implemented yet. Sorry!"
          Just ELS_DS -> go >=> denSemDesugar >=> fmap showASCII . denSem

-- Hackily replace some Unicode characters
showASCII :: Show a => a -> String
showASCII = concatMap ascii . show
  where ascii '\8746' = "U"
        ascii '\8800' = "/="
        ascii c = [c]

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
timTestType ('S' : _) = TPass
timTestType _         = TFail

timSkip :: String -> TimSkip
timSkip ('S' : _) = TimNone
timSkip s         = TimError s

----------------------------
runTest :: TestFlags -> Test -> IO TestRes
runTest tflg test@(TestVerify _ e)     = doTestCatchingExn tflg test e (Array [])
runTest tflg test@(TestDenSem _ e1 e2) = doTestCatchingExn tflg test e1 e2
runTest tflg test@(TestEvalEq _ e1 e2) = doTestCatchingExn tflg test e1 e2

mkFEFlags :: TestFlags -> Bool -> FrontEnd.Flags
mkFEFlags tflg add_verification
  = setPreludeFlag add_verification tflg $
    testFlagsToFEFlags tflg

----------------------------
data TestMode = TEval | TVerify Effect deriving (Eq, Show)

-- | `doTestCatchingExn` runs the actual test, catching any exceptions that are
-- thrown during parsing, desugaring, or execution/verification
doTestCatchingExn :: (HasCallStack) => TestFlags -> Test -> SrcExpr -> SrcExpr -> IO TestRes
doTestCatchingExn tflg test p1 p2
  | TS_Skip <- testStatus info
  = do { when (noisy tflg) (putStrLn $ test_herald ++ "Skipped")
       ; pure (TestRes { tr_info = info, tr_outcome = TO_Skipped }) }
  | otherwise
  = do { catch (doTest tflg test p1 p2)
               (\e -> do { exn_handler e
                         ; pure (TestRes { tr_info = info, tr_outcome = TO_Excn }) }) }
  where
    info        = testInfo test
    test_herald = testHerald test
    exn_handler :: SomeException -> IO ()
    exn_handler e
      = -- unless (noError tflg) $
        do { putStrLn $ test_herald ++ "Failure:"
           ; putStrLn "The expression";       putStrLn (show p1) -- ppIndent p1
           ; putStrLn "or the expression";    ppIndent p2
           ; putStrLn "caused an exception:"; print e
           ; putStrLn "" }


-- | `doTest` does the actual work of parsing, converting to core, and
-- evaluating/verifying; each of which can throw an exception.
doTest :: (HasCallStack) => TestFlags -> Test -> SrcExpr -> SrcExpr -> IO TestRes
doTest tflg test src1 src2 = do
  do { let flags     = mkFEFlags tflg add_verif
           add_verif = desugarForVerification test

     ; core1 <- srcToCore flags add_verif src1

     -- Display the desugared output
     ; when (showDesugared tflg) $
       displayDoc (sep [text (testHerald test) <+> text "desugared:", pPrint core1])

     ; mb_core2 <- case src2 of
                     Variable (Ident _ "wrong") -> pure Nothing
                     _       -> do { core2 <- srcToCore flags False src2
                                   ; pure (Just core2) }

     ; checkResults tflg test (src1, core1) (src2, mb_core2)

     }

desugarForVerification :: Test -> Bool
desugarForVerification TestEvalEq{}   = False
desugarForVerification TestDenSem{}   = False
desugarForVerification TestVerify{}   = True


-- Jeff: Yes this is a mess, but for now its necessary to preserve the output
-- format between standard tests and densem tests.
show_result
    :: ( Pretty a1
       , Pretty a2
       , Pretty a3
       , Pretty a4
       ) => TestFlags -> TestStatus -> TestOutcome -> String -> TestRes
         -> String -> String -> a1 -> a2 -> a3 -> a4 -> Int -> IO ()
show_result
  tflg status outcome test_herald test_res succ_what fail_what src1 v1 src2 mb_v2 n_steps
  | expectedTestRes test_res -- What to display if all is well
  = when (noisy tflg) $
    putStrLn $ test_herald ++ "Expected " ++ succ_what
                           ++ " in " ++ printf "%5d" n_steps ++ " steps"
  | TS_Broken <- status
  = putStrLn $ test_herald ++ "Broken test now pass"
  | TO_Abnormal NormInvalid <- outcome
  = putStrLn $ test_herald ++ "Crash: rewrite yields invalid results"
  | otherwise   -- TS_Normal
  = do { putStrLn $ test_herald ++ "Unexpected " ++ fail_what
       ; when (logUnexpected tflg) $ logUnexpectedToFile test_res
       ; unless (noError tflg) $
         do { putStrLn "-----------------------------------------------"
            ; putStrLn "The expression"; ppIndent src1
            ; putStrLn "evaluates to";   ppIndent v1
            ; putStrLn "while";          ppIndent src2
            ; putStrLn "evaluates to";   ppIndent mb_v2 } }


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
                        (_, TPass)    -> "success"
                        (_, TFail)    -> "failure"
                        (_, TLoop)    -> "loop   "

               fail_what
                 | failedWithLoop test_res = "timeout"
                 | otherwise = case typ of
                                 TPass -> "failure"
                                 TFail -> "success"
                                 TLoop -> "termination"

         ; show_result tflg status outcome test_herald test_res succ_what
           fail_what src1 v1 src2 mb_v2 n_steps
         ; -- Display the trace if asked for, regardless of success/failure
         ; when (showTrace tflg) $ putStrLn "Test is a den-sem test, trace not implemented"
         ; pure test_res }

checkResults tflg test (src1, core1) (src2, mb_core2)
  = do { show_result tflg status outcome test_herald test_res succ_what fail_what
                     src1 v1 src2 mb_core2 n_steps

       -- Display the trace if asked for, regardless of success/failure
       ; when (showTrace tflg) $
         do { putStrLn "Trace is:"; displayTraceV (traceVerbosity tflg) tr1 }

       ; pure (TestRes { tr_info = info, tr_outcome = outcome }) }
  where
    (res1,ln1,tr1) = evalExpr tflg test core1
    v1           = Core.Traced.term tr1
    n_steps      = ln1
    mb_v2        = fmap ((\(_,_,tr) -> term tr) . evalExpr tflg test) mb_core2
                   -- Really we should check res2 as well, but it is always boring
    test_herald  = testHerald test
    info         = testInfo test
    typ          = testType info
    status       = testStatus info
    test_passed  = equivValue v1 mb_v2

    outcome :: TestOutcome
    outcome = case res1 of
       NormOK | test_passed -> TO_Equal
              | otherwise   -> TO_NotEqual
       _                    -> TO_Abnormal res1

    test_res = TestRes { tr_info = info, tr_outcome = outcome }

    succ_what = case (status, typ) of
             (TS_Broken,_) -> "broken "
             (_, TPass)    -> "success"
             (_, TFail)    -> "failure"
             (_, TLoop)    -> "loop   "

    fail_what
      | failedWithLoop test_res = "timeout"
      | otherwise               = case typ of
                                    TPass -> "failure"
                                    TFail -> "success"
                                    TLoop -> "termination" -- this case probably never happens?


-- | Equivalence on values (or stuck expressions)
-- e2=Nothing <=> e2=WRONG <=> e1 gets stuck without reaching a value
equivValue :: Core.Expr -> Maybe Core.Expr -> Bool
equivValue e1 (Just e2) = Core.norm e1 == Core.norm e2
equivValue e1 Nothing   = not (isVal e1)

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

ppIndent :: Pretty a => a -> IO ()
ppIndent x = displayDoc (text "  " <+> pPrint x)
-----------------------------------------------
--
--     Parse a file of Verse source code
--     parseSourceOnly :: FilePath -> IO ()
--
-----------------------------------------------

parseSourceOnly :: FilePath -> IO ()
-- Just parse a file of Verse code
parseSourceOnly fn = do
  file <- readFile fn
  let e = parseDie pFile fn file
  if e == e then putStrLn $ "parsed " ++ fn else undefined
  putStrLn "SUCCESS"
  pure ()

-- See Note [Ticks in Tester]
parseSourceOnly' :: FilePath -> IO ()
parseSourceOnly' fn = do
  _ <- V.parseDie V.pFile fn <$> B.readFile fn
  -- if we get here we have succeeded
  putStrLn $ "parsed " ++ fn
  putStrLn "SUCCESS"
  pure ()

-----------------------------------------------
--
--     Read the test file, and parse it
--     readTests :: FilePath -> IO [Test]
--
-----------------------------------------------

readTests :: FilePath -> IO [Test]
-- Read the test file, and parse it
readTests fn = do
  tests <- parseDie pTestFile fn <$> readFile fn
  skips <- parseSkipped (fn ++ ".skip")
  pure   $ tests `skipping` skips

-- Parse a file of tests
pTestFile :: P [Test]
pTestFile = skip *> many pTest <* eof

-- Parse a test
pTest :: P Test
pTest = pTestEq OA.<|> pTestVerify OA.<|> pTimTest OA.<|> pTestDenSem

-- Parse an expression evaluation equality test
pTestEq :: P Test
pTestEq =
  pKeyword "testeq" *> do
    tId <- pParens pTestInfo
    TestEvalEq tId <$> pBraces pExprSeq <*> pBraces pExprSeq

-- Parse an expression verification test
pTestVerify :: P Test
pTestVerify =
  pKeyword "verify" *> do
    tId <- pParens pTestInfo
    src <- pBraces (pExprSeq <* optional (pOp ";"))
    locEnd <- getSourcePos
    pure $ TestVerify (tId { testLocEnd = locEnd }) src

pTimTest :: P Test
pTimTest =
  pKeyword "test" *> do
    tag <- pParens pIdent
    src <- pOp "{" *> pExprSeq <* optional (pOp ";") <* pOp "}"
    locEnd <- getSourcePos
    let ti = (timTestInfo tag) { testLocEnd = locEnd }
    pure $ TestVerify ti src

pTestDenSem :: P Test
pTestDenSem =
    pKeyword "testds" *> do
    tId <- pParens pTestInfo
    let pAnyString = fmap (Src.Lit . LStr) $ do
          _ <- single '"'
          lexeme $ manyTill (anySingleBut '"') (single '"')
    TestDenSem tId <$> pBraces pExprSeq <*> pBraces pAnyString

pStringLit :: P String
pStringLit = strOf <$> pString
  where
    strOf (Src.Lit (LStr s)) = s
    strOf _ = undefined

pTestInfo :: P TestInfo
pTestInfo = do
  loc   <- getSourcePos
  mname <- optional (pStringLit <* pOp ",")
  rnner <- try (optional pTestRunner)
  typ   <- pTestType
  stat  <- try (pOp "," *> pTestStatus) OA.<|> pure TS_Normal
  tim   <- (pOp "," *> pTimSkip) OA.<|> pure TimNone
  pure (TestInfo { testMName = mname, testLocStart = loc, testLocEnd = loc
                 , testType = typ, testRunner = rnner, testStatus = stat
                 , testTimSkip = tim })

pTestType :: P TestType
pTestType = do
  i <- pIdent
  case map toLower $ identString i of
    "pass"    -> pure TPass
    "fail"    -> pure TFail
    "loop"    -> pure TLoop
    d         -> fail $ "pTestType: " ++ d

pTestRunner :: P TestRunner
pTestRunner = choice
    [ string "tim" *> pure Tim_DS
    , string "sls" *> pure SLS_DS
    , string "dls" *> pure DLS_DS
    , string "els" *> pure ELS_DS
    ] <* pOp ","

pTestStatus :: P TestStatus
pTestStatus = do
  i <- pIdent
  case map toLower $ identString i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pTestStatus"

pTimSkip :: P TimSkip
pTimSkip = do
  i <- pIdent
  guard (map toLower (identString i) == "tim")
  _ <- pOp "="
  sk <- pIdent
  case identString sk of
    's':'k':'i':'p':s -> pure (TimSkip s)
    s                 -> pure (TimError s)
 OA.<|>
  pure TimNone

--------------------------------------------------------------------------------
--
--     Read the test file, and parse with the verse-parser library many
--     functions in this section are duplicates until verse-parser is at feature
--     parity
--
--------------------------------------------------------------------------------

{- Note [Ticks in Tester]
~~~~~~~~~~~~~~~~~~~~~~~~~~

You will find that many functions and data types in the tester are duplicates
with a "'". For example, there is 'readFile' and 'readFile''.

This is purposeful and part of the plan to integrate the verse-parser in
$ROOT/VersePrototypes/parser into the tester. When the parser is at parity with
the frontend parser we will remove the duplication.

This work in tracked in issue #66.
-}



-- Read the test file, and parse it
-- START: figure out these errors
readTests' :: FilePath -> IO [Test']
readTests' fn = do
  tests <- V.parseDie pTestFile' fn <$> B.readFile fn
  skips <- parseSkipped' (fn ++ ".skip")
  pure $ tests `skipping'` skips

-- Parse a file of tests
pTestFile' :: V.Parser [Test']
pTestFile' = V.skip *> V.many pTest' <* V.eof

-- Parse a test
pTest' :: V.Parser Test'
pTest' = V.skip *> (pTestEq' OA.<|> pTestVerify' OA.<|> pTimTest') <* V.skip

-- Parse an expression evaluation equality test
pTestEq' :: V.Parser Test'
pTestEq' =
  V.lexeme (V.pKeyword "testeq") *> do
    let pdExpr = PC.toSrcExpr <$> V.pcExpr
    tId <- V.lexeme $ V.pParens pTestInfo'
    TestEvalEq' tId <$> (V.lexeme $ V.pcBraces pdExpr) <*> V.lexeme (V.pBraces pdExpr)

-- Parse an expression verification test
pTestVerify' :: V.Parser Test'
pTestVerify' =
  V.pKeyword "verify" *> do
    tId <- V.lexeme $ V.pParens pTestInfo'
    src <- V.lexeme $ V.pcBraces (fmap PC.toSrcExpr V.pcExpr <* V.optionMaybe V.pSemi)
    locEnd <- V.getLoc
    pure $ TestVerify' (tId { testLocEnd' = locEnd }) src

pTimTest' :: V.Parser Test'
pTimTest' =
  V.pKeyword "test" *> do
    locB <- V.getLoc
    tag <- projectLoc <$> V.pParens V.pIdent
    src <- V.pLBrace *> V.pExpr <* V.optionMaybe V.pSemi <* V.pRBrace
    locE <- V.getLoc
    let ti = timTestInfo' locB locE tag
    pure $ TestVerify' ti $ PC.toSrcExpr src

pTestInfo' :: V.Parser TestInfo'
pTestInfo' = do
  locB  <- V.getLoc
  let pComma = V.lexeme V.pComma
  mname <- V.optionMaybe (V.pStringLit <* V.lexeme V.pComma)
  typ   <- pTestType' <* V.optional pComma
  stat  <- V.try (pTestStatus' <* V.optional pComma) OA.<|> pure TS_Normal
  tim   <- V.try (pTimSkip') OA.<|> pure TimNone
  locE  <- V.getLoc
  pure (TestInfo' { testMName'    = fmap (T.unpack . projectLoc) mname
                  , testLocStart' = locB
                  , testLocEnd'   = locE
                  , testType'     = typ
                  , testStatus'   = stat
                  , testTimSkip'  = tim
                  })

pTestType' :: V.Parser TestType
pTestType' = V.lexeme $ do
  i <- V.many V.pAlpha -- cannot use V.pIdent, "fail" is a reserved
  case T.toLower $ TE.decodeLatin1 $ B.pack $ i of
    "pass"    -> pure TPass
    "fail"    -> pure TFail
    "loop"    -> pure TLoop
    _         -> fail "pTestType"

pTestStatus' :: V.Parser TestStatus
pTestStatus' = V.lexeme $ do
  i <- V.pIdent
  case projectLoc $ fmap T.toLower i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pTestStatus"

pTimSkip' :: V.Parser TimSkip
pTimSkip' = do
  i <- V.pIdent
  guard (projectLoc (fmap T.toLower i) == "tim")
  _  <- V.pEqual
  sk <- V.pIdent
  case T.unpack $ projectLoc sk of
    's':'k':'i':'p':s -> pure (TimSkip s)
    s                 -> pure (TimError s)
  OA.<|> pure TimNone

pSkippedFile' :: V.Parser Skipped'
pSkippedFile' = V.skip *> V.many pSkipped' <* V.eof

parseSkipped' :: FilePath -> IO Skipped'
parseSkipped' fn = do
  exists <- doesFileExist fn
  if exists
    then V.parseDie pSkippedFile' fn <$> B.readFile fn
    else pure mempty

projectLoc :: V.L a -> a
projectLoc (V.L _ a) = a


{-
skip("reason"){ test }
broken("reason"){ test }
-}
pSkipped' :: V.Parser SkippedTest'
pSkipped' = do
  status <- pSkipTestStatus'
  (mname, reason) <- V.pParens pSkipInfo'
  code   <- PC.toSrcExpr <$> V.pExpr <* V.optionMaybe V.pSemi
  pure (MkSkippedTest' mname status reason code)

pSkipInfo' :: V.Parser (Maybe String, String)
pSkipInfo' = mkSkip <$> V.sepBy1 V.pStringLit V.pComma
  where
    mkSkip [a,b] = (Just $ T.unpack $ projectLoc a, T.unpack $ projectLoc b)
    mkSkip [b]   = (Nothing, T.unpack $ projectLoc b)
    mkSkip _     = undefined
  -- pParens (pSkipName OA.<|> pSkipAnon)
  -- where
  --   pSkipName = (\n r -> (Just n, r)) <$> (pStringLit <* pOp ",") <*> pStringLit
  --   pSkipAnon = (Nothing,) <$> pStringLit


pSkipTestStatus' :: V.Parser TestStatus
pSkipTestStatus' = do
  i <- V.pIdent
  case projectLoc $ fmap (T.unpack . T.toLower) i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pSkipType"

-- A Exp from the parser with location metadata that uses simplenames
type LSExp = Src.SrcExpr

-- | Convert from a @Test'@ to a @Test@, name is purposefully left ugly so that
-- the temporary does not become permanent
test'ToTest :: Test' -> Test
test'ToTest (TestEvalEq' ti l r) = TestEvalEq (convert ti) l r
test'ToTest (TestVerify' ti a)   = TestVerify (convert ti) a

locToLoc :: FilePath -> V.Loc -> Loc
locToLoc name (V.Loc pos_start pos_end) = SourcePos { sourceName   = name
                                                    , sourceLine   = mkPos $ V.line pos_start
                                                    , sourceColumn = mkPos $ V.column pos_end
                                                    }

convert :: TestInfo' -> TestInfo
convert TestInfo'{..} = TestInfo { testMName    = testMName'
                                 , testLocStart = locToLoc name testLocStart'
                                 , testLocEnd   = locToLoc name testLocEnd'
                                 , testType     = testType'
                                 , testRunner   = Nothing
                                 , testStatus   = testStatus'
                                 , testTimSkip  = testTimSkip'
                                 }
  where
    mkName (Just n) = n
    mkName Nothing  = "convert: name_lost_in_conversion"
    name = mkName testMName'

type Skipped'= [SkippedTest']
data SkippedTest' = MkSkippedTest'
  { skipName'   :: Maybe String  -- ^ The name of the test (optional)
  , skipStatus' :: TestStatus    -- ^ The type of the test (pass/fail)
  , skipReason' :: String        -- ^ The reason why the test is broken (in quotes)
  , skipCode'   :: LSExp         -- ^ The exact string (single line)
                                 -- corresponding to the test (in quotes)
  }
  deriving (Show)

data Test'
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq' TestInfo' LSExp LSExp -- testeq( name, code ){ value }
  | TestVerify' TestInfo' LSExp       -- verify( name, pass/fail){ code }
  deriving (Show)

data TestInfo' =  -- Per-test info e.g.  verify(pass, ICFPEverify=skip){ ...code... }
                 -- The stuff in the parens is the TestInfo
  TestInfo'
    { testMName'    :: !(Maybe String)
    , testLocStart' :: !V.Loc
    , testLocEnd'   :: !V.Loc
    , testType'     :: !TestType                      -- Default test type
    , testStatus'   :: !TestStatus
    , testTimSkip'  :: !TimSkip                       -- skip (or error code) when converting to Tim's format
    }
    deriving (Show)

timTestInfo' :: V.Loc -> V.Loc -> T.Text -> TestInfo'
timTestInfo' strt end name = TestInfo'
  { testMName'    = Just $ T.unpack name
  , testLocStart' = strt
  , testLocEnd'   = end
  , testType'     = timTestType $ T.unpack name
  , testStatus'   = TS_Normal
  , testTimSkip'  = timSkip $ T.unpack name
  }

testSrc' :: Test' -> LSExp
testSrc' (TestEvalEq' _ e1 _) = e1
testSrc' (TestVerify' _ e)    = e

skipping' :: [Test'] -> Skipped' -> [Test']
skipping' tests skips = skip1 <$> tests
  where
    m          = M.fromList [ (skipCode' s, skipStatus' s) | s <- skips ]
    skip1 test = case M.lookup (testSrc' test) m of
                   Just status -> test `testWithStatus'` status
                   Nothing     -> test

testWithStatus' :: Test' -> TestStatus -> Test'
testWithStatus' (TestVerify' ti e)    status = TestVerify' (ti { testStatus' = status }) e
testWithStatus' (TestEvalEq' ti e e') status = TestEvalEq' (ti { testStatus' = status }) e e'

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
  , useLibParser   :: !Bool                -- use the verse-parser library
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
  , testExpr       :: !(Maybe String)      -- use this expression as a test
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

       ; useLibParser <- OA.switch $
                         OA.long "use-lib-parser" <>
                         OA.help "Use the verse-parser library"

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
                           OA.value 2 <>
                           OA.help "Verbosity of rewrite trace (0,1,2)"

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
                     OA.value 1000 <>  -- test M28Jul24-1 takes ages with 1000 steps
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
                           OA.help "succeeds{} is a no-op"

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
       ; return (TestFlags { .. }) }

testFlagsToFEFlags :: TestFlags -> FrontEnd.Flags
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

parseSkipped :: FilePath -> IO Skipped
parseSkipped fn = do
  exists <- doesFileExist fn
  if exists
    then parseDie pSkippedFile fn <$> readFile fn
    else pure mempty

pSkippedFile :: P [SkippedTest]
pSkippedFile = skip *> many pSkipped <* eof

{-
skip("reason"){ test }
broken("reason"){ test }
-}
pSkipped :: P SkippedTest
pSkipped = do
  status <- pSkipTestStatus
  (mname, reason) <- pParens pSkipInfo
  code   <- pExprSeq <* optional (pOp ";")
  pure (MkSkippedTest mname status reason code)

pSkipInfo :: P (Maybe String, String)
pSkipInfo = mkSkip <$> sepBy1 pStringLit (pOp ",")
  where
    mkSkip [a,b] = (Just a, b)
    mkSkip [b]   = (Nothing, b)
    mkSkip _     = undefined
  -- pParens (pSkipName OA.<|> pSkipAnon)
  -- where
  --   pSkipName = (\n r -> (Just n, r)) <$> (pStringLit <* pOp ",") <*> pStringLit
  --   pSkipAnon = (Nothing,) <$> pStringLit


pSkipTestStatus :: P TestStatus
pSkipTestStatus = do
  i <- pIdent
  case map toLower $ identString i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pSkipType"


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
      retCode TFail | Just err <- bad = err
                    | otherwise = "F00"
      retCode TPass | Just err <- bad = err
                    | otherwise = "S00"
      retCode TLoop = "S00"  -- What should this be?

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
        Blk es -> ppBlk es
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
                unBlk (Blk es) = es
                unBlk ee = [ee]

        _ -> error $ "ppTim: unimplemented " ++ take 100 (show expr)

timRename :: [(Src.Ident, String)]
timRename = [ (Src.Ident noLoc x, y) | (x, y) <-
  [ ("intAdd$", "operator'+'")
  , ("fail", ":false")
  ] ]
