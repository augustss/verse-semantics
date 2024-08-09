{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE BlockArguments #-}
module Main(main) where

import Prelude

import FrontEnd.Desugar( desugar )
import FrontEnd.ToCore( convertToCore )
import FrontEnd.Flags
import FrontEnd.Expr as Src
import FrontEnd.Parse( P, parseDie, pFile, pOp, pIdent, pExprSeq, pBraces, pParens
                     , pString, pKeyword, many, optional, skip, eof )
import FrontEnd.Prelude( findPrelude )
import FrontEnd.Error

import Rules.Core             as Rules
import Rules.Verifier( verificationRules )
import TRS.Traced as TRS ( Traced, term, trace )
import TRS.Bind( bindList )

import Epic.Print hiding ( (<>) )

import Text.Megaparsec( getSourcePos, sourceName, sourceLine, unPos )

import GHC.Stack( HasCallStack )

import Data.List( isPrefixOf )
import Data.Char( toLower )
import Data.Maybe
import Control.Monad( unless, when )
import System.Directory( doesFileExist, removeFile )
import System.Exit( exitWith, ExitCode(..) )
import Text.Printf
import qualified Data.Map as M
import Control.Exception( catch, SomeException )
import qualified Options.Applicative as OA


-----------------------------------------------
--
--    Main progam
--
-----------------------------------------------

main :: IO ()
main = do
  do { tflg <- testArgs
     ; runTests tflg }

runTests :: TestFlags -> IO ()
runTests test_flags
  | parseOnly test_flags
  = mapM_ parseSourceOnly (fileNames test_flags)

  | Just expr_string <- testExpr test_flags
  , let fn = "<command-line>"
  = runTestFile test_flags (fn, parseDie pTestFile fn expr_string)

  | otherwise
  = mapM_ read_and_run (fileNames test_flags)
  where
    read_and_run :: FilePath -> IO ()
    read_and_run fn = do { tests <- readTests fn
                         ; runTestFile test_flags (fn, tests) }

-----------------------------------------------
--
--    Data types
--
--    Test, TestInfo, TestRes
--
-----------------------------------------------

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq TestInfo SrcExpr SrcExpr     -- testeq( name, code ){ value }
  | TestVerify TestInfo SrcExpr             -- verify( name, pass/fail){ code }
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
testInfo (TestVerify ti _)   = ti

testSrc :: Test -> SrcExpr
testSrc (TestEvalEq _ e1 _) = e1
testSrc (TestVerify _ e)    = e

data TestInfo =  -- Per-test info e.g.  verify(pass, ICFPEverify=skip){ ...code... }
                 -- The stuff in the parens is the TestInfo
  TestInfo
    { testMName    :: !(Maybe String)
    , testLocStart :: !Loc
    , testLocEnd   :: !Loc
    , testType     :: !TestType                      -- Default test type
    , testStatus   :: !TestStatus
    }
    deriving (Show)

data TestType = TPass | TFail | TLoop   -- Expected behaviour
  deriving (Eq)

instance Show TestType where
  show TPass = "pass"
  show TFail = "fail"
  show TLoop = "loop"

data TestStatus = TS_Normal
                | TS_Broken   -- Test is currently broken (i.e., pass/fail is negated)
                | TS_Skip     -- Test should be skipped
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
      TPass -> outcome == TO_Equal
      TFail -> outcome == TO_NotEqual
      TLoop -> outcome == TO_Abnormal NormExpired

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
       TPass -> outcome == TO_NotEqual
       _     -> False

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
 = do { putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)

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
           ; printSome failedWithLoop      unexpected "      %d went into an unexpected loop"
           ; printSome isBrokenPass        unexpected "      %d expected broken, but actually passed" }
      ; printNZ n_skipped  "%5d skipped"
      ; putStrLn "---------------------------------------------------------"
      ; unless (n_expected == n_tests) $ exitWith (ExitFailure 1) }
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

srcToCore :: Flags -> Bool -> SrcExpr -> IO Rules.Expr
srcToCore flags add_verification e
  = do { e1 :: SrcCore    <- FrontEnd.Desugar.desugar flags add_verification e
       ; e2 :: Rules.Expr <- FrontEnd.ToCore.convertToCore flags e1
       ; let e3 = Rules.prep e2
       ; return e3 }

evalExpr :: TestFlags -> Rules.Expr -> (NormResult, Traced Rules.Expr)
evalExpr flags e = Rules.normalize (maxSteps flags) verificationRules e

type TimTag = Src.Ident

timTestInfo :: TimTag -> TestInfo
timTestInfo (Ident loc status) = TestInfo
  { testMName    = Nothing
  , testLocStart = loc
  , testLocEnd   = loc
  , testType     = timTestType status
  , testStatus   = TS_Normal
  }

timTestType :: String -> TestType
timTestType "S00" = TPass
timTestType _     = TFail

----------------------------
runTest :: TestFlags -> Test -> IO TestRes
runTest tflg test@(TestVerify _ e)     = doTestCatchingExn tflg test e (Array [])
runTest tflg test@(TestEvalEq _ e1 e2) = doTestCatchingExn tflg test e1 e2

mkFlags :: TestFlags -> Bool -> Flags
mkFlags tflg add_verification
  = setPreludeFlag add_verification tflg $
    testFlagsToFlags tflg

----------------------------
data TestMode = TEval | TVerify Effect deriving (Eq, Show)


-- | `doTestCatchingExn` runs the actual test, catching any exceptions that are thrown during parsing, desugaring, or execution/verification
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
           ; putStrLn "The expression";       ppIndent p1
           ; putStrLn "or the expression";    ppIndent p2
           ; putStrLn "caused an exception:"; print e
           ; putStrLn "" }


-- | `doTest` does the actual work of parsing, converting to core, and evaluating/verifying; each of
--    which can throw an exception.
doTest :: (HasCallStack) => TestFlags -> Test -> SrcExpr -> SrcExpr -> IO TestRes
doTest tflg test src1 src2 = do
  do { let flags     = mkFlags tflg add_verif
           add_verif = desugarForVerification test

     ; core1 <- wrapTest add_verif <$> srcToCore flags add_verif src1

     -- Display the desugared output
     ; when (showDesugared tflg) $
       displayDoc (sep [text (testHerald test) <+> text "desugared:", pPrint core1])

     ; mb_core2 <- case src2 of
                     Variable (Ident _ "wrong") -> pure Nothing
                     _       -> do { core2 <- srcToCore flags False src2
                                   ; pure (Just core2) }

     ; res <- checkResults tflg test (src1, core1) (src2, mb_core2)

     ; pure res }

-- | `wrapTopEffect` wraps the expression in a toplevel verify if necessary
wrapTest :: Bool -> Expr -> Expr
wrapTest wrap_me core
  | wrap_me   = Rules.Verify (bindList [] ([], Rules.Check Succeeds core))
  | otherwise = core

desugarForVerification :: Test -> Bool
desugarForVerification TestEvalEq{}   = False
desugarForVerification TestVerify{}   = True

-- | `checkResults` just compares the results of two evaluations and prints out the appropriate message,
--   it does _not_ throw or catch any exceptions.
checkResults :: TestFlags -> Test -> (SrcExpr, Expr) -> (SrcExpr, Maybe Expr) -> IO TestRes
checkResults tflg test (src1, core1) (src2, mb_core2)
  = do { show_result

       -- Display the trace if asked for, regardless of success/failure
       ; when (showTrace tflg) $
         do { putStrLn "Trace is:"; display tr1 }

       ; pure (TestRes { tr_info = info, tr_outcome = outcome }) }
  where
    (res1, tr1)  = evalExpr tflg core1
    v1           = TRS.term tr1
    n_steps      = length (TRS.trace tr1)
    mb_v2        = fmap (TRS.term . snd . evalExpr tflg) mb_core2
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

    show_result :: IO ()
    show_result
      | expectedTestRes test_res -- What to display if all is well
      = when (noisy tflg) $
        putStrLn $ test_herald ++ "Expected " ++ succ_what
                               ++ " in " ++ printf "%5d" n_steps ++ " steps"

      | TS_Broken <- status
      = putStrLn $ test_herald ++ "Broken test now passes"

      | TO_Abnormal NormInvalid <- outcome
      = putStrLn $ test_herald ++ "Crash: rewrite yields invalid result"

      | otherwise   -- TS_Normal
      = do { putStrLn $ test_herald ++ "Unexpected " ++ fail_what
           ; when (logUnexpected tflg) $ logUnexpectedToFile test_res
           ; unless (noError tflg) $
             do { putStrLn "-----------------------------------------------"
                ; putStrLn "The expression"; ppIndent src1
                ; putStrLn "evaluates to";   ppIndent v1
                ; putStrLn "while";          ppIndent src2
                ; putStrLn "evaluates to";   ppIndent mb_v2 } }

    succ_what = case (status, typ) of
             (TS_Broken,_) -> "broken "
             (_, TPass)    -> "success"
             (_, TFail)    -> "failure"
             (_, TLoop)    -> "loop   "

    fail_what = case typ of
             TPass -> "failure"
             TFail -> "success"
             _     -> errorMessage "fail_what"


-- | Equivalence on values (or stuck expressions)
-- e2=Nothing <=> e2=WRONG <=> e1 gets stuck without reaching a value
equivValue :: Rules.Expr -> Maybe Rules.Expr -> Bool
equivValue e1 (Just e2) = Rules.norm e1 == Rules.norm e2
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
pTest = pTestEq OA.<|> pTestVerify OA.<|> pTimTest

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
    src <- pExprSeq <* optional (pOp ";")
    locEnd <- getSourcePos
    pure $ TestVerify (tId { testLocEnd = locEnd }) src

pTimTest :: P Test
pTimTest =
  pKeyword "test" *> do
    tag <- pParens pIdent
    src <- pExprSeq <* optional (pOp ";")
    locEnd <- getSourcePos
    let ti = (timTestInfo tag) { testLocEnd = locEnd }
    pure $ TestVerify ti src

pStringLit :: P String
pStringLit = strOf <$> pString
  where
    strOf (Src.Lit (LStr s)) = s
    strOf _ = undefined

pTestInfo :: P TestInfo
pTestInfo = do
  loc   <- getSourcePos
  mname <- optional (pStringLit <* pOp ",")
  typ   <- pTestType
  stat  <- (pOp "," *> pTestStatus) OA.<|> pure TS_Normal
  pure (TestInfo { testMName = mname, testLocStart = loc, testLocEnd = loc
                 , testType = typ, testStatus = stat })

pTestType :: P TestType
pTestType = do
  i <- pIdent
  case map toLower $ identString i of
    "pass"    -> pure TPass
    "fail"    -> pure TFail
    "loop"    -> pure TLoop
    _         -> fail "pTestType"

pTestStatus :: P TestStatus
pTestStatus = do
  i <- pIdent
  case map toLower $ identString i of
    "skip"    -> pure TS_Skip
    "broken"  -> pure TS_Broken
    _         -> fail "pTestStatus"

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
  , logUnexpected  :: !Bool                -- Log unexpected results
  , onlyTest       :: !(Maybe String)      -- run only this test
  , testExpr       :: !(Maybe String)      -- use this expression as a test
  , maxSteps       :: !Int                 -- max number of rewrite steps
  , maxNormSteps   :: !Int                 -- max number of normalization steps
  , ignoreFuelStop :: !Bool                -- ignore running out of fuel
  , assumeVerified :: !Bool                -- turn succeeds into a no-op
  , timRun         :: !Bool                -- run Tim's verifier tests
  , timVerify      :: !Bool                -- verify Tim's verifier tests
  , showDesugared  :: !Bool                -- show desugared version just before evaluation
  , preludeEval    :: !String              -- use this prelude in TestEval
  , preludeVerify  :: !String              -- use this prelude in TestVerify
  , desugarRules   :: !Desugar             -- desugaring rules
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
testFlags = TestFlags
  <$> OA.switch
      (  OA.long "dfs"
      <> OA.help "Only find one normal form"
      )
  <*> OA.switch
      (  OA.long "split"
      <> OA.help "Use split"
      )
  <*> OA.switch
      (  OA.long "parse-only"
      <> OA.help "Just do parsing"
      )
  <*> OA.switch
      (  OA.long "simplify"
      <> OA.help "Use simplifier"
      )
  <*> OA.switch
      (  OA.long "no-under-lambda"
      <> OA.help "do not reduce under lambda"
      )
  <*> OA.switch
      (  OA.long "quiet"
      <> OA.help "Be less noisy"
      )
  <*> OA.switch
      (  OA.long "verbose"
      <> OA.help "Be more noisy"
      )
  <*> OA.switch
      (  OA.long "no-error"
      <> OA.help "Do not show error message on failure"
      )
  <*> OA.switch
      (  OA.long "post-process"
      <> OA.help "Do post processing"
      )
  <*> OA.switch
      (  OA.long "summary"
      <> OA.help "Produce test summary"
      )
  <*> OA.switch
      (  OA.long "trace"
      <> OA.help "Print rewrite traces"
      )
  <*> OA.switch
      (  OA.long "log-unexpected"
      <> OA.help "log unexpected results to file"
      )
  <*> OA.optional (OA.strOption
         ( OA.long "only-test"
        <> OA.metavar "TEST"
        <> OA.help "Run only test named TEST" ))
  <*> OA.optional (OA.strOption
         ( OA.long "expr"
        <> OA.metavar "EXPR"
        <> OA.help "Use EXPR as a test" ))
  <*> OA.option OA.auto
         ( OA.long "max-steps"
        <> OA.short 'm'
        <> OA.metavar "NUM"
        <> OA.value 500   -- test M28Jul24-1 takes ages with 1000 steps
        <> OA.help "Maximum number of rewrite steps" )
  <*> OA.option OA.auto
         ( OA.long "max-norm-steps"
        <> OA.metavar "NUM"
        <> OA.value 10000
        <> OA.help "Maximum number of normalization steps" )
  <*> OA.switch
         ( OA.long "ignore-fuel-stop"
        <> OA.help "Ignore running out of fuel" )
  <*> OA.switch
         ( OA.long "assume-verified"
        <> OA.help "succeeds{} is a no-op" )
  <*> OA.switch
         ( OA.long "tim-run"
        <> OA.help "run a Tim test" )
  <*> OA.switch
         ( OA.long "tim-verify"
        <> OA.help "verify Tim test" )
  <*> OA.switch
         ( OA.long "show-desugared"
        <> OA.help "show desugared version" )
  <*> OA.strOption
         ( OA.long "eval-prelude"
        <> OA.metavar "NAME"
        <> OA.value "miniprelude"
        <> OA.help "use the given prelude for evaluation tests" )
  <*> OA.strOption
         ( OA.long "verify-prelude"
        <> OA.metavar "NAME"
        <> OA.value "miniverifyprelude"
        <> OA.help "use the given prelude for verification tests" )
  <*> OA.option OA.auto
         ( OA.long "desugar"
        <> OA.metavar "Name"
        <> OA.value (fDesugar defaultFlags)
        <> OA.help "Desugaring rules to use" )
  <*> OA.many (OA.argument OA.str (OA.metavar "FILES..."))

testFlagsToFlags :: TestFlags -> Flags
testFlagsToFlags t =
  let flags = defaultFlags
  in  flags{ fSplit = split t, fSimplify = simplify t,
             fTrace = showTrace t,
             fDfs = dfs t, fPostProcess = postProc t,
             fUnderLambda = not (noUnderLam t),
             fRewriteSteps = maxSteps t,
             fNoFuelStop = ignoreFuelStop t,
             fAssumeVerified = assumeVerified t,
             fTraceDesugar = verbose t,
             fDesugar = desugarRules t
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
  { skipStatus :: TestStatus    -- ^ The type of the test (pass/fail)
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
  reason <- pParens pStringLit
  -- code   <- pBraces pExprSeq
  code   <- pExprSeq <* optional (pOp ";")
  pure (MkSkippedTest status reason code)

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
