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
                     , pString, pKeyword, many, lexeme, optional, string, skip, eof )
import FrontEnd.Prelude( findPrelude )

import Rules.Core             as Rules
import Rules.Verifier( verificationRules )
import TRS.Traced as TRS ( Traced, term, trace )
import TRS.Bind( bindList )

import Epic.Print hiding ( (<>) )

import Text.Megaparsec(getSourcePos, sourceLine, unPos)

import GHC.Stack( HasCallStack )

import Data.Char( toLower )
import Data.Maybe

import Control.Monad( unless, when )

import System.Exit( exitWith, ExitCode(..) )
import Text.Printf

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
  | TestVerify TestInfo SrcExpr     -- verify( name, pass/fail){ code }
  | TestTim { timTag :: TimTag              -- test(D00){ code }
            , timExpr :: SrcExpr }
  | TestTimCrash TimTag String             -- test(D00){ code-that-crashes-the-parser }
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
testInfo (TestVerify ti _)   = ti
testInfo (TestTim    ti _)   = timTestInfo ti
testInfo (TestTimCrash ti _) = timTestInfo ti

data TestInfo =  -- Per-test info e.g.  verify(pass, ICFPEverify=skip){ ...code... }
                 -- The stuff in the parens is the TestInfo
  TestInfo
    { testMName  :: !(Maybe String)
    , testLocn   :: !Loc
    , testType   :: !TestType                      -- Default test type
    , testExcn   :: ![((String, Bool), TestType)]  -- The bool indicates the the string is just a prefix
    }
    deriving (Show)

data TestType
  = TPass                 -- test should pass
  | TFail                 -- test should fail
  | TSkip                 -- test should be skipped
  | TBroken               -- test is currently broken (i.e., pass/fail is negated)
  deriving (Show, Eq)

testName :: TestInfo -> String
testName ti = fromMaybe ("L" ++ show (unPos (sourceLine (testLocn ti)))) (testMName ti)

data TestRes = Good | Bad | Excn | Skip
  deriving (Eq, Show)

-----------------------------------------------
--
--     Run tests
--     runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
--
-----------------------------------------------

runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
runTestFile tflg (fn, ts)
 = do { putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)
      ; let p = maybe (const True) (\ s t -> testName (testInfo t) == s) (onlyTest tflg)
      ; res :: [TestRes] <- mapM (runTest tflg) (filter p ts)

      ; let n_tests   = length res
            n_skipped = count Skip res
            n_passed  = count Good res
            n_failed  = count Bad  res
            n_excn    = count Excn res
      ; putStrLn ""
      ; putStrLn "------------ Overall summary ---------------------------"
      ; putStrLn $ "Number of tests: " ++ show n_tests
      ; when (n_passed > 0)  $ putStrLn $ printf "%5d passed" n_passed
      ; when (n_failed > 0)  $ putStrLn $ printf "%5d failed" n_failed
      ; when (n_excn > 0)    $ putStrLn $ printf "%5d threw an exception" n_excn
      ; when (n_skipped > 0) $ putStrLn $ printf "%5d skipped" n_skipped
      ; putStrLn "---------------------------------------------------------"
      ; unless (n_passed == n_tests) $ exitWith (ExitFailure 1) }
  where
    count :: TestRes -> [TestRes] -> Int
    count r rs = length (filter (== r) rs)


widthTestName :: Int
widthTestName = 10

widthFileName :: Int
widthFileName = 40


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
  { testMName = Nothing
  , testLocn = loc
  , testType = timTestType status
  , testExcn = []
  }

timTestType :: String -> TestType
timTestType "S00" = TPass
timTestType _     = TFail

----------------------------
runTest :: TestFlags -> Test -> IO TestRes
runTest tflg (TestTim    ts  e)
  = runTest tflg (TestVerify (timTestInfo ts) e)
-- TestVerify: we try to verify
--   verify(;){ check<succeeds>{e} }
runTest tflg test@(TestVerify _ e)
  = doTestCatchingExn tflg test e (Array [])
runTest tflg test@(TestEvalEq _ e1 e2)
  = doTestCatchingExn tflg test e1 e2
runTest _    (TestTimCrash _ _)
  = pure Excn


mkFlags :: TestFlags -> Bool -> Flags
mkFlags tflg add_verification
  = setPreludeFlag add_verification tflg $
    testFlagsToFlags tflg

----------------------------
data TestMode = TEval | TVerify Effect deriving (Eq, Show)


-- | `doTestCatchingExn` runs the actual test, catching any exceptions that are thrown during parsing, desugaring, or execution/verification
doTestCatchingExn :: (HasCallStack) => TestFlags -> Test -> SrcExpr -> SrcExpr -> IO TestRes
doTestCatchingExn tflg test p1 p2
  | typ == TSkip = do { when (noisy tflg) (putStrLn $ test_herald ++ " skipped")
                      ; pure Skip }
  | otherwise    = do { catch (doTest tflg test p1 p2)
                              (\e -> do { exn_handler e;  pure Excn} )
                      }
  where
    typ         = testType (testInfo test)
    test_herald = testHerald test
    exn_handler :: SomeException -> IO ()
    exn_handler e
      = -- unless (noError tflg) $
        do { putStrLn $ test_herald ++ " failure:"
           ; putStrLn "The expression";       ppIndent p1
           ; putStrLn "or the expression";    ppIndent p2
           ; putStrLn "caused an exception:"; print e
           ; putStrLn "" }


-- | `doTest` does the actual work of parsing, converting to core, and evaluating/verifying; each of
--    which can throw an exception.
doTest :: (HasCallStack) => TestFlags -> Test -> SrcExpr -> SrcExpr -> IO TestRes
doTest tflg test p1 p2 = do
  let add_verif   = addVerification test
  let flags       = mkFlags tflg    add_verif
  c1             <- wrapTopEffect test <$> srcToCore flags add_verif p1
  c2             <-                        srcToCore flags add_verif p2
  let (res1, tr1) = evalExpr tflg c1
  let (res2, tr2) = evalExpr tflg c2
  res <- checkResults tflg test (p1, res1, tr1) (p2, res2, tr2)
  -- Display the desugared output
  when (showDesugared tflg) $
    displayDoc (sep [text (testHerald test) <+> text "desugared:", pPrint c1])
  -- Display the trace if asked for, regardless of success/failure
  when (showTrace tflg) $
    do { putStrLn "Trace is:"; display tr1 }
  pure res


addVerification :: Test -> Bool
addVerification (TestEvalEq {}) = False
addVerification _               = True

-- | `checkResults` just compares the results of two evaluations and prints out the appropriate message,
--   it does _not_ throw or catch any exceptions.
checkResults :: TestFlags -> Test -> (SrcExpr, NormResult, Traced Expr) -> (SrcExpr, NormResult, Traced Expr) -> IO TestRes
-- Really we should check res2, tr2 as well, but they are always boring
checkResults tflg test (p1, res1, tr1) (p2, _res2, tr2)
  | res1 /= NormOK
  = do { putStrLn (test_herald ++ " " ++ what ++ " " ++ showNormResult res1)
       ; pure Bad }
  | equivValue v1 v2 == expectOK
  = do { success_handler; pure Good }
  | otherwise
  = do { failure_handler; pure Bad }
  where
    v1           = TRS.term tr1
    v2           = TRS.term tr2
    n_steps      = length (TRS.trace tr1)
    test_herald  = testHerald test
    typ          = testType (testInfo test)
    expectOK     = typ == TPass
    success_handler -- What to display if all is well
      = when (noisy tflg) $
          putStrLn $ test_herald ++ " Expected " ++ succ_or_fail expectOK
                                 ++ " in " ++ printf "%5d" n_steps ++ " steps"
    failure_handler -- What to display if test fails
      | TBroken <- typ
      = putStrLn $ test_herald ++ " Broken test now passes"
      | otherwise
      = do { putStrLn $ test_herald ++ " Unexpected " ++ succ_or_fail (not expectOK)
           ; unless (noError tflg) $
             do { putStrLn "-----------------------------------------------"
                ; putStrLn "The expression"; ppIndent p1
                ; putStrLn "evaluates to";   ppIndent v1
                ; putStrLn "while";          ppIndent p2
                ; putStrLn "evaluates to";   ppIndent v2
                ; putStrLn ""
                ; when (prettyShow v1 == prettyShow v2) $ do
                    putStrLn "The unpretty printed values are"
                    print v1
                    putStrLn "resp."
                    print v2 } }

    what = case test of
             TestVerify{}   -> "Verification"
             TestEvalEq{}   -> "Evaluation"
             TestTim{}      -> "TestTim"
             TestTimCrash{} -> "TestTimCrash"

    succ_or_fail True  = "success"
    succ_or_fail False = "failure"

-- | `wrapTopEffect` wraps the expression in a toplevel check<EFF>{e} where EFF is Succeeds or Decides depending on the test.
wrapTopEffect :: Test -> Expr -> Expr
wrapTopEffect (TestVerify {}) c = Rules.Verify (bindList [] ([], Rules.Check Succeeds c))
wrapTopEffect _               c = c


-- | Equivalence on values (or stuck expressions)
equivValue :: Rules.Expr -> Rules.Expr -> Bool
equivValue e1 e2 = Rules.norm e1 == Rules.norm e2

testHerald :: Test -> String
testHerald test = printf "%-*s %-*s" widthTestName test_nm widthFileName loc_str
  where
    test_nm  = fromMaybe "<anon>" (testMName ti)
    loc_str  = prettyShow (testLocn ti)
    ti       = testInfo test

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
  file <- readFile fn
  let tests = parseDie pTestFile fn file
  pure tests

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
    TestVerify tId <$> pBraces pExprSeq

pTimTest :: P Test
pTimTest =
  pKeyword "test" *> do
    TestTim <$> pParens pIdent <*> (pExprSeq <* optional (pOp ";"))

pTestInfo :: P TestInfo
pTestInfo = do
  loc <- getSourcePos
  let strOf (Src.Lit (LStr s)) = s
      strOf _ = undefined
  mname <- fmap strOf <$> optional (pString <* pOp ",")
  typ <- pTestType
  let
    pSys :: P String
    pSys = pIdent >>= \case Ident _ s -> pure s
    pSysWild :: P (String, Bool)
    pSysWild = (,) <$> pSys <*> (isJust <$> optional (lexeme (string "*")))
  excns <- many ((,) <$> (pOp "," *> pSysWild) <*> (pOp "=" *> pTestType))
  pure $ TestInfo mname loc typ excns

pTestType :: P TestType
pTestType = do
  i <- pIdent
  case map toLower $ unIdent i of
    "pass"    -> pure TPass
    "fail"    -> pure TFail
    "skip"    -> pure TSkip
    "broken"  -> pure TBroken
    _         -> fail "pTestType"


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
        <> OA.value 1000
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
