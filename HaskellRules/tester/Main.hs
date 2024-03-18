{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import GHC.Stack
import Options.Applicative
import Text.Printf
import System.Exit
import System.IO(hFlush, stdout)

import Text.Megaparsec(getSourcePos, sourceLine, unPos)

import FrontEnd.Expr
import FrontEnd.Flags
import FrontEnd.Parse hiding (many)
import FrontEnd.ParseCore
import FrontEnd.Prelude
import FrontEnd.TRSAdapter(coreToTrs)
import Epic.Print (Pretty, prettyShow)
import FrontEnd.Desugar(desugar)
import FrontEnd.Run(run, runM, everySystem, findSystem, blockSystem, adjustFlags)
import Rules.Core(RuleEnv(..))
import qualified Rules.Core as R
import Rules.Equiv
import Rules.Systems(ESystem, TRSystem(..))
import Rules.Verifier(wrapAssert, verifyM)
import TRS.Traced(Traced, showTrace)
import qualified TRS.Tarjan as T
--------------

data TestFlags = TestFlags
  { dfs            :: !Bool                -- just find one normal form
--  , denSem         :: !Bool                -- evaluate with denotational semantics
  , split          :: !Bool                -- use split
  , parse          :: !Bool                -- parse only
  , simplify       :: !Bool                -- use simplifier
--  , alias          :: !Bool                -- eliminate aliases
--  , unifyEq        :: !Bool                -- unify as equals under barrier
  , noUnderLam     :: !Bool                -- do not reduce under lambda
  , quiet          :: !Bool                -- Less noisy
  , verbose        :: !Bool                -- More noisy
  , noError        :: !Bool                -- Don't show error message
  , postProc       :: !Bool                -- Post processing
  , system         :: !ESystem             -- rule system
  , summary        :: !Bool                -- produce a summary
  , trace          :: !Bool                -- Show traces
  , allRules       :: !Bool                -- test with all rule systems
  , onlyTest       :: !(Maybe String)      -- run only this test
  , testExpr       :: !(Maybe String)      -- use this expression as a test
  , maxSteps       :: !Int                 -- max number of rewrite steps
  , maxNormSteps   :: !Int                 -- max number of normalization steps
  , ignoreFuelStop :: !Bool                -- ignore running out of fuel
  , assumeVerified :: !Bool                -- turn succeeds into a no-op
  , timRun         :: !Bool                -- run Tim's verifier tests
  , timVerify      :: !Bool                -- verify Tim's verifier tests
  , showDesugared  :: !Bool                -- show desugared version just before evaluation
  , prelude        :: !(Maybe String)      -- use this prelude
  , desugarRules   :: !Desugar             -- desugaring rules
  , fileNames      :: ![FilePath]          -- input files
  }
  deriving (Show)

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq TestInfo Expr Expr
  | TestCoreEq TestInfo Core Core
  | TestVerify TestInfo Expr
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
testInfo (TestCoreEq ti _ _) = ti
testInfo (TestVerify   ti _) = ti

data TestInfo = TestInfo
  { testMName  :: !(Maybe String)
  , testLocn   :: !Loc
  , testType   :: !TestType                      -- default test type
  , testExcn   :: ![((String, Bool), TestType)]  -- the bool indicates the the string is just a prefix
  }
  deriving (Show)

data TestType
  = TPass                 -- test should pass
  | TFail                 -- test should fail
  | TSkip                 -- test should be skipped
  | TBroken               -- test is currently broken (i.e., pass/fail is negated)
  deriving (Show, Eq)

pTestInfo :: P TestInfo
pTestInfo = do
  loc <- getSourcePos
  let strOf (Lit (LitStr s)) = s
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

testName :: TestInfo -> String
testName ti = fromMaybe ("L" ++ show (unPos (sourceLine (testLocn ti)))) (testMName ti)

pTestType :: P TestType
pTestType = do
  i <- pIdent
  case map toLower $ unIdent i of
    "pass"    -> pure TPass
    "fail"    -> pure TFail
    "skip"    -> pure TSkip
    "broken"  -> pure TBroken
    _         -> fail "pTestType"

-- Parse an expression evaluation equality test
pTestEq :: P Test
pTestEq =
  pKeyword "testeq" *> do
    tId <- pParens pTestInfo
    TestEvalEq tId <$> pBraces pExprSeq <*> pBraces pExprSeq

-- Parse a core expression evaluation equality test
pTestCEq :: P Test
pTestCEq =
  pKeyword "testceq" *> do
    tId <- pParens pTestInfo
    TestCoreEq tId <$> pBraces pCore    <*> pBraces pCore

-- Parse an expression verification test
pTestVerify :: P Test
pTestVerify =
  pKeyword "verify" *> do
    tId <- pParens pTestInfo
    TestVerify tId <$> pBraces pExprSeq

-- Parse a test
pTest :: P Test
pTest = pTestEq <|> pTestCEq <|> pTestVerify

-- Parse a file of tests
pTestFile :: P [Test]
pTestFile = skip *> many pTest <* eof

readTests :: FilePath -> IO [Test]
readTests fn = do
  file <- readFile fn
  let tests = parseDie pTestFile fn file
  pure tests

------------

data TestRes = Good | Bad | Many | None | Excn | Skip
  deriving (Eq, Show)

assertEquivE :: HasCallStack => TestInfo -> TestFlags -> Expr -> Expr -> IO TestRes
assertEquivE ti flg e1 e2 = assertEquiv ti flg (e1, toCore e1) (e2, toCore e2)
  where toCore = desugar flags
        flags = testFlagsToFlags flg

assertEquivC :: HasCallStack => TestInfo -> TestFlags -> Core -> Core -> IO TestRes
assertEquivC ti flg e1 e2  = assertEquiv ti flg (e1, e1) (e2, e2)

assertEquiv :: (HasCallStack, Pretty a) => TestInfo -> TestFlags -> (a, Core) -> (a, Core) -> IO TestRes
assertEquiv ti tflg (p1, c1) (p2, c2) | typ == TSkip = do
  when noisy $
    putStrLn $ pos ++ " skipped"
  pure Skip
                                      | otherwise = do
  when showD $
    putStrLn $ "desugared:\n" ++ prettyShow c1

  let expectOK = typ == TPass
  let vs1 = runM flg sys c1  -- May return multiple answers
  let v2  = run flg sys c2   -- Returns just one

  catch (
    case vs1 of
      [] -> do
        when (not (noError tflg)) $ do
          putStrLn $ pos ++ " max rewrite steps exceeded"
        pure None
      vs@(_:_:_) -> do
        when (not (noError tflg)) $ do
          putStrLn $ pos ++ " the expression evaluated to multiple values:"
          mapM_ (putStrLn . (++ "-----") . unlines . map ("   " ++) . lines . prettyShow) vs
          putStrLn ""
        pure Many
      [v1] ->
       if (equivValue sys v1 v2) == expectOK
        then do
            when noisy $
              putStrLn $ pos ++ if expectOK then " success!" else " failure, expected"
            pure Good
        else do
            when (not (noError tflg)) $
             if expectOK
              then do
                putStrLn $ pos ++ " failure:"
                putStrLn "The expression"
                ppi p1
                putStrLn "evaluates to"
                ppi v1
                putStrLn "but"
                ppi p2
                putStrLn "evaluates to"
                ppi v2
                putStrLn ""
                when (prettyShow v1 == prettyShow v2) $ do
                    putStrLn "The unpretty printed values are"
                    print v1
                    putStrLn "resp."
                    print v2
                --undefined
              else do
                putStrLn pos
                when (typ == TBroken) $
                  putStrLn " broken test has"
                putStrLn " unexpected success"
            pure Bad
      ) (\e -> do
           when (not (noError tflg)) $ do
            putStrLn $ pos ++ " failure:"
            putStrLn "The expression"
            ppi p1
            putStrLn "or the expression"
            ppi p2
            putStrLn "caused an exception:"
            print (e :: SomeException)
            putStrLn ""
            --undefined
           pure Excn
      )
  where
    loc = testLocn ti
    noisy = not (quiet tflg)
    showD = showDesugared tflg
    pos = prettyShow loc ++ maybe "" ((", "++) . show) (testMName ti)
    sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
    typ = maybe (testType ti) snd $ find (\ (s,_) -> match s (sname sys)) (testExcn ti)
    ppi x = putStrLn . unlines . map ("   " ++) . lines . prettyShow $ x
    flg = testFlagsToFlags tflg
    match (n, w) m = map toLower n `tst` map toLower m
      where tst = if w then isPrefixOf else (==)

-- | Equivalence on values (or stuck expressions)
equivValue :: ESystem -> Core -> Core -> Bool
--equivValue sys e1 e2 | sname sys == "iblock" = e1 == e2  -- XXX temporary hack
equivValue sys e1 e2 =
  coreToTrs e1 == coreToTrs e2 ||        -- fast test first
  sname sys /= "iblock" &&               -- cannot convert to core
    equiv sys (coreToTrs e1) (coreToTrs e2)

--------------

data VerifyResult
  = VerifyError String
  | VerifyFail (Traced R.Expr)
  | VerifySuccess (Traced R.Expr)
  | VerifyTimeout (Traced R.Expr)
  deriving (Show)

verifyIt :: TestFlags -> Expr -> IO VerifyResult
verifyIt tflg e = do
  let flags = (testFlagsToFlags tflg){ fVerify = True, fSplit = False }
      e' = (if True then wrapAssert else id) . preProcess sys (ruleEnv sys) . coreToTrs . desugar flags $ e
      sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
      vres = verifyM sys e'
  eres <- Control.Exception.try (evaluate (seq (vres==vres) vres))
  pure $ case eres of
           Left err                    -> VerifyError (show (err :: SomeException))
           Right (T.Timeout (_, trc))    -> VerifyTimeout trc -- Error "time-out, use --max-norm-steps=N to change"
           Right (T.Finish (True, trc))  -> VerifySuccess trc
           Right (T.Finish (False, trc)) -> VerifyFail trc

assertVerify :: HasCallStack => TestInfo -> TestFlags -> Expr -> IO TestRes
assertVerify ti tflg e | typ == TSkip = do
  when noisy $
    putStrLn $ pos ++ " skipped"
  pure Skip
                       | otherwise = do
  let flags = (testFlagsToFlags tflg){ fVerify = True, fSplit = False }
      shouldVerify = if typ == TBroken then testType ti == TFail else typ == TPass

  res <- verifyIt tflg e

  let message (timeout, done, trc) = do
        when (fTrace flags) $ do
          putStrLn "Verification trace:"
          putStrLn $ unlines $ showTrace trc

        if timeout then do
           -- when noisy $
           putStrLn $ pos ++ " " ++ "time-out, use --max-norm-steps=N to change"
           pure None
         else
         if done == shouldVerify then do
           when noisy $
             putStrLn $ pos ++ " " ++ (if done then "    verified" else "not verified") ++ ", expected"
           pure Good
          else do
           putStrLn $ pos ++ " " ++ (if done then "    verified" else "not verified") ++ ", unexpected" ++
                      (if typ == TBroken then ", marked as broken" else "")
           pure Bad

  case res of
    VerifyError msg -> do
      putStrLn $ pos ++ " " ++ msg
      pure Excn
    VerifyFail trc -> message (False, False, trc)
    VerifySuccess trc -> message (False, True, trc)
    VerifyTimeout trc -> message (True, False, trc)

 where
    loc = testLocn ti
    noisy = not (quiet tflg)
    pos = prettyShow loc ++ maybe "" ((", "++) . show) (testMName ti)
    typ = maybe (testType ti) snd $ find (\ (s,_) -> match s (sname sys)) (testExcn ti)
    sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
    match (n, w) m = map toLower n `tst` map toLower m
      where tst = if w then isPrefixOf else (==)

--------------

runTest :: TestFlags -> Test -> IO TestRes
runTest tflg (TestEvalEq n e1 e2) = assertEquivE n tflg e1 e2
runTest tflg (TestCoreEq n e1 e2) = assertEquivC n tflg e1 e2
runTest tflg (TestVerify n e)     = assertVerify n tflg e

runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
runTestFile tflg (fn, ts) = do
  let allSys | allRules tflg = everySystem
             | otherwise = [system tflg]
  if summary tflg then
    runTestSummary tflg allSys ts
   else do
    putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)
    oks <- mapM (\ sys -> runTestFileSys tflg{system=sys} ts) allSys
    unless (and oks) $
      exitWith (ExitFailure 1)

runTestSummary :: TestFlags -> [ESystem] -> [Test] -> IO ()
runTestSummary atflg allSys tests = do
  let tflg = atflg{ noError = True }
  mapM_ (\ s -> printf "%-10s %s\n" (sname s) (description s)) allSys
  putStrLn "OK=success; nonc=non-confluent; BAD=wrong result; skip=test skipped; t.o.=time-out; excn=exception thrown"
  putStrLn $ testSummaryHeader allSys
  forM_ tests $ \ test -> do
    printf  "%-*s" widthTestName (testName (testInfo test))
    hFlush stdout
    forM_ allSys $ \ sys -> do
      r <- runTest tflg{system=sys} test
      printf " %*s" widthSysName (showRes r)
      hFlush stdout
    putStrLn ""

testSummaryHeader :: [ESystem] -> String
testSummaryHeader = (printf "%-*s" widthTestName "" ++) . concat . map (printf " %*s" widthSysName . sname)

showRes :: TestRes -> String
showRes Good = "OK"
showRes Bad  = "BAD"
showRes None = "t.o."
showRes Many = "nonc"
showRes Excn = "excn"
showRes Skip = "skip"

runTestFileSys :: TestFlags -> [Test] -> IO Bool
runTestFileSys tflg ts = do
  let p = maybe (const True) (\ s t -> testName (testInfo t) == s) (onlyTest tflg)
  res <- mapM (runTest tflg) (filter p ts)
  let ok = all (`elem` [Good, Skip]) res
  putStrLn $ sname (system tflg) ++ " " ++ if ok then "SUCCESS" else "FAILURE"
  pure ok

widthTestName :: Int
widthTestName = 10

widthSysName :: Int
widthSysName = 6

{-
test :: String -> IO ()
test n = runTestFile tflg verseTest
  where tflg = TestFlags { dfs=False, split=True, noInline = False
                         , parse=False, simplify=False, eval=False, quiet=False, fileNames=[]
                         , system = either error Just $ lookupSystem n
                         }
-}

-- Just parse
ptest :: FilePath -> IO ()
ptest fn = do
  file <- readFile fn
  let e = parseDie pFile fn file
  if e == e then putStrLn $ "parsed " ++ fn else undefined
  putStrLn "SUCCESS"
  pure ()

testFlags :: Parser TestFlags
testFlags = TestFlags
  <$> switch
      (  long "dfs"
      <> help "Only find one normal form"
      )
{-
  <*> switch
      (  long "densem"
      <> help "Use dontational semantics"
      )
-}
  <*> switch
      (  long "split"
      <> help "Use split"
      )
  <*> switch
      (  long "parse"
      <> help "Just do parsing"
      )
  <*> switch
      (  long "simplify"
      <> help "Use simplifier"
      )
{-
  <*> switch
      (  long "alias"
      <> help "Eliminate aliases"
      )
  <*> switch
      (  long "unify-equals"
      <> help "unify as equals under barrier"
      )
-}
  <*> switch
      (  long "no-under-lambda"
      <> help "do not reduce under lambda"
      )
{-
  <*> switch
      (  long "eval"
      <> help "Use fast evaluator"
      )
-}
  <*> switch
      (  long "quiet"
      <> help "Be less noisy"
      )
  <*> switch
      (  long "verbose"
      <> help "Be more noisy"
      )
  <*> switch
      (  long "no-error"
      <> help "Do not show error message on failure"
      )
  <*> switch
      (  long "post-process"
      <> help "Do post processing"
      )
  <*> option (eitherReader findSystem)
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> value blockSystem
        <> help "Use rule system NAME" )
  <*> switch
      (  long "summary"
      <> help "Produce test summary"
      )
  <*> switch
      (  long "trace"
      <> help "Print rewrite traces"
      )
  <*> switch
      (  long "all-rules"
      <> help "Test with all rule systems"
      )
{-
  <*> switch
      (  long "refimpl"
      <> help "Use reference implementation"
      )
-}
  <*> optional (strOption
         ( long "only-test"
        <> metavar "TEST"
        <> help "Run only test named TEST" ))
  <*> optional (strOption
         ( long "expr"
        <> metavar "EXPR"
        <> help "Use EXPR as a test" ))
  <*> option auto
         ( long "max-steps"
        <> short 'm'
        <> metavar "NUM"
        <> value 1000
        <> help "Maximum number of rewrite steps" )
  <*> option auto
         ( long "max-norm-steps"
        <> metavar "NUM"
        <> value 10000
        <> help "Maximum number of normalization steps" )
  <*> switch
         ( long "ignore-fuel-stop"
        <> help "Ignore running out of fuel" )
  <*> switch
         ( long "assume-verified"
        <> help "succeeds{} is a no-op" )
  <*> switch
         ( long "tim-run"
        <> help "run a Tim test" )
  <*> switch
         ( long "tim-verify"
        <> help "verify Tim test" )
  <*> switch
         ( long "show-desugared"
        <> help "show desugared version" )
  <*> optional (strOption
         ( long "prelude"
        <> metavar "NAME"
        <> help "use the given prelude" ))
  <*> option auto
         ( long "desugar"
        <> metavar "Name"
        <> value (fDesugar defaultFlags)
        <> help "Desugaring rules to use" )
  <*> many (argument str (metavar "FILES..."))

testFlagsToFlags :: TestFlags -> Flags
testFlagsToFlags t =
  let flags = adjustFlags (system t) defaultFlags
  in  flags{ fSplit = split t, fSimplify = simplify t,
             fTrace = trace t,
             fDfs = dfs t, fPostProcess = postProc t,
             fUnderLambda = not (noUnderLam t),
             fRewriteSteps = maxSteps t,
             fNoFuelStop = ignoreFuelStop t,
             fAssumeVerified = assumeVerified t,
             fPrelude = maybe (fPrelude flags) (either error id . findPrelude) (prelude t),
             fDesugar = desugarRules t
           }
main :: IO ()
main = do
  tflg <- testArgs
  let fns = fileNames tflg
  if parse tflg then
    mapM_ ptest fns
   else
    case testExpr tflg of
      Nothing ->
        if timRun tflg || timVerify tflg then
          mapM_ (timTest tflg) fns
        else
          mapM_ (\ fn -> do ts <- readTests fn; runTestFile tflg (fn, ts)) fns
      Just s -> runTestFile tflg (fn, parseDie pTestFile fn s)  where fn = "<command-line>"

testArgs :: IO TestFlags
testArgs = do
  let prf = prefs disambiguate
  t <- customExecParser prf $ info (testFlags <**> helper)
             ( fullDesc
            <> progDesc "Test Verse rules"
            <> header "tests - testing Verse rules"
             )
  let t' =
        case fileNames t of
          [] -> t{ fileNames = [if parse t then test1 else verseTest] }
          _  -> t
  pure t'

verseTest :: FilePath
verseTest = "tests.versetest"

test1 :: FilePath
test1     = "test1.verse"

------------------

data TimTest = TimTest { timTag :: Ident, timExpr :: Expr }
  deriving (Show)

timTestName :: TimTest -> String
timTestName test = "L" ++ show (unPos (sourceLine loc))
  where Ident loc _ = timTag test

timTest :: TestFlags -> FilePath -> IO ()
timTest tflg fn = do
  file <- readFile fn
  let tests = {- take 1000 $ -} parseDie pTimTestFile fn file
  putStrLn $ "Flags" ++ show tflg
  putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)
  putStrLn $ "Number of tests: " ++ show (length tests)
  status  <- mconcat <$> mapM (runTimTest tflg) (take 1000000 tests)
  let badFail = sBadFail status
  let badPass = sBadPass status
  printf "%5d skipped\n" (sSkip status)
  printf "%5d OK\n"      (sOK   status)
  printf "%5d bad (fail=%d, pass=%d)\n" (badFail + badPass) badFail badPass
  printf "%5d died\n"    (sDied status)
  printf "%5d unimplemented\n"    (sNotYet status)

pTimTestFile :: P [TimTest]
pTimTestFile = skip *> many pTimTest <* eof

pTimTest :: P TimTest
pTimTest =
  pKeyword "test" *> do
    TimTest <$> pParens pIdent <*> (pBlockM <* optional (pOp ";"))


-- runTimTest :: TestFlags -> TimTest -> IO (Int, Int, Int, Int)
runTimTest :: TestFlags -> TimTest -> IO TimStatus
runTimTest tflg test | Just s <- onlyTest tflg, s /= timTestName test = pure mempty
runTimTest tflg test | timRun tflg = do
  let sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
      flg = (testFlagsToFlags tflg) { fNoWarn = True }
      tag = timTag test
      Ident loc stag = tag
  putStr $ prettyShow loc ++ ": " ++ show tag ++ " "

  if take 1 stag `notElem` ["S", "F", "N"] then
    -- Fast path for unknown tests
    do putStrLn "skip"; pure (mempty { sSkip = 1})
   else do
    tres <- tryResult tflg $ run flg sys $ desugar flg $ timExpr test
    case take 1 stag of
      "S" -> case tres of
               ResOK x | x /= Fail -> do putStrLn "pass, OK";  pure (mempty {sOK = 1})
                       | otherwise -> do putStrLn "fail, bad"; pure (mempty {sBadFail = 1})
               _                   -> do putStrLn "exception"; pure (mempty {sDied = 1})
      "F" -> case tres of
               ResOK x | x == Fail -> do putStrLn "fail, OK";  pure (mempty {sOK = 1})
                       | otherwise -> do putStrLn "pass, bad"; pure (mempty {sBadPass = 1})
               _                   -> do putStrLn "exception"; pure (mempty {sDied = 1})
      "N" -> case tres of
               ResOK _             -> do putStrLn "pass, bad"; pure (mempty {sBadPass = 1})
               Undefined           -> do putStrLn "err,  OK";  pure (mempty {sOK = 1})
               Shadowing           -> do putStrLn "err,  OK";  pure (mempty {sOK = 1})
               _                   -> do putStrLn "exception"; pure (mempty {sDied = 1})
      _                            -> undefined

runTimTest tflg test | timVerify tflg = do
  let flags = (testFlagsToFlags tflg){ fVerify = True, fSplit = False, fNoWarn = True  }
      e' = (if True then wrapAssert else id) . preProcess sys (ruleEnv sys) . coreToTrs . desugar flags . timExpr $ test
      sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
      tag = timTag test
      Ident loc stag = tag
  -- putStrLn ("TRACE: Tim-Test " ++ systemDescr sys ++ " e' = " ++ prettyShow e')
  tres <- tryResult tflg $ verifyM sys e'
  putStr $ prettyShow loc ++ ": " ++ show tag ++ " "
  if take 1 stag == "Q" then
    do putStrLn "unimplemented"; pure (mempty {sNotYet = 1})
  else if take 1 stag `notElem` ("S" : verifierErrorCodes) then
    do putStrLn "skip";          pure (mempty {sSkip = 1})
   else do
    let disp trc =
          when (trace tflg) $
            putStrLn $ unlines $ showTrace trc
    when (verbose tflg) $
      putStrLn $ "test expr: " ++ prettyShow (timExpr test)
    case take 1 stag of
      "S" -> case tres of
               ResOK (T.Timeout _)         -> do putStrLn "timeout";     pure (mempty {sDied = 1})
               ResOK (T.Finish (True, _))  -> do putStrLn "pass, OK";    pure (mempty {sOK = 1})
               ResOK (T.Finish (False, t)) -> do putStrLn "fail, bad";   disp t; pure (mempty {sBadFail = 1})
               _                           -> do putStrLn "exception";   pure (mempty {sDied = 1})
      _   -> case tres of
               ResOK (T.Timeout _)         -> do putStrLn "timeout";     pure (mempty {sDied = 1})
               ResOK (T.Finish (True, t))  -> do putStrLn "pass, bad";   disp t; pure (mempty {sBadPass = 1})
               ResOK (T.Finish (False, _)) -> do putStrLn "fail, OK";    pure (mempty {sOK = 1})
               _                           -> do putStrLn "exception";   pure (mempty {sDied = 1})

runTimTest _ _ = error "impossible"

verifierErrorCodes :: [String]
verifierErrorCodes = ["A", "D", "F", "I", "U"]

data TimStatus = MkTimStatus {
    sSkip    :: !Int
  , sOK      :: !Int
  , sBadFail :: !Int
  , sBadPass :: !Int
  , sDied    :: !Int
  , sNotYet  :: !Int
  }
  deriving (Show)

instance Semigroup TimStatus where
  MkTimStatus s1 s2 s3 s4 s5 s6 <> MkTimStatus t1 t2 t3 t4 t5 t6 =
    MkTimStatus (s1+t1) (s2+t2) (s3+t3) (s4+t4) (s5+t5) (s6+t6)

instance Monoid TimStatus where
  mempty = MkTimStatus 0 0 0 0 0 0

---------------------

-- Results of compile&run, with somewhat decoded error messages
data Result a
  = ResOK a
  | Undefined
  | Shadowing
  | MultiplyDefined
  | BadLHS
  | SyntaxError
  | OtherError String
  deriving (Show)

-- Evaluate argument and catch any errors.
-- Use == to force the computation
tryResult :: (Eq a) => TestFlags -> a -> IO (Result a)
tryResult tflg a = do
  mres <- Control.Exception.try (evaluate (seq (a==a) a))
  when (verbose tflg) $
    case mres of
      Left msg -> print msg
      _ -> pure ()
  pure $
    case mres of
      Left exn ->
        case stripPrefix "error: " (show (exn :: SomeException)) of
          Nothing -> OtherError (show exn)
          Just msg | isPrefixOf "undefined:"        msg -> Undefined
                   | isPrefixOf "shadowing:"        msg -> Shadowing
                   | isPrefixOf "multiply defined:" msg -> MultiplyDefined
                   | isPrefixOf "Bad LHS"           msg -> BadLHS
                   | isPrefixOf "syntax error:"     msg -> SyntaxError
                   | otherwise                          -> OtherError msg
      Right x -> ResOK x
