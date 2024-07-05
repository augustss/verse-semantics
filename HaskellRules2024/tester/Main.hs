{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Rules.TRS2024( rules )
import TRS.Traced( term )

import Epic.Print hiding ( (<>) )

import Text.Megaparsec(getSourcePos, sourceLine, unPos)

import GHC.Stack( HasCallStack )

import Data.Char( toLower )
import Data.Maybe

import Control.Monad( unless, when, forM_ )

import System.Exit( exitWith, ExitCode(..) )
import System.IO( stdout, hFlush )

import Text.Printf( printf )
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
  | parse test_flags
  = mapM_ parseSourceOnly (fileNames test_flags)

  | Just expr_string <- testExpr test_flags
  , let fn = "<command-line>"
  = runTestFile test_flags (fn, parseDie pTestFile fn expr_string)

--  | timRun test_flags || timVerify test_flags
--  = mapM_ (timTest tflg) (fileNames test_flags)

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
  = TestEvalEq TestInfo SrcExpr SrcExpr
  | TestVerify TestInfo SrcExpr
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
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

data TestRes = Good | Bad | Many | None | Excn | Skip
  deriving (Eq, Show)

testName :: TestInfo -> String
testName ti = fromMaybe ("L" ++ show (unPos (sourceLine (testLocn ti)))) (testMName ti)


-----------------------------------------------
--
--     Run tests
--     runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
--
-----------------------------------------------

runTestFile :: TestFlags -> (FilePath, [Test]) -> IO ()
runTestFile tflg (fn, ts) = do
  if summary tflg then
    runTestSummary tflg ts
   else do
    putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)
    ok <- runTestFileSys tflg ts
    unless ok $ exitWith (ExitFailure 1)

runTest :: TestFlags -> Test -> IO TestRes
runTest tflg (TestEvalEq n e1 e2) = assertEquivE n tflg e1 e2
runTest _    (TestVerify {})      = error "runTest: verify" -- assertVerify n tflg e

runTestSummary :: TestFlags -> [Test] -> IO ()
runTestSummary atflg tests = do
  let tflg = atflg{ noError = True }
  putStrLn "OK=success; nonc=non-confluent; BAD=wrong result; skip=test skipped; t.o.=time-out; excn=exception thrown"
  forM_ tests $ \ test -> do
    printf  "%-*s" widthTestName (testName (testInfo test))
    hFlush stdout
    r <- runTest tflg test
    printf " %*s" widthSysName (showRes r)
    hFlush stdout
    putStrLn ""

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
  putStrLn $ if ok then "SUCCESS" else "FAILURE"
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


-----------------------------------------------
--
--    Testing evaluation
--
-----------------------------------------------

srcToCore :: Flags -> SrcExpr -> IO Rules.Expr
srcToCore flags e = do { e1 :: SrcCore    <- FrontEnd.Desugar.desugar flags e
                       ; e2 :: Rules.Expr <- FrontEnd.ToCore.convertToCore flags e1
                       ; let e3 = Rules.prep e2
                       ; return e3 }

evalExpr :: Rules.Expr -> Rules.Expr
evalExpr e = TRS.Traced.term (Rules.normalize (Rules.everywhere Rules.TRS2024.rules) e)

assertEquivE :: HasCallStack => TestInfo -> TestFlags -> SrcExpr -> SrcExpr -> IO TestRes
assertEquivE ti flg e1 e2
  = do { c1 <- srcToCore flags e1
       ; c2 <- srcToCore flags e2
       ; assertEquiv ti flg (e1, c1) (e2, c2) }
  where
    flags = testFlagsToFlags flg

assertEquiv :: (HasCallStack, Pretty a) => TestInfo -> TestFlags
            -> (a, Rules.Expr) -> (a, Rules.Expr) -> IO TestRes
assertEquiv ti tflg (p1, c1) (p2, c2)
  | typ == TSkip = do { when noisy (putStrLn $ pos ++ " skipped")
                      ; pure Skip }
  | otherwise = do
  when showD (putStrLn $ "desugared:\n" ++ prettyShow c1)

  let expectOK = typ == TPass
  let v1 = evalExpr c1
  let v2 = evalExpr c2

  catch (
       if (equivValue v1 v2) == expectOK
       then do
            when noisy $
              putStrLn $ pos ++ if expectOK then " success!" else " failure, expected"
            pure Good
       else do
            when (not (noError tflg)) $
             if expectOK
              then do
                putStrLn "-----------------------------------------------"
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
    typ = testType ti
    ppi x = putStrLn (render (text "  " <+> pPrint x))

-- | Equivalence on values (or stuck expressions)
equivValue :: Rules.Expr -> Rules.Expr -> Bool
equivValue e1 e2 = Rules.norm e1 == Rules.norm e2

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
pTest = pTestEq OA.<|> pTestVerify

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

-- Parse an expression verification test
pTestVerify :: P Test
pTestVerify =
  pKeyword "verify" *> do
    tId <- pParens pTestInfo
    TestVerify tId <$> pBraces pExprSeq

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


-----------------------------------------------
--
--    TestFlags, and command-line argument parsing
--        testArgs :: IO TestFlags
--
-----------------------------------------------

data TestFlags = TestFlags
  { dfs            :: !Bool                -- just find one normal form
  , split          :: !Bool                -- use split
  , parse          :: !Bool                -- parse only
  , simplify       :: !Bool                -- use simplifier
  , noUnderLam     :: !Bool                -- do not reduce under lambda
  , quiet          :: !Bool                -- Less noisy
  , verbose        :: !Bool                -- More noisy
  , noError        :: !Bool                -- Don't show error message
  , postProc       :: !Bool                -- Post processing
  , summary        :: !Bool                -- produce a summary
  , trace          :: !Bool                -- Show traces
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
             [] -> t{ fileNames = [if parse t then test1 else verseTest] }
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
      (  OA.long "parse"
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
  <*> OA.optional (OA.strOption
         ( OA.long "prelude"
        <> OA.metavar "NAME"
        <> OA.help "use the given prelude" ))
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
             fTrace = trace t,
             fDfs = dfs t, fPostProcess = postProc t,
             fUnderLambda = not (noUnderLam t),
             fRewriteSteps = maxSteps t,
             fNoFuelStop = ignoreFuelStop t,
             fAssumeVerified = assumeVerified t,
             fPrelude = maybe (fPrelude flags) (either error id . findPrelude) (prelude t),
             fDesugar = desugarRules t
           }



-----------------------------------------------
--
--     Verification testing
--     verifyIt :: TestFlags -> Expr -> IO VerifyResult
--
-----------------------------------------------
{-
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
           when noisy $
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

-----------------------------------------------
--
--     Tim tests
--
-----------------------------------------------


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


-}
