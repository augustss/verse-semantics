{-# LANGUAGE LambdaCase #-}
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
import FrontEnd.Desugar(exprToCore)
import FrontEnd.Flags
import FrontEnd.Parse hiding (many)
import FrontEnd.ParseCore
import FrontEnd.TRSAdapter(coreToTrs)
import Epic.Print (Pretty, prettyShow)
import FrontEnd.Desugar(desugar)
import FrontEnd.Run(run, runM, everySystem, findSystem, blockSystem)
import Rules.Core(RuleEnv(..))
import Rules.Equiv
import Rules.Systems(ESystem, TRSystem(..))

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
  , noError        :: !Bool                -- Don't show error message
  , finalInl       :: !Bool                -- No final inlining
  , system         :: !ESystem             -- rule system
  , summary        :: !Bool                -- produce a summary
  , trace          :: !Bool                -- Show traces
  , allRules       :: !Bool                -- test with all rule systems
  , onlyTest       :: !(Maybe String)      -- run only this test
  , testExpr       :: !(Maybe String)      -- use this expression as a test
  , maxSteps       :: !Int                 -- max number of rewrite steps
  , maxNormSteps   :: !Int                 -- max number of normalization steps
  , ignoreFuelStop :: !Bool                -- ignore running out of fuel
  , fileNames      :: ![FilePath]          -- input files
  }
  deriving (Show)

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq TestInfo Expr Expr
  | TestCoreEq TestInfo Core Core
  deriving (Show)

testInfo :: Test -> TestInfo
testInfo (TestEvalEq ti _ _) = ti
testInfo (TestCoreEq ti _ _) = ti

data TestInfo = TestInfo
  { testMName :: Maybe String
  , testLocn :: Loc
  , testType :: TestType
  , testExcn :: [((String, Bool), TestType)]  -- The bool indicates the the string is just a prefix
  }
  deriving (Show)

data TestType = TSkip | SEq | FEq
  deriving (Show, Eq)

pTestInfo :: P TestInfo
pTestInfo = do
  loc <- getSourcePos
  let strOf (LitStr s) = s
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
  case i of
    Ident _ "SEq"  -> pure SEq
    Ident _ "FEq"  -> pure FEq
    Ident _ "Skip" -> pure TSkip
    _ -> fail "pTestType"

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

-- Parse a test
pTest :: P Test
pTest = pTestEq <|> pTestCEq

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
assertEquivE ti flg e1 e2  = assertEquiv ti flg (e1, toCore e1) (e2, toCore e2)
  where toCore = exprToCore flags . desugar flags
        flags = testFlagsToFlags flg

assertEquivC :: HasCallStack => TestInfo -> TestFlags -> Core -> Core -> IO TestRes
assertEquivC ti flg e1 e2  = assertEquiv ti flg (e1, e1) (e2, e2)

assertEquiv :: (HasCallStack, Pretty a) => TestInfo -> TestFlags -> (a, Core) -> (a, Core) -> IO TestRes
assertEquiv ti tflg (p1, c1) (p2, c2) | typ == TSkip = do
  when noisy $
    putStrLn $ pos ++ " skipped"
  pure Skip
                                      | otherwise = do
  let expectOK = typ == SEq
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
                putStrLn $ pos ++ " unexpected success, please update test case!"
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
    pos = prettyShow loc ++ maybe "" ((", "++) . show) (testMName ti)
    sys = s{ ruleEnv = (ruleEnv s){ tfNormSteps = maxNormSteps tflg }} where s = system tflg
    typ = maybe (testType ti) snd $ find (\ (s,_) -> match s (sname sys)) (testExcn ti)
    ppi x = putStrLn . unlines . map ("   " ++) . lines . prettyShow $ x
    flg = testFlagsToFlags tflg
    match (n, w) m = map toLower n `tst` map toLower m
      where tst = if w then isPrefixOf else (==)

-- | Equivalence on values (or stuck expressions)
equivValue :: ESystem -> Core -> Core -> Bool
equivValue sys e1 e2 | sname sys == "eval" = e1 == e2  -- XXX temporary hack
equivValue sys e1 e2 =
  coreToTrs e1 == coreToTrs e2 ||        -- fast test first
  equiv sys (coreToTrs e1) (coreToTrs e2)

--------------

runTest :: TestFlags -> Test -> IO TestRes
runTest tflg (TestEvalEq n e1 e2) = assertEquivE n tflg e1 e2
runTest tflg (TestCoreEq n e1 e2) = assertEquivC n tflg e1 e2

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
      (  long "no-error"
      <> help "Do not show error message on failure"
      )
  <*> switch
      (  long "final-inline"
      <> help "Do final normalization"
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
  <*> many (argument str (metavar "FILES..."))

testFlagsToFlags :: TestFlags -> Flags
testFlagsToFlags t =
  defaultFlags{ fSplit = split t, fSimplify = simplify t,
                fTrace = trace t,
                fDfs = dfs t, fFinalInline = finalInl t,
                fUnderLambda = not (noUnderLam t),
                fRewriteSteps = maxSteps t,
                fNoFuelStop = ignoreFuelStop t
                }
main :: IO ()
main = do
  tflg <- testArgs
  let fns = fileNames tflg
  if parse tflg then
    mapM_ ptest fns
   else
    case testExpr tflg of
      Nothing -> mapM_ (\ fn -> do ts <- readTests fn; runTestFile tflg (fn, ts)) fns
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
