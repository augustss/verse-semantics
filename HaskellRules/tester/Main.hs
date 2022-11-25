module Main(main) where

import Control.Exception
import Control.Monad
import Data.Maybe
import GHC.Stack
import Options.Applicative
import System.Exit

import FrontEnd.Expr
import FrontEnd.Flags
import FrontEnd.Parse hiding (many)
import FrontEnd.Core
import Epic.Print (Pretty, prettyShow, pp)
import FrontEnd.Desugar(desugar)
import FrontEnd.Run
import Rules.Systems(ESystem, lookupSystem)

--------------

data TestFlags = TestFlags
  { dfs       :: !Bool                -- just find one normal form
--  , denSem    :: !Bool                -- evaluate with denotational semantics
  , split     :: !Bool                -- use split
  , parse     :: !Bool                -- parse only
  , simplify  :: !Bool                -- use simplifier
--  , alias     :: !Bool                -- eliminate aliases
--  , unifyEq   :: !Bool                -- unify as equals under barrier
--  , underLam  :: !Bool                -- reduce under lambda
  , eval      :: !Bool                -- Use fast evaluator
  , quiet     :: !Bool                -- Less noisy
  , noInline  :: !Bool                -- No final inlining
  , system    :: !(Maybe ESystem)     -- rule system
  , fileNames :: ![FilePath]          -- input files
  }
  deriving (Show)

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq Ident Expr Expr
  | TestCoreEq Ident Core Core
  deriving (Show)

-- Parse an evaluation equality test
pTestEq :: P Test
pTestEq =
  pKeyword "testeq" *> do
    tId <- pParens pIdent
    if tId == Ident noLoc "CEq" then
      TestCoreEq tId <$> pBraces pCore    <*> pBraces pCore
     else
      TestEvalEq tId <$> pBraces pExprSeq <*> pBraces pExprSeq

-- Parse a test
pTest :: P Test
pTest = pTestEq

-- Parse a file of tests
pTestFile :: P [Test]
pTestFile = skip *> many pTest <* eof

readTests :: FilePath -> IO [Test]
readTests fn = do
  file <- readFile fn
  let tests = parseDie pTestFile fn file
  pure tests

------------

assertEquivE :: HasCallStack => Bool -> TestFlags -> Loc -> Expr -> Expr -> IO Bool
assertEquivE ok flg name e1 e2  = assertEquiv ok flg name (e1, toCore e1) (e2, toCore e2)
  where toCore = exprToCore (testFlagsToFlags flg) . desugar

assertEquivC :: HasCallStack => Bool -> TestFlags -> Loc -> Core -> Core -> IO Bool
assertEquivC ok flg name e1 e2  = assertEquiv ok flg name (e1, e1) (e2, e2)

assertEquiv :: (HasCallStack, Pretty a) => Bool -> TestFlags -> Loc -> (a, Core) -> (a, Core) -> IO Bool
assertEquiv expectOK tflg loc (p1, c1) (p2, c2) = do
    let noisy = not (quiet tflg)
    let flg = testFlagsToFlags tflg
        sys = fromMaybe (error "no rule system specified") $ system tflg
    let v1 = run flg sys c1
    let v2 = run flg sys c2

    let pos = prettyShow loc

    catch
      ( if (v1 `equivValue` v2) == expectOK
        then do
            when noisy $
              putStrLn $ pos ++ if expectOK then " success!" else " failure, expected"
            pure True
        else do
            if expectOK
              then do
                putStrLn $ pos ++ " failure:"
                putStrLn "The expression"
                pp p1
                putStrLn "evaluates to"
                pp v1
                putStrLn "but"
                pp p2
                putStrLn "evaluates to"
                pp v2
                putStrLn ""
                when (prettyShow v1 == prettyShow v2) $ do
                    putStrLn "The unpretty printed values are"
                    print v1
                    putStrLn "resp."
                    print v2
                --undefined
              else do
                putStrLn $ pos ++ " unexpected success, please update test case!"
            pure False
      ) (\e -> do
            putStrLn $ pos ++ " failure:"
            putStrLn "The expression"
            pp p1
            putStrLn "or the expression"
            pp p2
            putStrLn "caused an exception:"
            print (e :: SomeException)
            putStrLn ""
            --undefined
            pure False
      )

-- | Equivalence on values (or stuck expressions)
--
--  * Ignores message on `wrong`
--  * TODO: α-equivalence on lambdas
equivValue :: Core -> Core -> Bool
equivValue (CWrong _) (CWrong _) = True
equivValue v1 v2 = v1 == v2

--------------

runTest :: TestFlags -> Test -> IO Bool
runTest tflg (TestEvalEq n e1 e2) =
  case n of
    Ident loc "SEq" -> assertEquivE True  tflg loc e1 e2
    Ident loc "FEq" -> assertEquivE False tflg loc e1 e2
    Ident _ s -> error $ "Unknown test type " ++ show s
runTest tflg (TestCoreEq n e1 e2) =
  case n of
    Ident loc "CEq" -> assertEquivC True  tflg loc e1 e2
    Ident _ s -> error $ "Unknown test type " ++ show s

runTests :: TestFlags -> [Test] -> IO Bool
runTests tflg ts = and <$> mapM (runTest tflg) ts

runTestFile :: TestFlags -> FilePath -> IO ()
runTestFile tflg fn = do
  putStrLn $ "Test " ++ show fn ++ " with: " ++ showFlags (testFlagsToFlags tflg)
  ok <- runTests tflg =<< readTests fn
  putStrLn $ if ok then "SUCCESS" else "FAILURE"
  unless ok $
    exitWith (ExitFailure 1)

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
      (  long "eval"
      <> help "Use fast evaluator"
      )
  <*> switch
      (  long "quiet"
      <> help "Be less noisy"
      )
  <*> switch
      (  long "no-final-inline"
      <> help "Do not do final normalization"
      )
  <*> optional (option (eitherReader lookupSystem)
         ( long "rules"
        <> short 'r'
        <> metavar "NAME"
        <> help "Use rule system NAME" ))
  <*> many (argument str (metavar "FILES..."))

testFlagsToFlags :: TestFlags -> Flags
testFlagsToFlags t =
  defaultFlags{ fSplit = split t, fSimplify = simplify t,
                fRewrite = not (eval t),
                fDfs = dfs t, fFinalInline = not (noInline t)
                }
main :: IO ()
main = do
  tflg <- testArgs
  let fns = fileNames tflg
  if parse tflg then
    mapM_ ptest fns
   else
    mapM_ (runTestFile tflg) fns

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
