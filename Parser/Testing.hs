module Testing(main, test, freshmain, testFRESH, parseFresh, testf) where

import Control.Exception
import Control.Monad
import GHC.Stack
import System.Environment

import Expr
import Parse
import Core
import Print
import Desugar
import Run
import qualified RulesPLDI
import qualified TRSCore as T
import TRS(normalFormsTrace, printTrace)
import TRSAdapter(coreToTrs)
--------------

data Test
  -- Test that two expressions evaluate to the same thing
  = TestEvalEq Ident Expr Expr
  deriving (Show)

-- Parse an evaluation equality test
pTestEq :: P Test
pTestEq =
  pKeyword "testeq" *> (TestEvalEq <$> pParens pIdent <*> pBraces pExprSeq <*> pBraces pExprSeq)

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

assertEquiv :: HasCallStack => Flags -> Ident -> Expr -> Expr -> IO ()
assertEquiv = assertEquiv' True

assertFail :: HasCallStack => Flags -> Ident -> Expr -> Expr -> IO ()
assertFail = assertEquiv' False

assertEquiv' :: HasCallStack => Bool -> Flags -> Ident -> Expr -> Expr -> IO ()
assertEquiv' expectOK flg name p1 p2 = do
    let d1 = desugar p1
    let d2 = desugar p2
    let c1 = exprToCore flg d1
    let c2 = exprToCore flg d2
    let v1 = run flg c1
    let v2 = run flg c2

    let pos =
          case name of
            Ident loc _ -> prettyShow loc

    catch
      ( if (v1 `equivValue` v2) == expectOK
        then do
            putStrLn $ pos ++ if expectOK then " success!" else " failure, expected"
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
      )

-- | Equivalence on values (or stuck expressions)
--
--  * Ignores message on `wrong`
--  * TODO: α-equivalence on lambdas
equivValue :: Core -> Core -> Bool
equivValue (CWrong _) (CWrong _) = True
equivValue v1 v2 = v1 == v2

--------------

runTest :: Flags -> Test -> IO ()
runTest flg (TestEvalEq n e1 e2) =
  case n of
    Ident _ "SEq" -> assertEquiv flg n e1 e2
    Ident _ "FEq" -> assertFail flg n e1 e2
    Ident _ s -> error $ "Unknown test type " ++ show s

runTests :: Flags -> [Test] -> IO ()
runTests flg = mapM_ (runTest flg)

runTestFile :: Flags -> FilePath -> IO ()
runTestFile flg = runTests flg <=< readTests

test :: Bool -> IO ()
test True = runTestFile defaultFlags verseTest
test False = runTestFile defaultFlags{ fRewrite = True } verseTest

testf :: IO ()
testf = runTestFile defaultFlags{ fRewrite = True, fSplit = False, fFresh = True } verseTest

-- Just parse
ptest :: FilePath -> IO ()
ptest fn = do
  file <- readFile fn
  let e = parseDie pFile fn file
  if e == e then putStrLn $ "parsed " ++ fn else undefined
  pure ()

main :: IO ()
main = do
  (flg, fn) <- testArgs
  runTestFile flg fn
  ptest test1

testArgs :: IO (Flags, FilePath)
testArgs = do
  args <- getArgs
  let (flg, args') =
        case args of
          "-"        : r -> (defaultFlags{ fRewrite = True }, r)
          "-rewrite" : r -> (defaultFlags{ fRewrite = True, fSplit = False, fFresh = False }, r)
          "-eval"    : r -> (defaultFlags,                    r)
          "-densem"  : r -> (defaultFlags{ fDenSem  = True, fTimLambda = True, fSplit = False, fSimplify = True }, r)
          "-fresh"   : r -> (defaultFlags{ fRewrite = True, fSplit = True, fFresh = True }, r)
          r              -> (defaultFlags,                    r)
  let fn =
        case args' of
          [] ->  verseTest
          [s] -> s
          _ -> error $ "Usage: tests [-rewrite|-densem|-eval|-fresh] [file]"
  pure (flg, fn)

verseTest :: FilePath
verseTest = "tests.versetest"
test1 :: FilePath
test1     = "test1.verse"

-------------
freshmain :: IO ()
freshmain = do
  testFRESH "{(x:int => y:int => x+2*y)[2][3]}"

testFRESH :: String -> IO ()
testFRESH s = do
  let e = parseFresh s
  let trs = normalFormsTrace RulesPLDI.rulesPLDI e
  mapM_ (\tr -> printTrace tr >> putStrLn"----------") trs

parseFresh :: String -> T.Expr
parseFresh = coreToTrs . exprToCore flags . desugar . parseDie (pBraces pExprSeq) ""
  where
    flags = defaultFlags{ fRewrite = True, fSplit = False, fTrace = True, fFresh = True }
------
