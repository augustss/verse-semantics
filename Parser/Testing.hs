module Testing(main) where
import Control.Exception
import Control.Monad
import GHC.Stack

import Expr
import Parse
import Core
import Print
import Desugar
import Eval

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

assertEquiv :: HasCallStack => Ident -> Expr -> Expr -> IO ()
assertEquiv = assertEquiv' True

assertFail :: HasCallStack => Ident -> Expr -> Expr -> IO ()
assertFail = assertEquiv' False

assertEquiv' :: HasCallStack => Bool -> Ident -> Expr -> Expr -> IO ()
assertEquiv' expectOK name p1 p2 = do
    let flg = Flags { underLambda = False, traceEval = False }
    let d1 = desugar p1
    let d2 = desugar p2
    let c1 = exprToCore d1
    let c2 = exprToCore d2
    let v1 = eval flg c1
    let v2 = eval flg c2

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
      )

-- | Equivalence on values (or stuck expressions)
--
--  * Ignores message on `wrong`
--  * TODO: α-equivalence on lambdas
equivValue :: Core -> Core -> Bool
equivValue (CWrong _) (CWrong _) = True
equivValue v1 v2 = v1 == v2

--------------

runTest :: Test -> IO ()
runTest (TestEvalEq n e1 e2) =
  case n of
    Ident _ "SEq" -> assertEquiv n e1 e2
    Ident _ "FEq" -> assertFail n e1 e2
    Ident _ s -> error $ "Unknown test type " ++ show s

runTests :: [Test] -> IO ()
runTests = mapM_ runTest

runTestFile :: FilePath -> IO ()
runTestFile = runTests <=< readTests

main :: IO ()
main = runTestFile "tests.versetest"
