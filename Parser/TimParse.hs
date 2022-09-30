module TimParse where
import System.Environment

import Parse
import Expr

data TimTest = TimTest Ident Expr

pFileTimTest :: P [TimTest]
pFileTimTest = skip *> many pTimTest <* eof

pTimTest :: P TimTest
pTimTest = TimTest <$> (pKeyword "test" *> pParens pIdent) <*> pBraces pExprSeq <* optional (symbol ";")

main :: IO ()
main = do
  args <- getArgs
  let fn =
        case args of
          [] -> "VerseTestFiles/TimVerse-tests.verse"
          [s] -> s
          _ -> error "Use 0 or 1 argument"
  file <- readFile fn
  let tests = parseDie pFileTimTest fn file
  putStrLn $ "Read " ++ fn ++ ", parsed tests: " ++ show (length tests)
  
