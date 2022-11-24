module Main(main) where
import System.Environment

import FrontEnd.Parse
import FrontEnd.Expr

data TimTest = TimTest Ident Expr

pFileTimTest :: P [TimTest]
pFileTimTest = skip *> many pTimTest <* eof

pTimTest :: P TimTest
pTimTest = TimTest <$> (pKeyword "test" *> pParens pIdent) <*> pBraces pExprSeq <* optional (symbol ";")

main :: IO ()
main = do
  args <- getArgs
  let (fn1, fn2) =
        case args of
          [] -> ("VerseTestFiles/TimVerse-tests.verse", "VerseTestFiles/TimAbout.verse")
          [s1, s2] -> (s1, s2)
          _ -> error "Use 0 or 2 arguments"
  file1 <- readFile fn1
  let tests1 = parseDie pFileTimTest fn1 file1
  putStrLn $ "Read " ++ fn1 ++ ", parsed tests: " ++ show (length tests1)
  file2 <- readFile fn2
  let tests2 = parseDie pFile fn2 file2
  if tests2 == tests2 then pure () else undefined
  putStrLn $ "Parsed " ++ fn2
  
