module Main where
import System.Environment
import ParseEBNF

main :: IO ()
main = do
  [n] <- getArgs
  f <- readFile n
  let r = parseDie pFile n f
  print r

