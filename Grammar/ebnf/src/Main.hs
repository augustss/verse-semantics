module Main where
import Control.Monad
import System.Environment
import ParseEBNF
import ParseVerse

main :: IO ()
main = do
  [n,v] <- getArgs
  fn <- readFile n
  let rs = parseEBNF n fn
  -- print rs
  let pVerse = mkRulesParse "File" rs
  fv <- readFile v
  let x = parseDie pVerse n fv
  print x
  when (flattenParseTree x /= fv) undefined
