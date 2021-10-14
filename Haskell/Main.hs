import Data.List
import System.Environment

import Parse
import ParseExpr

main :: IO ()
main = do
  args <- getArgs
  let (flags, files) = partition ((== "-") . take 1) args
  let fn =
        case files of
          [s] -> s
          _ -> error "Usage: prog [-v] [-c] FILE"
      verbose = "-v" `elem` flags
  file <- readFile fn
  let e = parseDie pFile fn file
  print e
