import Control.Monad
import Data.List
import System.Environment
import Text.PrettyPrint.HughesPJClass

import Desugar
import Parse
import ParseExpr

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

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
      d = desugar e
  when verbose $
    print e
  pp e
  when verbose $
    print d
  pp d

str :: String -> IO ()
str = pp . desugar . parseString
