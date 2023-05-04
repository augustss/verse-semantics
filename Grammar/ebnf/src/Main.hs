module Main where
import Control.Monad
import Data.List
import System.Environment
import ParseEBNF
import ParseVerse

main :: IO ()
main = do
  n : vs <- getArgs
  fn <- readFile n
  let rs = parseEBNF n (trim fn)
  when (rs /= rs) undefined  -- Just force evaluation
  let pVerse = mkRulesParse "File" rs
      doFile v = do
        putStrLn $ "===== " ++ v ++ " ====="
        fv <- readFile v
        let x = parseDie pVerse v fv
        print x
        when (flattenParseTree x /= fv) undefined
  mapM_ doFile vs

trim :: String -> String
trim = unlines . map addSemi . cutBottom . cutTop . lines
  where
    cutTop :: [String] -> [String]
    cutTop = dropWhile (\ l -> take 1 (words l) /= ["Alpha"])
    cutBottom :: [String] -> [String]
    cutBottom ls = t ++ take 1 b where (t, b) = span (\ l -> take 1 (words l) /= ["File"]) ls
    addSemi :: String -> String
    addSemi l | isInfixOf " := " l = ";;" ++ l
              | otherwise = l
