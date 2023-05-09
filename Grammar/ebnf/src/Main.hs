module Main where
import Control.Monad
import Data.List
import Text.PrettyPrint.HughesPJClass(prettyShow)
import System.Environment
import ParseEBNF
import ParseVerse
import AST

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
        let xs = parsesDie pVerse v fv
            asts = map parseTreeToAST xs
        -- print (head xs)
        when (length xs > 1) $ do
          putStrLn $ "Ambig " ++ show (length xs)
          mapM_ print xs
        case nub asts of
          [ast] -> do
            putStrLn $ prettyShow ast
            when (flattenParseTree (head xs) /= fv) $ do
              putStrLn "Roundtrip fail"
              putStrLn fv
              putStrLn "-----"
              putStrLn $ flattenParseTree (head xs)
              error "bad"
          asts' -> error $ "Ambiguous:\n" ++ unlines (map show asts')
  mapM_ doFile vs

trim :: String -> String
trim = unlines . map addSemi . map stripComment . cutBottom . cutTop . lines
  where
    cutTop :: [String] -> [String]
    cutTop = dropWhile (\ l -> take 1 (words l) /= ["Alpha"])
    cutBottom :: [String] -> [String]
    cutBottom ls = t ++ take 1 b where (t, b) = span (\ l -> take 1 (words l) /= ["File"]) ls
    addSemi :: String -> String
    addSemi l | isInfixOf " := " l = ";;" ++ l
              | otherwise = l
    stripComment ('-':'-':_) = ""
    stripComment "" = ""
    stripComment (c:cs) = c : stripComment cs
    
