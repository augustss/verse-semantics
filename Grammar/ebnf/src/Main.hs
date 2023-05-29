{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
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
  args <- getArgs
  let (showAst, n : vs) = if take 1 args == ["-"] then (False, drop 1 args) else (True, args)
  fn <- readFile n
  let rs = parseEBNF n (trim fn)
  when (rs /= rs) undefined  -- Just force evaluation
  let pVerse = mkRulesParse "File" rs
      doFile v = do
        fv <- trimFile <$> readFile v
        putStrLn $ "===== " ++ v ++ " " ++ show (length (lines fv)) ++ " lines ====="
        let xs = parsesDie pVerse v fv
            asts = map parseTreeToAST xs
            asts' = nub asts
        -- print (head xs)
        case asts' of
          [ast] -> do
            if showAst then
              putStrLn $ prettyShow ast
             else
              when (ast /= ast) undefined
            when (flattenParseTree (head xs) /= fv) $ do
              putStrLn "Roundtrip fail"
              putStrLn fv
              putStrLn "-----"
              putStrLn $ flattenParseTree (head xs)
              error "bad"
            when (length xs > 1) $ do
              putStrLn $ "Harmless ambiguity: " ++ show (length xs)
              --mapM_ print xs
          _ -> do
            putStrLn $ "Ambiguous:\n"
            mapM_ print xs
            putStrLn $ unlines (map prettyShow asts')
            putStrLn $ "Ambiguity: " ++ show (length asts')
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
    
trimFile :: String -> String
trimFile ('\65279':cs) = cs
trimFile cs = cs
