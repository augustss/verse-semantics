module Main where

import Parser.Parser

import System.Directory
import System.FilePath
import qualified Data.Text.IO as T
import qualified Data.Text    as T
import Text.PrettyPrint.HughesPJClass

import Paths_VersePrototypes

-- all tests are listed in Cabal's data-dir directory
main :: IO ()
main = do
  tests <- getDataDir
  getDataDir >>= print
  testFiles <- fmap (tests </>) <$> listDirectory tests
  mapM_ parseTestFile testFiles


parseTestFile :: FilePath -> IO ()
parseTestFile fn = do
  putStrLn $ replicate 80 '+'
  putStrLn $ "Parsing " ++ fn
  putStrLn $ replicate 80 '='
  fileContents <- T.unpack <$> T.readFile fn
  putStrLn $
    case parseTry pFile fn fileContents of
      Left  err -> err
      Right res -> render $ pPrint res
  putStrLn $ replicate 80 '-'
  putStrLn mempty
