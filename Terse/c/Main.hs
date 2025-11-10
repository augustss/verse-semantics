module Main
  ( main
  ) where

import Data.Foldable
import Data.Text.IO qualified as Text

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Ppr (pprint)

import System.Environment
import System.FilePath

import Verse.Comp
import Verse.TH

main :: IO ()
main = do
  args <- getArgs
  for_ args $ \ arg -> do
    let result = replaceExtension arg "hs"
    writeFile result . link =<< comp =<< parseQ =<< Text.readFile arg

link :: TH.Exp -> String
link = pprint
