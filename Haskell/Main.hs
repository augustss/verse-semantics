{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.List
import Options.Applicative
import System.Environment
import Text.PrettyPrint.HughesPJClass hiding ((<>))

import Desugar
import Parse
import Scope
import CoreExpr(flattenSeqs)

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

-----

data VerseOptions = VerseOptions {
  verbose :: !Bool,
  dumpParse :: !Bool,
  dumpDesugar :: !Bool,
  dumpExtrude :: !Bool,
  dumpScope :: !Bool,
  dumpCleanup :: !Bool,
  inputFile :: !String
  }
  deriving (Show)

verseOptions :: Parser VerseOptions
verseOptions = VerseOptions
  <$> switch
      ( long "verbose"
     <> short 'v'
     <> help "be talkative"
      )
  <*> switch
      ( long "dump-parse"
     <> help "dump after parsing"
      )
  <*> switch
      ( long "dump-desugar"
     <> help "dump after desugaring"
      )
  <*> switch
      ( long "dump-extrude"
     <> help "dump after scope extrusion"
      )
  <*> switch
      ( long "dump-scope"
     <> help "dump after scope checking"
      )
  <*> switch
      ( long "dump-cleanup"
     <> help "dump after cleanup"
      )
  <*> argument str (metavar "FILE")

verseInfo :: ParserInfo VerseOptions
verseInfo = info (verseOptions <**> helper)
  (fullDesc <>
   progDesc "Process FeatherWeight Verse" <>
   header "FWVerse interpreter")

-----

main :: IO ()
main = do
  _options@VerseOptions{..} <- execParser verseInfo
  file <- readFile inputFile
  let eParse = parseDie pFile inputFile file
      eDesugar = desugar eParse
      eExtrude = extrude eDesugar
      eScope = scopeCheck eExtrude
      eCleanup = flattenSeqs eScope
      dump d e = do
        when d $ do
          when verbose $
            print e
          pp e
  dump dumpParse eParse
  dump dumpDesugar eDesugar
  dump dumpExtrude eExtrude
  dump dumpScope eScope
  dump dumpCleanup eCleanup

comp :: String -> IO ()
comp = pp . flattenSeqs . scopeCheck . extrude . desugar . parseString
