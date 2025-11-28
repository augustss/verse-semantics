{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Data.Foldable
import Data.Text (Text)
import Data.Text.IO qualified as Text

import GHC.Exts

import System.Environment
import System.Exit
import System.FilePath
import System.Process

main :: IO ()
main = getArgs >>= traverse_ f
  where
    f verseFilePath = do
      let hsFilePath = replaceExtension verseFilePath "hs"
      Text.writeFile hsFilePath (contents verseFilePath)
      exitWith =<< rawSystem "ghc"
        [ "--make"
        , "-Wall"
        , "-O2"
        , "-package", "prettyprinter"
        , "-package", "terse"
        , hsFilePath
        ]

contents :: String -> Text
contents x = "\
\{-# OPTIONS_GHC -Wno-unused-matches #-}\n\
\{-# LANGUAGE QuasiQuotes #-}\n\
\module Main\n\
\  ( main\n\
\  ) where\n\
\\n\
\import Prettyprinter\n\
\\n\
\import Verse.TH\n\
\\n\
\main :: IO ()\n\
\main = print . pretty =<< [verseFile|" <> fromString x <> "|]\n\
\"
