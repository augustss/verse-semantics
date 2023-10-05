{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where


import Data.ByteString qualified as ByteString

import Language.Verse.Lexer
import Language.Verse.Parse

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main =
  do
    contents <- ByteString.getContents
    case runLexer parse contents of
         Right e -> hPutDoc stdout $ pretty e
         Left e -> hPutDoc stderr $ pretty e
