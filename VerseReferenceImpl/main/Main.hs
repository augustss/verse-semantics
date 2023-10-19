{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad.Supply
import Control.Monad.Trans.Except
import Control.Monad.Verse

import Data.ByteString qualified as ByteString
import Data.Foldable

import Language.Verse
import Language.Verse.Error
import Language.Verse.Mode

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main = ByteString.getContents >>= runExceptT . runSupplyT . runVerseT . eval Execution >>= \ case
  Right (Just xs) -> for_ xs $ putDoc . (<> line) . pretty
  Right Nothing -> hPutDoc stderr $ pretty StuckError <> line
  Left e -> hPutDoc stderr $ pretty e <> line
