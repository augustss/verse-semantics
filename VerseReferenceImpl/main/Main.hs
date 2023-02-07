{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Except
import Control.Monad.Verse

import Data.ByteString qualified as ByteString
import Data.Foldable

import Language.Verse
import Language.Verse.Pretty

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main = ByteString.getContents >>= runExceptT . runSupplyT . runVerseT . (prettyM <=< eval) >>= \ case
  Left e -> hPutDoc stderr $ pretty e <> line
  Right xs -> for_ xs $ putDoc . (<> line)
