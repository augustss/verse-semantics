{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad.Supply
import Control.Monad.Trans.Except

import Data.ByteString qualified as ByteString
import Data.Foldable

import Language.Verse

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main = ByteString.getContents >>= runExceptT . runSupplyT . eval >>= \ case
  Right xs -> for_ xs $ putDoc . (<> line) . pretty
  Left e -> hPutDoc stderr $ pretty e <> line
