{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad.Trans.Except

import Data.ByteString qualified as ByteString
import Data.Foldable (for_)

import Language.Verse

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main = ByteString.readFile "in" >>= runExceptT . eval >>= \ case
  Left e -> withFile "err" WriteMode $ \ err ->
    hPutDoc err $ pretty e <> line
  Right xs -> withFile "out" WriteMode $ \ out ->
    for_ xs $ hPutDoc out . (<> line) . pretty
