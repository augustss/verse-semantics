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
    renderIO err . layoutSmart layoutOptions . (<> line) $ pretty e
  Right xs -> withFile "out" WriteMode $ \ out ->
    for_ xs $ renderIO out . layoutSmart layoutOptions . (<> line) . pretty

layoutOptions :: LayoutOptions
layoutOptions = defaultLayoutOptions
  { layoutPageWidth = AvailablePerLine 60 1.0
  }
