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
import Data.Foldable (for_)

import Language.Verse
import Language.Verse.Pretty

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO

main :: IO ()
main = ByteString.readFile "in" >>= runExceptT . runSupplyT . runVerseT . (prettyM <=< eval) >>= \ case
  Left e -> withFile "err" WriteMode $ \ err ->
    renderIO err . layoutSmart layoutOptions . (<> line) $ pretty e
  Right xs -> withFile "out" WriteMode $ \ out ->
    for_ xs $ renderIO out . layoutSmart layoutOptions . (<> line)

layoutOptions :: LayoutOptions
layoutOptions = defaultLayoutOptions
  { layoutPageWidth = AvailablePerLine 60 1.0
  }
