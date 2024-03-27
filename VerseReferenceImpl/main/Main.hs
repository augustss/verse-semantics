{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Except

import Data.ByteString (getContents)
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor.Compose.Instances ()

import Language.Verse

import Prettyprinter
import Prettyprinter.Render.Text

import System.IO (IO, stderr)

main :: IO ()
main = getContents >>= runExceptT . runSupplyT . eval "<command line>" >>= \ case
  Right xs -> for_ (join xs) $ putDoc . (<> line) . pretty
  Left e -> hPutDoc stderr $ pretty e <> line
