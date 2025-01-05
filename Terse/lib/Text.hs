module Text
  ( module Data.Text
  , slice
  , sliceWord8
  ) where

import Data.Text
import Data.Text.Unsafe qualified as Unsafe

import Prelude (Int, (.))

slice :: Int -> Int -> Text -> Text
slice i j = drop i . take j

sliceWord8 :: Int -> Int -> Text -> Text
sliceWord8 i j = Unsafe.dropWord8 i . Unsafe.takeWord8 j
