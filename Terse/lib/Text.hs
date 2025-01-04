module Text
  ( module Data.Text
  , slice
  ) where

import Data.Text

import Prelude (Int, (.))

slice :: Int -> Int -> Text -> Text
slice i j = drop i . take j
