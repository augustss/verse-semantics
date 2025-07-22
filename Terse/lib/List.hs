{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module List
  ( module Data.List
  , last1
  , reverse2
  ) where

import Data.List

last1 :: a -> [a] -> a
last1 x = \ case
  [] -> x
  x:xs -> last1 x xs

reverse2 :: a -> a -> [a] -> [a]
reverse2 x y = foldl (flip (:)) [y, x]
