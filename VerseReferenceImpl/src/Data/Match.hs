{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Match
  ( RowMatch (..)
  , RowMatchable (..)
  , ZipMatch
  , ZipMatchable (..)
  ) where

import Control.Monad

import Data.Functor
import Data.Functor.Const

data RowMatch f a b
  = Zip (ZipMatch a b)
  | Subset [(a, f b)]
  | Superset [(f a, b)]
  | Undecidable
  | Uncons (a -> f a) a (b -> f b) b

class Traversable f => RowMatchable f where
  rowMatch :: Bool -> f a -> f b -> RowMatch f a b
  default rowMatch :: ZipMatchable f => Bool -> f a -> f b -> RowMatch f a b
  rowMatch _ x y = Zip $ zipMatch x y

type ZipMatch a b = Maybe [(a, b)]

class RowMatchable f => ZipMatchable f where
  zipMatch :: f a -> f b -> ZipMatch a b

instance RowMatchable []

instance ZipMatchable [] where
  zipMatch = curry $ \ case
    ([], []) -> Just []
    (x:xs, y:ys) -> ((x, y):) <$> zipMatch xs ys
    _ -> Nothing

instance RowMatchable Maybe

instance ZipMatchable Maybe where
  zipMatch = curry $ \ case
    (Nothing, Nothing) -> Just []
    (Just x, Just y) -> Just [(x, y)]
    _ -> Nothing

instance Eq a => RowMatchable (Const a)

instance Eq a => ZipMatchable (Const a) where
  zipMatch x y = guard (getConst x == getConst y) $> []
