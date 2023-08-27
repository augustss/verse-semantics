{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Match
  ( RowMatch (..)
  , RowMatchable (..)
  , ZipMatch
  , ZipMatchable (..)
  ) where

import Control.Monad

import Data.Coerce
import Data.Functor
import Data.Functor.Const

data RowMatch f a b
  = Zip (ZipMatch f a b)
  | Uncons (a -> f a) a (b -> f b) b

class Traversable f => RowMatchable f where
  rowMatch :: f a -> f b -> RowMatch f a b
  default rowMatch :: ZipMatchable f => f a -> f b -> RowMatch f a b
  rowMatch x y = Zip $ zipMatch x y

type ZipMatch f a b = Maybe (f (a, b))

class RowMatchable f => ZipMatchable f where
  zipMatch :: f a -> f b -> ZipMatch f a b

instance RowMatchable []

instance ZipMatchable [] where
  zipMatch = curry $ \ case
    ([], []) -> Just []
    (x:xs, y:ys) -> ((x, y):) <$> zipMatch xs ys
    _ -> Nothing

instance Eq a => RowMatchable (Const a)

instance Eq a => ZipMatchable (Const a) where
  zipMatch x y = guard (getConst x == getConst y) $> coerce x
