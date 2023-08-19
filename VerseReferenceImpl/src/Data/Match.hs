{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Match
  ( RowMatch (..)
  , RowMatchable (..)
  , ZipMatch
  , ZipMatchable (..)
  ) where

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
