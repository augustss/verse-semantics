{-# LANGUAGE LambdaCase #-}
module Data.Unifiable
  ( Unifiable (..)
  ) where

class Traversable f => Unifiable f where
  zipMatch :: f a -> f b -> Maybe (f (a, b))

instance Unifiable [] where
  zipMatch = curry $ \ case
    (x:xs, y:ys) -> ((x, y):) <$> zipMatch xs ys
    ([], []) -> Just []
    _ -> Nothing
