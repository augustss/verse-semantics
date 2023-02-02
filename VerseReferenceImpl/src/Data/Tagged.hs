{-# LANGUAGE DeriveTraversable #-}
module Data.Tagged
  ( Tagged (..)
  ) where

newtype Tagged a b = Tagged
  { getTagged :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
