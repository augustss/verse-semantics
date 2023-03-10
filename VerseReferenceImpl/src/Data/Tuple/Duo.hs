{-# LANGUAGE DeriveTraversable #-}
module Data.Tuple.Duo
  ( Duo (..)
  ) where

data Duo a = Duo a a deriving (Show, Functor, Foldable, Traversable)
