module Language.Verse.Class
  ( Class (..)
  ) where

data Class f a = Class
  !Label
  !(IdentMap Name (f a))
  (Maybe a)
  !(IdentMap Name Bool)
  Exp deriving (Show, Functor, Foldable, Traversable)

