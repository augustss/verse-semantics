{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Ident
  ( Ident (..)
  , name
  ) where

import Data.Hashable

import Language.Verse.Label

import Prettyprinter

data Ident a = Ident !Label !(Maybe a) deriving Show

name :: Ident a -> Maybe a
name (Ident _ x) = x

instance Eq (Ident a) where
  Ident x _ == Ident y _ = x == y

instance Ord (Ident a) where
  compare (Ident x _) (Ident y _) = compare x y

instance Hashable (Ident a) where
  hashWithSalt x (Ident y _) = hashWithSalt x y
  hash (Ident x _) = hash x

instance Pretty a => Pretty (Ident a) where
  pretty (Ident i x) = case x of
    Nothing -> "t" <> pretty i
    Just x -> pretty x
