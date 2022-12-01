{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Language.Verse.Loc
  ( Loc (..)
  , minBound
  , L (..)
  , loc
  , uncurryL
  ) where

import Control.Comonad

import Data.Bool
import Data.Eq
import Data.Foldable
import Data.Functor.Apply
import Data.Ord
import Data.Semigroup
import Data.Traversable

import Language.Verse.Pos (Pos)
import Language.Verse.Pos qualified as Pos

import Prettyprinter

import Text.Show

data Loc = Loc !Pos !Pos deriving Show

instance Semigroup Loc where
  Loc x y <> Loc x' y' = Loc (min x x') (max y y')

instance Pretty Loc where
  pretty (Loc x y)
    | x.offset == y.offset = pretty x
    | otherwise = pretty x <> pretty '-' <> pretty y

minBound :: Loc
minBound = Loc Pos.minBound Pos.minBound

data L a = L !Loc a deriving (Show, Functor, Foldable, Traversable)

instance Apply L where
  L x f <.> L y a = L (x <> y) (f a)

instance Comonad L where
  extract (L _ x) = x
  duplicate x@(L y _) = L y x

loc :: L a -> Loc
loc (L x _) = x

uncurryL :: (Loc -> a -> b) -> L a -> b
uncurryL f (L x y) = f x y
