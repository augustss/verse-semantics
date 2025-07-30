{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Language.Verse.Pos
  ( Pos (..)
  , minBound
  ) where

import Data.Eq
import Data.Int
import Data.Ord
import Data.Semigroup

import Prettyprinter (Pretty (..), colon)

import Text.Show

data Pos = Pos
  { line :: !Int
  , column :: !Int
  , offset :: !Int
  } deriving Show

instance Eq Pos where
  x == y = x.offset == y.offset

instance Ord Pos where
  compare x y = compare x.offset y.offset

instance Pretty Pos where
  pretty x = pretty x.line <> colon <> pretty x.column

minBound :: Pos
minBound = Pos { line = 1, column = 1, offset = 0 }
