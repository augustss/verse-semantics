{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Ident
  ( Ident (..)
  , IdentMap
  ) where

import Data.Functor.Classes
import Data.Hashable
import Data.Hashable.Lifted
import Data.HashMap.Strict (HashMap)

import Language.Verse.Label

import Prettyprinter

data Ident a
  = Pure a
  | Label !Label deriving (Show, Eq, Ord)

type IdentMap a v = HashMap (Ident a) v

instance Eq1 Ident where
  liftEq f = curry $ \ case
    (Pure x, Pure y) -> f x y
    (Label x, Label y) -> x == y
    _ -> False

instance Hashable a => Hashable (Ident a) where
  hash = \ case
    Pure x -> 0 `hashWithSalt` x
    Label x -> distinguisher `hashWithSalt` x
  hashWithSalt = hashWithSalt1

instance Hashable1 Ident where
  liftHashWithSalt f s = \ case
    Pure x -> s `hashWithSalt` distinguisher `f` x
    Label x -> s `hashWithSalt` distinguisher `hashWithSalt` x

instance Pretty a => Pretty (Ident a) where
  pretty = \ case
    Pure x -> pretty x
    Label x -> "t" <> prettyLabel x

distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}
