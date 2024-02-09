{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Ident
  ( Ident (..)
  , IdentMap
  ) where

import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.String

import Language.Verse.Label
import Language.Verse.SimpleName

import Prettyprinter

data Ident
  = Name !SimpleName
  | Label !Label deriving (Show, Eq, Ord)

instance Hashable Ident where
  hash = \ case
    Name x -> 0 `hashWithSalt` x
    Label x -> distinguisher `hashWithSalt` x
  hashWithSalt s = \ case
    Name x -> s `hashWithSalt` distinguisher `hashWithSalt` x
    Label x -> s `hashWithSalt` distinguisher `hashWithSalt` x

instance IsString Ident where
  fromString = Name . fromString

instance Pretty Ident where
  pretty = \ case
    Name x -> pretty x
    Label x -> "t" <> prettyLabel x

type IdentMap = HashMap Ident

distinguisher :: Int
distinguisher = fromIntegral $ (maxBound :: Word) `quot` 3
{-# INLINE distinguisher #-}
