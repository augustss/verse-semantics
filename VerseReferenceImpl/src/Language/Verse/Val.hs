{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Val
  ( Val (..)
  ) where

import Control.Applicative

import Data.Eq
import Data.Function
import Data.Functor
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Maybe
import Data.Ratio
import Data.Traversable
import Data.Tuple
import Data.Unifiable

import Language.Verse.Ident
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Simplify.Exp qualified as Simplify

import Prelude (Double, Integer)
import Prettyprinter

import Text.Show

data Val a
  = Int !Integer
  | Float !Double
  | Rational !Rational
  | Truth a
  | Function !(IdentMap Name a) !(IdentSet Name) !Exp !Exp
  | Tuple [a] deriving (Show, Functor, Foldable, Traversable)

type IdentSet a = HashSet (Ident a)

type IdentMap a v = HashMap (Ident a) v

type Exp = L (Simplify.Exp L (Ident Name))

instance Unifiable Val where
  zipMatch  = curry $ \ case
    (Truth x, Truth y) -> Just $ Truth (x, y)
    (Int x, Int y) | x == y -> Just $ Int x
    (Rational x, Rational y) | x == y -> Just $ Rational x
    (Float x, Float y) | x == y -> Just $ Float x
    (Tuple xs, Tuple ys) -> Tuple <$> zipMatch xs ys
    _ -> Nothing

instance Pretty a => Pretty (Val a) where
  pretty = \ case
    Int x -> pretty x
    Float x -> pretty x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x -> "truth" <> lbrace <> pretty x <> rbrace
    Function _ _ _ _ -> "function"
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
