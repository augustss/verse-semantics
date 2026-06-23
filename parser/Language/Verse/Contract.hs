{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Contract
  ( Contract (..)
  ) where

import Prettyprinter

data Contract
  = Any
  | Rational
  | Int
  | Float
  | Char
  | Char32
  | Function deriving (Eq, Show)

instance Pretty Contract where
  pretty = \ case
    Any -> "any"
    Rational -> "rational"
    Int -> "int"
    Float -> "float"
    Char -> "char"
    Char32 -> "char32"
    Function -> "function"
