{-# LANGUAGE LambdaCase #-}
module Language.Verse.Intrinsic
  ( Intrinsic (..)
  , toString
  ) where

import Data.String

import Prettyprinter

data Intrinsic
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Plus
  | PrefixPlus
  | Minus
  | PrefixMinus
  | Multiply
  | Divide
  | To
  | Any
  | Int
  | Float
  | Char
  | Char32
  | Function
  | Query deriving (Show, Eq)

toString :: Intrinsic -> String
toString = \ case
  Less -> "operator'<'"
  LessEqual -> "operator'<='"
  Greater -> "operator'>'"
  GreaterEqual -> "operator'>='"
  Plus -> "operator'+'"
  PrefixPlus -> "prefix'+'"
  Minus -> "operator'-'"
  PrefixMinus -> "prefix'-'"
  Multiply -> "operator'*'"
  Divide -> "operator'/'"
  To -> "operator'..'"
  Any -> "any"
  Int -> "int"
  Float -> "float"
  Char -> "char"
  Char32 -> "char32"
  Function -> "function"
  Query -> "postfix'?'"

instance Pretty Intrinsic where
  pretty = fromString . toString
