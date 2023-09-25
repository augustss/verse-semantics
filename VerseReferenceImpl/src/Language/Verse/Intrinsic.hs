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
  | Int
  | Float
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
  Int -> "int"
  Float -> "float"
  Query -> "postfix'?'"

instance Pretty Intrinsic where
  pretty = fromString . toString
