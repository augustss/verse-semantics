{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Eval.Fun
  ( Fun (..)
  ) where

import Prettyprinter

data Fun
  = Plus
  | Minus
  | Less
  | Alloc
  | Read
  | Write
  | GetLine
  | ReadInt
  | Print
  | Map deriving (Show, Bounded, Enum)

instance Pretty Fun where
  pretty = \ case
    Plus -> "operator'+'"
    Minus -> "operator'-'"
    Less -> "operator'<'"
    Alloc -> "Alloc"
    Read -> "Read"
    Write -> "Write"
    GetLine -> "GetLine"
    ReadInt -> "ReadInt"
    Print -> "Print"
    Map -> "Map"
