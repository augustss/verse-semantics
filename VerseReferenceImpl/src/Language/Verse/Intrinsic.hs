module Language.Verse.Intrinsic
  ( Intrinsic (..)
  ) where

import Prettyprinter

data Intrinsic
  = Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Plus
  | Minus
  | Multiply
  | Divide
  | Int
  | Float deriving (Show, Eq)

instance Pretty Intrinsic where
  pretty = unsafeViaShow
