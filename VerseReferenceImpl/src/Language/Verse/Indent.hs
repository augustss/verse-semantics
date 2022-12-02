{-# LANGUAGE LambdaCase #-}
module Language.Verse.Indent
  ( Indent
  , White (..)
  ) where

import Prettyprinter

type Indent = [White]

data White = Space | Tab deriving (Show, Eq)

instance Pretty White where
  pretty = \ case
    Space ->space
    Tab -> backslash <> pretty 't'
