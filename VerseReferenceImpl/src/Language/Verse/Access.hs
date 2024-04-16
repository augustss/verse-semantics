{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Access
  ( Access (..)
  ) where

import Prettyprinter

data Access
  = Public
  | Protected
  | Private
  | Internal deriving (Eq, Show)

instance Pretty Access where
  pretty = \ case
    Private -> "private"
    Protected -> "protected"
    Public -> "public"
    Internal -> "internal"
