{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Verse.Access
  ( Access (..)
  ) where

import Data.Hashable
import GHC.Generics

import Prettyprinter

data Access
  = Public
  | Protected
  | Private
  | Internal deriving (Eq, Show, Generic, Hashable)

instance Pretty Access where
  pretty = \ case
    Private -> "private"
    Protected -> "protected"
    Public -> "public"
    Internal -> "internal"
