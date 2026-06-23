{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Verse.Effect.Split
  ( Effect (..)
  ) where

import Data.Hashable
import GHC.Generics
import Prettyprinter

data Effect = Fails | Succeeds | Decides
  deriving (Eq, Show, Generic, Hashable)

instance Pretty Effect where
  pretty = \ case
    Fails -> "fails"
    Succeeds -> "succeeds"
    Decides -> "decides"
