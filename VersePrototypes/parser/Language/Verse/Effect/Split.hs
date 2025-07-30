{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Effect.Split
  ( Effect (..)
  ) where

import Prettyprinter

data Effect = Fails | Succeeds | Decides deriving (Eq, Show)

instance Pretty Effect where
  pretty = \ case
    Fails -> "fails"
    Succeeds -> "succeeds"
    Decides -> "decides"
