{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Verse.Path
  ( Path (..)
  ) where

import Language.Verse.SimpleName

import Prettyprinter

import Data.Hashable
import GHC.Generics

data Path = Path
  {-# UNPACK #-} !SimpleName
  [(Maybe Path, SimpleName)]
  deriving (Eq, Show, Generic, Hashable)

instance Pretty Path where
  pretty (Path label pathIdents) =
    "/" <> pretty label <> foldr f mempty pathIdents
    where
      f (path, ident) doc = case path of
        Nothing -> "/" <> pretty ident <> doc
        Just path -> "/(" <> pretty path <> ":)" <> pretty ident <> doc
