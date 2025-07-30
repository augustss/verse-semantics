{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Path
  ( Path (..)
  ) where

import Language.Verse.SimpleName

import Prettyprinter

data Path = Path
  {-# UNPACK #-} !SimpleName
  [(Maybe Path, SimpleName)] deriving (Eq, Show)

instance Pretty Path where
  pretty (Path label pathIdents) =
    "/" <> pretty label <> foldr f mempty pathIdents
    where
      f (path, ident) doc = case path of
        Nothing -> "/" <> pretty ident <> doc
        Just path -> "/(" <> pretty path <> ":)" <> pretty ident <> doc
