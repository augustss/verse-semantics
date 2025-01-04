{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse
  ( run
  ) where

import Control.Monad.IO.Class

import Data.Functor
import Data.Functor.Identity

import Prettyprinter
import Prettyprinter.Render.Terminal

import Fix
import Loc
import Ref
import Text (Text)

import Verse.Eval
import Verse.Parse
import Verse.Val

run
  :: (MonadIO m, MonadRef m)
  => Text -> m (Either (Doc AnsiStyle) [Fix (Val Identity)])
run xs = case parse xs of
  Left e ->
    pure . Left . annotate bold $ "Parse" <+> "error" <> colon <+> pretty e
  Right x ->
    let
      prettyStuck' = prettyStuck xs
    in
      eval x <&> \ case
        Right xs -> Right xs
        Left xs -> Left $ prettyStuck' xs
