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
import Pos
import Ref
import Text (Text)

import Verse.Eval
import Verse.Parse
import Verse.Val

run
  :: (MonadIO m, MonadRef m)
  => Text -> m (Either (Doc AnsiStyle) [Fix (Val Identity)])
run input = case parse input of
  Left (pos, ann) ->
    pure . Left $ prettyParseError input pos ann
  Right x -> eval x <&> \ case
    Right xs -> Right xs
    Left xs -> Left $ prettyStuckError input xs
