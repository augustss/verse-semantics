{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse
  ( run
  ) where

import Data.Functor
import Data.Functor.Identity

import Prettyprinter
import Prettyprinter.Render.Terminal

import Fix
import Loc
import Pos
import Text (Text)

import Verse.Core.Eval
import Verse.Core.Parse
import Verse.Core.Val

run :: Text -> IO (Either (Doc AnsiStyle) [Fix (Val Identity)])
run input = case parse input of
  Left pos ->
    pure . Left $ prettyParseError input pos
  Right x -> eval x <&> \ case
    Right xs -> Right xs
    Left xs -> Left $ prettyStuckError input xs
