{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
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

import Verse.Eval
import Verse.Eval.Val
import Verse.Parse

run :: Text -> IO (Either (Doc AnsiStyle) [Fix (Val Identity)])
run input = case parse input of
  Left pos -> pure . Left $ prettyParseError input pos
  Right x -> eval x <&> \ case
    Right xs -> Right xs
    Left xs -> Left $ prettyStuckError input xs
