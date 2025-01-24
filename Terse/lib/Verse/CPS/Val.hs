{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.CPS.Val
  ( Val (..)
  , Env
  ) where

import Data.HashMap.Strict (HashMap)

import Prettyprinter

import Verse.CPS.Exp (Label, LExp)
import Verse.Fun
import Verse.Name

data Val
  = Int !Integer
  | Lam
    !Env
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
    LExp
  | Tup [Val]
  | Fun !Fun deriving Show

type Env = HashMap Name Val

instance Pretty Val where
  pretty = \ case
    Int x ->
      pretty x
    Lam _ x _r _s _yk _sk _fk _ek _e ->
      "fun" <>
      lparen <> pretty x <> rparen <+>
      lbrace <+> ".." <+> rbrace
    Tup [x] ->
      "all" <> lbrace <> pretty x <> rbrace
    Tup x ->
      lparen <> hcat (punctuate comma $ pretty <$> x) <> rparen
    Fun x ->
      pretty x
