{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
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

data Val a
  = Int !Integer
  | Lam
    !(Env a)
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
    LExp
  | Tup [a]
  | Fun !Fun deriving Show

type Env = HashMap Name

instance Pretty a => Pretty (Val a) where
  pretty = \ case
    Int x ->
      pretty x
    Lam _ x _s _sk _fk _ek _e ->
      "fun" <>
      lparen <> pretty x <> rparen <+>
      lbrace <+> ".." <+> rbrace
    Tup [x] ->
      "all" <> lbrace <> pretty x <> rbrace
    Tup x ->
      lparen <> hcat (punctuate comma $ pretty <$> x) <> rparen
    Fun x ->
      pretty x
