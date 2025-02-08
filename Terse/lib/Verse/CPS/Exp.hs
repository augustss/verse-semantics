{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.CPS.Exp
  ( ExpF (..)
  , Exp
  , LExp
  , Val (..)
  , Label
  ) where

import Prettyprinter

import Fix
import Loc

import Verse.Name

data ExpF a
  = Let
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Name -- Parameter
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
    a
  | App
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | LetSucceed
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Fail continuation
    a
    a
  | AppSucceed
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Fail continuation
  | LetFail
    {-# UNPACK #-} !Label
    a
    a
  | AppFail {-# UNPACK #-} !Label
  | Exi {-# UNPACK #-} !Name a
  | Tup
    [Val]
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
  | Eq
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | Less
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | Plus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
  | Minus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
  deriving Show

instance Pretty a => Pretty (ExpF a) where
  pretty = \ case
    LetSucceed f x state fail e1 e2 ->
      "letSucceed" <+>
      prettyLabel f <+>
      prettyLabel x <+>
      prettyLabel state <+>
      prettyLabel fail <+> equals <> nest 2 (line <> pretty e1) <> line <>
      "in" <+> pretty e2
    AppSucceed f x state fail ->
      prettyLabel f <+>
      pretty x <+>
      prettyLabel state <+>
      prettyLabel fail
    LetFail f e1 e2 ->
      "letFail" <+>
      prettyLabel f <+> equals <> nest 2 (line <> pretty e1) <> line <>
      "in" <+> pretty e2
    AppFail f ->
      prettyLabel f
    Less x y env state yield succeed fail empty ->
      parens (pretty x <+> pretty '<' <+> pretty y) <>
      braced [env, state, yield, succeed, fail, empty]
    Plus x y env state yield succeed fail ->
      parens (pretty x <+> pretty '+' <+> pretty y) <>
      braced [env, state, yield, succeed, fail]
    Minus x y env state yield succeed fail ->
      parens (pretty x <+> pretty '-' <+> pretty y) <>
      braced [env, state, yield, succeed, fail]
    where
      braced = encloseSep lbrace rbrace comma . fmap prettyLabel

type Exp = Fix ExpF

type LExp = L ExpF

data Val
  = Var {-# UNPACK #-} !Name
  | Label {-# UNPACK #-} !Label
  | Int !Integer deriving Show

instance Pretty Val where
  pretty = \ case
    Var x -> pretty x
    Label x -> prettyLabel x
    Int x -> pretty x

type Label = Int

prettyLabel :: Label -> Doc ann
prettyLabel = (pretty '#' <>) . pretty
