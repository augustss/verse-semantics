{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Verse.Exp
  ( ExpF (..)
  , Exp
  , LExp
  ) where

import Prettyprinter

import Fix
import Loc

import Verse.Name

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

instance PrettyPrec (f (Fix f)) => PrettyPrec (Fix f) where
  prettyPrec prec = prettyPrec prec . getFix

prettyParens :: Bool -> Doc ann -> Doc ann
prettyParens = \ case
  True -> parens
  False -> id

data ExpF a
  = Var {-# UNPACK #-} !Name
  | Abs {-# UNPACK #-} !Name a
  | App a a
  | Exi {-# UNPACK #-} !Name a
  | Int !Integer
  | a :& a
  | Tup [a]
  | a := a
  | a :< a
  | a :| a
  | a :.. a
  | a :+ a
  | a :- a
  | Fail
  | All a
  | For a {-# UNPACK #-} !Name a
  | One a
  | If a {-# UNPACK #-} !Name a a deriving Show

instance PrettyPrec a => Pretty (ExpF a) where
  pretty = prettyPrec 0

instance PrettyPrec a => PrettyPrec (ExpF a) where
  prettyPrec prec = \ case
    Var x ->
      pretty x
    Abs x e ->
      "fun" <>
      lparen <> pretty x <> rparen <+>
      lbrace <+> prettyPrec 0 e <+> rbrace
    App e1 e2 ->
      prettyPrec 7 e1 <>
      lbracket <> prettyPrec 0 e2 <> rbracket
    Exi x e ->
      "exists" <+> pretty x <> semi <+>
      prettyPrec 0 e
    Int x ->
      pretty x
    e1 :& e2 ->
      prettyParens (prec > 0) $
      prettyPrec 1 e1 <> semi <+> prettyPrec 1 e2
    Tup xs ->
      prettyParens (prec > 1) . hcat . punctuate comma $ prettyPrec 2 <$> xs
    e1 := e2 ->
      prettyParens (prec > 2) $
      prettyPrec 3 e1 <> equals <> prettyPrec 3 e2
    e1 :< e2 ->
      prettyParens (prec > 3) $
      prettyPrec 4 e1 <> langle <> prettyPrec 4 e2
    e1 :| e2 ->
      prettyParens (prec > 4) $
      prettyPrec 5 e1 <> pipe <> prettyPrec 5 e2
    e1 :.. e2 ->
      prettyParens (prec > 5) $
      prettyPrec 6 e1 <> dot <> dot <> prettyPrec 6 e2
    e1 :+ e2 ->
      prettyParens (prec > 6) $
      prettyPrec 7 e1 <> pretty '+' <> prettyPrec 7 e2
    e1 :- e2 ->
      prettyParens (prec > 6) $
      prettyPrec 7 e1 <> pretty '-' <> prettyPrec 7 e2
    Fail ->
      "fail"
    All e ->
      "all" <> lbrace <> prettyPrec 0 e <> rbrace
    For e1 x e2 ->
      "for" <+>
      lparen <> prettyPrec 0 e1 <> rparen <+>
      "do" <> lparen <> pretty x <> rparen <+>
      lbrace <> prettyPrec 0 e2 <> rbrace
    One e ->
      "one" <> lbrace <> prettyPrec 0 e <> rbrace
    If e1 x e2 e3 ->
      "if" <+>
      lparen <> prettyPrec 0 e1 <> rparen <+>
      "then" <> lparen <> pretty x <> rparen <+>
      lbrace <> prettyPrec 0 e2 <+> rbrace <>
      "else" <+> lbrace <> prettyPrec 0 e3 <> rbrace

type Exp = Fix ExpF

instance PrettyPrec (f (L f)) => PrettyPrec (L f) where
  prettyPrec prec = prettyPrec prec . unwrap

type LExp = L ExpF
