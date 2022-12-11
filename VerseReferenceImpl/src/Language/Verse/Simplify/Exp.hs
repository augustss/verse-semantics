{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Simplify.Exp
  ( Exp (..)
  ) where

import Control.Comonad

import Data.HashSet (HashSet)

import Prettyprinter

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :<: f (Exp f a)
  | f (Exp f a) :<=: f (Exp f a)
  | f (Exp f a) :>: f (Exp f a)
  | f (Exp f a) :>=: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | f (Exp f a) :+: f (Exp f a)
  | f (Exp f a) :-: f (Exp f a)
  | f (Exp f a) :*: f (Exp f a)
  | f (Exp f a) :/: f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Query (f (Exp f a))
  | IfThenElse !(HashSet a) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(HashSet a) (f (Exp f a)) (f (Exp f a))
  | Exists (f a) (f (Exp f a))
  | Invoke (f (Exp f a)) (f (Exp f a))
  | Lambda (f a) !(HashSet a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int Integer
  | Float Double
  | Name a
  | IsInt (f (Exp f a))

deriving instance (Show (f (Exp f a)), Show (f a), Show a) => Show (Exp f a)

instance (Comonad f, Pretty a) => Pretty (Exp f a) where
  pretty = prettyPrec 0

prettyPrec :: (Comonad f, Pretty a) => Int -> Exp f a -> Doc ann
prettyPrec prec = \ case
  e1 :*>: e2 ->
    prettyParens (prec > 0) $ prettyPrecC 0 e1 <> line <> prettyPrecC 0 e2
  e1 :=: e2 ->
    prettyParens (prec > 2) $ prettyPrecC 2 e1 <+> pretty '=' <+> prettyPrecC 2 e2
  e1 :<: e2 ->
    prettyParens (prec > 4) $ prettyPrecC 4 e1 <+> pretty '<' <+> prettyPrecC 4 e2
  e1 :<=: e2 ->
    prettyParens (prec > 4) $ prettyPrecC 4 e1 <+> "<=" <+> prettyPrecC 4 e2
  e1 :>: e2 ->
    prettyParens (prec > 4) $ prettyPrecC 4 e1 <+> pretty '>' <+> prettyPrecC 4 e2
  e1 :>=: e2 ->
    prettyParens (prec > 4) $ prettyPrecC 4 e1 <+> ">=" <+> prettyPrecC 4 e2
  e1 :|: e2 ->
    prettyParens (prec > 6) $ prettyPrecC 6 e1 <+> pretty '|' <+> prettyPrecC 6 e2
  e1 :+: e2 ->
    prettyParens (prec > 7) $ prettyPrecC 7 e1 <+> pretty '+' <+> prettyPrecC 7 e2
  e1 :-: e2 ->
    prettyParens (prec > 7) $ prettyPrecC 7 e1 <+> pretty '-' <+> prettyPrecC 7 e2
  e1 :*: e2 ->
    prettyParens (prec > 8) $ prettyPrecC 8 e1 <+> pretty '*' <+> prettyPrecC 8 e2
  e1 :/: e2 ->
    prettyParens (prec > 8) $ prettyPrecC 8 e1 <+> pretty '/' <+> prettyPrecC 8 e2
  Fail ->
    "fail"
  One e ->
    nest 4 $ "one:" <> line <> prettyPrecC 0 e
  All e ->
    nest 4 $ "all:" <> line <> prettyPrecC 0 e
  Not e ->
    prettyParens (prec > 5) "not" <+> prettyPrecC 5 e
  Query e ->
    prettyParens (prec > 9) $ prettyPrecC 9 e <> pretty '?'
  IfThenElse _ e1 e2 e3 ->
    nest 4 ("if:" <> line <> prettyPrecC 0 e1) <> line <>
    nest 4 ("then:" <> line <> prettyPrecC 0 e2) <> line <>
    nest 4 ("else:" <> line <> prettyPrecC 0 e3)
  ForDo _ e1 e2 ->
    nest 4 ("for:" <> line <> prettyPrecC 0 e1) <> line <>
    nest 4 ("do:" <> line <> prettyPrecC 0 e2)
  Exists x e ->
    "exists" <+> prettyC x <+> dot <> line <>
    prettyPrecC 0 e
  Invoke e1 e2 ->
    prettyParens (prec > 11) $ prettyPrecC 11 e1 <> prettyPrecC 11 e2
  Lambda x _ e ->
    parens $ nest 4 $ "lambda" <+> prettyC x <+> dot <> line <> prettyPrecC 0 e
  Tuple es ->
    prettyParens (prec > 1) $ concatWith (\ x y -> x <> comma <+> y) $ prettyPrecC 1 <$> es
  Truth e ->
    nest 4 $ "truth:" <> line <> prettyPrecC 0 e
  Int x ->
    prettyParens (prec > 10) $ pretty x
  Float x ->
    prettyParens (prec > 10) $ pretty x
  Name x ->
    prettyParens (prec > 10) $ pretty x
  IsInt e ->
    "isInt" <> prettyPrecC 11 e
  where
    prettyParens = \ case
      False -> id
      True -> parens
    prettyC =
      pretty . extract
    prettyPrecC prec =
      prettyPrec prec . extract
