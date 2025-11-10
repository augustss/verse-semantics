{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Eval.Val
  ( Val (..)
  , Env
  ) where

import Control.Applicative
import Control.Monad

import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Prettyprinter

import Verse.Eval.Fun (Fun)
import Verse.Exp (LExp)
import Verse.Monad
import Verse.Name

data Val f a
  = Int !Integer
  | Lam !(Env a) {-# UNPACK #-} !Name LExp
  | Tup [a]
  | Fun !Fun
  | Ptr !(f a)
  | Map !(IntMap [a]) deriving Show

type Env = HashMap Name

instance (Pretty (f a), Pretty a) => Pretty (Val f a) where
  pretty = \ case
    Int x ->
      pretty x
    Lam _ x e ->
      "fun" <>
      lparen <> pretty x <> rparen <+>
      lbrace <+> pretty e <+> rbrace
    Tup [x] ->
      "all" <> lbrace <> pretty x <> rbrace
    Tup x ->
      lparen <> hcat (punctuate comma $ pretty <$> x) <> rparen
    Fun x ->
      pretty x
    Ptr x ->
      pretty x
    Map (IntMap.toList -> x)
      | null x ->
          "fun" <>
          lparen <> "Arg" <> rparen <+>
          lbrace <+> "fail" <+> rbrace
      | otherwise ->
          let
            one x =
              "one" <> lbrace <> x <> rbrace
            f (k, v) =
              lparen <>
              one ("Arg" <+> equals <+> pretty k) <> semi <+>
              hcat (punctuate pipe $ pretty <$> v) <>
              rparen
          in
            "fun" <>
            lparen <> "Arg" <> rparen <>
            lbrace <+> hcat (punctuate pipe $ f <$> x) <+> rbrace

instance Vars a m => Vars (Val f a) m where
  vars f = \ case
    x@Int {} -> pure x
    Lam r x e -> vars f r <&> \ r -> Lam r x e
    Tup x -> Tup <$> vars f x
    x@Fun {} -> pure x
    x@Ptr {} -> pure x
    Map xs -> Map <$> traverse (vars f) xs

instance (Eq (f a), ZipVars_ a m) => ZipVars_ (Val f a) m where
  zipVars_ f = curry $ \ case
    (Int x, Int y) -> guard $ x == y
    (Tup x, Tup y) -> zipVars_ f x y
    (Ptr x, Ptr y) -> guard $ x == y
    _ -> empty
