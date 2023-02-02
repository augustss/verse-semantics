{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Overload
  ( Overload (..)
  , hoist
  ) where

import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Name

import Prettyprinter

data Overload f a
  = Function !Label !(IdentMap Name (f a)) !(IdentMap Name Bool) Exp Exp
  | Struct !Label !(IdentMap Name (f a)) !(IdentMap Name Bool) Exp
  | Class !Label !(IdentMap Name (f a)) (Maybe a) !(IdentMap Name Bool) Exp
  | Intrinsic Intrinsic deriving (Show, Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L (Ident Name))

instance Eq (Overload f a) where
  (==) = curry $ \ case
    (Function x _ _ _ _, Function y _ _ _ _) -> x == y
    (Struct x _ _ _, Struct y _ _ _) -> x == y
    (Class x _ _ _ _, Class y _ _ _ _) -> x == y
    (Intrinsic x, Intrinsic y) -> x == y
    _ -> False

instance Pretty (Overload f a) where
  pretty = \ case
    Function x _ _ _ _ -> "function#" <> prettyLabel x
    Struct x _ _ _ -> "struct#" <> prettyLabel x
    Class x _ _ _ _ -> "class#" <> prettyLabel x
    Intrinsic x -> pretty x

hoist :: (forall b . f b -> g b) -> Overload f a -> Overload g a
hoist f = \ case
  Function i env xs e1 e2 -> Function i (f <$> env) xs e1 e2
  Struct i env xs e -> Struct i (f <$> env) xs e
  Class i env x xs e -> Class i (f <$> env) x xs e
  Intrinsic x -> Intrinsic x
