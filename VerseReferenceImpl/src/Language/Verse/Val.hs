{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Val
  ( Val (..)
  , VarVal
  , FrozenVal
  , Overload (..)
  , Named (..)
  ) where

import Control.Monad
import Control.Monad.Verse (Var, VarRef, Freezable (..), Frozen, Freshenable (..))

import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Match
import Data.Ratio
import Data.Traversable

import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Name

import Prettyprinter

data Val ref a
  = Int !Integer
  | Float {-# UNPACK #-} !Double
  | Rational !Rational
  | Truth a
  | Tuple [a]
  | Module {-# UNPACK #-} !Label !(Env Name ref a)
  | StructInst {-# UNPACK #-} !Label !(Env Name ref a)
  | ClassInst {-# UNPACK #-} !Label !(Maybe a) !(Env Name ref a)
  | Overloads !(Overload ref a) a deriving (Functor, Foldable, Traversable)

type VarVal m = Var m (Val (VarRef m))

type FrozenVal = Frozen (Val Frozen)

instance Eq (ref (Val ref)) => RowMatchable (Val ref) where
  rowMatch = curry $ \ case
    (Truth x, Truth y) ->
      Zip . Just $ Truth (x, y)
    (Int x, Int y) ->
      Zip $ guard (x == y) $> Int x
    (Int x, Rational y) ->
      Zip $ guard (1 == denominator y && x == numerator y) $> Int x
    (Rational x, Int y) ->
      Zip $ guard (denominator x == 1 && numerator x == y) $> Int y
    (Rational x, Rational y) ->
      Zip $ guard (x == y) $> Rational x
    (Float x, Float y) ->
      Zip $ guard (if isNaN x then isNaN y else x == y) $> Float x
    (Tuple xs, Tuple ys) ->
      Zip $ Tuple <$> zipMatch xs ys
    (StructInst i xs, StructInst j ys) ->
      Zip $ guard (i == j) $>
      StructInst i (zipMatchEnv xs ys)
    (ClassInst i x xs, ClassInst j y ys) ->
      Zip $ guard (i == j) $>
      ClassInst i (liftA2 (,) x y) (zipMatchEnv xs ys)
    (Overloads x xs, Overloads y ys) -> case zipMatch x y of
      Just x -> Zip . Just $ Overloads x (xs, ys)
      Nothing -> Uncons (Overloads x) xs (Overloads y) ys
    _ -> Zip Nothing

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Val f a) (Val g b) m where
  freeze = \ case
    Int x -> pure $ Int x
    Float x -> pure $ Float x
    Rational x -> pure $ Rational x
    Truth x -> Truth <$> freeze x
    Tuple xs -> Tuple <$> for xs freeze
    Module i xs -> Module i <$> for xs freeze
    StructInst i xs -> StructInst i <$> for xs freeze
    ClassInst i x xs -> ClassInst i <$> for x freeze <*> for xs freeze
    Overloads x xs -> Overloads <$> freeze x <*> freeze xs

instance (Pretty (ref (Val ref)), Pretty a) => Pretty (Val ref a) where
  pretty = \ case
    Int x -> pretty x
    Float x -> pretty x
    Rational x | denominator x == 1 -> pretty $ numerator x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x -> align $ "truth" <> group (braces $ pretty x)
    Overloads {} -> "function"
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
    Module i xs ->
      align $
      "module#" <>
      prettyLabel i <>
      group (braced $ prettyNames xs)
    StructInst i xs ->
      align $
      "struct#" <>
      prettyLabel i <>
      group (braced $ prettyNames xs)
    ClassInst i Nothing xs ->
      align $
      "class#" <>
      prettyLabel i <>
      group (braced $ prettyNames xs)
    ClassInst i (Just x) xs ->
      align $
      "class#" <>
      prettyLabel i <>
      parens (pretty x) <>
      group (braced $ prettyNames xs)
    where
      prettyNames xs = HashMap.toList xs <&> \ (k, v) ->
        align $ pretty k <+> ":=" <> group (nest 2 $ line <> pretty v)
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      braces x =
        flatAlt (hardline <> "{ ") lbrace <>
        x <>
        flatAlt (hardline <> rbrace) rbrace
      braced =
        group .
        encloseSep
        (flatAlt (hardline <> "{ ") lbrace)
        (flatAlt (hardline <> rbrace) rbrace)
        ", "

data Overload ref a
  = Function {-# UNPACK #-} !Label !(Env Ident ref a) !(IdentMap Bool) Exp Exp
  | Struct {-# UNPACK #-} !Label !(Env Ident ref a) !(IdentMap Bool) Exp
  | Class {-# UNPACK #-} !Label !(Env Ident ref a) (Maybe a) !(IdentMap Bool) Exp
  | Intrinsic !Intrinsic deriving (Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L Ident)

instance Eq (ref (Val ref)) => RowMatchable (Overload ref)

instance Eq (ref (Val ref)) => ZipMatchable (Overload ref) where
  zipMatch = curry $ \ case
    (Function i_x env_x xs e1 e2, Function i_y env_y _ _ _) ->
      guard (i_x == i_y) $>
      Function i_x (zipMatchEnv env_x env_y) xs e1 e2
    (Struct i_x env_x xs e1, Struct i_y env_y _ _) ->
      guard (i_x == i_y) $>
      Struct i_x (zipMatchEnv env_x env_y) xs e1
    (Class i_x env_x sup_x xs e1, Class i_y env_y sup_y _ _) ->
      guard (i_x == i_y) $>
      Class i_x (zipMatchEnv env_x env_y) (liftA2 (,) sup_x sup_y) xs e1
    (Intrinsic x, Intrinsic y) -> guard (x == y) $> Intrinsic x
    _ -> Nothing

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Overload f a) (Overload g b) m where
  freeze = \ case
    Function i env xs e1 e2 -> for env freeze <&> \ env ->
      Function i env xs e1 e2
    Struct i env xs e1 -> for env freeze <&> \ env ->
      Struct i env xs e1
    Class i env sup xs e1 ->
      (\ env sup -> Class i env sup xs e1) <$> for env freeze <*> for sup freeze
    Intrinsic x -> pure $ Intrinsic x

data Named ref a
  = Val a
  | Ref (ref (Val ref)) deriving (Functor, Foldable, Traversable)

instance Eq (ref (Val ref)) => RowMatchable (Named ref)

instance Eq (ref (Val ref)) => ZipMatchable (Named ref) where
  zipMatch = curry $ \ case
    (Val x, Val y) -> Just $ Val (x, y)
    (Ref x, Ref y) -> guard (x == y) $> Ref x
    _ -> Nothing

instance Freshenable a m => Freshenable (Named ref a) m where
  freshen = \ case
    Val x -> Val <$> freshen x
    Ref x -> pure $ Ref x

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Named f a) (Named g b) m where
  freeze = \ case
    Val x -> Val <$> freeze x
    Ref x -> Ref <$> freeze x

instance (Pretty (ref (Val ref)), Pretty a) => Pretty (Named ref a) where
  pretty = \ case
    Val x -> pretty x
    Ref x -> pretty x

type Env k ref a = HashMap k (Named ref a)

zipMatchEnv :: (Hashable k, Eq (ref (Val ref)))
            => Env k ref a
            -> Env k ref b
            -> Env k ref (a, b)
zipMatchEnv x y = HashMap.mapMaybe id $ HashMap.intersectionWith zipMatch x y
