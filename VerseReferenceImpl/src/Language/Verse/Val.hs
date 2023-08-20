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
  , Overload (..)
  ) where

import Control.Monad
import Control.Monad.Verse

import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Match
import Data.Ratio

import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Name

data Val ref a
  = Int !Integer
  | Float {-# UNPACK #-} !Double
  | Rational !Rational
  | Truth a
  | Tuple [a]
  | Module {-# UNPACK #-} !Label !(HashMap Name a)
  | StructInst {-# UNPACK #-} !Label !(HashMap Name a)
  | ClassInst {-# UNPACK #-} !Label !(Maybe a) !(HashMap Name a)
  | Overloads !(Overload a) a
  | Ptr !(ref (Val ref)) deriving (Functor, Foldable, Traversable)

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
      StructInst i (HashMap.intersectionWith (,) xs ys)
    (ClassInst i x xs, ClassInst j y ys) ->
      Zip $ guard (i == j) $>
      ClassInst i (liftA2 (,) x y) (HashMap.intersectionWith (,) xs ys)
    (Overloads x xs, Overloads y ys) -> case zipMatch x y of
      Just x -> Zip . Just $ Overloads x (xs, ys)
      Nothing -> Uncons (Overloads x) xs (Overloads y) ys
    (Ptr x, Ptr y) -> Zip $ guard (x == y) $> Ptr x
    _ -> Zip Nothing

instance ( Freezable a b m
         , Freezable (f (Val f)) (g (Val g)) m
         ) => Freezable (Val f a) (Val g b) m where
  freeze = \ case
    Int x -> pure $ Int x
    Float x -> pure $ Float x
    Rational x -> pure $ Rational x
    Truth x -> Truth <$> freeze x
    Tuple xs -> Tuple <$> traverse freeze xs
    Module i xs -> Module i <$> traverse freeze xs
    StructInst i xs -> StructInst i <$> traverse freeze xs
    ClassInst i x xs -> ClassInst i <$> traverse freeze x <*> traverse freeze xs
    Overloads x xs -> Overloads <$> freeze x <*> freeze xs
    Ptr x -> Ptr <$> freeze x

data Overload a
  = Function {-# UNPACK #-} !Label !(IdentMap a) !(IdentMap Bool) Exp Exp
  | Struct {-# UNPACK #-} !Label !(IdentMap a) !(IdentMap Bool) Exp
  | Class {-# UNPACK #-} !Label !(IdentMap a) (Maybe a) !(IdentMap Bool) Exp
  | Intrinsic !Intrinsic deriving (Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L Ident)

instance RowMatchable Overload

instance ZipMatchable Overload where
  zipMatch = curry $ \ case
    (Function i_x env_x xs e1 e2, Function i_y env_y _ _ _) ->
      guard (i_x == i_y) $>
      Function i_x (HashMap.intersectionWith (,) env_x env_y) xs e1 e2
    (Struct i_x env_x xs e1, Struct i_y env_y _ _) ->
      guard (i_x == i_y) $>
      Struct i_x (HashMap.intersectionWith (,) env_x env_y) xs e1
    (Class i_x env_x x xs e1, Class i_y env_y y _ _) ->
      guard (i_x == i_y) $>
      Class i_x (HashMap.intersectionWith (,) env_x env_y) (liftA2 (,) x y) xs e1
    (Intrinsic x, Intrinsic y) -> guard (x == y) $> Intrinsic x
    _ -> Nothing

instance Freezable a b m => Freezable (Overload a) (Overload b) m where
  freeze = \ case
    Function i env xs e1 e2 -> traverse freeze env <&> \ env ->
      Function i env xs e1 e2
    Struct i env xs e1 -> traverse freeze env <&> \ env ->
      Struct i env xs e1
    Class i env x xs e1 ->
      (\ env x -> Class i env x xs e1) <$>
      traverse freeze env <*>
      traverse freeze x
    Intrinsic x -> pure $ Intrinsic x
