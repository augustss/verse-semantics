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
import Control.Monad.Ref
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

data Val m a
  = Int !Integer
  | Float {-# UNPACK #-} !Double
  | Rational !Rational
  | Truth a
  | Tuple [a]
  | Module {-# UNPACK #-} !Label !(HashMap Name a)
  | StructInst {-# UNPACK #-} !Label !(HashMap Name a)
  | ClassInst {-# UNPACK #-} !Label !(Maybe a) !(HashMap Name a)
  | Overloads !(Overload a) a
  | Ptr !(VarRef m (Val m)) deriving (Functor, Foldable, Traversable)

instance Eq (Ref m (Var m (Val m))) => RowMatchable (Val m) where
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
    (Class i_x env_x super_x xs e1, Class i_y env_y super_y _ _) ->
      guard (i_x == i_y) $>
      Class i_x (HashMap.intersectionWith (,) env_x env_y)
      (liftA2 (,) super_x super_y) xs e1
    (Intrinsic x, Intrinsic y) -> guard (x == y) $> Intrinsic x
    _ -> Nothing
