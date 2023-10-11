{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Val
  ( Val (..)
  , VarVal
  , FrozenVal
  , Overload (..)
  , Named (..)
  , Env
  ) where

import Control.Monad
import Control.Monad.Verse (Var, VarRef, Freezable (..), Frozen, Freshenable (..))

import Data.Foldable
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
  = Any
  | AnyRational
  | Rational !Rational
  | AnyInt
  | Int !Integer
  | AnyFloat
  | Float {-# UNPACK #-} !Double
  | Truth a
  | Tuple [a]
  | Module {-# UNPACK #-} !Label !(Env Name ref a)
  | Enum {-# UNPACK #-} !Label !(Env Name ref a) [a]
  | EnumValue {-# UNPACK #-} !Label {-# UNPACK #-} !Name
  | StructInst {-# UNPACK #-} !Label !(Env Name ref a)
  | ClassInst {-# UNPACK #-} !Label !(Maybe a) !(Env Name ref a)
  | Overloads !(Overload ref a) a deriving (Functor, Foldable, Traversable)

type VarVal m = Var m (Val (VarRef m))

type FrozenVal = Frozen (Val Frozen)

instance Eq (ref (Val ref)) => RowMatchable (Val ref) where
  rowMatch = curry $ \ case
    (x, Any) -> LE $ toList x <&> (, Any)
    (Any, x) -> GE $ (Any,) <$> toList x
    (AnyRational, AnyRational) -> LE []
    (AnyRational, Rational _) -> GE []
    (AnyRational, AnyInt) -> GE []
    (AnyRational, Int _) -> GE []
    (Rational _, AnyRational) -> LE []
    (Rational x, Rational y) -> Zip $ guard (x == y) $> []
    (Rational x, AnyInt) | denominator x == 1 -> LE []
    (Rational x, Int y) | denominator x == 1 -> Zip $ guard (numerator x == y) $> []
    (Rational x, Rational y) -> Zip $ guard (x == y) $> []
    (AnyInt, AnyRational) -> LE []
    (AnyInt, Rational y) | 1 == denominator y -> GE []
    (AnyInt, AnyInt) -> LE []
    (AnyInt, Int _) -> GE []
    (Int _, AnyRational) -> LE []
    (Int x, Rational y) | 1 == denominator y -> Zip $ guard (x == numerator y) $> []
    (Int _, AnyInt) -> LE []
    (Int x, Int y) -> Zip $ guard (x == y) $> []
    (AnyFloat, AnyFloat) -> LE []
    (AnyFloat, Float _) -> GE []
    (Float _, AnyFloat) -> LE []
    (Float x, Float y) -> Zip $ guard (if isNaN x then isNaN y else x == y) $> []
    (Truth x, Truth y) -> Zip $ Just [(x, y)]
    (Tuple xs, Tuple ys) -> Zip $ zipMatch xs ys
    (Enum i xs xs', Enum j ys ys') -> Zip $ do
      guard (i == j)
      zs <- zipMatchEnv xs ys
      zs' <- zipMatch xs' ys'
      pure $ zs ++ zs'
    (EnumValue i x, EnumValue j y) -> Zip $ guard (i == j && x == y) $> []
    (StructInst i xs, StructInst j ys) -> Zip $ guard (i == j) *> zipMatchEnv xs ys
    (ClassInst i x xs, ClassInst j y ys) -> Zip $ do
      guard (i == j)
      z <- zipMatch x y
      zs <- zipMatchEnv xs ys
      pure $ z ++ zs
    (Overloads x xs, Overloads y ys) -> case zipMatch x y of
      Just z -> Zip . Just $ (xs, ys):z
      Nothing -> Uncons (Overloads x) xs (Overloads y) ys
    _ -> Zip Nothing

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Val f a) (Val g b) m where
  freeze = \ case
    Any -> pure Any
    AnyRational -> pure AnyRational
    Rational x -> pure $ Rational x
    AnyInt -> pure AnyInt
    Int x -> pure $ Int x
    AnyFloat -> pure AnyFloat
    Float x -> pure $ Float x
    Truth x -> Truth <$> freeze x
    Tuple xs -> Tuple <$> for xs freeze
    Module i xs -> Module i <$> for xs freeze
    Enum i xs xs' -> Enum i <$> for xs freeze <*> for xs' freeze
    EnumValue i x -> pure $ EnumValue i x
    StructInst i xs -> StructInst i <$> for xs freeze
    ClassInst i x xs -> ClassInst i <$> for x freeze <*> for xs freeze
    Overloads x xs -> Overloads <$> freeze x <*> freeze xs

instance (Pretty (ref (Val ref)), Pretty a) => Pretty (Val ref a) where
  pretty = \ case
    Any -> "any"
    AnyRational -> "rational" <> lbracket <> pretty '_' <> rbracket
    Rational x | denominator x == 1 -> pretty $ numerator x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    AnyInt -> "int" <> lbracket <> pretty '_' <> rbracket
    Int x -> pretty x
    AnyFloat -> "float" <> lbracket <> pretty '_' <> rbracket
    Float x -> pretty x
    Truth x -> align $ "truth" <> group (braces $ pretty x)
    Overloads {} -> "function"
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
    Module i xs ->
      align $
      "module#" <>
      prettyLabel i <>
      group (braced $ prettyNames xs)
    Enum i _ xs ->
      align $
      "enum#" <>
      prettyLabel i <>
      group (braced $ pretty <$> xs)
    EnumValue i x ->
      "enum#" <> prettyLabel i <> dot <> pretty x
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
  = Fun
    {-# UNPACK #-} !Label
    !(Env Ident ref a)
    !(Desugar.Env L Ident)
    !Exp
    !Exp
  | Struct
    {-# UNPACK #-} !Label
    !(Env Ident ref a)
    !(Desugar.Env L Ident)
    !Exp
  | Class
    {-# UNPACK #-} !Label
    !(Env Ident ref a)
    !(Maybe a)
    !(Desugar.Env L Ident)
    !Exp
  | Intrinsic !Intrinsic deriving (Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L Ident)

instance Eq (ref (Val ref)) => RowMatchable (Overload ref)

instance Eq (ref (Val ref)) => ZipMatchable (Overload ref) where
  zipMatch = curry $ \ case
    (Fun i_x env_x _ _ _, Fun i_y env_y _ _ _) ->
      guard (i_x == i_y) *>
      zipMatchEnv env_x env_y
    (Struct i_x env_x _ _, Struct i_y env_y _ _) ->
      guard (i_x == i_y) *>
      zipMatchEnv env_x env_y
    (Class i_x env_x sup_x _ _, Class i_y env_y sup_y _ _) -> do
      guard (i_x == i_y)
      sup_z <- zipMatch sup_x sup_y
      env_z <- zipMatchEnv env_x env_y
      pure $ sup_z ++ env_z
    (Intrinsic x, Intrinsic y) -> guard (x == y) $> []
    _ -> Nothing

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Overload f a) (Overload g b) m where
  freeze = \ case
    Fun i env xs e1 e2 -> do
      env <- for env freeze
      pure $ Fun i env xs e1 e2
    Struct i env xs e -> do
      env <- for env freeze
      pure $ Struct i env xs e
    Class i env sup xs e -> do
      env <- for env freeze
      sup <- for sup freeze
      pure $ Class i env sup xs e
    Intrinsic x -> pure $ Intrinsic x

data Named ref a
  = Val a
  | Ref (ref (Val ref)) a deriving (Functor, Foldable, Traversable)

instance Eq (ref (Val ref)) => RowMatchable (Named ref)

instance Eq (ref (Val ref)) => ZipMatchable (Named ref) where
  zipMatch = curry $ \ case
    (Val x, Val y) -> Just [(x, y)]
    (Ref x _, Ref y _) -> guard (x == y) $> []
    _ -> Nothing

instance Freshenable a m => Freshenable (Named ref a) m where
  freshen = \ case
    Val x -> Val <$> freshen x
    Ref x y -> Ref x <$> freshen y

instance ( Freezable (f (Val f)) (g (Val g)) m
         , Freezable a b m
         ) => Freezable (Named f a) (Named g b) m where
  freeze = \ case
    Val x -> Val <$> freeze x
    Ref x y -> Ref <$> freeze x <*> freeze y

instance (Pretty (ref (Val ref)), Pretty a) => Pretty (Named ref a) where
  pretty = \ case
    Val x -> pretty x
    Ref x _ -> pretty x

type Env k ref a = HashMap k (Named ref a)

zipMatchEnv
  :: (Hashable k, ZipMatchable f)
  => HashMap k (f a)
  -> HashMap k (f b)
  -> Maybe [(a, b)]
zipMatchEnv x y = fmap concat . sequenceA . toList $ HashMap.intersectionWith zipMatch x y
