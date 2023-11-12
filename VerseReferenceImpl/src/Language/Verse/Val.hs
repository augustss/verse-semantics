{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Val
  ( Val (..)
  , forVal_
  , VarVal
  , VarRefVal
  , FrozenVal
  , Overload (..)
  , forOverload_
  , Named (..)
  , VarNamed
  , forNamed_
  , Env
  , forEnv_
  , VarEnv
  ) where

import Control.Monad.Verse (Var, VarRef, Freezable (..), Freshenable (..))

import Data.Fix
import Data.Foldable (for_)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Compose.Instances ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
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
  | Comparable
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
  | AnyOverloads
  | Overloads !(Overload ref a) a

forVal_ :: Applicative m => Val ref a -> (a -> m b) -> m ()
forVal_ x f = case x of
  Any -> pure ()
  Comparable -> pure ()
  AnyRational -> pure ()
  Rational _ -> pure ()
  AnyInt -> pure ()
  Int _ -> pure ()
  AnyFloat -> pure ()
  Float _ -> pure ()
  Truth x -> void $ f x
  Tuple xs -> for_ xs f
  Module _ env -> forEnv_ env f
  Enum _ env xs -> forEnv_ env f *> for_ xs f
  EnumValue {} -> pure ()
  StructInst _ env -> forEnv_ env f
  ClassInst _ sup env -> for_ sup f *> forEnv_ env f
  AnyOverloads -> pure ()
  Overloads x xs -> forOverload_ x f *> void (f xs)

type VarVal m = Fix (Compose (Var m) (Val (VarRef m)))

type VarRefVal m = VarRef m (Val (VarRef m) (VarVal m))

type FrozenVal = Fix (Compose Maybe (Val Maybe))

instance Freshenable a m => Freshenable (Val f a) m where
  freshen x = case x of
    Any -> pure x
    Comparable -> pure x
    AnyRational -> pure x
    Rational _ -> pure x
    AnyInt -> pure x
    Int _ -> pure x
    AnyFloat -> pure x
    Float _ -> pure x
    Truth x -> Truth <$> freshen x
    Tuple xs -> Tuple <$> for xs freshen
    Module i xs -> Module i <$> for xs freshen
    Enum i xs xs' -> Enum i <$> for xs freshen <*> for xs' freshen
    EnumValue {} -> pure x
    StructInst i xs -> StructInst i <$> for xs freshen
    ClassInst i x xs -> ClassInst i <$> for x freshen <*> for xs freshen
    AnyOverloads -> pure x
    Overloads x xs -> Overloads <$> freshen x <*> freshen xs

instance ( Freezable (f (Val f a)) (g (Val g b)) m
         , Freezable a b m
         ) => Freezable (Val f a) (Val g b) m where
  freeze = \ case
    Any -> pure Any
    Comparable -> pure Comparable
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
    AnyOverloads -> pure AnyOverloads
    Overloads x xs -> Overloads <$> freeze x <*> freeze xs

instance (Pretty (ref (Val ref a)), Pretty a) => Pretty (Val ref a) where
  pretty = \ case
    Any -> "any"
    Comparable -> "comparable"
    AnyRational -> "rational" <> lbracket <> pretty '_' <> rbracket
    Rational x | denominator x == 1 -> pretty $ numerator x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    AnyInt -> "int" <> lbracket <> pretty '_' <> rbracket
    Int x -> pretty x
    AnyFloat -> "float" <> lbracket <> pretty '_' <> rbracket
    Float x -> pretty x
    Truth x -> align $ "truth" <> group (braces $ pretty x)
    AnyOverloads -> "function"
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
        align .
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
  | Intrinsic !Intrinsic

type Exp = L (Desugar.Exp L Ident)

forOverload_ :: Applicative m => Overload ref a -> (a -> m b) -> m ()
forOverload_ x f = case x of
  Fun env _ _ _ -> forEnv_ env f
  Struct _ env _ _ -> forEnv_ env f
  Class _ env sup _ _ -> forEnv_ env f *> for_ sup f
  Intrinsic _ -> pure ()

instance Freshenable a m => Freshenable (Overload f a) m where
  freshen x = case x of
    Fun env xs e1 e2 -> do
      env <- for env freshen
      pure $ Fun env xs e1 e2
    Struct i env xs e -> do
      env <- for env freshen
      pure $ Struct i env xs e
    Class i env sup xs e -> do
      env <- for env freshen
      sup <- for sup freshen
      pure $ Class i env sup xs e
    Intrinsic _ -> pure x

instance ( Freezable (f (Val f a)) (g (Val g b)) m
         , Freezable a b m
         ) => Freezable (Overload f a) (Overload g b) m where
  freeze = \ case
    Fun env xs e1 e2 -> do
      env <- for env freeze
      pure $ Fun env xs e1 e2
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
  | Ref (ref (Val ref a)) a

forNamed_ :: Applicative m => Named ref a -> (a -> m b) -> m ()
forNamed_ x f = case x of
  Val x -> void $ f x
  Ref _ x -> void $ f x

type VarNamed m = Named (VarRef m) (VarVal m)

instance Freshenable a m => Freshenable (Named ref a) m where
  freshen = \ case
    Val x -> Val <$> freshen x
    Ref x y -> Ref x <$> freshen y

instance ( Freezable (f (Val f a)) (g (Val g b)) m
         , Freezable a b m
         ) => Freezable (Named f a) (Named g b) m where
  freeze = \ case
    Val x -> Val <$> freeze x
    Ref x y -> Ref <$> freeze x <*> freeze y

instance (Pretty (ref (Val ref a)), Pretty a) => Pretty (Named ref a) where
  pretty = \ case
    Val x -> pretty x
    Ref x _ -> pretty x

type Env k ref a = HashMap k (Named ref a)

forEnv_ :: Applicative m => Env k ref a -> (a -> m b) -> m ()
forEnv_ x f = for_ x $ \ x -> forNamed_ x f

type VarEnv k m = Env k (VarRef m) (VarVal m)
