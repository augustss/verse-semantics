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
  , Named (..)
  , VarNamed
  , forNamed_
  , Env
  , forEnv_
  , VarEnv
  , Scope (..)
  , AccessScope (..)
  ) where

import Control.Monad.Verse (Var, VarRef, Freezable (..), Freshenable (..))

import Data.ByteString.Internal (w2c)
import Data.Char
import Data.Fix
import Data.Foldable (for_)
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Compose.Instances ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ratio
import Data.Traversable
import Data.Word

import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.SimpleName

import Numeric (showHex)
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
  | AnyChar
  | Char {-# UNPACK #-} !Word8
  | AnyChar32
  | Char32 {-# UNPACK #-} !Char
  | Path [SimpleName]
  | Truth a
  | Tuple [a]
  | Ptr (ref (Val ref a)) a
  | Module
    {-# UNPACK #-} !Label
    !(Env SimpleName a)
  | Enum
    {-# UNPACK #-} !Label
    !(Env SimpleName a) [a]
  | EnumValue
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !SimpleName
  | Struct
    {-# UNPACK #-} !Label
    !(NonEmpty Scope)
    !(Env Ident a)
    !(Desugar.Env Ident)
    !Exp
  | StructInst
    {-# UNPACK #-}
    !Label
    !(Env SimpleName a)
  | Class
    {-# UNPACK #-} !Label
    !(NonEmpty Scope)
    !(Env Ident a)
    !(Maybe a)
    !(Desugar.Env Ident)
    !Exp
  | ClassInst
    {-# UNPACK #-} !Label
    !(Maybe a)
    !(Env SimpleName a)
  | Lam
    !(NonEmpty Scope)
    !(Env Ident a)
    !Ident
    !Exp
  | AnyOLam
  | OLam
    !(NonEmpty Scope)
    !(Env Ident a)
    !(Desugar.Env Ident)
    !Exp
    !Exp
    a
  | Intrinsic !Intrinsic a

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
  AnyChar -> pure ()
  Char _ -> pure ()
  AnyChar32 -> pure ()
  Char32 _ -> pure ()
  Path _ -> pure ()
  Truth x -> void $ f x
  Tuple xs -> for_ xs f
  Ptr _ x -> void $ f x
  Module _ env -> forEnv_ env f
  Enum _ env xs -> forEnv_ env f *> for_ xs f
  EnumValue {} -> pure ()
  Struct _ _ env _ _ -> forEnv_ env f
  StructInst _ env -> forEnv_ env f
  Class _ _ env sup _ _ -> forEnv_ env f *> for_ sup f
  ClassInst _ sup env -> for_ sup f *> forEnv_ env f
  Lam _ env _ _ -> forEnv_ env f
  AnyOLam -> pure ()
  OLam _ _ _ _ _ tail -> void $ f tail
  Intrinsic _ tail -> void $ f tail

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
    AnyChar -> pure x
    Char _ -> pure x
    AnyChar32 -> pure x
    Char32 _ -> pure x
    Path _ -> pure x
    Truth x -> Truth <$> freshen x
    Tuple xs -> Tuple <$> for xs freshen
    Ptr ref x -> Ptr ref <$> freshen x
    Module i env -> Module i <$> for env freshen
    Enum i env xs -> Enum i <$> for env freshen <*> for xs freshen
    EnumValue {} -> pure x
    Struct i scope env xs e -> do
      env <- for env freshen
      pure $ Struct i scope env xs e
    StructInst i env -> StructInst i <$> for env freshen
    Class i scope env sup xs e -> do
      env <- for env freshen
      sup <- for sup freshen
      pure $ Class i scope env sup xs e
    ClassInst i sup env -> ClassInst i <$> for sup freshen <*> for env freshen
    Lam scope env x e -> do
      env <- for env freshen
      pure $ Lam scope env x e
    AnyOLam -> pure x
    OLam scope env xs e1 e2 tail -> do
      env <- freshen env
      tail <- freshen tail
      pure $ OLam scope env xs e1 e2 tail
    Intrinsic i tail -> Intrinsic i <$> freshen tail

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
    AnyChar -> pure AnyChar
    Char x -> pure $ Char x
    AnyChar32 -> pure AnyChar32
    Char32 x -> pure $ Char32 x
    Path x -> pure $ Path x
    Truth x -> Truth <$> freeze x
    Tuple xs -> Tuple <$> for xs freeze
    Ptr ref x -> Ptr <$> freeze ref <*> freeze x
    Module i env -> Module i <$> for env freeze
    Enum i env xs -> Enum i <$> for env freeze <*> for xs freeze
    EnumValue i x -> pure $ EnumValue i x
    Struct i scope env xs e -> do
      env <- for env freeze
      pure $ Struct i scope env xs e
    StructInst i env -> StructInst i <$> for env freeze
    Class i scope env sup xs e -> do
      env <- for env freeze
      sup <- for sup freeze
      pure $ Class i scope env sup xs e
    ClassInst i sup env -> ClassInst i <$> for sup freeze <*> for env freeze
    Lam scope env x e -> for env freeze <&> \ env -> Lam scope env x e
    AnyOLam -> pure AnyOLam
    OLam scope env xs e1 e2 tail -> do
      env <- for env freeze
      tail <- freeze tail
      pure $ OLam scope env xs e1 e2 tail
    Intrinsic i tail -> Intrinsic i <$> freeze tail

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
    AnyChar -> "char" <> lbracket <> pretty '_' <> rbracket
    Char x -> "'" <> pretty (w2c x) <> "'"
    AnyChar32 -> "char32" <> lbracket <> pretty '_' <> rbracket
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Path xs ->  prettyPath xs
    Truth x -> align $ "truth" <> group (braces $ pretty x)
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
    Ptr ref x -> "ptr" <> parens (pretty x) <> parens (pretty ref)
    Module i env ->
      align $
      "module#" <>
      prettyLabel i <>
      group (braced $ prettyNames env)
    Enum i _ xs ->
      align $
      "enum#" <>
      prettyLabel i <>
      group (braced $ pretty <$> xs)
    EnumValue i x ->
      "enum#" <> prettyLabel i <> dot <> pretty x
    Struct i _ _ _ _ ->
      align $
      "struct#" <>
      prettyLabel i
    StructInst i env ->
      align $
      "struct#" <>
      prettyLabel i <>
      group (braced $ prettyNames env)
    Class i _ _ _ _ _ ->
      align $
      "class#" <>
      prettyLabel i
    ClassInst i Nothing env ->
      align $
      "class#" <>
      prettyLabel i <>
      group (braced $ prettyNames env)
    ClassInst i (Just x) env ->
      align $
      "class#" <>
      prettyLabel i <>
      parens (pretty x) <>
      group (braced $ prettyNames env)
    Lam {} -> "function"
    AnyOLam -> "function"
    OLam {} -> "function"
    Intrinsic {} -> "function"
    where
      prettyPath xs = foldr ( \ a b -> "/" <> pretty a <> b ) mempty xs
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

type VarVal m = Fix (Compose (Var m) (Val (VarRef m)))

type VarRefVal m = VarRef m (Val (VarRef m) (VarVal m))

type FrozenVal = Fix (Compose Maybe (Val Maybe))

type Exp = L (Desugar.Exp L Ident)

data Named a = Val a | Ref a

forNamed_ :: Applicative m => Named a -> (a -> m b) -> m ()
forNamed_ x f = case x of
  Val x -> void $ f x
  Ref x -> void $ f x

type VarNamed m = Named (VarVal m)

instance Freshenable a m => Freshenable (Named a) m where
  freshen = \ case
    Val x -> Val <$> freshen x
    Ref x -> Ref <$> freshen x

instance Freezable a b m => Freezable (Named a) (Named b) m where
  freeze = \ case
    Val x -> Val <$> freeze x
    Ref x -> Ref <$> freeze x

instance Pretty a => Pretty (Named a) where
  pretty = \ case
    Val x -> pretty x
    Ref x -> pretty x

type Env k a = HashMap k (AccessScope, Named a)

forEnv_ :: Applicative m => Env k a -> (a -> m b) -> m ()
forEnv_ x f = for_ x $ \ (_access, x) -> forNamed_ x f

type VarEnv k m = Env k (VarVal m)

data Scope = Scope
  !Label          -- The identifier for this scope, must match for <private>/<internal>
  ![Label]        -- List of identifiers for all scopes that could contain <protected> items
  !Label          -- Enclosing module
  deriving Show

instance Pretty Scope where
  pretty = \ case
    Scope label labels mLabel -> "Scope{" <> prettyLabel label <>  prettySup labels <+> "in" <+> prettyLabel mLabel <> "}"
   where
     prettySup = \ case
       [] -> mempty
       labels -> "," <> "sup=[" <> concatWith (<+>) (map prettyLabel labels) <> "]"

data AccessScope
  = AccessScope
    Desugar.Access
    Label           -- enclosing scope
    Label           -- enclosing module
  deriving Show

instance Monad m => Freezable AccessScope AccessScope m where
  freeze = pure

instance Monad m => Freshenable AccessScope m where
  freshen = pure

instance Pretty AccessScope where
  pretty (AccessScope access sLabel mLabel) = "AC{" <> pretty access <+> "scope" <+> prettyLabel sLabel <+> "in" <+> prettyLabel mLabel <> "}"
