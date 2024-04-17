{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Language.Verse.Val
  ( Val (..)
  , forVal_
  , Sign
  , Class (..)
  , ClassInst (..)
  , List (..)
  , VarVal (..)
  , VarList (..)
  , RefVarVal
  , FrozenVal (..)
  , FrozenList (..)
  , Named (..)
  , VarNamed
  , forNamed_
  , Env
  , forEnv_
  , VarEnv
  , Scope (..)
  , AccessScope (..)
  ) where

import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Verse
  ( Defaultable (..)
  , Freezable (..)
  , Freshenable (..)
  , GVar
  , Var
  , VerseRef
  , defaultGVar
  )

import Data.ByteString.Internal (w2c)
import Data.Char
import Data.Coerce
import Data.Foldable (for_)
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty(..))
import Data.Ratio
import Data.Word

import Language.Verse.Access
import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.SimpleName

import Numeric (showHex)
import Prettyprinter

data Val ref a b
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
  | Truth b
  | Tuple [b]
  | Ptr (ref b) b
  | Module
    {-# UNPACK #-} !Label
    !(Env SimpleName b)
  | Enum
    {-# UNPACK #-} !Label
    !(Env SimpleName b) [b]
  | EnumValue
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !SimpleName
  | Struct
    {-# UNPACK #-} !Label
    !(NonEmpty Scope)
    !(Env Ident b)
    !(Desugar.Env Ident)
    !Exp
  | StructInst
    {-# UNPACK #-}
    !Label
    !(Env SimpleName b)
  | Class !(Class b)
  | ClassInst !(ClassInst b)
  | Lam
    !(NonEmpty Scope)
    !Sign
    !(Env Ident b)
    !Ident
    !Exp
  | AnyOLam
  | OLam
    !(NonEmpty Scope)
    !Sign
    !(Env Ident b)
    !(Desugar.Env Ident)
    !Exp
    !Exp
    b
  | Intrinsic !Intrinsic b
  | Type !Sign a deriving Show

type Sign = Bool

forVal_ :: Applicative m => Val ref a b -> (b -> m c) -> m ()
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
  Class x -> forEnv_ x.env f *> for_ x.super f
  ClassInst x -> for_ x.super f *> forEnv_ x.members f
  Lam _ _ env _ _ -> forEnv_ env f
  AnyOLam -> pure ()
  OLam _ _ _ _ _ _ tail -> void $ f tail
  Intrinsic _ tail -> void $ f tail
  Type {} -> pure ()

instance ( Freshenable a m
         , Freshenable b m
         ) => Freshenable (Val f a b) m where
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
    Tuple xs -> Tuple <$> freshen xs
    Ptr ref x -> Ptr ref <$> freshen x
    Module i env -> Module i <$> freshen env
    Enum i env xs -> Enum i <$> freshen env <*> freshen xs
    EnumValue {} -> pure x
    Struct i scope env xs e -> do
      env <- freshen env
      pure $ Struct i scope env xs e
    StructInst i env -> StructInst i <$> freshen env
    Class x -> Class <$> freshen x
    ClassInst x -> ClassInst <$> freshen x
    Lam scope sign env x e -> do
      env <- freshen env
      pure $ Lam scope sign env x e
    AnyOLam -> pure x
    OLam scope sign env xs e1 e2 tail -> do
      env <- freshen env
      tail <- freshen tail
      pure $ OLam scope sign env xs e1 e2 tail
    Intrinsic i tail -> Intrinsic i <$> freshen tail
    Type x y -> Type x <$> freshen y

instance ( Freezable (f b) (g d) m
         , Freezable a c m
         , Freezable b d m
         ) => Freezable (Val f a b) (Val g c d) m where
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
    Tuple xs -> Tuple <$> freeze xs
    Ptr ref x -> Ptr <$> freeze ref <*> freeze x
    Module i env -> Module i <$> freeze env
    Enum i env xs -> Enum i <$> freeze env <*> freeze xs
    EnumValue i x -> pure $ EnumValue i x
    Struct i scope env xs e -> do
      env <- freeze env
      pure $ Struct i scope env xs e
    StructInst i env -> StructInst i <$> freeze env
    Class x -> Class <$> freeze x
    ClassInst x -> ClassInst <$> freeze x
    Lam scope sign env x e -> freeze env <&> \ env -> Lam scope sign env x e
    AnyOLam -> pure AnyOLam
    OLam scope sign env xs e1 e2 tail -> do
      env <- freeze env
      tail <- freeze tail
      pure $ OLam scope sign env xs e1 e2 tail
    Intrinsic i tail -> Intrinsic i <$> freeze tail
    Type x y -> Type x <$> freeze y

instance ( Pretty (ref b)
         , Pretty b
         ) => Pretty (Val ref a b) where
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
    Class x ->
      align $
      "class#" <>
      prettyLabel x.evalLabel
    ClassInst x ->
      align $
      "class#" <>
      prettyLabel x.evalLabel <>
      maybe mempty (\ super -> parens $ pretty super) x.super <>
      group (braced $ prettyNames x.members)
    Lam {} -> "function"
    AnyOLam -> "function"
    OLam {} -> "function"
    Intrinsic {} -> "function"
    Type {} -> "type"
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

data Class a = MkClass
  { expLabel :: {-# UNPACK #-} !Label
  , evalLabel :: {-# UNPACK #-} !Label
  , scopes :: !(NonEmpty Scope)
  , env :: !(Env Ident a)
  , super :: !(Maybe a)
  , members :: !(Desugar.Env Ident)
  , body :: !Exp
  } deriving Show

instance Freshenable a m => Freshenable (Class a) m where
  freshen MkClass {..} = do
    env <- freshen env
    super <- freshen super
    pure MkClass {..}

instance Freezable a b m => Freezable (Class a) (Class b) m where
  freeze MkClass {..} = do
    env <- freeze env
    super <- freeze super
    pure MkClass {..}

data ClassInst b = MkClassInst
  { expLabel :: {-# UNPACK #-} !Label
  , evalLabel :: {-# UNPACK #-} !Label
  , super :: !(Maybe b)
  , members :: !(Env SimpleName b)
  } deriving Show

instance Freshenable a m => Freshenable (ClassInst a) m where
  freshen MkClassInst {..} = do
    super <- freshen super
    members <- freshen members
    pure MkClassInst {..}

instance Freezable a b m => Freezable (ClassInst a) (ClassInst b) m where
  freeze MkClassInst {..} = do
    super <- freeze super
    members <- freeze members
    pure MkClassInst {..}

data List a b
  = Nil
  | Cons a b deriving Show

instance ( MonadRef m
         , MonadSupply Int m
         ) => Defaultable (List a (VarList m)) m where
  defaultVars = \ case
    Nil -> pure ()
    Cons _ x -> defaultGVar (coerce x) Nil

instance (Freshenable a m, Freshenable b m) => Freshenable (List a b) m where
  freshen = \ case
    Nil -> pure Nil
    Cons x y -> Cons <$> freshen x <*> freshen y

instance ( Freezable a b m
         , Freezable c d m
         ) => Freezable (List a c) (List b d) m where
  freeze = \ case
    Nil -> pure Nil
    Cons x y -> Cons <$> freeze x <*> freeze y

newtype VarVal m = VarVal (Var m (Val (VerseRef m) (VarList m) (VarVal m)))

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         ) => Freshenable (VarVal m) m where
  freshen (VarVal x) = VarVal <$> freshen x

instance (MonadFix m, MonadRef m) => Freezable (VarVal m) FrozenVal m where
  freeze (VarVal x) = FrozenVal <$> freeze x

newtype VarList m = VarList (GVar m (List (VarVal m) (VarList m)))

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         ) => Freshenable (VarList m) m where
  freshen (VarList x) = VarList <$> freshen x

instance (MonadFix m, MonadRef m) => Freezable (VarList m) FrozenList m where
  freeze (VarList x) = FrozenList <$> freeze x

type RefVarVal m = VerseRef m (VarVal m)

newtype FrozenVal = FrozenVal (Maybe (Val Identity FrozenList FrozenVal)) deriving Show

instance Pretty FrozenVal where
  pretty (FrozenVal x) = pretty x

newtype FrozenList = FrozenList (Maybe (List FrozenVal FrozenList)) deriving Show

type Exp = L (Desugar.Exp L Ident)

data Named a = Val a | Ref a deriving Show

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
  -- The identifier for this scope, must match for <private>/<internal>
  {-# UNPACK #-} !Label
  -- List of identifiers for all scopes that could contain <protected> items
  ![Label]
  -- Enclosing module
  {-# UNPACK #-} !Label deriving Show

instance Pretty Scope where
  pretty = \ case
    Scope label labels mLabel ->
      "Scope" <>
      braces (prettyLabel label <>
              prettySup labels <+> "in" <+>
              prettyLabel mLabel)
    where
      prettySup = \ case
        [] -> mempty
        labels -> "," <> "sup=[" <> concatWith (<+>) (map prettyLabel labels) <> "]"

data AccessScope = AccessScope
  !Access
  -- enclosing scope
  {-# UNPACK #-} !Label
  -- enclosing module
  {-# UNPACK #-} !Label deriving Show

instance Monad m => Freezable AccessScope AccessScope m where
  freeze = pure

instance Monad m => Freshenable AccessScope m where
  freshen = pure

instance Pretty AccessScope where
  pretty (AccessScope access sLabel mLabel) =
    "AC" <>
    braces (pretty access <+>
            "scope" <+> prettyLabel sLabel <+>
            "in" <+> prettyLabel mLabel)
