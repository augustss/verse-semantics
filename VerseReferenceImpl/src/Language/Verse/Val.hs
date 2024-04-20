{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Language.Verse.Val
  ( Val (..)
  , pattern SomeAny
  , pattern SomeRational
  , pattern SomeInt
  , pattern SomeFloat
  , pattern SomeChar
  , pattern SomeChar32
  , pattern SomeFunction
  , forVal_
  , Sign
  , Struct (..)
  , StructInst (..)
  , Class (..)
  , ClassInst (..)
  , Lam (..)
  , OLam (..)
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
import Language.Verse.Contract (Contract)
import Language.Verse.Contract qualified as Contract
import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.SimpleName

import Numeric (showHex)
import Prettyprinter
  ( Doc
  , Pretty (..)
  , (<+>)
  , align
  , brackets
  , concatWith
  , dot
  , encloseSep
  , flatAlt
  , group
  , hardline
  , lbrace
  , line
  , lparen
  , nest
  , parens
  , rbrace
  , rparen
  )

data Val ref a b
  = Some !Contract
  | Rational !Rational
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Char {-# UNPACK #-} !Word8
  | Char32 {-# UNPACK #-} !Char
  | Path [SimpleName]
  | Truth b
  | Tuple [b]
  | Ptr (ref b) b
  | Module {-# UNPACK #-} !Label !(Env SimpleName b)
  | Enum {-# UNPACK #-} !Label !(Env SimpleName b) [b]
  | EnumValue {-# UNPACK #-} !Label {-# UNPACK #-} !SimpleName
  | Struct !(Struct b)
  | StructInst !(StructInst b)
  | Class !(Class b)
  | ClassInst !(ClassInst b)
  | Lam !(Lam b)
  | OLam !(OLam b) b
  | Intrinsic !Intrinsic b
  | Type !Sign a deriving Show

pattern SomeAny :: Val ref a b
pattern SomeAny = Some Contract.Any

pattern SomeRational :: Val ref a b
pattern SomeRational = Some Contract.Rational

pattern SomeInt :: Val ref a b
pattern SomeInt = Some Contract.Int

pattern SomeFloat :: Val ref a b
pattern SomeFloat = Some Contract.Float

pattern SomeChar :: Val ref a b
pattern SomeChar = Some Contract.Char

pattern SomeChar32 :: Val ref a b
pattern SomeChar32 = Some Contract.Char32

pattern SomeFunction :: Val ref a b
pattern SomeFunction = Some Contract.Function

type Sign = Bool

forVal_ :: Applicative m => Val ref a b -> (b -> m c) -> m ()
forVal_ x f = case x of
  Some _ -> pure ()
  Rational _ -> pure ()
  Int _ -> pure ()
  Float _ -> pure ()
  Char _ -> pure ()
  Char32 _ -> pure ()
  Path _ -> pure ()
  Truth x -> void $ f x
  Tuple xs -> for_ xs f
  Ptr _ x -> void $ f x
  Module _ env -> forEnv_ env f
  Enum _ env xs -> forEnv_ env f *> for_ xs f
  EnumValue {} -> pure ()
  Struct x -> forEnv_ x.env f
  StructInst x -> forEnv_ x.members f
  Class x -> forEnv_ x.env f *> for_ x.super f
  ClassInst x -> for_ x.super f *> forEnv_ x.members f
  Lam x -> forEnv_ x.env f
  OLam x xs -> forEnv_ x.env f <* f xs
  Intrinsic _ xs -> void $ f xs
  Type {} -> pure ()

instance ( Freshenable a m
         , Freshenable b m
         ) => Freshenable (Val f a b) m where
  freshen x = case x of
    Some _ -> pure x
    Rational _ -> pure x
    Int _ -> pure x
    Float _ -> pure x
    Char _ -> pure x
    Char32 _ -> pure x
    Path _ -> pure x
    Truth x -> Truth <$> freshen x
    Tuple xs -> Tuple <$> freshen xs
    Ptr ref x -> Ptr ref <$> freshen x
    Module i env -> Module i <$> freshen env
    Enum i env xs -> Enum i <$> freshen env <*> freshen xs
    EnumValue {} -> pure x
    Struct x -> Struct <$> freshen x
    StructInst x -> StructInst <$> freshen x
    Class x -> Class <$> freshen x
    ClassInst x -> ClassInst <$> freshen x
    Lam x -> Lam <$> freshen x
    OLam x xs -> OLam <$> freshen x <*> freshen xs
    Intrinsic x xs -> Intrinsic x <$> freshen xs
    Type x y -> Type x <$> freshen y

instance ( Freezable (f b) (g d) m
         , Freezable a c m
         , Freezable b d m
         ) => Freezable (Val f a b) (Val g c d) m where
  freeze = \ case
    Some x -> pure $ Some x
    Rational x -> pure $ Rational x
    Int x -> pure $ Int x
    Float x -> pure $ Float x
    Char x -> pure $ Char x
    Char32 x -> pure $ Char32 x
    Path x -> pure $ Path x
    Truth x -> Truth <$> freeze x
    Tuple xs -> Tuple <$> freeze xs
    Ptr ref x -> Ptr <$> freeze ref <*> freeze x
    Module i env -> Module i <$> freeze env
    Enum i env xs -> Enum i <$> freeze env <*> freeze xs
    EnumValue i x -> pure $ EnumValue i x
    Struct x -> Struct <$> freeze x
    StructInst x -> StructInst <$> freeze x
    Class x -> Class <$> freeze x
    ClassInst x -> ClassInst <$> freeze x
    Lam x -> Lam <$> freeze x
    OLam x xs -> OLam <$> freeze x <*> freeze xs
    Intrinsic x xs -> Intrinsic x <$> freeze xs
    Type x y -> Type x <$> freeze y

instance ( Pretty (ref b)
         , Pretty b
         ) => Pretty (Val ref a b) where
  pretty = \ case
    Some x -> pretty x <> brackets (pretty '_')
    Rational x | denominator x == 1 -> pretty $ numerator x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Int x -> pretty x
    Float x -> pretty x
    Char x -> "'" <> pretty (w2c x) <> "'"
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Path xs ->  prettyPath xs
    Truth x -> align $ "truth" <> group (braces $ pretty x)
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
    Ptr ref x -> "ptr" <> parens (pretty x) <> parens (pretty ref)
    Module i env ->
      align $
      "module#" <>
      prettyLabel i <+>
      group (braced $ prettyNames env)
    Enum i _ xs ->
      align $
      "enum#" <>
      prettyLabel i <+>
      group (braced $ pretty <$> xs)
    EnumValue i x -> "enum#" <> prettyLabel i <> dot <> pretty x
    Struct x -> pretty x
    StructInst x -> pretty x
    Class x -> pretty x
    ClassInst x -> pretty x
    Lam _ -> "function"
    OLam {} -> "function"
    Intrinsic {} -> "function"
    Type {} -> "type"
    where
      prettyPath = foldr ( \ a b -> "/" <> pretty a <> b ) mempty

data Struct a = MkStruct
  { label :: {-# UNPACK #-} !Label
  , expLabel :: {-# UNPACK #-} !Label
  , scopes :: !(NonEmpty Scope)
  , env :: !(Env Ident a)
  , members :: !(Desugar.Env Ident)
  , exp :: !Exp
  } deriving Show

instance Freshenable a m => Freshenable (Struct a) m where
  freshen MkStruct {..} = do
    env <- freshen env
    pure MkStruct {..}

instance Freezable a b m => Freezable (Struct a) (Struct b) m where
  freeze MkStruct {..} = do
    env <- freeze env
    pure MkStruct {..}

instance Pretty (Struct a) where
  pretty MkStruct {..} = align $ "struct#" <> prettyLabel label

data StructInst a = MkStructInst
  { label :: {-# UNPACK #-} !Label
  , expLabel :: {-# UNPACK #-} !Label
  , members :: !(Env SimpleName a)
  } deriving Show

instance Freshenable a m => Freshenable (StructInst a) m where
  freshen MkStructInst {..} = do
    members <- freshen members
    pure MkStructInst {..}

instance Freezable a b m => Freezable (StructInst a) (StructInst b) m where
  freeze MkStructInst {..} = do
    members <- freeze members
    pure MkStructInst {..}

instance Pretty a => Pretty (StructInst a) where
  pretty MkStructInst {..} =
    align $
    "struct#" <>
    prettyLabel label <+>
    group (braced $ prettyNames members)

data Class a = MkClass
  { label :: {-# UNPACK #-} !Label
  , expLabel :: {-# UNPACK #-} !Label
  , scopes :: !(NonEmpty Scope)
  , env :: !(Env Ident a)
  , super :: !(Maybe a)
  , members :: !(Desugar.Env Ident)
  , exp :: !Exp
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

instance Pretty (Class a) where
  pretty MkClass {..} = align $ "class#" <> prettyLabel label

data ClassInst b = MkClassInst
  { label :: {-# UNPACK #-} !Label
  , expLabel :: {-# UNPACK #-} !Label
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

instance Pretty a => Pretty (ClassInst a) where
  pretty MkClassInst {..} =
    align $
    "class#" <>
    prettyLabel label <+>
    maybe mempty (parens . pretty) super <>
    group (braced $ prettyNames members)

data Lam a = MkLam
  { scopes :: !(NonEmpty Scope)
  , sign :: !Sign
  , env :: !(Env Ident a)
  , param :: !Ident
  , exp :: !Exp
  } deriving Show

instance Freshenable a m => Freshenable (Lam a) m where
  freshen MkLam {..} = do
    env <- freshen env
    pure MkLam {..}

instance Freezable a b m => Freezable (Lam a) (Lam b) m where
  freeze MkLam {..} = do
    env <- freeze env
    pure MkLam {..}

data OLam a = MkOLam
  { scopes :: !(NonEmpty Scope)
  , sign :: !Sign
  , env :: !(Env Ident a)
  , params :: !(Desugar.Env Ident)
  , domain :: !Exp
  , range :: !Exp
  } deriving Show

instance Freshenable a m => Freshenable (OLam a) m where
  freshen MkOLam {..} = do
    env <- freshen env
    pure MkOLam {..}

instance Freezable a b m => Freezable (OLam a) (OLam b) m where
  freeze MkOLam {..} = do
    env <- freeze env
    pure MkOLam {..}

data List a b
  = Nil
  | Var a b
  | Contract !Contract b deriving Show

instance ( MonadRef m
         , MonadSupply Int m
         ) => Defaultable (List a (VarList m)) m where
  defaultVars = \ case
    Nil -> pure ()
    Var _ x -> defaultGVar (coerce x) Nil
    Contract _ x -> defaultGVar (coerce x) Nil

instance (Freshenable a m, Freshenable b m) => Freshenable (List a b) m where
  freshen = \ case
    Nil -> pure Nil
    Var x xs -> Var <$> freshen x <*> freshen xs
    Contract x xs -> Contract x <$> freshen xs

instance ( Freezable a b m
         , Freezable c d m
         ) => Freezable (List a c) (List b d) m where
  freeze = \ case
    Nil -> pure Nil
    Var x xs -> Var <$> freeze x <*> freeze xs
    Contract x xs -> Contract x <$> freeze xs

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
  pretty (Scope label labels moduleLabel) =
    "Scope" <>
    braces (prettyLabel label <>
            prettySuper labels <+> "in" <+>
            prettyLabel moduleLabel)
    where
      prettySuper = \ case
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

prettyNames :: (Pretty k, Pretty v) => HashMap k v -> [Doc ann]
prettyNames xs = HashMap.toList xs <&> \ (k, v) ->
  align $ pretty k <+> ":=" <> group (nest 2 $ line <> pretty v)

tupled :: [Doc ann] -> Doc ann
tupled =
  align .
  group .
  encloseSep
  (flatAlt "( " lparen)
  (flatAlt (hardline <> rparen) rparen)
  ", "

braces :: Doc ann -> Doc ann
braces x =
  flatAlt (hardline <> "{ ") lbrace <>
  x <>
  flatAlt (hardline <> rbrace) rbrace

braced :: [Doc ann] -> Doc ann
braced =
  group .
  encloseSep
  (flatAlt (hardline <> "{ ") lbrace)
  (flatAlt (hardline <> rbrace) rbrace)
  ", "
