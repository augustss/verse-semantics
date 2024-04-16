{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  , Scope
  , Quantifier (..)
  , Env
  , unify
  , verify
  , check
  , assume
  , forall'
  , bracketInvoke
  , olam
  , name
  , then'
  , seq'
  , domain
  ) where

import Data.ByteString.Internal (w2c)
import Data.Char (ord)
import Data.Functor.Apply
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Word (Word8)

import Language.Verse.Access
import Language.Verse.Effect.Split qualified as Split
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Path (Path)
import Language.Verse.SimpleName

import Numeric (showHex)

import Prettyprinter

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :>>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !SimpleName
  | f (Exp f a) :|: f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Verify (f (Exp f a))
  | Check !Split.Effect (f (Exp f a))
  | Assume !Split.Effect (f (Exp f a))
  | Module {-# UNPACK #-} !Label !(Env a) (f (Exp f a))
  | Struct {-# UNPACK #-} !Label !(Env a) (f (Exp f a))
  | Class {-# UNPACK #-} !Label (Maybe (f (Exp f a))) !(Env a) (f (Exp f a))
  | Inst (f (Exp f a)) !(Env a) (f (Exp f a))
  | Enum {-# UNPACK #-} !Label [SimpleName]
  | IfThenElse !(Env a) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(Env a) (f (Exp f a)) (f (Exp f a))
  | Def !Access !Quantifier (f a) (f (Exp f a))
  | Alloc (f a) (f (Exp f a)) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Char {-# UNPACK #-} !Word8
  | Char32 {-# UNPACK #-} !Char
  | Lam a (f (Exp f a))
  | OLam (f (Exp f a)) !(Env a) (f (Exp f a)) (f (Exp f a))
  | Intrinsic !Intrinsic
  | Name a
  | QualName (f (Exp f a)) {-# UNPACK #-} !SimpleName
  | Path !Path
  | IfArchetypeName (f a) (f a) (f (Exp f a)) (f (Exp f a))
  | ArchetypeName a
  | TopLevel !(Env a) (f (Exp f a)) -- Used to define the top level for paths
  | Domain (f (Exp f a))

infixl 1 :*>:
infixl 1 :>>:

deriving instance ( Show (f (Exp f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

instance ( Pretty (f (Exp f a))
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :*>: e2 ->
      align $
      pretty e1 <> ssemi <>
      pretty e2
    e1 :>>: e2 ->
      parens $
      align $
      pretty e1 <> dsemi <>
      pretty e2
    e1 :=: e2 -> parens $ pretty e1 <+> equals <+> pretty e2
    e :.: x -> pretty e <> dot <> pretty x
    e1 :|: e2 -> parens $ pretty e1 <+> pipe <+> pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    Not e -> "not" <+> parens (pretty e)
    Verify e -> "verify" <+> braces (pretty e)
    Check eff e -> "check" <> angles (pretty eff) <+> braces (pretty e)
    Assume eff e -> "assume" <> angles (pretty eff) <+> braces (pretty e)
    Module i xs e ->
      "module" <> pretty '#' <> prettyLabel i <> braces (bindings xs $ pretty e)
    Struct i xs e ->
      "struct" <> pretty '#' <> prettyLabel i <> braces (bindings xs $ pretty e)
    Class i e1 xs e2 ->
      "class" <> pretty '#' <> prettyLabel i <>
      maybe mempty (parens . pretty) e1 <+>
      braces (bindings xs $ pretty e2)
    Inst e1 xs e2 -> parens (pretty e1) <+> braces (bindings xs $ pretty e2)
    IfThenElse xs e1 e2 e3 ->
      "if" <+> parens (bindings xs $ pretty e1) <+>
      braces (pretty e2) <+>
      braces (pretty e3)
    ForDo xs e1 e2 ->
      "for" <+> parens (bindings xs $ pretty e1) <+> braces (pretty e2)
    Def access t x e ->
      align $
      prettyBindingAccess (access,t) x <> ssemi <>
      pretty e
    Alloc x e1 e2 ->
      "alloc" <> parens (pretty x) <+> pretty e1 <> parens (pretty e2)
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Tuple es -> tupled $ pretty <$> es
    Truth e -> "truth" <+> braces (pretty e)
    Int x -> pretty x
    Float x -> pretty x
    Char x -> "'" <> pretty (w2c x) <> "'" -- FIXME add escape
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Lam x e2 ->
      backslash <+> pretty x <+> braces (pretty e2)
    OLam f xs e1 e2 ->
      "olam" <+> pretty f <+> parens (bindings xs $ pretty e1) <+> braces (pretty e2)
    Intrinsic x -> pretty x
    Name x -> pretty x
    QualName x y -> "(" <> pretty x <> ":)" <> pretty y
    Path x -> pretty x
    IfArchetypeName  x y e1 e2 ->
      "if" <+> parens (pretty y <+> ":=" <+> "archetype" <> parens (pretty x)) <+>
      braces (pretty e1) <+>
      "else" <+> braces (pretty e2)
    ArchetypeName x -> "archetype" <> parens (pretty x)
    TopLevel xs e ->
      bindings xs $ pretty e
    Domain e ->
      "domain" <+> braces (pretty e)
    _ -> "unimplemented"
    where
      ssemi = flatAlt hardline (semi <> space)
      dsemi = flatAlt (semi <> semi <> hardline) (semi <> semi <> space)
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      braces x =
        nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
        flatAlt (hardline <> rbrace) " }"
      bindings xs y = align $
        concatWith' ssemi (uncurry (flip prettyBindingAccess) <$> HashMap.toList xs) <>
        ssemi <>
        y
      concatWith' x = concatWith $ \ y z -> y <> x <> z
      prettyBindingAccess (access, t) x = case t of
        Exists -> "exists" <+> pretty access <+> pretty x
        Forall -> "forall" <+> pretty access <+> pretty x
        Var -> "var" <+> pretty access <+> pretty x

-- The current scope. If empty then top level
type Scope = [Label]

data Quantifier = Exists | Forall | Var deriving Show

type Env a = HashMap a (Access, Quantifier)

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

verify :: Functor f => f (Exp f a) -> f (Exp f a)
verify = liftL1 Verify

check :: Functor f => Split.Effect -> f (Exp f a) -> f (Exp f a)
check = liftL1 . Check

assume :: Functor f => Split.Effect -> f (Exp f a) -> f (Exp f a)
assume = liftL1 . Assume

forall' :: Apply f => Access -> f a -> f (Exp f a) -> f (Exp f a)
forall' access = liftL2 (Def access Forall)

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

olam
  :: Apply f
  => f (Exp f a)
  -> Env a
  -> f (Exp f a)
  -> f (Exp f a)
  -> f (Exp f a)
olam f env e1 e2 = OLam f env e1 e2 <$ f <. e1 <. e2

name :: Functor f => f a -> f (Exp f a)
name = fmap Name

then' :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
then' = liftL2 (:*>:)
infixl 1 `then'`

seq' :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
seq' = liftL2 (:>>:)
infixl 1 `seq'`

domain :: Functor f => f (Exp f a) -> f (Exp f a)
domain = liftL1 Domain
