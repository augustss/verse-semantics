{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  , Quantifier (..)
  , Env
  , unify
  , verify
  , succeeds
  , decides
  , assume
  , forall'
  , bracketInvoke
  , fun
  , name
  , then'
  ) where

import Data.Char(ord)
import Data.Functor.Apply
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Numeric(showHex)

import Language.Verse.Label
import Language.Verse.Name

import Prettyprinter

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !Name
  | f (Exp f a) :|: f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Verify (f (Exp f a))
  | Succeeds (f (Exp f a))
  | Fails (f (Exp f a))
  | Decides (f (Exp f a))
  | Assume (f (Exp f a))
  | Module {-# UNPACK #-} !Label !(Env f a) (f (Exp f a))
  | Struct {-# UNPACK #-} !Label !(Env f a) (f (Exp f a))
  | Class {-# UNPACK #-} !Label (Maybe (f (Exp f a))) !(Env f a) (f (Exp f a))
  | Inst (f (Exp f a)) !(Env f a) (f (Exp f a))
  | Enum {-# UNPACK #-} !Label [Name]
  | IfThenElse !(Env f a) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(Env f a) (f (Exp f a)) (f (Exp f a))
  | Def (Quantifier f a) (f a) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Char {-# UNPACK #-} !Char
  | Char32 {-# UNPACK #-} !Char
  | Fun !(Env f a) (f (Exp f a)) (f (Exp f a))
  | Name a
  | IfArchetypeName (f a) (f a) (f (Exp f a)) (f (Exp f a))
  | ArchetypeName a

infixl 4 :*>:

deriving instance ( Show (f (Exp f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

data Quantifier f a
  = Exists
  | Var (f a)
  | Forall deriving Show

instance ( Pretty (f (Exp f a))
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :=: e2 -> parens $ pretty e1 <+> equals <+> pretty e2
    e :.: x -> pretty e <> dot <> pretty x
    e1 :|: e2 -> parens $ pretty e1 <+> pipe <+> pretty e2
    e1 :*>: e2 ->
      align $
      pretty e1 <> separator <>
      pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    Not e -> "not" <+> parens (pretty e)
    Verify e -> "verify" <+> braces (pretty e)
    Succeeds e -> "succeeds" <+> braces (pretty e)
    Decides e -> "decides" <+> braces (pretty e)
    Assume e -> "assume" <+> braces (pretty e)
    Class i e1 xs e2 ->
      "class" <> pretty '#' <> prettyLabel i <>
      maybe mempty (parens . pretty) e1 <+>
      braces (quantified xs $ pretty e2)
    Inst e1 xs e2 -> parens (pretty e1) <+> braces (quantified xs $ pretty e2)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    ForDo xs e1 e2 ->
      "for" <+> parens (quantified xs $ pretty e1) <+> braces (pretty e2)
    Def q x e ->
      align $
      prettyQuantified x q <> separator <>
      pretty e
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    Tuple es -> tupled $ pretty <$> es
    Truth e -> "truth" <+> braces (pretty e)
    Int x -> pretty x
    Float x -> pretty x
    Char x -> "'" <> pretty x <> "'"  -- FIXME add escape
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Fun xs e1 e2 ->
      "fun" <> parens (quantified xs $ pretty e1) <+> braces (pretty e2)
    Name x -> pretty x
    IfArchetypeName x y e1 e2 ->
      "if" <+> parens (pretty y <+> ":=" <+> "archetype" <> parens (pretty x)) <+>
      braces (pretty e1) <+>
      "else" <+> braces (pretty e2)
    ArchetypeName x -> "archetype" <> parens (pretty x)
    _ -> "unimplemented"
    where
      separated = concatWith (\x y -> x <> separator <> y)
      separator = flatAlt hardline (semi <> space)
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      braces x =
        nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
        flatAlt (hardline <> rbrace) " }"
      quantified xs y = align $
        separated (uncurry prettyQuantified <$> HashMap.toList xs) <> separator <>
        y
      prettyQuantified x = \ case
        Exists -> "exists" <+> pretty x
        Var y -> "var" <+> pretty x <> colon <> pretty y
        Forall -> "forall" <+> pretty x

type Env f a = HashMap a (Quantifier f a)

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

verify :: Functor f => f (Exp f a) -> f (Exp f a)
verify = liftL1 Verify

succeeds :: Functor f => f (Exp f a) -> f (Exp f a)
succeeds = liftL1 Succeeds

decides :: Functor f => f (Exp f a) -> f (Exp f a)
decides = liftL1 Decides

assume :: Functor f => f (Exp f a) -> f (Exp f a)
assume = liftL1 Assume

forall' :: Apply f => f a -> f (Exp f a) -> f (Exp f a)
forall' = liftL2 (Def Forall)

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

fun :: Apply f => Env f a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
fun = liftL2 . Fun

name :: Functor f => f a -> f (Exp f a)
name = fmap Name

then' :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
then' = liftL2 (:*>:)
infixl 4 `then'`

liftL1 :: Functor f => (f a -> b) -> f a -> f b
liftL1 f x = f x <$ x

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y
