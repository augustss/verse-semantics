{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Rewrite.Exp
  ( Exp (..)
  , Quantifier (..)
  , OC (..)
  , ofType
  , bracketInvoke
  , alloc2
  , alloc3
  , infixColonEqual
  , prefixColon
  , mixfixArrowColonEqual
  , name
  , ifArchetypeName
  ) where

import Data.ByteString.Internal (w2c)
import Data.Char
import Data.Functor.Apply
import Data.Text (Text)

import Language.Verse.Access
import Language.Verse.Effect.Split qualified as Split
import Language.Verse.Loc
import Language.Verse.Path (Path)
import Language.Verse.SimpleName

import Data.Word (Word8)

import Numeric (showHex)

import Prettyprinter

data Exp f a
  = f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !SimpleName
  | f (Exp f a) :|: f (Exp f a)
  | List [f (Exp f a)]
  | f (Exp f a) `Where` f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Verify (f (Exp f a))
  | Check Split.Effect (f (Exp f a))
  | f (Exp f a) `OfType` f (Exp f a)
  | Assume (f (Exp f a))
  | Module (f (Exp f a))
  | Struct (f (Exp f a))
  | Class (Maybe (f (Exp f a))) (f (Exp f a))
  | Inst (f (Exp f a)) (f (Exp f a))
  | Enum [SimpleName]
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Block (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Exists (f a)
  | Forall (f a)
  | Alloc2 !Access (f a) (f (Exp f a))
  | Alloc3 !Access (f a) (f (Exp f a)) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Array [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Char {-# UNPACK #-} !Word8
  | Char32 {-# UNPACK #-} !Char
  | Lam (f (Exp f a)) !OC !Split.Effect (f (Exp f a))
  | InfixColonEqual !Access !Quantifier (f a) (f (Exp f a))
  | PrefixColon (f (Exp f a))
  | MixfixArrowColonEqual (f a) (f a) (f (Exp f a))
  | Name a
  | QualName (f (Exp f a)) {-# UNPACK #-} !SimpleName
  | Path !Path
  | IfArchetypeName (f a) (f (Exp f a)) (f (Exp f a))
  | Domain (f (Exp f a))

deriving instance ( Show (f (Exp f a))
                  , Show (f Text)
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

instance ( Pretty (f (Exp f a))
         , Pretty (f Text)
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :=: e2 -> pretty e1 <+> equals <+> pretty e2
    e :.: x -> pretty e <> dot <> pretty x
    e1 :|: e2 -> pretty e1 <+> pipe <+> pretty e2
    List es -> vcat $ pretty <$> es
    e1 `Where` e2 -> pretty e1 <+> "where" <+> pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    Not e -> "not" <+> parens (pretty e)
    Verify e -> "verify" <+> braces (pretty e)
    Check eff e -> "check" <> angles (pretty eff) <+> braces (pretty e)
    e1 `OfType` e2 -> parens (pretty e1) <+> "|>" <+> parens (pretty e2)
    Class e1 e2 ->
      "class" <>
      maybe mempty (parens . pretty) e1 <+>
      braces (pretty e2)
    Inst e1 e2 -> parens (pretty e1) <+> braces (pretty e2)
    Module e -> "module" <> braces (pretty e)
    Struct e -> "struct" <> braces (pretty e)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Exists x -> "exists" <+> pretty x
    Forall x -> "forall" <+> pretty x
    Alloc2 access x e ->
      "alloc" <> parens (pretty x <> prettySpec access) <+> pretty e
    Alloc3 access x e1 e2 ->
      "alloc" <> parens (pretty x <> prettySpec access) <+> pretty e1 <> parens (pretty e2)
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    Tuple es -> tupled $ pretty <$> es
    Array es -> "array" <> encloseSep lbrace rbrace semi (pretty <$> es)
    Int x -> pretty x
    Float x -> pretty x
    Char x -> "'" <> pretty (w2c x) <> "'"  -- FIXME add escape
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Lam e1 oc eff e2 ->
      "function" <>
      parens (pretty e1) <>
      angles (pretty oc) <>
      angles (pretty eff) <>
      braces (pretty e2)
    InfixColonEqual access _ x e -> pretty x <> prettySpec access <+> ":=" <+> pretty e
    PrefixColon e -> colon <> pretty e
    MixfixArrowColonEqual x y e ->
      pretty x <+> "->" <+> pretty y <+> ":=" <+> pretty e
    Name x -> pretty x
    QualName x y -> "(" <> pretty x <> ":)" <> pretty y
    Path x -> pretty x
    IfArchetypeName x e1 e2 ->
      "if" <+> parens ("archetype" <> parens (pretty x)) <+> braces (pretty e1) <+>
      "else" <+> braces (pretty e2)
    _ -> "unimplemented"
    where
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      braces x =
        nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
        flatAlt (hardline <> rbrace) " }"
      prettySpec access = "<" <> pretty access <> ">"

data Quantifier = Val | Fun | Var deriving Show

data OC = O | C deriving Show

instance Pretty OC where
  pretty = \ case
    O -> "open"
    C -> "closed"

ofType :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ofType = liftL2 OfType

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

alloc2 :: Apply f => Access -> f a -> f (Exp f a) -> f (Exp f a)
alloc2 access = liftL2 (Alloc2 access)

alloc3 :: Apply f => Access -> f a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
alloc3 access = liftL3 (Alloc3 access)

infixColonEqual :: Apply f => Quantifier -> f a -> f (Exp f a) -> f (Exp f a)
infixColonEqual = liftL2 . InfixColonEqual Public -- HACK

prefixColon :: Functor f => f (Exp f a) -> f (Exp f a)
prefixColon = liftL1 PrefixColon

mixfixArrowColonEqual
  :: Apply f
  => f a
  -> f a
  -> f (Exp f a)
  -> f (Exp f a)
mixfixArrowColonEqual = liftL3 MixfixArrowColonEqual

name :: Functor f => f a -> f (Exp f a)
name = fmap Name

ifArchetypeName :: Apply f => f a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ifArchetypeName = liftL3 IfArchetypeName
