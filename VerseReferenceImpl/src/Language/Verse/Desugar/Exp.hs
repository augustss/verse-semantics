{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  , Env
  ) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

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
  | Query (f (Exp f a))
  | Module {-# UNPACK #-} !Label !(Env a) (f (Exp f a))
  | Struct {-# UNPACK #-} !Label !(Env a) (f (Exp f a))
  | Class {-# UNPACK #-} !Label (Maybe (f (Exp f a))) !(Env a) (f (Exp f a))
  | Inst (f (Exp f a)) !(Env a) (f (Exp f a))
  | Enum {-# UNPACK #-} !Label [Name]
  | IfThenElse !(Env a) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(Env a) (f (Exp f a)) (f (Exp f a))
  | Exists (f a) (Maybe (f a)) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Fun !(Env a) (f (Exp f a)) (f (Exp f a))
  | Name a
  | IfArchetypeName (f a) (f a) (f (Exp f a)) (f (Exp f a))
  | ArchetypeName a

deriving instance ( Show (f (Exp f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

instance ( Pretty (f (Exp f a))
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :=: e2 -> pretty e1 <+> equals <+> pretty e2
    e :.: x -> pretty e <> dot <> pretty x
    e1 :|: e2 -> pretty e1 <+> pipe <+> pretty e2
    e1 :*>: e2 -> align $ pretty e1 <> flatAlt hardline (semi <> space) <> pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    Not e -> "not" <+> parens (pretty e)
    Query e -> parens (pretty e) <> pretty '?'
    Class i e1 xs e2 ->
      "class" <> pretty '#' <> prettyLabel i <>
      maybe mempty (parens . pretty) e1 <+>
      braces (exists xs $ pretty e2)
    Inst e1 xs e2 -> parens (pretty e1) <+> braces (exists xs $ pretty e2)
    ParenInvoke e1 e2 -> pretty e1 <> parens (pretty e2)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Exists x y e -> align $ "exists" <+> prettyName x y <+> dot <> line <> pretty e
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    Tuple es -> tupled $ pretty <$> es
    Int x -> pretty x
    Float x -> pretty x
    Fun xs e1 e2 -> "fun" <> parens (exists xs $ pretty e1) <+> braces (pretty e2)
    Name x -> pretty x
    IfArchetypeName x y e1 e2 ->
      "if" <+> parens (pretty y <+> ":=" <+> "archetype" <> parens (pretty x)) <+>
      braces (pretty e1) <+>
      "else" <+> braces (pretty e2)
    ArchetypeName x -> "archetype" <> parens (pretty x)
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
      exists xs y = align $
        "exists" <+> hsep (uncurry prettyName <$> HashMap.toList xs) <+> dot <> line <>
        y
      prettyName x = \ case
        Nothing -> pretty x
        Just y -> parens ("var" <+> pretty x <> colon <> pretty y)

type Env a = HashMap a (Maybe a)
