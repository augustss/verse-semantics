{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Rewrite.Exp
  ( Exp (..)
  ) where

import Language.Verse.Name

import Prettyprinter

data Exp f a
  = f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !Name
  | f (Exp f a) :|: f (Exp f a)
  | List [f (Exp f a)]
  | f (Exp f a) `Where` f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Query (f (Exp f a))
  | Module (f (Exp f a))
  | Struct (f (Exp f a))
  | Class (Maybe (f (Exp f a))) (f (Exp f a))
  | Inst (f (Exp f a)) (f (Exp f a))
  | Enum [Name]
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Block (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Exists (f a)
  | Var (f a)
  | Set (f a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Fun (f (Exp f a)) (f (Exp f a))
  | InfixColonEqual !Bool (f a) (f (Exp f a))
  | PrefixColon (f (Exp f a))
  | MixfixArrowColonEqual (f a) (f a) (f (Exp f a))
  | Name a
  | IfArchetypeName a a (f (Exp f a)) (f (Exp f a))
  | f (Exp f a) :|>: f (Exp f a)

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
    List es -> vcat $ pretty <$> es
    e1 `Where` e2 -> pretty e1 <+> "where" <+> pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    Not e -> "not" <+> parens (pretty e)
    Query e -> parens (pretty e) <> pretty '?'
    Class e1 e2 ->
      "class" <>
      maybe mempty (parens . pretty) e1 <+>
      braces (pretty e2)
    Inst e1 e2 -> parens (pretty e1) <+> braces (pretty e2)
    ParenInvoke e1 e2 -> pretty e1 <> parens (pretty e2)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Exists x -> "exists" <+> pretty x
    Tuple es -> tupled $ pretty <$> es
    Int x -> pretty x
    Fun e1 e2 -> "fun" <> parens (pretty e1) <+> braces (pretty e2)
    InfixColonEqual _ x e -> pretty x <+> ":=" <+> pretty e
    PrefixColon e -> colon <> pretty e
    MixfixArrowColonEqual x y e ->
      pretty x <+> "->" <+> pretty y <+> ":=" <+> pretty e
    Name x -> pretty x
    IfArchetypeName x y e1 e2 ->
      "if" <+> parens (pretty y <+> ":=" <+> "archetype" <> parens (pretty x)) <+>
      braces (pretty e1) <+>
      "else" <+> braces (pretty e2)
    e1 :|>: e2 -> pretty e1 <+> "|>" <+> pretty e2
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
