{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Error
  ( Error (..)
  ) where

import Language.Verse.Access
import Language.Verse.Ident
import Language.Verse.Indent
import Language.Verse.Loc
import Language.Verse.Pos
import Language.Verse.SimpleName
import Language.Verse.Token
import Language.Verse.Val

import Prettyprinter

data Error
  = LexError !Pos
  | IndentError !Pos !Indent !Indent
  | ParseError !Loc !Token
  | OpenClosedError !Loc !Loc
  | MultipleAccessError !Loc !Loc
  | SplitEffectError !Loc !Loc
  | SpecError !Loc
  | DefError !Loc !Loc !Ident
  | NameError !Loc !SimpleName
  | IdentError !Loc !Ident
  | SucceedsError !Loc
  | FailsError !Loc
  | DecidesError !Loc
  | UndecidableError !Loc
  | UnknownInvokeError !Loc
  | InvokeError !Loc
  | InstError !Loc
  | ClassError !Loc
  | ValError !Loc
  | RefError !Loc
  | EnvError !Loc
  | DomError !Loc !Loc !FrozenVal
  | OLamDomError !Loc !Loc !Loc !FrozenVal
  | IntrinsicDomError !Loc
  | AccessError !Loc Access
  | StuckError
  | OtherError !Pos String -- Used for Parsec error type
  | NotImplemented String deriving Show

instance Pretty Error where
  pretty = \ case
    LexError x ->
      pretty x <> colon <+> "unexpected" <+> "character"
    IndentError i x y ->
      pretty i <> colon <+>
      "indentation" <+> prettyIndent x <+>
      "does" <+> "not" <+> "match" <+> "previous" <+> "indentation" <+>
      prettyIndent y
    ParseError x y ->
      pretty x <> colon <+> "parse" <+> "error" <> colon <+>
      "unexpected" <+> pretty y
    OpenClosedError x y ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "multiple" <+> "open" <+> "or" <+> "closed" <+> "specifiers"
    SplitEffectError x y ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "multiple" <+> "effect" <+> "specifiers"
    MultipleAccessError x y ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "multiple" <+> "access" <+> "specifiers"
    SpecError x ->
      pretty x <> colon <+> "unexpected" <+> "specifier"
    DefError x y z ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "conflicting" <+> "definitions" <> colon <+>
      pretty z
    NameError x y ->
      pretty x <> colon <+> "name" <+> pretty y <+> "not" <+> "in" <+> "scope"
    IdentError x y ->
      pretty x <> colon <+> "identifier" <+> pretty y <+> "not" <+> "in" <+> "scope"
    SucceedsError x ->
      pretty x <> colon <+> "expected" <+> "one" <+> "value"
    FailsError x ->
      pretty x <> colon <+> "expected" <+> "zero" <+> "values"
    DecidesError x ->
      pretty x <> colon <+> "expected" <+> "zero" <+> "or" <+> "one" <+> "value"
    UndecidableError x ->
      pretty x <> colon <+> "undecidable" <+> "unification"
    UnknownInvokeError x ->
      pretty x <> colon <+> "unknown" <+> "invocable"
    InvokeError x ->
      pretty x <> colon <+> "expected" <+> "invocable"
    InstError x ->
      pretty x <> colon <+> "expected" <+> "class" <+> "or" <+> "struct"
    ClassError x ->
      pretty x <> colon <+> "expected" <+> "class"
    ValError x ->
      pretty x <> colon <+> "unexpected" <+> "var"
    RefError x ->
      pretty x <> colon <+> "expected" <+> "var"
    EnvError x ->
      pretty x <> colon <+> "expected" <+> "environment"
    DomError x y z ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "overlapping" <+> "function" <+> "domains" <+> "for" <+> pretty z
    OLamDomError a b c d ->
      pretty a <+> "and" <+> pretty b <+> "and" <+> pretty c <> colon <+>
      "overlapping" <+> "function" <+> "domains" <+> "for" <+> pretty d
    IntrinsicDomError x ->
      pretty x <> colon <+>
      "overlapping" <+> "function" <+> "domains"
    AccessError x access ->
      pretty x <> colon <+>
      "specifier" <+> "<" <> pretty access <> ">" <+> "can't" <+> "be" <+> "used" <+> "here"
    StuckError ->
      "stuck"
    OtherError x msg ->
      pretty x <> colon <+> pretty msg
    NotImplemented msg ->
      "not" <+> "implemented" <+> pretty msg

prettyIndent :: Indent -> Doc ann
prettyIndent = dquotes . hcat . fmap pretty
