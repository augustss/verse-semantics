{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Error
  ( Error (..)
  ) where

import Language.Verse.Ident
import Language.Verse.Indent
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Pos
import Language.Verse.Token

import Prettyprinter

data Error
  = LexError !Pos
  | IndentError !Pos !Indent !Indent
  | ParseError !Loc !Token
  | DefError !Loc !Loc !Ident
  | NameError !Loc !Name
  | IdentError !Loc !Ident
  | SucceedsError !Loc
  | FailsError !Loc
  | DecidesError !Loc
  | UndecidableError !Loc
  | UnknownInvokeError !Loc
  | InvokeError !Loc
  | InstError !Loc
  | ClassError !Loc
  | SetError !Loc
  | EnvError !Loc
  | DomError !Loc !Loc
  | OLamDomError !Loc !Loc !Loc
  | IntrinsicDomError !Loc
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
    SetError x ->
      pretty x <> colon <+> "expected" <+> "var"
    EnvError x ->
      pretty x <> colon <+> "expected" <+> "environment"
    DomError x y ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "overlapping" <+> "function" <+> "domains"
    OLamDomError x y z ->
      pretty x <+> "and" <+> pretty y <+> "and" <+> pretty z <> colon <+>
      "overlapping" <+> "function" <+> "domains"
    IntrinsicDomError x ->
      pretty x <> colon <+>
      "overlapping" <+> "function" <+> "domains"
    StuckError ->
      "stuck"
    OtherError x msg ->
      pretty x <> colon <+> pretty msg
    NotImplemented msg ->
      "Not" <+> "implemented" <+> pretty msg

prettyIndent :: Indent -> Doc ann
prettyIndent = dquotes . hcat . fmap pretty
