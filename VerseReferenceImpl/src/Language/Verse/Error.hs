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
  | DefError !Loc !Loc !Name
  | AnonError !Loc
  | NameError !Loc !Name
  | IdentError !Loc !Ident
  | DomainError !Loc
  | WrongError !Loc
  | StuckError deriving Show

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
      pretty x <> colon <+> "unexpected" <+> pretty y
    DefError x y z ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "conflicting" <+> "definitions" <> colon <+>
      pretty z
    AnonError x ->
      pretty x <> colon <+>
      "unnamed" <+> "value" <+> "in" <+> "abstraction" <+> "context"
    NameError x y ->
      varNotInScope x y
    IdentError x y ->
      varNotInScope x y
    DomainError x ->
      pretty x <> colon <+> "unexpected" <+> "value"
    WrongError x ->
      pretty x <> colon <+> "wrong"
    StuckError -> "stuck"
    where
      varNotInScope x y =
         pretty x <> colon <+>
         "variable" <+> "not" <+> "in" <+> "scope" <> colon <+> pretty y

prettyIndent :: Indent -> Doc ann
prettyIndent = dquotes . hcat . fmap pretty
