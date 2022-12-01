{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Error
  ( Error (..)
  ) where

import Language.Verse.Ident
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Pos
import Language.Verse.Token

import Prettyprinter

data Error
  = LexError !Pos
  | ParseError !Loc !Token
  | DefError !Loc !Loc !Name
  | NameError !Loc !Name
  | IdentError !Loc !(Ident Name)
  | DomainError !Loc
  | DivideByZeroError !Loc
  | UnboundError deriving Show

instance Pretty Error where
  pretty = \ case
    LexError x ->
      pretty x <> colon <+> "unexpected" <+> "character"
    ParseError x y ->
      pretty x <> colon <+> "unexpected" <+> pretty y
    DefError x y z ->
      pretty x <+> "and" <+> pretty y <> colon <+>
      "conflicting" <+> "definitions" <> colon <+>
      pretty z
    NameError x y ->
      varNotInScope x y
    IdentError x y ->
      varNotInScope x y
    DomainError x ->
      pretty x <> colon <+> "unexpected" <+> "value"
    DivideByZeroError x ->
      pretty x <> colon <+> "divide" <+> "by" <+> "zero"
    UnboundError ->
      "unbound" <+> "variable"
    where
      varNotInScope x y =
         pretty x <> colon <+>
         "variable" <+> "not" <+> "in" <+> "scope" <> colon <+>
         pretty y
