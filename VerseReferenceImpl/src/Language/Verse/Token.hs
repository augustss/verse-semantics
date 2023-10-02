{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Token
  ( StringDelimiter(..), Token (..)
  ) where

import Language.Verse.Name

import Prettyprinter
import Prelude (Char, String, Double, Integer, Rational, fromRational)

import Text.Show

data StringDelimiter
  = Quote
  | Brace
  deriving Show

data Token
  = All
  | Ampersand
  | Array
  | At
  | Block
  | Caret
  | Catch
  | Char Char
  | Class
  | Colon
  | ColonEOL
  | ColonEqual
  | ColonRightParen
  | Comma
  | Dedent
  | Divide
  | Do
  | Dot
  | DotDot
  | EOF
  | Else
  | Enum
  | Equal
  | Exists
  | Fail
  | False
  | FatArrow
  | Float !Rational
  | For
  | Function
  | Greater
  | GreaterEqual
  | If
  | Indent
  | Int !Integer
  | Label String
  | LabelCont String
  | LeftBrace
  | LeftBracket
  | LeftParen
  | Less
  | LessEqual
  | Minus
  | Module
  | Multiply
  | Name Name
  | Newline
  | Not
  | NotEqual
  | One
  | Option
  | Pipe
  | Plus
  | QuestionMark
  | RightBrace
  | RightBracket
  | RightParen
  | Semi
  | Set
  | String StringDelimiter String StringDelimiter
  | Struct
  | Sync
  | Then
  | ThinArrow
  | Tilde
  | True
  | Truth
  | Until
  | Var
  | Where deriving Show

instance Pretty Token where
  pretty = \ case
    All -> "all"
    Ampersand -> "&"
    Array -> "array"
    At -> "@"
    Block -> "block"
    Caret -> "^"
    Catch -> "catch"
    Class -> "class"
    Char x -> pretty ['\'', x, '\'']
    Colon -> colon
    ColonEOL -> colon
    ColonEqual -> colon <> equals
    ColonRightParen -> colon <> rparen
    Comma -> comma
    Dedent -> "dedent"
    Divide -> pretty '/'
    Do -> "do"
    Dot -> dot
    DotDot -> ".."
    EOF -> "end" <+> "of" <+> "file"
    Else -> "else"
    Enum -> "enum"
    Equal -> equals
    Exists -> "exists"
    Fail -> "fail"
    False -> "false"
    FatArrow -> equals <> rangle
    Float x -> pretty (fromRational x :: Double)
    For -> "for"
    Function -> "function"
    Greater -> pretty '>'
    GreaterEqual -> ">="
    If -> "if"
    Indent -> "indent"
    Int x -> pretty x
    Label p -> pretty p
    LabelCont p -> pretty p
    LeftBrace -> lbrace
    LeftBracket -> lbracket
    LeftParen -> lparen
    Less -> pretty '<'
    LessEqual -> "<="
    Minus -> pretty '-'
    Module -> "module"
    Multiply -> pretty '*'
    Name x -> pretty x
    Newline -> "newline"
    Not -> "not"
    NotEqual -> "<>"
    One -> "one"
    Option -> "option"
    Pipe -> pipe
    Plus -> pretty '+'
    QuestionMark -> pretty '?'
    RightBrace -> rbrace
    RightBracket -> rbracket
    RightParen -> rparen
    Semi -> semi
    Set -> "set"
    String begin x end -> prettyBegin begin <> pretty x <> prettyEnd end
    Struct -> "struct"
    Sync -> "sync"
    Then -> "then"
    ThinArrow -> pretty '-' <> rangle
    Tilde -> "~"
    True -> "true"
    Truth -> "truth"
    Until -> "until"
    Var -> "var"
    Where -> "where"
    where
      prettyBegin Quote = "\""
      prettyBegin Brace = "}"

      prettyEnd Quote = "\""
      prettyEnd Brace = "{"
