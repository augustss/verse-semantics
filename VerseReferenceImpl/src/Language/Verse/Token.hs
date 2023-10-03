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
  | And
  | Ampersand
  | Array
  | At
  | AtSign
  | Block
  | Caret
  | Catch
  | Char {-# UNPACK #-} !Char
  | Class
  | Colon
  | ColonEOL
  | ColonEqual
  | ColonRightParen
  | Comma
  | Dedent
  | Divide
  | DivideEqual
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
  | LeftBrace
  | LeftBracket
  | LeftParen
  | Less
  | LessEqual
  | Minus
  | MinusEqual
  | Module
  | Multiply
  | MultiplyEqual
  | Name Name
  | Newline
  | Not
  | NotEqual
  | Of
  | One
  | Option
  | Or
  | Pipe
  | Plus
  | PlusEqual
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
    And -> "and"
    Ampersand -> "&"
    Array -> "array"
    At -> "at"
    AtSign -> "@"
    Block -> "block"
    Caret -> "^"
    Catch -> "catch"
    Char x -> pretty ['\'', x, '\'']
    Class -> "class"
    Colon -> colon
    ColonEOL -> colon
    ColonEqual -> colon <> equals
    ColonRightParen -> colon <> rparen
    Comma -> comma
    Dedent -> "dedent"
    Divide -> pretty '/'
    DivideEqual -> "/="
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
    LeftBrace -> lbrace
    LeftBracket -> lbracket
    LeftParen -> lparen
    Less -> pretty '<'
    LessEqual -> "<="
    Minus -> pretty '-'
    MinusEqual -> "-="
    Module -> "module"
    Multiply -> pretty '*'
    MultiplyEqual -> "*="
    Name x -> pretty x
    Newline -> "newline"
    Not -> "not"
    NotEqual -> "<>"
    Of -> "of"
    One -> "one"
    Option -> "option"
    Or -> "or"
    Pipe -> pipe
    Plus -> pretty '+'
    PlusEqual -> "+="
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
