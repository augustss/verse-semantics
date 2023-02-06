{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Token
  ( Token (..)
  ) where

import Language.Verse.Name

import Prettyprinter
import Prelude (Double, Integer, Rational, fromRational)

import Text.Show

data Token
  = LeftParen
  | RightParen
  | LeftBrace
  | Indent
  | RightBrace
  | Dedent
  | LeftBracket
  | RightBracket
  | Semi
  | Colon
  | Comma
  | Dot
  | DotDot
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Pipe
  | ColonEqual
  | ThinArrow
  | FatArrow
  | QuestionMark
  | Plus
  | Minus
  | Multiply
  | Divide
  | If
  | Then
  | Else
  | For
  | Do
  | Block
  | Class
  | Struct
  | Module
  | Exists
  | Function
  | Truth
  | Array
  | False
  | True
  | Var
  | Set
  | Fail
  | All
  | One
  | Not
  | Sync
  | Int Integer
  | Float Rational
  | Name Name
  | Newline
  | EOF deriving Show

instance Pretty Token where
  pretty = \ case
    LeftParen -> lparen
    RightParen -> rparen
    LeftBrace -> lbrace
    Indent -> "indent"
    RightBrace -> rbrace
    Dedent -> "dedent"
    LeftBracket -> lbracket
    RightBracket -> rbracket
    Semi -> semi
    Colon -> colon
    Comma -> comma
    Dot -> dot
    DotDot -> ".."
    Equal -> equals
    NotEqual -> "<>"
    Less -> pretty '<'
    LessEqual -> "<="
    Greater -> pretty '>'
    GreaterEqual -> ">="
    Pipe -> pipe
    ColonEqual -> colon <> equals
    ThinArrow -> pretty '-' <> rangle
    FatArrow -> equals <> rangle
    QuestionMark -> pretty '?'
    Plus -> pretty '+'
    Minus -> pretty '-'
    Multiply -> pretty '*'
    Divide -> pretty '/'
    If -> "if"
    Then -> "then"
    Else -> "else"
    For -> "for"
    Do -> "do"
    Block -> "block"
    Class -> "class"
    Struct -> "struct"
    Module -> "module"
    Exists -> "exists"
    Function -> "function"
    Truth -> "truth"
    Array -> "array"
    False -> "false"
    True -> "true"
    Var -> "var"
    Set -> "set"
    Fail -> "fail"
    All -> "all"
    One -> "one"
    Not -> "not"
    Sync -> "sync"
    Int x -> pretty x
    Float x -> pretty (fromRational x :: Double)
    Name x -> pretty x
    Newline -> "newline"
    EOF -> "end" <+> "of" <+> "file"
