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
  = All
  | Array
  | Block
  | Class
  | Colon
  | ColonEOL
  | ColonEqual
  | Comma
  | Dedent
  | Divide
  | Do
  | Dot
  | DotDot
  | EOF
  | Else
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
  | Struct
  | Sync
  | Then
  | ThinArrow
  | True
  | Truth
  | Var
  | Where deriving Show

instance Pretty Token where
  pretty = \ case
    All -> "all"
    Array -> "array"
    Block -> "block"
    Class -> "class"
    Colon -> colon
    ColonEOL -> colon
    ColonEqual -> colon <> equals
    Comma -> comma
    Dedent -> "dedent"
    Divide -> pretty '/'
    Do -> "do"
    Dot -> dot
    DotDot -> ".."
    EOF -> "end" <+> "of" <+> "file"
    Else -> "else"
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
    Struct -> "struct"
    Sync -> "sync"
    Then -> "then"
    ThinArrow -> pretty '-' <> rangle
    True -> "true"
    Truth -> "truth"
    Var -> "var"
    Where -> "where"
