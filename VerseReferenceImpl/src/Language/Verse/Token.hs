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
  | Semi
  | Colon
  | Comma
  | Dot
  | Equals
  | Pipe
  | ColonEquals
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
  | Lambda
  | Truth
  | False
  | True
  | Fail
  | All
  | One
  | Not
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
    Semi -> semi
    Colon -> colon
    Comma -> comma
    Dot -> dot
    Equals -> equals
    Pipe -> pipe
    ColonEquals -> colon <> equals
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
    Lambda -> "lambda"
    Truth -> "truth"
    False -> "false"
    True -> "true"
    Fail -> "fail"
    All -> "all"
    One -> "one"
    Not -> "not"
    Int x -> pretty x
    Float x -> pretty (fromRational x :: Double)
    Name x -> pretty x
    Newline -> "newline"
    EOF -> "end" <+> "of" <+> "file"
