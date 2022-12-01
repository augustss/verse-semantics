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
  | RightBrace
  | Semi
  | Colon
  | Comma
  | Dot
  | Equals
  | Pipe
  | ColonEquals
  | EqualsGreaterThan
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
  | EOF deriving Show

instance Pretty Token where
  pretty = \ case
    LeftParen -> lparen
    RightParen -> rparen
    LeftBrace -> lbrace
    RightBrace -> rbrace
    Semi -> semi
    Colon -> colon
    Comma -> comma
    Dot -> dot
    Equals -> equals
    Pipe -> pipe
    ColonEquals -> colon <> equals
    EqualsGreaterThan -> equals <> rangle
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
    EOF -> "end" <+> "of" <+> "file"
