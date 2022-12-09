{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Parse
  ( parse
  ) where

import Control.Comonad

import Data.Functor
import Data.Functor.Apply

import Language.Verse.Parse.Exp (Exp ( (:*>:)
                                     , (:=:)
                                     , (:<>:)
                                     , (:<:)
                                     , (:<=:)
                                     , (:>:)
                                     , (:>=:)
                                     , (:|:)
                                     , (:+:)
                                     , (:-:)
                                     , (:*:)
                                     , (:/:)
                                     ))
import Language.Verse.Parse.Exp qualified as Exp
import Language.Verse.Lexer (Lexer)
import Language.Verse.Lexer qualified as Lexer
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Pos qualified as Pos
import Language.Verse.Token (Token)
import Language.Verse.Token qualified as Token
}

%expect 0
%name parse
%tokentype { L Token }
%monad { Lexer }
%lexer { lexer } { L _ Token.EOF }
%error { uncurryL Lexer.throwError }

%nonassoc IF
%left ';' newline
%left ','
%left '=' ':='
%nonassoc '<>'
%nonassoc '<' '<=' '>' '>='
%nonassoc not
%left '|'
%left '+' '-'
%left '*' '/'
%nonassoc '?'
%nonassoc ':'
%left '('

%token
  '(' { L _ Token.LeftParen }
  ')' { L _ Token.RightParen }
  '{' { L _ Token.LeftBrace }
  ind { L _ Token.Indent }
  '}' { L _ Token.RightBrace }
  ded { L _ Token.Dedent }
  ';' { L _ Token.Semi }
  newline { L _ Token.Newline }
  ':' { L _ Token.Colon }
  ':=' { L _ Token.ColonEqual }
  ',' { L _ Token.Comma }
  '.' { L _ Token.Dot }
  '=' { L _ Token.Equal }
  '<>' { L _ Token.NotEqual }
  '<' { L _ Token.Less }
  '<=' { L _ Token.LessEqual }
  '>' { L _ Token.Greater }
  '>=' { L _ Token.GreaterEqual }
  '|' { L _ Token.Pipe }
  '->' { L _ Token.ThinArrow }
  '=>' { L _ Token.FatArrow }
  '?' { L _ Token.QuestionMark }
  '+' { L _ Token.Plus }
  '-' { L _ Token.Minus }
  '*' { L _ Token.Multiply }
  '/' { L _ Token.Divide }
  all { L _ Token.All }
  block { L _ Token.Block }
  do { L _ Token.Do }
  else { L _ Token.Else }
  exists { L _ Token.Exists }
  fail { L _ Token.Fail }
  false { L _ Token.False }
  for { L _ Token.For }
  if { L _ Token.If }
  isInt { L _ Token.IsInt }
  lambda { L _ Token.Lambda }
  not { L _ Token.Not }
  one { L _ Token.One }
  then { L _ Token.Then }
  true { L _ Token.True }
  truth { L _ Token.Truth }
  int { (int -> Just $$) }
  float { (float -> Just $$) }
  name { (name -> Just $$) }

%%

List
  : Scan exists name '.' List { Exp.Exists <\$> duplicate $3 <.> duplicate $5 }
  | Scan lambda name '.' List { Exp.Lambda <\$> duplicate $3 <.> duplicate $5 }
  | Scan MaybeCommas { $2 }
  | Scan MaybeCommas Separator { $2 <. $3 }
  | Scan MaybeCommas Separator List { (:*>:) <\$> duplicate $2 <.> duplicate $4 }

Separator
  : ';' { $1 }
  | newline { $1 }

MaybeCommas :: { L (Exp L Name) }
  : Commas { Exp.Tuple . reverse <\$> $1 }
  | Exp { $1 }

Commas :: { L [L (Exp L Name)] }
  : Exp ',' Exp { (\ x y -> [x, y]) <\$> duplicate $3 <.> duplicate $1 }
  | Commas ',' Exp { (:) <\$> duplicate $3 <.> $1 }

Exp :: { L (Exp L Name) }
  : '(' ')' { $1 \$> Exp.Tuple [] <. $2 }
  | '(' List ')' { $1 .> $2 <. $3 }
  | Exp '=' Exp { (:=:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '=' BraceInd { (:=:) <\$> duplicate $1 <.> duplicate $3 }
  | name ':' Exp { Exp.InfixColon <\$> duplicate $1 <.> duplicate $3 }
  | name ':=' Exp { Exp.InfixColonEqual <\$> duplicate $1 <.> duplicate $3 }
  | Exp '<>' Exp { (:<>:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '<' Exp { (:<:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '<=' Exp { (:<=:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '>' Exp { (:>:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '>=' Exp { (:>=:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp '|' Scan Exp { (:|:) <\$> duplicate $1 <.> duplicate $4 }
  | Exp '+' Scan Exp { (:+:) <\$> duplicate $1 <.> duplicate $4 }
  | Exp '-' Scan Exp { (:-:) <\$> duplicate $1 <.> duplicate $4 }
  | Exp '*' Scan Exp { (:*:) <\$> duplicate $1 <.> duplicate $4 }
  | Exp '/' Scan Exp { (:/:) <\$> duplicate $1 <.> duplicate $4 }
  | Exp '?' { Exp.Query <\$> duplicate $1 <. $2 }
  | ':' Exp { Exp.PrefixColon <\$ $1 <.> duplicate $2 }
  | Exp '(' ')' { Exp.Invoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.Tuple [] <. $3) }
  | Exp '(' List ')' { Exp.Invoke <\$> duplicate $1 <.> duplicate $3 <. $4 }
  | truth '{' List '}' { Exp.Truth <\$ $1 <.> duplicate $3 <. $4 }
  | false { Exp.False <\$ $1 }
  | true { Exp.True <\$ $1 }
  | fail { Exp.Fail <\$ $1 }
  | one Block { Exp.One <\$ $1 <.> duplicate $2 }
  | all Block { Exp.All <\$ $1 <.> duplicate $2 }
  | not Exp { Exp.Not <\$ $1 <.> duplicate $2 }
  | If { $1 }
  | For { $1 }
  | block Block { Exp.Block <\$ $1 <.> duplicate $2 }
  | int { Exp.Int <\$> $1 }
  | float { Exp.Float <\$> $1 }
  | name { Exp.Name <\$> $1 }
  | isInt '(' List ')' { Exp.IsInt <\$ $1 <.> duplicate $3 }

If
  : if Block {
      Exp.If <\$ $1 <.> duplicate $2
    }
  | if Paren Block {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Paren Then {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Block Then {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Paren Block Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }
  | if Paren Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }
  | if Block Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }

Then
  : then Block { $1 .> $2 }

Else
  : else Block { $1 .> $2 }

For
  : for Block {
      Exp.For <\$ $1 <.> duplicate $2
    }
  | for Paren Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | for Block do Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $4
    }

Paren
  : '(' List ')' { $1 .> $2 <. $3 }

BraceInd
  : Brace { $1 }
  | ind List ded { $1 .> $2 <. $3 }

Brace
  : Scan '{' List '}' { $2 .> $3 <. $4 }

Block
  : Brace { $1 }
  | ':' ind List ded { $1 .> $3 <. $4 }

Scan
  : { () }
  | newline { () }

{
lexer :: (L Token -> Lexer a) -> Lexer a
lexer = (Lexer.getToken >>=)

int :: L Token -> Maybe (L Integer)
int = \ case
  L x (Token.Int y) -> Just $ L x y
  _ -> Nothing

float :: L Token -> Maybe (L Double)
float = \ case
  L x (Token.Float y) -> Just $ L x $ fromRational y
  _ -> Nothing

name :: L Token -> Maybe (L Name)
name = \ case
  L x (Token.Name y) -> Just $ L x y
  _ -> Nothing
}
