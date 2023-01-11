{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Parse
  ( parse
  ) where

import Control.Comonad

import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Apply

import Language.Verse.Parse.Exp (Exp ( (:=:)
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
import Language.Verse.Loc (L (..), loc, uncurryL)
import Language.Verse.Loc qualified as Loc
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

%nonassoc '.'
%left ';' newline
%left ','
%left IF IF_THEN FOR
%left then else do
%right '=' ':=' '=>'
%nonassoc '<>'
%right '<' '<=' '>' '>='
%nonassoc not
%left '|'
%left '+' '-'
%left '*' '/'
%nonassoc '?'
%nonassoc ':'
%nonassoc name
%left '(' '['

%token
  '(' { L _ Token.LeftParen }
  ')' { L _ Token.RightParen }
  '*' { L _ Token.Multiply }
  '+' { L _ Token.Plus }
  ',' { L _ Token.Comma }
  '-' { L _ Token.Minus }
  '->' { L _ Token.ThinArrow }
  '.' { L _ Token.Dot }
  '/' { L _ Token.Divide }
  ':' { L _ Token.Colon }
  ':=' { L _ Token.ColonEqual }
  ';' { L _ Token.Semi }
  '<' { L _ Token.Less }
  '<=' { L _ Token.LessEqual }
  '<>' { L _ Token.NotEqual }
  '=' { L _ Token.Equal }
  '=>' { L _ Token.FatArrow }
  '>' { L _ Token.Greater }
  '>=' { L _ Token.GreaterEqual }
  '?' { L _ Token.QuestionMark }
  '[' { L _ Token.LeftBracket }
  ']' { L _ Token.RightBracket }
  '{' { L _ Token.LeftBrace }
  '|' { L _ Token.Pipe }
  '}' { L _ Token.RightBrace }
  all { L _ Token.All }
  block { L _ Token.Block }
  colonEOL { L _ Token.ColonEOL }
  ded { L _ Token.Dedent }
  do { L _ Token.Do }
  else { L _ Token.Else }
  exists { L _ Token.Exists }
  fail { L _ Token.Fail }
  false { L _ Token.False }
  float { (float -> Just $$) }
  for { L _ Token.For }
  function { L _ Token.Function }
  if { L _ Token.If }
  ind { L _ Token.Indent }
  int { (int -> Just $$) }
  isInt { L _ Token.IsInt }
  name { (name -> Just $$) }
  newline { L _ Token.Newline }
  not { L _ Token.Not }
  one { L _ Token.One }
  then { L _ Token.Then }
  true { L _ Token.True }
  truth { L _ Token.Truth }

%%

File :: { L (Exp L Name) }
  : List { L (foldl' (\ z x -> z <> loc x) Loc.minBound $1) (Exp.List $1) }

List :: { [L (Exp L Name)] }
  : Scan { [] }
  | Scan ReversedList MaybeSeparator { reverse $2 }

ReversedList :: { [L (Exp L Name)] }
  : MaybeCommas { [$1] }
  | ReversedList Separator MaybeCommas { $3 : $1 }

MaybeSeparator
  : { () }
  | Separator { () }

Separator
  : ';' Scan { $1 }
  | newline Scan { $1 }

MaybeCommas :: { L (Exp L Name) }
  : Commas { Exp.Tuple . reverse <\$> $1 }
  | Exp { $1 }

Commas :: { L [L (Exp L Name)] }
  : Exp ',' Scan Exp { (\ x y -> [x, y]) <\$> duplicate $4 <.> duplicate $1 }
  | Commas ',' Scan Exp { (:) <\$> duplicate $4 <.> $1 }

Exp :: { L (Exp L Name) }
  : Paren { $1 }
  | Exp '=' Scan Exp {
      (:=:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '=' BraceInd {
      (:=:) <\$> duplicate $1 <.> duplicate $3
    }
  | name ':' Exp {
      Exp.InfixColon <\$> duplicate $1 <.> duplicate $3
    }
  | name ':=' Exp {
      Exp.InfixColonEqual <\$> duplicate $1 <.> duplicate $3
    }
  | name ':=' BraceInd {
      Exp.InfixColonEqual <\$> duplicate $1 <.> duplicate $3
    }
  | name Paren {
      Exp.ParenInvoke <\$> duplicate (Exp.Name <\$> $1) <.> duplicate $2
    }
  | name Paren ':=' Exp {
      Exp.Overload <\$> duplicate $1 <.> duplicate $2 <.> duplicate $4
    }
  | name Paren ':=' BraceInd {
      Exp.Overload <\$> duplicate $1 <.> duplicate $2 <.> duplicate $4
    }
  | Exp '=>' Exp {
      Exp.Function <\$> duplicate $1 <.> duplicate $3
    }
  | Exp '=>' BraceInd {
      Exp.Function <\$> duplicate $1 <.> duplicate $3
    }
  | Exp '<>' Scan Exp {
      (:<>:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '<' Scan Exp {
      (:<:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '<=' Scan Exp {
      (:<=:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '>' Scan Exp {
      (:>:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '>=' Scan Exp {
      (:>=:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '|' Scan Exp {
      (:|:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '+' Scan Exp {
      (:+:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '-' Scan Exp {
      (:-:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '*' Scan Exp {
      (:*:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '/' Scan Exp {
      (:/:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '?' {
      Exp.Query <\$> duplicate $1 <. $2
    }
  | ':' Exp {
      Exp.PrefixColon <\$ $1 <.> duplicate $2
    }
  | Exp '(' List ')' {
      Exp.ParenInvoke <\$> duplicate $1 <.> duplicate (Exp.List $3 <\$ $2 <. $4)
    }
  | Exp '[' List ']' {
      Exp.BracketInvoke <\$> duplicate $1 <.> duplicate (Exp.List $3 <\$ $2 <. $4)
    }
  | truth Block {
      Exp.Truth <\$ $1 <.> duplicate $2
    }
  | false {
      Exp.False <\$ $1
    }
  | true {
      Exp.True <\$ $1
    }
  | fail {
      Exp.Fail <\$ $1
    }
  | one Block {
      Exp.One <\$ $1 <.> duplicate $2
    }
  | all Block {
      Exp.All <\$ $1 <.> duplicate $2
    }
  | not Exp {
      Exp.Not <\$ $1 <.> duplicate $2
    }
  | block Block { Exp.Block <\$ $1 <.> duplicate $2 }
  | int { Exp.Int <\$> $1 }
  | float { Exp.Float <\$> $1 }
  | name { Exp.Name <\$> $1 }
  | If { $1 }
  | For { $1 }
  | Exists { $1 }
  | Function { $1 }
  | isInt Paren {
      Exp.IsInt <\$ $1 <.> duplicate $2
    }

If
  : if Block %prec IF {
      Exp.If <\$ $1 <.> duplicate $2
    }
  | if Paren Block %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Paren Then %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Block Then %prec IF_THEN {
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
  | then Exp { $1 .> $2 }

Else
  : else Block { $1 .> $2 }
  | else Exp { $1 .> $2 }

For
  : for Block %prec FOR {
      Exp.For <\$ $1 <.> duplicate $2
    }
  | for Paren Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | for Block do Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $4
    }

Exists
  : exists name {
      Exp.Exists <\$ $1 <.> duplicate $2
    }

Function
  : function Paren Block {
      Exp.Function <\$ $1 <.> duplicate $2 <.> duplicate $3
    }

Paren
  : '(' List ')' { Exp.List $2 <\$ $1 <. $3 }

BraceInd
  : Brace { $1 }
  | ind List ded { Exp.List $2 <\$ $1 <. $3 }

Brace
  : Scan '{' List '}' { Exp.List $3 <\$ $2 <. $4 }

Block
  : Brace { $1 }
  | '.' Exp { $1 .> $2 }
  | colonEOL ind List ded { Exp.List $3 <\$ $1 <. $4 }

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
