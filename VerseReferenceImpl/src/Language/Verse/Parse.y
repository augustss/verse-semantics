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

import Language.Verse.Parse.Exp (Exp ((:*>:), (:=:), (:|:), (:+:), (:-:), (:*:), (:/:)))
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

%left '=' ':='
%left '+' '-'
%left '*' '/'
%right '|'
%right not
%left '?'
%right ':'
%left '('

%token
  '(' { L _ Token.LeftParen }
  ')' { L _ Token.RightParen }
  '{' { L _ Token.LeftBrace }
  indent { L _ Token.Indent }
  '}' { L _ Token.RightBrace }
  dedent { L _ Token.Dedent }
  ';' { L _ Token.Semi }
  ':' { L _ Token.Colon }
  ':=' { L _ Token.ColonEquals }
  ',' { L _ Token.Comma }
  '.' { L _ Token.Dot }
  '=' { L _ Token.Equals }
  '|' { L _ Token.Pipe }
  '->' { L _ Token.ThinArrow }
  '=>' { L _ Token.FatArrow }
  '?' { L _ Token.QuestionMark }
  '+' { L _ Token.Plus }
  '-' { L _ Token.Minus }
  '*' { L _ Token.Multiply }
  '/' { L _ Token.Divide }
  exists { L _ Token.Exists }
  lambda { L _ Token.Lambda }
  truth { L _ Token.Truth }
  false { L _ Token.False }
  true { L _ Token.True }
  fail { L _ Token.Fail }
  all { L _ Token.All }
  one { L _ Token.One }
  not { L _ Token.Not }
  if { L _ Token.If }
  then { L _ Token.Then }
  else { L _ Token.Else }
  for { L _ Token.For }
  do { L _ Token.Do }
  block { L _ Token.Block }
  int { (int -> Just $$) }
  float { (float -> Just $$) }
  name { (name -> Just $$) }

%%

Exp0
  : Exp1 { $1 }
  | exists name '.' Exp0 { Exp.Exists <\$ $1 <.> duplicate $2 <.> duplicate $4 }
  | lambda name '.' Exp0 { Exp.Lambda <\$ $1 <.> duplicate $2 <.> duplicate $4 }
  | Exp1 ';' { $1 <. $2 }
  | Exp1 ';' Exp0 { (:*>:) <\$> duplicate $1 <.> duplicate $3 }

Exp1 :: { L (Exp L Name) }
  : Exp2 { Exp.Tuple . reverse <\$> $1 }
  | Exp3 { $1 }

Exp2 :: { L [L (Exp L Name)] }
  : Exp3 ',' Exp3 { (\ x y -> [x, y]) <\$> duplicate $3 <.> duplicate $1 }
  | Exp2 ',' Exp3 { (:) <\$> duplicate $3 <.> $1 }

Exp3 :: { L (Exp L Name) }
  : '(' ')' { $1 \$> Exp.Tuple [] <. $2 }
  | '(' Exp0 ')' { $1 .> $2 <. $3 }
  | Exp3 '=' Exp3 { (:=:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '|' Exp3 { (:|:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '?' { Exp.Query <\$> duplicate $1 <. $2 }
  | Exp3 '+' Exp3 { (:+:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '-' Exp3 { (:-:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '*' Exp3 { (:*:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '/' Exp3 { (:/:) <\$> duplicate $1 <.> duplicate $3 }
  | Exp3 '(' ')' { Exp.Invoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.Tuple [] <. $3) }
  | Exp3 '(' Exp0 ')' { Exp.Invoke <\$> duplicate $1 <.> duplicate $3 <. $4 }
  | truth '{' Exp0 '}' { Exp.Truth <\$ $1 <.> duplicate $3 <. $4 }
  | false { Exp.False <\$ $1 }
  | true { Exp.True <\$ $1 }
  | fail { Exp.Fail <\$ $1 }
  | one Block { Exp.One <\$ $1 <.> duplicate $2 }
  | all Block { Exp.All <\$ $1 <.> duplicate $2 }
  | not Exp3 { Exp.Not <\$ $1 <.> duplicate $2 }
  | if Block {
      Exp.If <\$ $1 <.> duplicate $2
    }
  | if '(' Exp0 ')' Block {
      Exp.IfThen <\$ $1 <.> duplicate $3 <.> duplicate $5
    }
  | if Block then Block {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | if '(' Exp0 ')' Block else Block {
      Exp.IfThenElse <\$ $1 <.> duplicate $3 <.> duplicate $5 <.> duplicate $7
    }
  | if Block then Block else Block {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $4 <.> duplicate $6
    }
  | for Block {
      Exp.For <\$ $1 <.> duplicate $2
    }
  | for '(' Exp0 ')' Block {
      Exp.ForDo <\$ $1 <.> duplicate $3 <.> duplicate $5
    }
  | for Block do Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | block Block { Exp.Block <\$ $1 <.> duplicate $2 }
  | int { Exp.Int <\$> $1 }
  | float { Exp.Float <\$> $1 }
  | name { Exp.Name <\$> $1 }
  | ':' Exp3 { Exp.PrefixColon <\$ $1 <.> duplicate $2 }
  | name ':' Exp3 { Exp.InfixColon <\$> duplicate $1 <.> duplicate $3 }
  | name ':=' Exp3 { Exp.InfixColonEquals <\$> duplicate $1 <.> duplicate $3 }

Block
  : '{' Exp0 '}' { $1 .> $2 <. $3 }
  | ':' indent Exp0 dedent { $1 .> $3 <. $4 }

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
