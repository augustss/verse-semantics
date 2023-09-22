{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Parse
  ( parse
  ) where

import Control.Comonad

import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Apply

import Language.Verse.Parse.Exp (Exp
                                , pattern (:=:)
                                , pattern (:<>:)
                                , pattern (:.:)
                                , pattern (:..:)
                                , pattern (:<:)
                                , pattern (:<=:)
                                , pattern (:>:)
                                , pattern (:>=:)
                                , pattern (:|:)
                                , pattern (:+:)
                                , pattern (:-:)
                                , pattern (:*:)
                                , pattern (:/:)
                                )
import Language.Verse.Parse.Exp qualified as Exp
import Language.Verse.Parse.Exp ( Pat
                                , pattern (:->:)
                                )
import Language.Verse.Parse.Exp qualified as Pat
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

%left ';' newline
%left ','
%right '=>'
%left where
%nonassoc DOT_SPACE
%left IF IF_THEN FOR
%left then else do
%right '=' ':='
%left ':'
%nonassoc '<>'
%right '<' '<=' '>' '>='
%nonassoc not
%left '|'
%right '..' '->'
%left '+' '-'
%left '*' '/'
%left '.'
%nonassoc '?'
%nonassoc '{'
%left PREFIX_BRACKET
%left '(' '['
%nonassoc ':\n' indent dedent

%token
  '(' { L _ Token.LeftParen }
  ')' { L _ Token.RightParen }
  '*' { L _ Token.Multiply }
  '+' { L _ Token.Plus }
  ',' { L _ Token.Comma }
  '-' { L _ Token.Minus }
  '->' { L _ Token.ThinArrow }
  '.' { L _ Token.Dot }
  '..' { L _ Token.DotDot }
  '/' { L _ Token.Divide }
  ':' { L _ Token.Colon }
  ':=' { L _ Token.ColonEqual }
  ':\n' { L _ Token.ColonEOL }
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
  class { L _ Token.Class }
  dedent { L _ Token.Dedent }
  do { L _ Token.Do }
  else { L _ Token.Else }
  exists { L _ Token.Exists }
  fail { L _ Token.Fail }
  false { L _ Token.False }
  float { (float -> Just $$) }
  for { L _ Token.For }
  function { L _ Token.Function }
  if { L _ Token.If }
  indent { L _ Token.Indent }
  int { (int -> Just $$) }
  module { L _ Token.Module }
  name { (name -> Just $$) }
  newline { L _ Token.Newline }
  not { L _ Token.Not }
  one { L _ Token.One }
  option { L _ Token.Option }
  set { L _ Token.Set }
  struct { L _ Token.Struct }
  enum { L _ Token.Enum }
  sync { L _ Token.Sync }
  then { L _ Token.Then }
  true { L _ Token.True }
  truth { L _ Token.Truth }
  var { L _ Token.Var }
  where { L _ Token.Where }

%%

File :: { L (Exp L Name) }
  : List { L (foldl' (\ z x -> z <> loc x) Loc.minBound $1) (Exp.List $1) }

List :: { [L (Exp L Name)] }
  : Scan { [] }
  | Scan ReversedList MaybeSeparator { reverse $2 }

ReversedList :: { [L (Exp L Name)] }
  : MaybeCommas { [$1] }
  | ReversedList Separator MaybeCommas { $3 : $1 }

MaybeSeparator :: { () }
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
  | Exp where Exp {
      Exp.Where <\$> duplicate $1 <.> duplicate $3
    }
  | Exp '=' Scan Exp {
      (:=:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '=' BraceInd {
      (:=:) <\$> duplicate $1 <.> duplicate $3
    }
  | set name '=' Exp {
      Exp.Set <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | var name {
      Exp.Var <\$ $1 <.> duplicate $2
    }
  | Pat ':=' Exp {
      Exp.InfixColonEqual <\$> duplicate $1 <.> duplicate $3
    }
  | Pat ':=' BraceInd {
      Exp.InfixColonEqual <\$> duplicate $1 <.> duplicate $3
    }
  | Exp '{' List '}' {
      Exp.Inst <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $3 <. $4)
    }
  | Exp ':\n' indent List dedent {
      Exp.Inst <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $4 <. $5)
    }
  | Exp '=>' Exp {
      Exp.Fun $1 $3 <\$ $1 <. $3
    }
  | Exp '=>' BraceInd {
      Exp.Fun $1 $3 <\$ $1 <. $3
    }
  | Exp '<>' Scan Exp {
      (:<>:) <\$> duplicate $1 <.> duplicate $4
    }
  | Exp '.' name {
      (:.:) <\$> duplicate $1 <.> $3
    }
  | Exp '..' Exp {
      (:..:) <\$> duplicate $1 <.> duplicate $3
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
  | '+' Scan Exp {
      Exp.PrefixPlus <$ $1 <.> duplicate $3
    }
  | Exp '+' Scan Exp {
      (:+:) <\$> duplicate $1 <.> duplicate $4
    }
  | '-' Scan Exp {
      Exp.PrefixMinus <$ $1 <.> duplicate $3
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
  | '[' ']' Exp %prec PREFIX_BRACKET {
      Exp.PrefixBracket <\$ $1 <. $2 <.> duplicate $3
    }
  | '?' Exp {
      Exp.PrefixQuery <\$ $1 <.> duplicate $2
    }
  | Exp '?' {
      Exp.Query <\$> duplicate $1 <. $2
    }
  | truth Block {
      Exp.Truth <\$ $1 <.> duplicate $2
    }
  | option Block {
      Exp.Option <\$ $1 <.> duplicate $2
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
  | block Block {
      Exp.Block <\$ $1 <.> duplicate $2
    }
  | module Block {
      Exp.Module <\$ $1 <.> duplicate $2
    }
  | struct Block {
      Exp.Struct <\$ $1 <.> duplicate $2
    }
  | enum NameBlock {
     Exp.Enum <\$ $1 <.> $2
    }
  | class Block {
      Exp.Class Nothing <\$ $1 <.> duplicate $2
    }
  | class Paren Block {
      Exp.Class (Just $2) $3 <\$ $1 <. $3
    }
  | Exp '(' List ')' {
      Exp.ParenInvoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $3 <. $4)
    }
  | Exp '[' List ']' {
      Exp.BracketInvoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $3 <. $4)
    }
  | Exp '->' Exp {
      $1 :->: $3 <\$ $1 <. $3
    }
  | int { Exp.Int <\$> $1 }
  | float { Exp.Float <\$> $1 }
  | If { $1 }
  | For { $1 }
  | Exists { $1 }
  | Function { $1 }
  | Pat %shift { Exp.Pat <\$> $1 }

Pat :: { L (Pat L Name) }
  : name { Pat.Name <\$> $1 }
  | ':' Pat {
      Pat.PrefixColon <\$ $1 <.> duplicate (Exp.Pat <\$> $2)
    }
  | ':' Exp {
      Pat.PrefixColon <\$ $1 <.> duplicate $2
    }
  | Pat ':' Pat {
      Pat.InfixColon $1 (Exp.Pat <\$> $3) <\$ $1 <. $3
    }
  | Pat ':' Exp {
      Pat.InfixColon <\$> duplicate $1 <.> duplicate $3
    }
  | Pat '->' Pat {
      Pat.InfixArrow $1 $3 <\$ $1 <. $3
    }
  | Pat '(' List ')' {
      Pat.Invoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $3 <. $4)
    }

If :: { L (Exp L Name) }
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
  | if Paren Else {
      Exp.IfElse <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Block Else {
      Exp.IfElse <\$ $1 <.> duplicate $2 <.> duplicate $3
    }

Then :: { L (Exp L Name) }
  : then Block { $1 .> $2 }
  | then Exp { $1 .> $2 }

Else :: { L (Exp L Name) }
  : else Block { $1 .> $2 }
  | else Exp { $1 .> $2 }

For :: { L (Exp L Name) }
  : for Block %prec FOR {
      Exp.For <\$ $1 <.> duplicate $2
    }
  | for Paren Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | for Block Do {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }

Do :: { L (Exp L Name) }
  : do Block { $1 .> $2 }
  | do Exp { $1 .> $2 }

Exists :: { L (Exp L Name) }
  : exists name {
      Exp.Exists <\$ $1 <.> duplicate $2
    }

Function :: { L (Exp L Name) }
  : function Paren Block {
      Exp.Fun $2 $3 <\$ $1 <. $3
    }

Paren :: { L (Exp L Name) }
  : '(' List ')' { Exp.List $2 <\$ $1 <. $3 }

BraceInd :: { L (Exp L Name) }
  : Brace { $1 }
  | indent List dedent { Exp.List $2 <\$ $1 <. $3 }

Brace :: { L (Exp L Name) }
  : Scan '{' List '}' { Exp.List $3 <\$ $2 <. $4 }

Block :: { L (Exp L Name) }
  : Brace { $1 }
  | '.' Exp %prec DOT_SPACE { $1 .> $2 }
  | ':\n' indent List dedent { $1 \$> Exp.List $3 <. $4 }

NameBlock :: { L [Name] }
  : Scan '{' Scan NameList '}' { $2 \$> $4 <. $5 }
  | '.' name %prec DOT_SPACE { $1 .> ((:[]) <\$> $2) }
  | ':\n' indent Scan NameList dedent { $1 \$> $4 <. $5 }

NameList :: { [Name] }
  : { [] }
  | name { [extract $1] }
  | name ',' Scan ReversedNameCommas Scan { extract $1 : reverse $4 }
  | name Separator ReversedNameList MaybeSeparator { extract $1 : reverse $3 }

ReversedNameCommas :: { [Name] }
  : name { [extract $1] }
  | ReversedNameCommas ',' Scan name { extract $4 : $1 }

ReversedNameList :: { [Name] }
  : name { [extract $1] }
  | ReversedNameList Separator name { extract $3 : $1 }

Scan :: { () }
  : { () }
  | Scan newline { () }

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
