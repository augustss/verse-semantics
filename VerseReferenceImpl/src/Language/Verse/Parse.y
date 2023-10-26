{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Parse
  ( parse
  ) where

import Control.Comonad

import Data.Foldable (foldl')
import Data.Functor
import Data.Functor.Apply
import Data.Text qualified as Text

import Language.Verse.Parse.Exp ( Exp
                                , AttributePart
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
import Prettyprinter
import Debug.Trace(trace)

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
%left IF IF_THEN FOR INVOKE
%left then else do
%left until catch
%nonassoc return
%left or
%left and
%right '=' ':=' '+=' '-=' '*=' '/='
%nonassoc SET
%left NO_COLON
%left ':'
%nonassoc '<>'
%right '<' '<='
%right '>' '>='
%nonassoc SPEC
%nonassoc not all
%left '|'
%right '..' '->'
%left '+' '-'
%left '*' '/'
%left '.'
%nonassoc '?' '^' at of
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
  '. ' { L _ Token.DotSpace }
  '..' { L _ Token.DotDot }
  '/' { L _ Token.Divide }
  ':' { L _ Token.Colon }
  ':=' { L _ Token.ColonEqual }
  '+=' { L _ Token.PlusEqual }
  '-=' { L _ Token.MinusEqual }
  '*=' { L _ Token.MultiplyEqual }
  '/=' { L _ Token.DivideEqual }
  ':)' { L _ Token.ColonRightParen }
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
  '^' { L _ Token.Caret }
  '@' { L _ Token.AtSign }
  at { L _ Token.At }
  of { L _ Token.Of }
  '[' { L _ Token.LeftBracket }
  ']' { L _ Token.RightBracket }
  '{' { L _ Token.LeftBrace }
  '|' { L _ Token.Pipe }
  '}' { L _ Token.RightBrace }
  all { L _ Token.All }
  and { L _ Token.And }
  array { L _ Token.Array }
  block { L _ Token.Block }
  catch { L _ Token.Catch }
  class { L _ Token.Class }
  char { (char -> Just $$) }
  dedent { L _ Token.Dedent }
  do { L _ Token.Do }
  else { L _ Token.Else }
  enum { L _ Token.Enum }
  exists { L _ Token.Exists }
  fail { L _ Token.Fail }
  fails { L _ Token.Fails }
  false { L _ Token.False }
  float { (float -> Just $$) }
  for { L _ Token.For }
  forall { L _ Token.Forall }
  if { L _ Token.If }
  indent { L _ Token.Indent }
  int { (int -> Just $$) }
  name { (name -> Just $$) }
  path { (path -> Just $$) }
  newline { L _ Token.Newline }
  not { L _ Token.Not }
  one { L _ Token.One }
  option { L _ Token.Option }
  or { L _ Token.Or }
  return { L _ Token.Return }
  set { L _ Token.Set }
  struct { L _ Token.Struct }
  string { (string -> Just $$) }
  stringBegin { (stringBegin -> Just $$) }
  stringCont { (stringCont -> Just $$) }
  stringEnd { (stringEnd -> Just $$) }
  sync { L _ Token.Sync }
  then { L _ Token.Then }
  true { L _ Token.True }
  truth { L _ Token.Truth }
  until { L _ Token.Until }
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

Defs :: { L [L (Exp L Name)] }
  : Defs ',' Scan Def { (\ xs x -> x:xs) <\$> $1 <.> duplicate $4 }
  | Def %shift { (:[]) <\$> duplicate $1 }

Base :: { L (Exp L Name) }
  : Paren { Exp.Paren <\$> duplicate $1 }
  | int { Exp.Int <\$> $1 }
  | float { Exp.Float <\$> $1 }
  | char { Exp.Char <\$> $1 }
  | string { ( \ x -> Exp.String x []) <\$> $1 }
  | stringBegin StringCont { Exp.String <\$> $1 <.> $2 }
  | false {
      Exp.False <\$ $1
    }
  | true {
      Exp.True <\$ $1
    }
  | fail {
      Exp.Fail <\$ $1
    }
  | Call '.' name {
      (:.:) <\$> duplicate $1 <.> (([], extract $3) <\$ $3)
    }
  | Call '.' '(' List ':)' name {
      (:.:) <\$> duplicate $1 <.> (($4, extract $6) <\$ $6)
    }
  | PatName %shift { Exp.Pat <\$> $1 }

Call :: { L (Exp L Name) }
  : Call Paren {
      Exp.ParenInvoke <\$> duplicate $1 <.> duplicate $2
    }
  | Call '[' List ']' {
      Exp.BracketInvoke <\$> duplicate $1 <.> duplicate ($2 \$> Exp.List $3 <. $4)
    }
  | Call Brace {
      Exp.Inst <\$> duplicate $1 <.> duplicate (Exp.List <\$> $2)
  }
  | Call '. ' Def {
      Exp.Inst <\$> duplicate $1 <.> duplicate $3
    }
  | Call at Exp {
      Exp.ParenInvoke <\$> duplicate $1 <.> duplicate $3
    }
  | Call of Exp {
      Exp.BracketInvoke <\$> duplicate $1 <.> duplicate $3 -- Same as ShipVerse
    }
  | Call '?' {
      Exp.PostfixQuery <\$> duplicate $1 <. $2
    }
  | Call '^' {
      Exp.PostfixCaret <\$> duplicate $1 <. $2
    }
  | array Brace {
       Exp.Array <\$> $2 <. $1
    }
  | option Block {
      Exp.Option <\$ $1 <.> duplicate $2
    }
  | truth Block {
      Exp.Truth <\$ $1 <.> duplicate $2
    }
  | one Block {
      Exp.One <\$ $1 <.> duplicate $2
    }
  | all Block {
      Exp.All <\$ $1 <.> duplicate $2
    }
  | Forall { $1 }
  | fails Block {
      Exp.Fails <\$ $1 <.> duplicate $2
    }
  | block Block {
      Exp.Block <\$ $1 <.> duplicate $2
    }
  | struct Block {
      Exp.Struct <\$ $1 <.> duplicate $2
    }
  | enum Attributes NameBlock {
      Exp.Enum (fixAttributes $2) <\$ $1 <.> $3
    }
  | class Block {
      Exp.Class Nothing <\$ $1 <.> duplicate $2
    }
  | class Paren Block {
      Exp.Class (Just $2) $3 <\$ $1 <. $3
    }
  | If { $1 }
  | For { $1 }
  | Exists { $1 }
  | Base %shift { $1 }

Attribute :: { L (Exp L Name) }
  : '<' PrefixColon '>' {
      $2 <. $1 <. $3
    }

Attributes :: { [L (Exp L Name)] }
  : Attributes Attribute { (\ xs x -> x : xs) $1 $2 }
  | { ([]) }

Prefix :: { L (Exp L Name) }
  : '[' List ']' Prefix {
      Exp.PrefixBracket $2 <\$ $1 <.> duplicate $4
    }
  | '?' Prefix {
      Exp.PrefixQuery <\$ $1 <.> duplicate $2
    }
  | '^' Prefix {
      Exp.PrefixCaret <\$ $1 <.> duplicate $2
     }
  | '*' Scan Prefix {
      Exp.PrefixMultiply <\$ $1 <.> duplicate $3
    }
  | '+' Scan Prefix {
      Exp.PrefixPlus <\$ $1 <.> duplicate $3
    }
  | '-' Scan Prefix {
      Exp.PrefixMinus <\$ $1 <.> duplicate $3
    }
  | Call %shift {
       $1
    }

Mul :: { L (Exp L Name) }
  : Prefix '*' Scan Mul {
      (:*:) <\$> duplicate $1 <.> duplicate $4
    }
  | Prefix '/' Scan Mul {
      (:/:) <\$> duplicate $1 <.> duplicate $4
    }
  | Prefix %shift {
       $1
    }

Add :: { L (Exp L Name) }
  : Mul '+' Scan Add {
      (:+:) <\$> duplicate $1 <.> duplicate $4
    }
  | Mul '-' Scan Add {
      (:-:) <\$> duplicate $1 <.> duplicate $4
    }
  | Mul %shift {
       $1
    }

To :: { L (Exp L Name) }
  : Add '->' To {
      $1 :->: $3 <\$ $1 <. $3
    }
  | Add '..' To {
      (:..:) <\$> duplicate $1 <.> duplicate $3
    }
  | Add %shift {
       $1
  }

Choose :: { L (Exp L Name) }
  : To '|' Scan Choose {
      (:|:) <\$> duplicate $1 <.> duplicate $4
    }
  | To %shift {
       $1
  }

InfixColon :: { L (Exp L Name) }
  : InfixColon ':' Choose {
      fixInfixColon $1 $3
    }
  | Choose %shift {
       $1
    }

PrefixColon :: { L (Exp L Name) }
  : ':' Scan InfixColon {
      Exp.Pat <\$> (Pat.PrefixColon <\$ $1 <.> duplicate $3)
    }
  | InfixColon %shift {
      $1
    }

ComparePart :: { L (AttributePart L Name) }
  : '>' Scan {
      Exp.GreaterThan <\$ $1
    }
  | '>=' Scan {
      Exp.GreaterEqual <\$ $1
    }
  | '<' Scan {
      Exp.LessThan <\$ $1
    }
  | '<=' Scan {
      Exp.LessEqual <\$ $1
    }
  | Brace {
      Exp.Part <\$> duplicate (Exp.Brace <\$> duplicate (Exp.List <\$> $1) )
    }
  | ':\n' indent List dedent {
      Exp.Part <\$> duplicate (Exp.Brace <\$> duplicate ($1 \$> Exp.List $3 <. $4))
    }
  | '. ' Def {
      Exp.Part <\$> duplicate (Exp.Brace <\$> duplicate $2 <. $1)
    }
  | PrefixColon %shift {
      Exp.Part <\$> duplicate $1
    }

CompareParts :: { L [L (AttributePart L Name)] }
  : CompareParts ComparePart { (\ xs x -> x : xs) <\$> $1 <.> duplicate $2 }
  | ComparePart %shift { (:[]) <\$> duplicate $1 }

Less :: { L (Exp L Name) }
  : CompareParts %shift {
      fixCompare $1
    }

Invoke :: { L (Exp L Name) }
  : Invoke Until {
      Exp.Until <\$> duplicate $1 <.> duplicate $2
    }
  | Invoke Do {
      Exp.Do <\$> duplicate $1 <.> duplicate $2
    }
  | Invoke Catch {
      Exp.Catch <\$> duplicate $1 <.> duplicate $2
    }
  | Less %shift {
       $1
    }

NotEq :: { L (Exp L Name) }
  : NotEq '<>' Scan Invoke {
      (:<>:) <\$> duplicate $1 <.> duplicate $4
    }
  | Invoke %shift {
       $1
    }

Return :: { L (Exp L Name) }
  : return Scan Def {
     Exp.Return . Just <\$ $1 <.> duplicate $3
    }
  | return Scan %shift {
      Exp.Return Nothing <\$ $1
    }
  | NotEq %shift {
       $1
    }

Eq :: { L (Exp L Name) }
  : NotEq '=' Eq {
      (:=:) <\$> duplicate $1 <.> duplicate $3
    }
  | NotEq '=' BraceInd {
      (:=:) <\$> duplicate $1 <.> duplicate $3
    }
  | Return %shift {
       $1
    }

Not :: { L (Exp L Name) }
  : not Not {
      Exp.Not <\$ $1 <.> duplicate $2
    }
  | Eq %shift {
       $1
  }

And :: { L (Exp L Name) }
  : Not and Scan And {
      Exp.And <\$> duplicate $1 <.> duplicate $4
    }
  | Not %shift {
       $1
    }

Or :: { L (Exp L Name) }
  : And or Scan Or {
      Exp.Or <\$> duplicate $1 <.> duplicate $4
    }
  | And %shift {
       $1
    }

Where :: { L (Exp L Name) }
  : Where where Defs {
      Exp.Where <\$> duplicate $1 <.> duplicate ((Exp.List \$ reverse \$ extract $3) <\$ $3)
    }
  | Or %shift {
       $1
    }

Def :: { L (Exp L Name) }
  : set Choose '=' Def {
      Exp.Set <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '=' BraceInd {
      Exp.Set <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '+=' Def {
      Exp.SetInfixPlusEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '+=' BraceInd {
      Exp.SetInfixPlusEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '-=' Def {
      Exp.SetInfixMinusEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '-=' BraceInd {
      Exp.SetInfixMinusEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '*=' Def {
      Exp.SetInfixMultiplyEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '*=' BraceInd {
      Exp.SetInfixMultiplyEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '/=' Def {
      Exp.SetInfixDivideEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | set Choose '/=' BraceInd {
      Exp.SetInfixDivideEqual <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | Or ':=' Exp {
      fixInfixColonEqual $1 $3
    }
  | Or ':=' BraceInd {
      fixInfixColonEqual $1 $3
    }
  | NotEq '=' Exp {
      (:=:) <\$> duplicate $1 <.> duplicate $3
    }
  | Where %shift {
       $1
    }

Fun :: { L (Exp L Name) }
  : Def '=>' Fun {
      Exp.Fun $1 $3 <\$ $1 <. $3
    }
  | Def '=>' BraceInd {
      Exp.Fun $1 $3 <\$ $1 <. $3
    }
  | Def %shift {
       $1
    }

Exp :: { L (Exp L Name) }
  : Fun %shift {
      $1
    }

StringCont :: { L [(L (Exp L Name), L String)] }
  : File stringEnd { ( \ e s -> [(e,s)]) <\$> duplicate $1 <.> duplicate $2 }
  | File stringCont StringCont { ( \ e s es -> (e,s):es) <\$> duplicate $1 <.> duplicate $2 <.> $3 }

PatName :: { L (Pat L Name) }
  : name {
      Pat.Name [] <\$> $1
    }
  | '(' List ':)' name {
      Pat.Name $2 <\$> $4
    }
  | var Attributes name {
      Pat.Var (fixAttributes $2) <\$ $1 <.> duplicate $3
    }
  | path {
      Pat.Path <\$> $1
    }

If :: { L (Exp L Name) }
  : if Block %prec IF {
      Exp.If <\$ $1 <.> duplicate $2
    }
  | if Brace %prec IF {
      Exp.If <\$ $1 <.> duplicate (Exp.List <\$> $2)
    }
  | if '. ' Def %prec IF {
      Exp.If <\$ $1 <.> duplicate $3
    }
  | if Paren Block %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Paren Brace %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate (Exp.List <\$> $3)
    }
  | if Paren '. ' Def %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | if '. ' Def Then %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $3 <.> duplicate $4
    }
  | if Paren Then %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Block Then %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Brace Then %prec IF_THEN {
      Exp.IfThen <\$ $1 <.> duplicate (Exp.List <\$> $2) <.> duplicate $3
    }
  | if Paren Block Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }
  | if Paren Brace Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate (Exp.List <\$> $3) <.> duplicate $4
    }
  | if Paren '. ' Def Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $4 <.> duplicate $5
    }
  | if Paren Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }
  | if Block Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $2 <.> duplicate $3 <.> duplicate $4
    }
  | if Brace Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate (Exp.List <\$> $2) <.> duplicate $3 <.> duplicate $4
    }
  | if '. ' Def Then Else {
      Exp.IfThenElse <\$ $1 <.> duplicate $3 <.> duplicate $4 <.> duplicate $5
    }
  | if Paren Else {
      Exp.IfElse <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Block Else {
      Exp.IfElse <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | if Brace Else {
      Exp.IfElse <\$ $1 <.> duplicate (Exp.List <\$> $2) <.> duplicate $3
    }

Then :: { L (Exp L Name) }
  : then Exp { $1 .> $2 }
  | then '. ' Exp %prec DOT_SPACE { $1 .> $3 }

Else :: { L (Exp L Name) }
  : else Exp { $1 .> $2 }
  | else '. ' Exp %prec DOT_SPACE { $1 .> $3 }

For :: { L (Exp L Name) }
  : for Block %prec FOR {
      Exp.For <\$ $1 <.> duplicate $2
    }
  | for Brace %prec FOR {
      Exp.For <\$ $1 <.> duplicate (Exp.List <\$> $2)
    }
  | for Paren Block {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }
  | for Paren Brace {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate (Exp.List <\$> $3)
    }
  | for Paren '. ' Def {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $4
    }
  | for Paren Do {
      Exp.ForDo <\$ $1 <.> duplicate $2 <.> duplicate $3
    }

Do :: { L (Exp L Name) }
  : do Exp { $1 .> $2 }
  | do '. ' Exp %prec DOT_SPACE { $1 .> $3 }

Until :: { L (Exp L Name) }
  : until Exp { $1 .> $2 }

Catch :: { L (Exp L Name) }
  : catch Invoke { $1 .> $2 }

Exists :: { L (Exp L Name) }
  : exists name {
      Exp.Exists <\$ $1 <.> duplicate $2
    }

Forall :: { L (Exp L Name) }
  : forall name {
      Exp.Forall <\$ $1 <.> duplicate $2
    }

Paren0 :: { L[L (Exp L Name)] }
  : '(' List ')' {
      $2 <\$ $1 <. $3
    }

Paren :: { L (Exp L Name) }
  : Paren0 {
      Exp.List <\$> $1
    }

Brace :: { L [L (Exp L Name)] }
  : '{' List '}' { $2 <\$ $1 <. $3 }

BraceInd :: { L (Exp L Name) }
  : indent List dedent { Exp.List $2 <\$ $1 <. $3 }

Block :: { L (Exp L Name) }
  : '.' Exp %prec DOT_SPACE { $1 .> $2 }
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

char :: L Token -> Maybe (L Char)
char = \ case
  L x (Token.Char y) -> Just $ L x y
  _ -> Nothing

string :: L Token -> Maybe (L String)
string = \ case
  L x (Token.String Token.Quote y Token.Quote) -> Just $ L x y
  _ -> Nothing

stringBegin :: L Token -> Maybe (L String)
stringBegin = \ case
  L x (Token.String Token.Quote y Token.Brace) -> Just $ L x y
  _ -> Nothing

stringCont :: L Token -> Maybe (L String)
stringCont = \ case
  L x (Token.String Token.Brace y Token.Brace) -> Just $ L x y
  _ -> Nothing

stringEnd :: L Token -> Maybe (L String)
stringEnd = \ case
  L x (Token.String Token.Brace y Token.Quote) -> Just $ L x y
  _ -> Nothing

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
  L x Token.At -> Just $ L x "at"
  L x Token.Of -> Just $ L x "of"
  _ -> Nothing

path :: L Token -> Maybe (L Name)
path = \ case
  L x (Token.Path y) -> Just $ L x y
  _ -> Nothing

expToPat :: L (Exp L a) -> Maybe (L (Pat L a))
expToPat exp@(extract -> Exp.Pat pat) = Just (pat <$ exp)
expToPat _ex@(extract -> Exp.Paren e) = expToPat e
expToPat _ex@(extract -> Exp.List [e]) = expToPat e
expToPat exp@(extract -> Exp.ParenInvoke e1@(expToPat -> Just p1) e2) = Just (Pat.Invoke <$> duplicate p1 <.> duplicate e2 <. exp)
expToPat exp@(extract -> Exp.ExpInfixColon e1@(expToPat -> Just p1) e2) = Just (Pat.InfixColon <$> duplicate p1 <.> duplicate e2 <. exp)
expToPat exp@(extract -> e1@(expToPat -> Just p1) :->: e2@(expToPat -> Just p2)) = Just (Pat.InfixArrow <$> duplicate p1 <.> duplicate p2 <. exp)
expToPat exp = Nothing

fixInfixColonEqual :: L (Exp L Name) -> L (Exp L Name) -> L (Exp L Name)
fixInfixColonEqual lhs@(expToPat -> Just pat) rhs = Exp.InfixColonEqual <$> duplicate pat <.> duplicate rhs
fixInfixColonEqual lhs rhs = error (show (pretty (extract lhs)) ++ ": fixInfixColonEqual must have a Pat as lhs, got:" ++ show lhs)

fixInfixColon :: L (Exp L Name) -> L (Exp L Name) -> L (Exp L Name)
fixInfixColon lhs@(expToPat -> Just pat) rhs = Exp.Pat <$> (Pat.InfixColon <$> duplicate pat <.> duplicate rhs)
fixInfixColon lhs rhs = Exp.ExpInfixColon <$> duplicate lhs <.> duplicate rhs

fixAttributes :: [L (Exp L Name) ] -> [L (Exp L Name)]
fixAttributes parts = reverse parts


fixCompare :: L [L (AttributePart L Name) ] -> L (Exp L Name)
fixCompare parts = scanGreater [] [] $ reverse $ extract parts

scanGreater :: (Show a, Pretty a) => [L (Exp L a)] -> [L (AttributePart L a)] -> [L (AttributePart L a)] -> L (Exp L a)
scanGreater [] [] (L _ (Exp.Part e) : []) = e
scanGreater es ops (L _ (Exp.Part e) : []) = buildCompare (e:es) ops
scanGreater es ops (L _ (Exp.Part e) : op@(L _ Exp.GreaterEqual) : xs) = scanGreater (e:es) (op : ops) xs
scanGreater es ops (L _ (Exp.Part e) : op@(L _ Exp.GreaterThan) : xs) = scanGreater (e:es) (op : ops) xs
scanGreater es ops xs = scanLess es ops xs

scanLess :: (Show a, Pretty a) => [L (Exp L a)] -> [L (AttributePart L a)] -> [L (AttributePart L a)] -> L (Exp L a)
scanLess es ops (L _ (Exp.Part e) : []) = buildCompare (e:es) ops
scanLess es ops (L _ (Exp.Part e) : op@(L _ Exp.LessEqual) : xs) = scanLess (e:es) (op : ops) xs
scanLess es ops (L _ (Exp.Part e) : op@(L _ Exp.LessThan) : xs) = scanLess (e:es) (op : ops) xs
scanLess es ops (L _ (Exp.Part e) : op@(L l Exp.GreaterEqual) : xs) = error (show (pretty l <> ": unexpected >=, maybe need to add paranthesis"))
scanLess [expToPat-> Just pat] [lt@(extract -> Exp.LessThan)] xxs@((extract -> Exp.Part e) : op@(extract -> Exp.GreaterThan) : xs) =
  buildAttribute pat (lt : xxs)
scanLess [] [] [extract -> Exp.Part e1, extract -> Exp.Part (extract -> Exp.Brace e2)] =
  Exp.Inst <$> duplicate e1 <.> duplicate e2
scanLess [] [] [extract -> Exp.Part e1, extract -> Exp.Part (extract -> Exp.BracketInvoke (extract -> Exp.Brace e2) e3)] =
  Exp.BracketInvoke <$> duplicate (Exp.Inst <$> duplicate e1 <.> duplicate e2) <.> duplicate e3
scanLess [] [] [extract -> Exp.Part e1, extract -> Exp.Part (extract -> Exp.ParenInvoke (extract -> Exp.Brace e2) e3)] =
  Exp.ParenInvoke <$> duplicate (Exp.Inst <$> duplicate e1 <.> duplicate e2) <.> duplicate e3
scanLess [] [] [extract -> Exp.Part e1, extract -> Exp.Part (extract -> Exp.ParenInvoke (extract -> Exp.Brace e2) e3)] =
  Exp.ParenInvoke <$> duplicate (Exp.Inst <$> duplicate e1 <.> duplicate e2) <.> duplicate e3
scanLess [] [] [extract -> Exp.Part (expToPat -> Just p1), extract -> Exp.Part (extract -> Exp.Pat (Pat.PrefixColon e2))] =
  Exp.Pat <$> (Pat.InfixColon <$> duplicate p1 <.> duplicate e2)
scanLess [] [] ((extract -> Exp.Part e1) : (extract -> Exp.Part (extract -> Exp.Brace e2)) : xs) =
  scanLess [] [] ((Exp.Part <$> duplicate (Exp.Inst <$> duplicate e1 <.> duplicate e2)) : xs)
scanLess [] [] (L l e: xs) = error (show (pretty l <> ": scanLess[] [] can not parse \ne=" <+> pretty e <+> "\nxs =") ++ show xs)
scanLess es ops (_exp@(L l e): xs) = error (show (pretty l <> ": scanLess can not parse \nes = " <+> pretty es <> "\ne=" <+> pretty e <+> "\nxs =" <+> pretty xs <+> "\nops =" <+> pretty ops))
scanLess es ops [] = error ("Can not parse expression with < and >")

buildCompare :: [L (Exp L a)] -> [L (AttributePart L a)] -> L (Exp L a)
buildCompare [e] [] = e
buildCompare (e2:e1:es) (op:ops) = buildCompare (apply e1 (extract op) e2:es) ops

buildAttribute :: (Show a, Pretty a) => L (Pat L a) -> [L (AttributePart L a)] -> L (Exp L a)
buildAttribute pat  [] = Exp.Pat <$> pat
buildAttribute pat (rp@(L _ Exp.LessThan) : L _ (Exp.Part e) : lp@(L _ Exp.GreaterThan) : xs) =
  buildAttribute (Exp.Spec <$> duplicate pat <.> duplicate e <. lp) xs
buildAttribute pat ((extract -> Exp.Part (extract -> Exp.Pat (Pat.PrefixColon e))) : xs) =
  buildAttribute (Pat.InfixColon <$> duplicate pat <.> duplicate e) xs
buildAttribute pat ((extract -> Exp.Part (extract -> Exp.ExpInfixColon (extract -> Exp.Paren args) e)) : xs) =
  buildAttribute (Pat.InfixColon <$> duplicate (Pat.Invoke <$> duplicate pat <.> duplicate args) <.> duplicate e) xs
buildAttribute pat ((extract ->  Exp.Part lp@(extract -> Exp.Paren list)) : xs) =
  buildAttribute (Pat.Invoke <$> duplicate pat <.> duplicate list <. lp) xs
buildAttribute pat [extract ->  Exp.Part lp@(extract -> Exp.Inst (extract -> Exp.Paren args) body)] =
 Exp.Inst <$> duplicate (Exp.Pat <$> (Pat.Invoke <$> duplicate pat <.> duplicate args)) <.> duplicate body
buildAttribute pat [extract ->  Exp.Part lp@(extract -> Exp.Brace es)] =
  Exp.Inst <$> duplicate (Exp.Pat <$> pat) <.> duplicate es
buildAttribute exp xs = error $ show ( pretty (loc exp) <> ":buildAttribute exp =" <+> pretty exp <+> ", xs =") ++ show xs

apply :: L (Exp L a) -> AttributePart L a -> L (Exp L a) -> L (Exp L a)
apply e1 Exp.LessThan e2 = (:<:) <$> duplicate e1 <.> duplicate e2
apply e1 Exp.LessEqual e2 = (:<=:) <$> duplicate e1 <.> duplicate e2
apply e1 Exp.GreaterEqual e2 = (:>=:) <$> duplicate e1 <.> duplicate e2
apply e1 Exp.GreaterThan e2 = (:>:) <$> duplicate e1 <.> duplicate e2

}
