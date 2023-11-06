{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Parse.Exp
  ( Exp (..)
  , Pat (..)
  , AttributePart(..)
  , expToPat
  ) where

import Control.Comonad
import Data.Char
import Data.Function
import Data.Functor.Apply
import Data.Maybe
import Data.Monoid (mempty)
import Data.Text(Text)

import Prelude (Double, Integer, foldr, error, (++), show)
import Prettyprinter ( Pretty (..)
                     , Doc
                     , (<>)
                     , (<+>)
                     , brackets
                     , colon
                     , dot
                     , encloseSep
                     , equals
                     , flatAlt
                     , group
                     , hardline
                     , lbrace
                     , lparen
                     , nest
                     , parens
                     , pipe
                     , rbrace
                     , rparen
                     , vcat
                     )

import Text.Show (Show)

data Exp f a
  = f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :<>: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | f (Exp f a) :.: ([f (Exp f a)], a) -- list is for qualifications
  | f (Exp f a) :..: f (Exp f a)
  | f (Exp f a) :<: f (Exp f a)
  | f (Exp f a) :<=: f (Exp f a)
  | f (Exp f a) :>: f (Exp f a)
  | f (Exp f a) :*: f (Exp f a)
  | f (Exp f a) :+: f (Exp f a)
  | f (Exp f a) :-: f (Exp f a)
  | f (Exp f a) :->: f (Exp f a)
  | f (Exp f a) :/: f (Exp f a)
  | f (Exp f a) :>=: f (Exp f a)
  | All (f (Exp f a))
  | And (f (Exp f a)) (f (Exp f a))
  | Array [f (Exp f a)]
  | AttributeParts [f (AttributePart f a)]
  | Block (f (Exp f a))
  | Brace (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Break
  | Catch (f (Exp f a)) (f (Exp f a))
  | Char {-# UNPACK #-} !Char
  | Class (Maybe (f (Exp f a))) (f (Exp f a))
  | Continue
  | Do (f (Exp f a)) (f (Exp f a))
  | Enum [f (Exp f a)] [([f (Exp f a)], f a)]  -- expression lists are for attributes on enum and names
  | Exists (f a)
  | ExpInfixColon (f (Exp f a)) (f (Exp f a))
  | Fail
  | Fails (f (Exp f a))
  | False
  | Float {-# UNPACK #-} !Double
  | Units (f (Exp f a)) (f a)     -- Need type for literals since they are the only that can have units
  | For (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Forall (f a)
  | Fun (f (Exp f a)) (f (Exp f a))
  | If (f (Exp f a))
  | IfElse (f (Exp f a)) (f (Exp f a))
  | IfThen (f (Exp f a)) (f (Exp f a))
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | InfixColonEqual (f (Exp f a)) (f (Exp f a))
  | InfixDivideEqual (f (Exp f a)) (f (Exp f a))
  | InfixMinusEqual (f (Exp f a)) (f (Exp f a))
  | InfixMultiplyEqual (f (Exp f a)) (f (Exp f a))
  | InfixPlusEqual (f (Exp f a)) (f (Exp f a))
  | Inst (f (Exp f a)) (f (Exp f a))
  | Int !Integer
  | List [f (Exp f a)]
  | Module (f (Exp f a))
  | Not (f (Exp f a))
  | One (f (Exp f a))
  | Option (f (Exp f a))
  | Or (f (Exp f a)) (f (Exp f a))
  | Paren (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | Pat (Pat f a)
  | PostfixCaret (f (Exp f a))
  | PostfixQuery (f (Exp f a))
  | PrefixBracket [f (Exp f a)] (f (Exp f a))
  | PrefixCaret (f (Exp f a))
  | PrefixMinus (f (Exp f a))
  | PrefixMultiply (f (Exp f a))
  | PrefixPlus (f (Exp f a))
  | PrefixQuery (f (Exp f a))
  | PrefixAmpersand (f (Exp f a))
  | PrefixDotDot (f (Exp f a))
  | Return (Maybe (f (Exp f a)))
  | ExpVar (f (Exp f a))
  | ExpSet (f (Exp f a))
  | ExpRef (f (Exp f a))
  | ExpAlias (f (Exp f a))
  | Set (f (Exp f a)) (f (Exp f a))
  | SetInfixDivideEqual (f (Exp f a)) (f (Exp f a))
  | SetInfixMinusEqual (f (Exp f a)) (f (Exp f a))
  | SetInfixMultiplyEqual (f (Exp f a)) (f (Exp f a))
  | SetInfixPlusEqual (f (Exp f a)) (f (Exp f a))
  | ExpSpecs (f (Exp f a)) [f (Exp f a)]
  | AtSpec (f (Exp f a)) (f (Exp f a))     -- @attribute e
  | SpecAt (f (Exp f a)) (f (Exp f a))     -- e @attribute
  | String !Text [(f (Exp f a), f Text)] -- the list is for the more complicated strings, e.g., "abc{ whatever }def"
  | Struct (f (Exp f a))
  | True
  | Truth (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Until (f (Exp f a)) (f (Exp f a))
  | Yield
  | f (Exp f a) `Where` f (Exp f a)
  | f (Exp f a) `Is` f (Exp f a)

deriving instance ( Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f Text)
                  , Show (f (AttributePart f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

data Pat f a
  = Name [(f (Exp f a))] a -- list is for qualification
  | Path a
  | Var [f (Exp f a)] (f a)   -- expression list is for attributes
  | PrefixColon (f (Exp f a))
  | InfixColon (f (Pat f a)) (f (Exp f a))
  | InfixArrow (f (Pat f a)) (f (Pat f a))
  | Invoke (f (Pat f a)) (f (Exp f a))
  | Specs (f (Pat f a)) [f (Exp f a)]
  | Extension (f (Exp f a)) (f (Pat f a)) -- The lhs is always a name
  | Hack (Exp f a)

deriving instance ( Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f a)
                  , Show (f Text)
                  , Show (f (AttributePart f a))
                  , Show a
                  ) => Show (Pat f a)


data AttributePart f a
  = LessThan
  | LessEqual
  | GreaterEqual
  | GreaterThan
  | Part (f (Exp f a))

deriving instance ( Show (f (Exp f a))
                  ) => Show (AttributePart f a)



instance ( Pretty (f Text)
         , Pretty (f (Pat f a))
         , Pretty (f (Exp f a))
         , Pretty (f (AttributePart f a))
         , Pretty (f a)
         , Pretty a
         , Show (f (AttributePart f a))
         , Show (f (Exp f a))
         , Show (f (Pat f a))
         , Show (f Text)
         , Show (f a)
         , Show a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :=: e2 -> pretty e1 <+> equals <+> pretty e2
    e1 :<>: e2 -> parens (pretty e1 <+> "<>" <+> pretty e2)
    e1 :|: e2 -> parens (pretty e1 <+> pipe <+> pretty e2)
    e :.: ([],x) -> pretty e <+> dot <> pretty x
    e :.: (es,x) -> pretty e <+> dot <> "(" <> list es <> ":)" <> pretty x
    e :..: x -> pretty e <> ".." <> pretty x
    e :<: x -> parens (pretty e <+> "<" <+> pretty x)
    e :<=: x -> parens (pretty e <+> "<=" <+> pretty x)
    e :>: x -> parens (pretty e <+> ">" <+> pretty x)
    e :>=: x -> parens (pretty e <+> ">=" <+> pretty x)
    AttributeParts parts -> pretty parts
    Paren e -> lparen <> pretty e <> rparen
    Brace e -> lbrace <> pretty e <> rbrace
    PrefixPlus x -> "+" <+> pretty x
    e :+: x -> pretty e <+> "+" <+> pretty x
    PrefixMinus x -> "-" <+> pretty x
    e :-: x -> pretty e <+> "-" <+> pretty x
    PrefixMultiply x -> "*" <+> pretty x
    e :*: x -> pretty e <+> "*" <+> pretty x
    e :/: x -> pretty e <+> "/" <+> pretty x
    e :->: x -> pretty e <+> "->" <+> pretty x
    List es -> vcat $ pretty <$> es
    e1 `Where` e2 -> pretty e1 <+> "where" <> lbrace <+> pretty e2 <+> rbrace
    e1 `Is` e2 -> pretty e1 <+> "is" <> lbrace <+> pretty e2 <+> rbrace
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    And e1 e2 -> pretty e1 <+> "and" <+> pretty e2
    Or e1 e2 -> pretty e1 <+> "or" <+> pretty e2
    Return (Just e) -> "return" <+> parens (pretty e)
    Return Nothing -> "return"
    ExpVar e -> "var" <+> pretty e
    ExpSet e -> "set" <+> pretty e
    ExpRef e -> "ref" <+> pretty e
    ExpAlias e -> "alias" <+> pretty e
    Not e -> "not" <+> parens (pretty e)
    PrefixBracket e1 e2 -> "[" <> pretty e1 <> "]" <> pretty e2
    PrefixQuery e -> "?" <> pretty e
    PrefixAmpersand e -> "&" <> pretty e
    PrefixDotDot e -> ".." <> pretty e
    PostfixQuery e -> parens (pretty e) <> pretty '?'
    PrefixCaret e -> "^" <> pretty e
    PostfixCaret e -> parens (pretty e) <> pretty '^'
    Array es -> "array" <> lbrace <> pretty es <> rbrace
    Module e -> "module" <> braces (pretty e)
    Struct e -> "struct" <> braces (pretty e)
    Class e1 e2 ->
      "class" <>
      maybe mempty (parens . pretty) e1 <+>
      braces (pretty e2)
    Enum es e -> "enum" <> specs es <> braces (vcat $ atNames <$> e) -- FIXME should use ,
    Inst e1 e2 -> pretty e1 <> braces (pretty e2)
    If e1 ->
      "if" <+> parens (pretty e1)
    IfThen e1 e2 ->
      "if" <+> parens (pretty e1) <> braces (pretty e2)
    IfElse e1 e3 ->
      "if" <+> braces (pretty e1) <+>
      "else" <+> braces (pretty e3)
    IfThenElse e1 e2 e3 ->
      "if" <+> parens (pretty e1) <+>
      braces (pretty e2) <+>
      "else" <+> braces (pretty e3)
    For e1 -> "for" <> parens (pretty e1)
    ForDo e1 e2 -> "for" <> parens (pretty e1) <> braces (pretty e2)
    Forall n -> "forall" <+> pretty n
    Block e -> braces (pretty e)
    ParenInvoke e1 e2 -> pretty e1 <> parens (pretty e2)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Exists x -> "exists" <+> pretty x
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    SetInfixPlusEqual p e -> "set" <+> pretty p <+> "+=" <+> pretty e
    SetInfixMinusEqual p e -> "set" <+> pretty p <+> "-=" <+> pretty e
    SetInfixMultiplyEqual p e -> "set" <+> pretty p <+> "*=" <+> pretty e
    SetInfixDivideEqual p e -> "set" <+> pretty p <+> "/=" <+> pretty e
    Tuple es -> tupled $ pretty <$> es
    True -> "true"
    False -> "false"
    Char x -> "'" <> pretty x <> "'"  -- FIXME add escape
    Int x -> pretty x
    Float x -> pretty x
    Units e u -> pretty e <> pretty u
    String s [] -> "\"" <> pretty s <> "\"" -- FIXME add escape
    String s (x:xs) -> "\"" <> pretty s <> "{" <+> stringCont x xs
    Fun e1 e2 -> parens (pretty e1) <+> "=>" <+> braces (pretty e2)
    InfixColonEqual p e -> pretty p <+> ":=" <+> pretty e
    InfixPlusEqual p e -> pretty p <+> "+=" <+> pretty e
    InfixMinusEqual p e -> pretty p <+> "-=" <+> pretty e
    InfixMultiplyEqual p e -> pretty p <+> "*=" <+> pretty e
    InfixDivideEqual p e -> pretty p <+> "/=" <+> pretty e
    ExpInfixColon p e -> parens(pretty p <+> ":" <+> pretty e)
    Do p e -> pretty p <+> "do" <> braces (pretty e)
    Until p e -> pretty p <+> "until" <> braces (pretty e)
    Catch p e -> pretty p <+> "catch" <+> pretty e
    ExpSpecs e ss -> pretty e <> specs ss
    AtSpec e1 e2 -> "@" <> pretty e1 <+> pretty e2
    SpecAt e1 e2 -> pretty e1 <+> "@" <> pretty e2
    Yield -> "yield"
    Continue -> "continue"
    Break -> "break"
    Pat p -> pretty p
    x -> error ("No pretty print for " ++ show x)
    where
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      stringCont (e, s) [] =
        pretty e <+> "}" <> pretty s <> "\"" -- FIXME add escape
      stringCont (e, s) (x:xs) =
        pretty e <+> "}" <> pretty s <> "{" <+> stringCont x xs -- FIXME add escape

      atNames ([],e) = pretty e
      atNames (ats, e) = vcat $ (prettyAt <$> ats) ++ [pretty e]

      prettyAt a = "@" <> pretty a




instance ( Pretty (f (Pat f a))
         , Pretty (f (Exp f a))
         , Pretty (f a)
         , Pretty (f (AttributePart f a))
         , Pretty (f Text)
         , Pretty a
         , Show (f (AttributePart f a))
         , Show (f (Exp f a))
         , Show (f (Pat f a))
         , Show (f Text)
         , Show (f a)
         , Show a
         ) => Pretty (Pat f a) where
  pretty = \ case
    Name [] x -> pretty x
    Name es x -> "(" <> list es <> ":)" <> pretty x
    Path x -> pretty x
    Var es x -> "var" <+> specs es <> pretty x
    PrefixColon e -> parens(colon <> pretty e)
    InfixColon p e -> parens (pretty p <> colon <> pretty e)
    InfixArrow p1 p2 -> parens( pretty p1 <+> "->" <+> pretty p2)
    Invoke p e1 -> pretty p <> parens (pretty e1)
    Specs p ss -> pretty p <> specs ss
    Extension e p -> parens (pretty e) <> "." <> pretty p
    Hack e -> pretty e

list :: Pretty a => [a] -> Doc ann
list [] = mempty
list (x:[]) = pretty x
list (x:xs) = pretty x <> ";" <+> list xs

braces :: Doc a -> Doc a
braces x =
  nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
  flatAlt (hardline <> rbrace) " }"

specs :: (Pretty a) => [a] -> Doc ann
specs es = foldr ( \ e doc -> "<" <> pretty e <> ">" <> doc ) mempty es

instance ( Pretty (f (Exp f a))
         ) => Pretty (AttributePart f a) where
  pretty = \ case
    LessThan -> "<"
    LessEqual -> "<="
    GreaterEqual -> "<="
    GreaterThan -> ">"
    Part e -> pretty e

expToPat :: (Apply f, Comonad f) => f (Exp f a) -> Maybe (f (Pat f a))
expToPat exp@(extract -> Pat pat) = Just (pat <$ exp)
expToPat _ex@(extract -> Paren e) = expToPat e
expToPat _ex@(extract -> List [e]) = expToPat e
expToPat exp@(extract -> ParenInvoke (expToPat -> Just p1) e2) = Just (Invoke <$> duplicate p1 <.> duplicate e2 <. exp)
expToPat exp@(extract -> ExpInfixColon (expToPat -> Just p1) e2) = Just (InfixColon <$> duplicate p1 <.> duplicate e2 <. exp)
expToPat exp@(extract -> ExpVar (expToPat -> Just p@(extract -> Name qual name))) = Just (Var qual (name <$ p) <$ exp)
expToPat exp@(extract -> (expToPat -> Just p1) :->: (expToPat -> Just p2)) = Just (InfixArrow <$> duplicate p1 <.> duplicate p2 <. exp)
expToPat _exp = Nothing
