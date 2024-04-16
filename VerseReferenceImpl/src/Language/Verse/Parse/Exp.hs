{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Parse.Exp
  ( Exp (..)
  , Pat (..)
  , IdentExp (..)
  , Path (..)
  , expToPat
  ) where

import Control.Applicative
import Control.Comonad

import Data.Char
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid (mempty)
import Data.Text (Text)
import Data.Traversable (traverse)

import Language.Verse.Loc (L (..))

import Numeric (showHex)

import Prelude (Double, Integer, error, foldr, show, (++))

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

data Exp a
  = L (Exp a) :=: L (Exp a)
  | L (Exp a) :<>: L (Exp a)
  | L (Exp a) :|: L (Exp a)
  | L (Exp a) :.: L (IdentExp a)
  | L (Exp a) :..: L (Exp a)
  | L (Exp a) :<: L (Exp a)
  | L (Exp a) :<=: L (Exp a)
  | L (Exp a) :>: L (Exp a)
  | L (Exp a) :*: L (Exp a)
  | L (Exp a) :+: L (Exp a)
  | L (Exp a) :-: L (Exp a)
  | L (Exp a) :->: L (Exp a)
  | L (Exp a) :/: L (Exp a)
  | L (Exp a) :>=: L (Exp a)
  | All (L (Exp a))
  | And (L (Exp a)) (L (Exp a))
  | Array [L (Exp a)]
  | Block (L (Exp a))
  | Brace (L (Exp a))
  | BracketInvoke (L (Exp a)) (L (Exp a))
  | Break
  | Catch (L (Exp a)) (L (Exp a))
  | Char {-# UNPACK #-} !Char
  | Char32 {-# UNPACK #-} !Char
  | Class (Maybe (L (Exp a))) (L (Exp a))
  | Continue
  | Do (L (Exp a)) (L (Exp a))
  | Enum [L (Exp a)] [([L (Exp a)], L a)]  -- expression lists are for attributes on enum and names
  | Exists (L a)
  | ExpInfixColon (L (Exp a)) (L (Exp a))
  | Fail
  | Fails (L (Exp a))
  | False
  | Float {-# UNPACK #-} !Double
  | Units (L (Exp a)) (L a)     -- Need type for literals since they are the only that can have units
  | For (L (Exp a))
  | ForDo (L (Exp a)) (L (Exp a))
  | Forall (L a)
  | Lam (L (Exp a)) (L (Exp a))
  | If (L (Exp a))
  | IfElse (L (Exp a)) (L (Exp a))
  | IfThen (L (Exp a)) (L (Exp a))
  | IfThenElse (L (Exp a)) (L (Exp a)) (L (Exp a))
  | InfixColonEqual (L (Exp a)) (L (Exp a))
  | InfixDivideEqual (L (Exp a)) (L (Exp a))
  | InfixMinusEqual (L (Exp a)) (L (Exp a))
  | InfixMultiplyEqual (L (Exp a)) (L (Exp a))
  | InfixPlusEqual (L (Exp a)) (L (Exp a))
  | Inst (L (Exp a)) (L (Exp a))
  | Int !Integer
  | List [L (Exp a)]
  | Module (L (Exp a))
  | Not (L (Exp a))
  | One (L (Exp a))
  | Option (L (Exp a))
  | Or (L (Exp a)) (L (Exp a))
  | Paren (L (Exp a))
  | ParenInvoke (L (Exp a)) (L (Exp a))
  | Pat (Pat a)
  | PostfixCaret (L (Exp a))
  | PostfixQuery (L (Exp a))
  | PrefixBracket [L (Exp a)] (L (Exp a))
  | PrefixCaret (L (Exp a))
  | PrefixMinus (L (Exp a))
  | PrefixMultiply (L (Exp a))
  | PrefixPlus (L (Exp a))
  | PrefixQuery (L (Exp a))
  | PrefixAmpersand (L (Exp a))
  | PrefixDotDot (L (Exp a))
  | Return (Maybe (L (Exp a)))
  | ExpVar (L (Exp a))
  | ExpSet (L (Exp a))
  | ExpRef (L (Exp a))
  | ExpAlias (L (Exp a))
  | Set (L (Exp a)) (L (Exp a))
  | SetInfixDivideEqual (L (Exp a)) (L (Exp a))
  | SetInfixMinusEqual (L (Exp a)) (L (Exp a))
  | SetInfixMultiplyEqual (L (Exp a)) (L (Exp a))
  | SetInfixPlusEqual (L (Exp a)) (L (Exp a))
  | ExpSpecs (L (Exp a)) [L (Exp a)]
  | AtSpec (L (Exp a)) (L (Exp a))     -- @attribute e
  | SpecAt (L (Exp a)) (L (Exp a))     -- e @attribute
  | String !Text [(L (Exp a), L Text)] -- the list is for the more complicated strings, e.g., "abc{ whatever }def"
  | Struct (L (Exp a))
  | True
  | Truth (L (Exp a))
  | Tuple [L (Exp a)]
  | Until (L (Exp a)) (L (Exp a))
  | Yield
  | Next (L (Exp a)) (L (Exp a))
  | Over (L (Exp a)) (L (Exp a))
  | When (L (Exp a)) (L (Exp a))
  | While (L (Exp a)) (L (Exp a))
  | L (Exp a) `Where` L (Exp a)
  | L (Exp a) `Is` L (Exp a)

deriving instance ( Show a
                  , Show (IdentExp a)
                  , Show (Pat a)
                  ) => Show (Exp a)

data Pat a
  = Name (IdentExp a)
  | Var [L (Exp a)] (L (IdentExp a)) [L (Exp a)]   -- expression lists are for attributes, can be both after "var" and after identifier
  | PrefixColon (L (Exp a))
  | InfixColon (L (Pat a)) (L (Exp a))
  | InfixArrow (L (Pat a)) (L (Pat a))
  | Invoke (L (Pat a)) (L (Exp a))
  | Specs (L (Pat a)) [L (Exp a)]
  | Extension (L (Exp a)) (L (Pat a)) -- The lhs is always a name

deriving instance ( Show a
                  , Show (Exp a)
                  , Show (IdentExp a)
                  , Show (Pat a)
                  ) => Show (Pat a)

data IdentExp a
 = IdentName a
 | IdentQualName [L (Exp a)] (L a)
 | IdentPath (Path a)

deriving instance ( Show a
                  , Show (Exp a)
                  ) => Show (IdentExp a)

data Path a = Path (L a) [(Maybe (Path a), L a)] deriving Show

instance ( Pretty a
         , Pretty (Exp a)
         , Pretty (IdentExp a)
         , Pretty (Pat a)
         , Show a
         , Show (Exp a)
         , Show (IdentExp a)
         , Show (Pat a)
         ) => Pretty (Exp a) where
  pretty = \ case
    e1 :=: e2 -> pretty e1 <+> equals <+> pretty e2
    e1 :<>: e2 -> parens (pretty e1 <+> "<>" <+> pretty e2)
    e1 :|: e2 -> parens (pretty e1 <+> pipe <+> pretty e2)
    e :.: x -> pretty e <+> dot <> pretty x
    e :..: x -> pretty e <> ".." <> pretty x
    e :<: x -> parens (pretty e <+> "<" <+> pretty x)
    e :<=: x -> parens (pretty e <+> "<=" <+> pretty x)
    e :>: x -> parens (pretty e <+> ">" <+> pretty x)
    e :>=: x -> parens (pretty e <+> ">=" <+> pretty x)
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
    Char32 x -> "0u" <> pretty (showHex (ord x) "")
    Int x -> pretty x
    Float x -> pretty x
    Units e u -> pretty e <> pretty u
    String s [] -> "\"" <> pretty s <> "\"" -- FIXME add escape
    String s (x:xs) -> "\"" <> pretty s <> "{" <+> stringCont x xs
    Lam e1 e2 -> parens (pretty e1) <+> "=>" <+> braces (pretty e2)
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
    Next e1 e2 -> pretty e1 <+> "next" <+> pretty e2
    Over e1 e2 -> pretty e1 <+> "over" <+> braces (pretty e2)
    When e1 e2 -> pretty e1 <+> "when" <+> braces (pretty e2)
    While e1 e2 -> pretty e1 <+> "while" <+> braces (pretty e2)
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

instance ( Pretty a
         , Pretty (Exp a)
         , Pretty (IdentExp a)
         , Pretty (Pat a)
         , Show a
         , Show (Exp a)
         , Show (IdentExp a)
         , Show (Pat a)
         ) => Pretty (Pat a) where
  pretty = \ case
    Name ident -> pretty ident
    Var es1 x es2 -> "var" <> specs es1 <+> pretty x <> specs es2
    PrefixColon e -> parens(colon <> pretty e)
    InfixColon p e -> parens (pretty p <> colon <> pretty e)
    InfixArrow p1 p2 -> parens( pretty p1 <+> "->" <+> pretty p2)
    Invoke p e1 -> pretty p <> parens (pretty e1)
    Specs p ss -> pretty p <> specs ss
    Extension e p -> parens (pretty e) <> "." <> pretty p

braces :: Doc a -> Doc a
braces x =
  nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
  flatAlt (hardline <> rbrace) " }"

specs :: (Pretty a) => [a] -> Doc ann
specs = foldr ( \ e doc -> "<" <> pretty e <> ">" <> doc ) mempty

instance ( Pretty a
         , Pretty (Exp a)
         , Pretty (Pat a)
         , Show a
         , Show (Exp a)
         , Show (Pat a)
         ) => Pretty (IdentExp a) where
  pretty = \ case
    IdentName x -> pretty x
    IdentQualName es x -> "(" <> list es <> ":)" <> pretty x
    IdentPath x -> pretty x

list :: Pretty a => [a] -> Doc ann
list [] = mempty
list [x] = pretty x
list (x:xs) = pretty x <> ";" <+> list xs

instance ( Pretty a
         ) => Pretty (Path a) where
  pretty (Path label pathIdents) = "/" <> pretty label <> foldr prettyPath mempty pathIdents
   where
    prettyPath (Nothing, ident) doc = "/" <> pretty (extract ident) <> doc
    prettyPath (Just path, ident) doc = "/(" <> pretty path <> ":)" <> pretty (extract ident) <> doc

expToPat :: L (Exp a) -> Maybe (L (Pat a))
expToPat = traverse $ \ case
  Pat p -> Just p
  Paren e -> extract <$> expToPat e
  List [e] -> extract <$> expToPat e
  ParenInvoke e1 e2 -> expToPat e1 <&> \ p1 -> Invoke p1 e2
  ExpInfixColon e1 e2 -> expToPat e1 <&> \ p1 -> InfixColon p1 e2
  ExpVar (expToPat -> Just p@(extract -> Name x)) -> Just . (\ x -> Var [] x []) $ x <$ p
  ExpVar (expToPat -> Just (extract -> Specs p@(extract -> Name x) e2)) -> Just . (\ x -> Var [] x e2) $ x <$ p
  ExpSpecs e es -> expToPat e <&> \ p -> Specs p es
  e1 :->: e2 -> InfixArrow <$> expToPat e1 <*> expToPat e2
  _ -> Nothing
