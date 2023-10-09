{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Parse.Exp
  ( Exp (..)
  , Pat (..)
  , CatchUntil(..)
  ) where

import Language.Verse.Name
import Prettyprinter (Pretty(..), Doc, (<+>), vcat, parens, brackets, encloseSep, flatAlt, nest, hardline, group, equals, pipe, dot, colon, lparen, rparen, lbrace, rbrace)
import Prelude hiding(True, False)

data Exp f a
  = f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :<>: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !Name
  | f (Exp f a) :..: f (Exp f a)
  | f (Exp f a) :<: f (Exp f a)
  | f (Exp f a) :<=: f (Exp f a)
  | f (Exp f a) :>: f (Exp f a)
  | f (Exp f a) :>=: f (Exp f a)
  | PrefixPlus (f (Exp f a))
  | f (Exp f a) :+: f (Exp f a)
  | PrefixMinus (f (Exp f a))
  | f (Exp f a) :-: f (Exp f a)
  | f (Exp f a) :*: f (Exp f a)
  | f (Exp f a) :/: f (Exp f a)
  | f (Exp f a) :->: f (Exp f a)
  | List [f (Exp f a)]
  | f (Exp f a) `Where` f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | And (f (Exp f a)) (f (Exp f a))
  | Not (f (Exp f a))
  | PrefixBracket (f (Exp f a))
  | PrefixQuery (f (Exp f a))
  | PostfixQuery (f (Exp f a))
  | PrefixCaret (f (Exp f a))
  | PostfixCaret (f (Exp f a))
  | Module (f (Exp f a))
  | Struct (f (Exp f a))
  | Class (Maybe (f (Exp f a))) (f (Exp f a))
  | Enum [a]
  | Inst (f (Exp f a)) (f (Exp f a))
  | InstDo (f (Exp f a)) (f (Exp f a)) (Maybe (f (Exp f a))) (Maybe (f (CatchUntil f a))) -- Do and Until at the end
  | If (f (Exp f a))
  | IfThen (f (Exp f a)) (f (Exp f a))
  | IfElse (f (Exp f a)) (f (Exp f a))
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | For (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Block (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Exists (f a)
  | Set (f (Pat f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Option (f (Exp f a))
  | Or (f (Exp f a)) (f (Exp f a))
  | True
  | False
  | Char {-# UNPACK #-} !Char
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | String !String [(f (Exp f a), f String)] -- the list is for the more complicated strings, e.g., "abc{ whatever }def"
  | Fun (f (Exp f a)) (f (Exp f a))
  | InfixColonEqual (f (Pat f a)) (f (Exp f a))
  | InfixPlusEqual (f (Pat f a)) (f (Exp f a))
  | InfixMinusEqual (f (Pat f a)) (f (Exp f a))
  | InfixMultiplyEqual (f (Pat f a)) (f (Exp f a))
  | InfixDivideEqual (f (Pat f a)) (f (Exp f a))
  | Pat (Pat f a)

deriving instance ( Show (f String)
                  , Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f (CatchUntil f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

data Pat f a
  = Name a
  | QualName [(f (Exp f a))] a
  | Var (f a) (f (Exp f a))
  | Var0 (f a)
  | PrefixColon (f (Exp f a))
  | InfixColon (f (Pat f a)) (f (Exp f a))
  | InfixArrow (f (Pat f a)) (f (Pat f a))
  | Invoke (f (Pat f a)) (f (Exp f a))
  | InvokeDo (f (Pat f a)) (f (Exp f a)) (Maybe (f (Exp f a))) (Maybe (f (CatchUntil f a))) -- Do and Until at the end

deriving instance ( Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f (CatchUntil f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Pat f a)


data CatchUntil f a
  = Catch (f (Exp f a))
  | Until (f (Exp f a))

deriving instance ( Show (f (Exp f a))
                  ) => Show (CatchUntil f a)


instance ( Pretty (f String)
         , Pretty (f (Pat f a))
         , Pretty (f (Exp f a))
         , Pretty (f (CatchUntil f a))
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Exp f a) where
  pretty = \ case
    e1 :=: e2 -> pretty e1 <+> equals <+> pretty e2
    e1 :<>: e2 -> pretty e1 <+> "<>" <+> pretty e2
    e1 :|: e2 -> pretty e1 <+> pipe <+> pretty e2
    e :.: x -> pretty e <> dot <> pretty x
    e :..: x -> pretty e <> ".." <> pretty x
    e :<: x -> pretty e <+> "<" <+> pretty x
    e :<=: x -> pretty e <+> "<=" <+> pretty x
    e :>: x -> pretty e <+> ">" <+> pretty x
    e :>=: x -> pretty e <+> ">=" <+> pretty x
    PrefixPlus x -> "+" <+> pretty x
    e :+: x -> pretty e <+> "+" <+> pretty x
    PrefixMinus x -> "-" <+> pretty x
    e :-: x -> pretty e <+> "-" <+> pretty x
    e :*: x -> pretty e <+> "*" <+> pretty x
    e :/: x -> pretty e <+> "/" <+> pretty x
    e :->: x -> pretty e <+> "->" <+> pretty x
    List es -> vcat $ pretty <$> es
    e1 `Where` e2 -> pretty e1 <+> "where" <+> pretty e2
    Fail -> "fail"
    One e -> "one" <+> braces (pretty e)
    All e -> "all" <+> braces (pretty e)
    And e1 e2 -> pretty e1 <+> "and" <+> pretty e2
    Or e1 e2 -> pretty e1 <+> "or" <+> pretty e2
    Not e -> "not" <+> parens (pretty e)
    PrefixBracket e -> "[]" <> pretty e
    PrefixQuery e -> "?" <> pretty e
    PostfixQuery e -> parens (pretty e) <> pretty '?'
    PrefixCaret e -> "^" <> pretty e
    PostfixCaret e -> parens (pretty e) <> pretty '^'
    Module e -> "module" <> braces (pretty e)
    Struct e -> "struct" <> braces (pretty e)
    Class e1 e2 ->
      "class" <>
      maybe mempty (parens . pretty) e1 <+>
      braces (pretty e2)
    Enum e -> "enum" <> braces (pretty e) -- FIXME should use ,
    Inst e1 e2 -> parens (pretty e1) <> braces (pretty e2)
    InstDo e1 e2 e3 e4 -> parens (pretty e1) <> braces (pretty e2) <+> maybe "" (\ e -> "do" <> braces (pretty e)) e3 <+> maybe "" pretty e4
    If e1 -> "if" <> parens (pretty e1)
    IfThen e1 e2 -> "if" <> parens (pretty e1) <> braces (pretty e2)
    IfElse e1 e3 -> "if" <> parens (pretty e1) <> "{}" <+> "else" <> braces (pretty e3)
    IfThenElse e1 e2 e3 -> "if" <> parens (pretty e1) <> braces (pretty e2) <+> "else" <> braces (pretty e3)
    For e1 -> "for" <> parens (pretty e1)
    ForDo e1 e2 -> "for" <> parens (pretty e1) <> braces (pretty e2)
    Block e -> braces (pretty e)
    ParenInvoke e1 e2 -> pretty e1 <> parens (pretty e2)
    BracketInvoke e1 e2 -> pretty e1 <> brackets (pretty e2)
    Exists x -> "exists" <+> pretty x
    Set x e -> "set" <+> pretty x <+> equals <+> pretty e
    Tuple es -> tupled $ pretty <$> es
    True -> "true"
    False -> "false"
    Char x -> "'" <> pretty x <> "'"  -- FIXME add escape
    Int x -> pretty x
    Float x -> pretty x
    String s [] -> "\"" <> pretty s <> "\"" -- FIXME add escape
    String s (x:xs) -> "\"" <> pretty s <> "{" <+> stringCont x xs
    Fun e1 e2 -> "fun" <> parens (pretty e1) <+> braces (pretty e2)
    InfixColonEqual p e -> pretty p <+> ":=" <+> pretty e
    InfixPlusEqual p e -> pretty p <+> "+=" <+> pretty e
    InfixMinusEqual p e -> pretty p <+> "-=" <+> pretty e
    InfixMultiplyEqual p e -> pretty p <+> "*=" <+> pretty e
    InfixDivideEqual p e -> pretty p <+> "/=" <+> pretty e
    Pat p -> pretty p
    _ -> "unimplemented"
    where
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "

      stringCont (e, s) [] = pretty e <+> "}" <> pretty s <> "\"" -- FIXME add escape
      stringCont (e, s) (x:xs) = pretty e <+> "}" <> pretty s <> "{" <+> stringCont x xs -- FIXME add escape



instance ( Pretty (f (Pat f a))
         , Pretty (f (Exp f a))
         , Pretty (f (CatchUntil f a))
         , Pretty (f a)
         , Pretty a
         ) => Pretty (Pat f a) where
  pretty = \ case
    Name x -> pretty x
    QualName es x -> "(" <> list es <> ":)" <> pretty x
    Var x e -> "var" <+> pretty x <+> ":=" <+> pretty e
    Var0 x -> "var" <+> pretty x
    PrefixColon e -> colon <> pretty e
    InfixColon p e -> pretty p <> colon <> pretty e
    InfixArrow p1 p2 -> pretty p1 <> colon <> pretty p2
    Invoke p e1 -> pretty p <> parens (pretty e1)
    InvokeDo p e1 e2 e3 -> pretty p <> parens (pretty e1) <+> maybe "" (\ e -> "do" <> braces (pretty e)) e2 <+> maybe "" pretty e3
    where
      list [] = ""
      list (x:[]) = pretty x
      list (x:xs) = pretty x <> ";" <+> list xs


instance ( Pretty (f (Exp f a))
         ) => Pretty (CatchUntil f a) where
  pretty = \ case
    Catch e -> "catch" <+> pretty e
    Until e -> "until" <> braces (pretty e)



braces :: Doc a -> Doc a
braces x =
  nest 2 (flatAlt (lbrace <> hardline) "{ " <> x) <>
  flatAlt (hardline <> rbrace) " }"
