{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Core.Parse
  ( parse
  , parse'
  , Result (..)
  ) where

import Control.Applicative

import Data.Functor
import Data.Monoid ((<>))
import Data.Text (Text)

import Prelude
  ( Either
  , ($)
  , (.)
  , (=<<)
  , (>>)
  , (>>=)
  )

import List (last1, reverse2)
import Loc
import Parser
  ( Parser
  , Result (..)
  , chainl1
  , char
  , eof
  , get
  , runParser
  )
import Parser qualified
import Pos

import Verse.Core.Exp
  ( ExpF
  , LExp
  , pattern (:&)
  , pattern (:=)
  , pattern (:<)
  , pattern (:|)
  , pattern (:..)
  , pattern (:+)
  , pattern (:-)
  )
import Verse.Core.Exp qualified as Exp
import Verse.Token

parse :: Text -> Either Pos LExp
parse = Parser.parse $ spaces *> exp <* eof

parse' :: Text -> Result LExp
parse' = runParser $ spaces *> exp <* eof

exp :: Parser LExp
exp = and

and :: Parser LExp
and = chainl1 tup $ wrap2 (:&) <$ semi

tup :: Parser LExp
tup = (eq >>= loop []) <|> done0
  where
    loop xs x =
      (comma >> eq >>= loop (x:xs)) <|>
      pure (done1 x xs)
    done0 =
      spaces *> get <&> \ i -> L (Loc i i) $ Exp.Tup []
    done1 x = \ case
      [] -> x
      y:xs -> wrapReverse2 Exp.Tup x y xs

eq :: Parser LExp
eq = chainl1 less $ wrap2 (:=) <$ equal

less :: Parser LExp
less = chainl1 or $ wrap2 (:<) <$ langle

or :: Parser LExp
or = chainl1 dotDot $ wrap2 (:|) <$ pipe

dotDot :: Parser LExp
dotDot = chainl1 plusMinus $ wrap2 (:..) <$ token ".."

plusMinus :: Parser LExp
plusMinus =
  chainl1 app $
  wrap2 (:+) <$ token (char '+') <|>
  wrap2 (:-) <$ token (char '-')

app :: Parser LExp
app = base >>= loop
  where
    loop x =
      (loop . wrap2 Exp.App x =<< arg) <|>
      pure x
    arg = do
      i <- get
      lbracket
      L _ x <- exp
      rbracket
      j <- get
      pure $ L (Loc i j) x

base :: Parser LExp
base = parens <|> wrapM baseF

parens :: Parser LExp
parens = do
  i <- get
  lparen
  L _ x <- exp
  rparen
  j <- get
  pure $ L (Loc i j) x

baseF :: Parser (ExpF LExp)
baseF =
  Exp.Abs
  <$> (fun *> lparen *> name <* rparen)
  <*> (lbrace *> exp <* rbrace) <|>
  Exp.Exi
  <$> (exists *> name)
  <*> (semi *> exp) <|>
  Exp.Int
  <$> integer <|>
  Exp.Fail
  <$ fail <|>
  Exp.All
  <$> (all *> lbrace *> exp <* rbrace) <|>
  Exp.For
  <$> (for *> lparen *> exp <* rparen)
  <*> (do' *> lparen *> name <* rparen)
  <*> (lbrace *> exp <* rbrace) <|>
  Exp.One
  <$> (one *> lbrace *> exp <* rbrace) <|>
  Exp.If
  <$> (if' *> lparen *> exp <* rparen)
  <*> (then' *> lparen *> name <* rparen)
  <*> (lbrace *> exp <* rbrace)
  <*> (else' *> lbrace *> exp <* rbrace) <|>
  Exp.Var
  <$> name

wrapM :: Parser (f (L f)) -> Parser (L f)
wrapM m = do
  spaces
  i <- get
  x <- m
  j <- get
  pure $ L (Loc i j) x

wrap2 :: (L f -> L f -> f (L f)) -> L f -> L f -> L f
wrap2 f x@(L i _) y@(L j _) = L (i <> j) (f x y)

wrapReverse2 :: ([L f] -> f (L f)) -> L f -> L f -> [L f] -> L f
wrapReverse2 f x@(L i _) y xs =
  L (extract (last1 y xs) <> i) . f $ reverse2 x y xs

fun :: Parser ()
fun = token $ void "fun"

exists :: Parser ()
exists = token $ void "exists"
