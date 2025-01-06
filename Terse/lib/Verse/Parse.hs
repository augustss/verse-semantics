{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Parse
  ( parse
  , parse'
  , Result (..)
  ) where

import Control.Applicative

import Data.Char
import Data.Functor
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude
  ( Bool
  , Either
  , Integer
  , Integral
  , Num
  , ($)
  , (&&)
  , (.)
  , (<=)
  , (=<<)
  , (>>)
  , (>>=)
  , (+)
  , (*)
  , (-)
  , flip
  , foldl
  , fromIntegral
  , negate
  , subtract
  )

import Loc
import Parser
  ( Parser
  , Result (..)
  , char
  , eof
  , get
  , runParser
  , satisfy
  , skipWhile
  , takeWhile
  )
import Parser qualified
import Pos

import Verse.Exp
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
import Verse.Exp qualified as Exp

parse :: Text -> Either (Pos, [Text]) LExp
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
      y:xs -> wrapRev2 Exp.Tup x y xs
    wrapRev2 f x@(L i _) y xs =
      L (extract (last1 y xs) <> i) . f $ reverse2 x y xs
    last1 x = \ case
      [] -> x
      x:xs -> last1 x xs
    reverse2 x y =
      foldl (flip (:)) [y, x]

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
      spaces
      i <- get
      lbracket
      L _ x <- exp
      rbracket
      j <- get
      pure $ L (Loc i j) x

base :: Parser LExp
base = parens <|> wrap baseF

parens :: Parser LExp
parens = do
  spaces
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

wrap :: Parser (f (L f)) -> Parser (L f)
wrap m = do
  spaces
  i <- get
  x <- m
  j <- get
  pure $ L (Loc i j) x

wrap2 :: (L f -> L f -> f (L f)) -> L f -> L f -> L f
wrap2 f x@(L i _) y@(L j _) = L (i <> j) (f x y)

fun :: Parser ()
fun = token $ void "fun"

exists :: Parser ()
exists = token $ void "exists"

integer :: Parser Integer
integer = token $ signed decimal

fail :: Parser ()
fail = token $ void "fail"

all :: Parser ()
all = token $ void "all"

for :: Parser ()
for = token $ void "for"

do' :: Parser ()
do' = token $ void "do"

one :: Parser ()
one = token $ void "one"

if' :: Parser ()
if' = token $ void "if"

then' :: Parser ()
then' = token $ void "then"

else' :: Parser ()
else' = token $ void "else"

name :: Parser Text
name = token $
  "operator'+'" <|>
  "operator'-'" <|>
  "operator'<'" <|>
  Text.cons <$> head <*> tail
  where
    head = alpha <|> char '_'
    tail = takeWhile isAlphaNum

lparen :: Parser ()
lparen = token . void $ char '('

rparen :: Parser ()
rparen = token . void $ char ')'

lbrace :: Parser ()
lbrace = token . void $ char '{'

rbrace :: Parser ()
rbrace = token . void $ char '}'

lbracket :: Parser ()
lbracket = token . void $ char '['

rbracket :: Parser ()
rbracket = token . void $ char ']'

langle :: Parser ()
langle = token . void $ char '<'

semi :: Parser ()
semi = token . void $ char ';'

comma :: Parser ()
comma = token . void $ char ','

pipe :: Parser ()
pipe = token . void $ char '|'

equal :: Parser ()
equal = token . void $ char '='

token :: Parser a -> Parser a
token m = m <* spaces

spaces :: Parser ()
spaces = skipWhile isSpace

alpha :: Parser Char
alpha = satisfy isAlpha

decimal :: Integral a => Parser a
decimal = do
  z <- fromIntegral . (subtract 48) . ord <$> satisfy isDecimal
  Text.foldl' f z <$> takeWhile isDecimal
  where
    f z x = z * 10 + fromIntegral (ord x - 48)

isDecimal :: Char -> Bool
isDecimal x = '0' <= x && x <= '9'

signed :: Num a => Parser a -> Parser a
signed m =
  negate <$> (char '-' *> m) <|>
  char '+' *> m <|>
  m

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 m n = do
  x <- m
  loop x
  where
    loop x =
      loop1 x <|> pure x
    loop1 x = do
      f <- n
      y <- m
      loop $ f x y
