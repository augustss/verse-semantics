{-# LANGUAGE OverloadedStrings #-}
module Verse.Token
  ( integer
  , fail
  , all
  , for
  , do'
  , one
  , if'
  , then'
  , else'
  , name
  , lparen
  , rparen
  , lbrace
  , rbrace
  , lbracket
  , rbracket
  , langle
  , semi
  , comma
  , pipe
  , equal
  , spaces
  , token
  ) where

import Control.Applicative

import Data.Char
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude
  ( Bool
  , Integer
  , Integral
  , Num
  , ($)
  , (&&)
  , (.)
  , (<=)
  , (+)
  , (*)
  , (-)
  , fromIntegral
  , negate
  , subtract
  )

import Parser

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
