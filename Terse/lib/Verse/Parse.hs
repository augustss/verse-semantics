{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Parse
  ( parse
  , parse'
  , Result
  , pattern Fail
  , pattern Partial
  , pattern Done
  , eitherResult
  ) where

import Control.Applicative

import Data.Attoparsec.Internal qualified as Internal
import Data.Attoparsec.Internal.Types qualified as Internal
import Data.Attoparsec.Text
  ( Parser
  , Result
  , pattern Fail
  , pattern Partial
  , pattern Done
  , char
  , decimal
  , eitherResult
  , letter
  , signed
  , skipWhile
  , takeWhile
  )
import Data.Attoparsec.Text qualified as Parser
import Data.Char
import Data.Functor
import Data.Monoid (Any (..), (<>))
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude
  ( Either
  , Int
  , Integer
  , String
  , ($)
  , (.)
  , (<)
  , (=<<)
  , (==)
  , (>>)
  , (>>=)
  , flip
  , foldl
  , otherwise
  , undefined
  )

import Loc

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

parse :: Text -> Either String LExp
parse = Parser.parseOnly $ exp <* spaces <* endOfInput

parse' :: Text -> Result LExp
parse' = Parser.parse $ exp <* spaces <* endOfInput

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
    head = letter <|> char '_'
    tail = takeWhile $ getAny . (Any . isAlpha <> Any . isDigit)

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
token m = spaces *> m

spaces :: Parser ()
spaces = skipWhile isSpace

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

get :: Parser Int
get = Internal.Parser $ \ t !pos more _lose succ ->
  succ t pos more $ Internal.fromPos pos

endOfInput :: Parser ()
endOfInput = Internal.Parser $ \ t pos more lose succ ->
  if
    | pos < Internal.atBufferEnd (undefined :: Text) t ->
        lose t pos more [] "end of input"
    | more == Internal.Complete ->
        succ t pos more ()
    | otherwise ->
        let
          lose' t' pos' more' _ctx _msg = succ t' pos' more' ()
          succ' t' pos' more' _a = lose t' pos' more' [] "end of input"
        in
          Internal.runParser Internal.demandInput t pos more lose' succ'
