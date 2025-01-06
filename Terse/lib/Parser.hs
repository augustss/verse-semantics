{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Parser
  ( Parser
  , runParser
  , parse
  , Result (..)
  , done
  , step
  , (<?>)
  , get
  , skipWhile
  , takeWhile
  , satisfy
  , char
  , eof
  ) where

import Control.Applicative
import Control.Monad

import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Unsafe qualified as Unsafe

import Prelude
  ( Bool (..)
  , Char
  , Either (..)
  , Int
  , Maybe (..)
  , ($)
  , (&&)
  , (+)
  , (.)
  , (<)
  , (<>)
  , (==)
  , (>=)
  , const
  , mconcat
  , mempty
  , reverse
  )

import Pos (Pos (..))
import Pos qualified

newtype Parser a = Parser
  { unParser
    :: forall r . S
    -> [Text]
    -> Yield r
    -> Succeed r a
    -> Fail r
    -> Fail r
    -> r
  }

data S = S
  { input :: {-# UNPACK #-} !Text
  , pos :: {-# UNPACK #-} !Pos
  }

type Yield r = (Text -> r) -> r

type Succeed r a = a -> S -> [Text] -> Yield r -> Fail r -> r

type Fail r = S -> [Text] -> Yield r -> r

data Result a
  = Yield (Text -> Result a)
  | Pure a {-# UNPACK #-} !Text
  | Empty {-# UNPACK #-} !Text {-# UNPACK #-} !Pos [Text]

runParser :: Parser a -> Text -> Result a
runParser m input =
  let
    s = S { input, pos = Pos.empty }
  in
    unParser m s [] yk sk fk fk
  where
    yk = Yield
    sk x s _ann _yk _fk = Pure x s.input
    fk s ann _yk = Empty s.input s.pos ann

parse :: Parser a -> Text -> Either (Pos, [Text]) a
parse m input =
  let
    s = S { input, pos = Pos.empty }
  in
    unParser m s [] yk sk fk fk
  where
    yk f = f mempty
    sk x _s _ann _yk _fk = Right x
    fk s ann _yk = Left (s.pos, ann)

done :: Result a -> (Text, Either (Pos, [Text]) a)
done = \ case
  Yield f -> done $ f mempty
  Pure x input -> (input, Right x)
  Empty input pos ann -> (input, Left (pos, ann))

step :: Result a -> Text -> Result a
step = \ case
  Yield f -> f
  x@Pure {} -> const x
  x@Empty {} -> const x

instance Functor Parser where
  fmap f x = Parser $ \ s ann yk sk ->
    unParser x s ann yk $ sk . f

instance Applicative Parser where
  pure x = Parser $ \ s ann yk sk fk _ak ->
    sk x s ann yk fk
  f <*> x = Parser $ \ s ann yk sk fk ak ->
    unParser f s ann yk
    (\ f s ann yk fk -> unParser x s ann yk (sk . f) fk ak)
    fk
    ak

instance Alternative Parser where
  empty = Parser $ \ s ann yk _sk fk _ak ->
    fk s ann yk
  x <|> y = Parser $ \ s ann yk sk fk ak ->
    unParser x s ann yk sk
    (\ S { input } _ann yk -> unParser y s { input } ann yk sk fk ak)
    ak

instance Monad Parser where
  x >>= f = Parser $ \ s ann yk sk fk ak ->
    unParser x s ann yk
    (\ x s ann yk fk -> unParser (f x) s ann yk sk fk ak)
    fk
    ak

instance MonadPlus Parser

instance (Text ~ a) => IsString (Parser a) where
  fromString (fromString -> x) =
    Parser $ \ s ann yk sk fk ak ->
      let
        loop x s =
          let
            !n_x = Unsafe.lengthWord8 x
            !y = Unsafe.dropWord8 s.pos.indexWord8 s.input
            !n_y = Unsafe.lengthWord8 y
          in
            if n_y < n_x
            then
              if Unsafe.takeWord8 n_y x == y
              then yk $ \ input ->
                if Text.null input
                then fk s ann ($ mempty)
                else
                  let
                    !indexWord8 = s.pos.indexWord8 + n_y
                    !column = s.pos.column + n_y
                    !pos = s.pos { indexWord8, column }
                  in
                    loop (Unsafe.dropWord8 n_y x) s
                      { input = s.input <> input
                      , pos
                      }
              else fk s ann yk
            else
              if Unsafe.takeWord8 n_x y == x
              then
                if n_x == 0
                then sk x s ann yk fk
                else
                  let
                    !indexWord8 = s.pos.indexWord8 + n_x
                    !column = s.pos.column + n_x
                    !pos = s.pos { indexWord8, column }
                  in
                    sk x s { pos } ann yk ak
              else fk s ann yk
      in
        loop x s

infixl 0 <?>
(<?>) :: Parser a -> Text -> Parser a
m <?> ann = Parser $ \ s -> unParser m s . (ann:)

get :: Parser Pos
get = Parser $ \ s ann yk sk fk _ak -> sk s.pos s ann yk fk

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \ s ann yk sk fk ak ->
  let
    loop !z s =
      let
        !x = Unsafe.dropWord8 s.pos.indexWord8 s.input
        (!y, !pos) = takeWhileAcc g x s.pos
        !n = Unsafe.lengthWord8 y
      in
        if n == Unsafe.lengthWord8 x
        then yk $ \ input ->
          if Text.null input
          then
            if z && n == 0
            then sk () s ann ($ mempty) fk
            else sk () s { pos } ann ($ mempty) ak
          else loop (z && n == 0) s { input = s.input <> input, pos }
        else
          if z && n == 0
          then sk () s ann yk fk
          else sk () s { pos } ann yk ak
  in
    loop True s
  where
    g x i z =
      if f x
      then Just (Pos.add z i x)
      else Nothing

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \ s ann yk sk fk ak ->
  let
    loop z s =
      let
        !x = Unsafe.dropWord8 s.pos.indexWord8 s.input
        (!y, !pos) = takeWhileAcc g x s.pos
        !n = Unsafe.lengthWord8 y
      in
        if n == Unsafe.lengthWord8 x
        then yk $ \ input ->
          if Text.null input
          then
            let
              !x = mconcat $ reverse (y:z)
            in
              if Text.null x
              then sk x s ann ($ mempty) fk
              else sk x s { pos } ann ($ mempty) ak
          else loop (y:z) s { input = s.input <> input, pos }
        else
          let
            !x = mconcat $ reverse (y:z)
          in
            if Text.null x
            then sk x s ann yk fk
            else sk x s { pos } ann yk ak
  in
    loop [] s
  where
    g x i z =
      if f x
      then Just (Pos.add z i x)
      else Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.pos.indexWord8
      in
        if f x
        then
          let
            !pos = Pos.add s.pos i x
          in
            sk x s { pos } ann yk ak
        else fk s ann yk
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ann ($ mempty)
      else loop s { input = s.input <> input }
    else loop s

char :: Char -> Parser Char
char x = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter y i = Unsafe.iter s.input s.pos.indexWord8
      in
        if y == x
        then
          let
            !pos = Pos.add s.pos i x
          in
            sk x s { pos } ann yk ak
        else fk s ann yk
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ann ($ mempty)
      else loop s { input = s.input <> input }
    else loop s

eof :: Parser ()
eof = Parser $ \ s ann yk sk fk _ak ->
  if Unsafe.lengthWord8 s.input == s.pos.indexWord8
  then yk $ \ input ->
    if Text.null input
    then sk () s ann ($ mempty) fk
    else fk s { input = s.input <> input } ann yk
  else fk s ann yk

takeWhileAcc :: (Char -> Int -> a -> Maybe a) -> Text -> a -> (Text, a)
takeWhileAcc f !xs = loop 0
  where
    loop !i !z =
      if i >= Unsafe.lengthWord8 xs
      then (xs, z)
      else
        let
          Unsafe.Iter x j = Unsafe.iter xs i
        in
          case f x j z of
            Nothing -> (Unsafe.takeWord8 i xs, z)
            Just z -> loop (i + j) z
