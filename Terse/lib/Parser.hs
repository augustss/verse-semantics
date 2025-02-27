{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Parser
  ( Parser
  , runParser
  , parse
  , Result (..)
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
  , ($!)
  , ($)
  , (&&)
  , (+)
  , (.)
  , (<)
  , (<>)
  , (==)
  , (>=)
  , mconcat
  , mempty
  , reverse
  )

import Pos (Pos (..))
import Pos qualified

newtype Parser a = Parser
  { unParser
    :: forall r . S
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

type Succeed r a = a -> S -> Yield r -> Fail r -> r

type Fail r = S -> Yield r -> r

data Result a
  = Yield (Text -> Result a)
  | Pure a {-# UNPACK #-} !Text
  | Empty {-# UNPACK #-} !Text {-# UNPACK #-} !Pos

runParser :: Parser a -> Text -> Result a
runParser m input = unParser m s yk sk fk fk
  where
    s = S { input, pos = Pos.empty }
    yk = Yield
    sk x s _yk _fk = Pure x s.input
    fk s _yk = Empty s.input s.pos

parse :: Parser a -> Text -> Either Pos a
parse m input = unParser m s yk sk fk fk
  where
    s = S { input, pos = Pos.empty }
    yk f = f mempty
    sk x _s _yk _fk = Right x
    fk s _yk = Left s.pos

instance Functor Parser where
  fmap f x = Parser $ \ s yk sk -> unParser x s yk $ sk . f

instance Applicative Parser where
  pure x = Parser $ \ s yk sk fk _ak ->
    sk x s yk fk
  f <*> x = Parser $ \ s yk sk fk ak ->
    unParser f s yk (\ f s yk fk -> unParser x s yk (sk . f) fk ak) fk ak

instance Alternative Parser where
  empty = Parser $ \ s yk _sk fk _ak ->
    fk s yk
  x <|> y = Parser $ \ s yk sk fk ak ->
    unParser x s yk sk (\ S { input } yk -> unParser y s { input } yk sk fk ak) ak

instance Monad Parser where
  x >>= f = Parser $ \ s yk sk fk ak ->
    unParser x s yk (\ x s yk fk -> unParser (f x) s yk sk fk ak) fk ak

instance MonadPlus Parser

instance (Text ~ a) => IsString (Parser a) where
  fromString (fromString -> x) =
    Parser $ \ s yk sk fk ak ->
      let
        loop z s =
          let
            !n_z = Unsafe.lengthWord8 z
            !y = Unsafe.dropWord8 s.pos.indexWord8 s.input
            !n_y = Unsafe.lengthWord8 y
          in
            if n_y < n_z
            then
              if Unsafe.takeWord8 n_y z == y
              then yk $ \ input ->
                if Text.null input
                then fk s ($ mempty)
                else
                  let
                    !indexWord8 = s.pos.indexWord8 + n_y
                    !column = s.pos.column + n_y
                    !pos = s.pos { indexWord8, column }
                  in
                    loop (Unsafe.dropWord8 n_y z) s
                      { input = s.input <> input
                      , pos
                      }
              else fk s yk
            else
              if Unsafe.takeWord8 n_z y == z
              then
                if Text.null x
                then sk x s yk fk
                else
                  let
                    !indexWord8 = s.pos.indexWord8 + n_z
                    !column = s.pos.column + n_z
                    !pos = s.pos { indexWord8, column }
                  in
                    sk x s { pos } yk ak
              else fk s yk
      in
        loop x s

get :: Parser Pos
get = Parser $ \ s yk sk fk _ak -> sk s.pos s yk fk

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \ s yk sk fk ak ->
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
            then sk () s ($ mempty) fk
            else sk () s { pos } ($ mempty) ak
          else loop (z && n == 0) s { input = s.input <> input, pos }
        else
          if z && n == 0
          then sk () s yk fk
          else sk () s { pos } yk ak
  in
    loop True s
  where
    g x i z =
      if f x
      then Just $! Pos.add z x i
      else Nothing

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \ s yk sk fk ak ->
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
              then sk x s ($ mempty) fk
              else sk x s { pos } ($ mempty) ak
          else loop (y:z) s { input = s.input <> input, pos }
        else
          let
            !x = mconcat $ reverse (y:z)
          in
            if Text.null x
            then sk x s yk fk
            else sk x s { pos } yk ak
  in
    loop [] s
  where
    g x i z =
      if f x
      then Just $! Pos.add z x i
      else Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \ s yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.pos.indexWord8
      in
        if f x
        then
          let
            !pos = Pos.add s.pos x i
          in
            sk x s { pos } yk ak
        else fk s yk
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ($ mempty)
      else loop s { input = s.input <> input }
    else loop s

char :: Char -> Parser Char
char x = Parser $ \ s yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter y i = Unsafe.iter s.input s.pos.indexWord8
      in
        if y == x
        then
          let
            !pos = Pos.add s.pos x i
          in
            sk x s { pos } yk ak
        else fk s yk
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ($ mempty)
      else loop s { input = s.input <> input }
    else loop s

eof :: Parser ()
eof = Parser $ \ s yk sk fk _ak ->
  if Unsafe.lengthWord8 s.input == s.pos.indexWord8
  then yk $ \ input ->
    if Text.null input
    then sk () s ($ mempty) fk
    else fk s { input = s.input <> input } yk
  else fk s yk

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
