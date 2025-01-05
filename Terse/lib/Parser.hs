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
  , char
  , tab
  , newline
  , eof
  ) where

import Control.Applicative

import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Unsafe qualified as Unsafe

import Prelude
  ( Bool (..)
  , Char
  , Either (..)
  , Functor (..)
  , ($)
  , (&&)
  , (+)
  , (.)
  , (<)
  , (<>)
  , (==)
  , const
  , mconcat
  , mempty
  , reverse
  )

import Pos

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

type Succeed r a = a -> S -> [Text] -> Fail r -> r

type Fail r = S -> [Text] -> r

data Result a
  = Yield (Text -> Result a)
  | Pure a
  | Empty {-# UNPACK #-} !Text {-# UNPACK #-} !Pos [Text]

runParser :: Parser a -> Text -> Result a
runParser m input =
  let
    s = S { input, pos = emptyPos }
  in
    unParser m s [] yk sk fk fk
  where
    yk = Yield
    sk x _s _ann _fk = Pure x
    fk s = Empty s.input s.pos

parse :: Parser a -> Text -> Either (Pos, [Text]) a
parse m input =
  let
    s = S { input, pos = emptyPos }
  in
    unParser m s [] yk sk fk fk
  where
    yk f = f mempty
    sk x _s _ann _fk = Right x
    fk s ann = Left (s.pos, ann)

done :: Result a -> Either (Text, Pos, [Text]) a
done = \ case
  Yield f -> done $ f mempty
  Pure x -> Right x
  Empty input pos ann -> Left (input, pos, ann)

step :: Result a -> Text -> Result a
step = \ case
  Yield f -> f
  x@Pure {} -> const x
  x@Empty {} -> const x

instance Functor Parser where
  fmap f x = Parser $ \ s ann yk sk ->
    unParser x s ann yk $ sk . f

instance Applicative Parser where
  pure x = Parser $ \ s ann _yk sk fk _ak ->
    sk x s ann fk
  f <*> x = Parser $ \ s ann yk sk fk ak ->
    unParser f s ann yk
    (\ f s ann fk -> unParser x s ann yk (sk . f) fk ak)
    fk
    ak

instance Alternative Parser where
  empty = Parser $ \ s ann _yk _sk fk _ak ->
    fk s ann
  x <|> y = Parser $ \ s ann yk sk fk ak ->
    unParser x s ann yk sk
    (\ S { input } _ann -> unParser y s { input } ann yk sk fk ak)
    ak

instance IsString (Parser ()) where
  fromString (fromString -> x) =
    let
      !n = Unsafe.lengthWord8 x
    in
      Parser $ \ s ann yk sk fk ak ->
        let
          loop s =
            let
              !y = Unsafe.dropWord8 s.pos.indexWord8 s.input
            in
              if Unsafe.lengthWord8 y < n
              then yk $ \ input ->
                if Text.null input
                then fk s ann
                else loop s { input = s.input <> input }
              else
                if Unsafe.takeWord8 n y == x
                then
                  if n == 0
                  then sk () s ann fk
                  else
                    let
                      !indexWord8 = s.pos.indexWord8 + n
                      !column = s.pos.column + n
                      !pos = s.pos { indexWord8, column }
                    in
                      sk () s { pos } ann ak
                else fk s ann
        in
          loop s

infixl 0 <?>
(<?>) :: Parser a -> Text -> Parser a
m <?> ann = Parser $ \ s ->
  unParser m s . (ann:)

get :: Parser S
get = Parser $ \ s ann _yk sk fk _ak -> sk s s ann fk

skipWhile :: (Char -> Bool) -> Parser ()
skipWhile f = Parser $ \ s ann yk sk fk ak ->
  let
    loop !z s =
      let
        !x = Unsafe.dropWord8 s.pos.indexWord8 s.input
        !y = Text.takeWhile f x
        !n = Unsafe.lengthWord8 y
      in
        if n == Unsafe.lengthWord8 x
        then yk $ \ input ->
          if Text.null input
          then
            if z && n == 0
            then sk () s ann fk
            else
              let
                !indexWord8 = s.pos.indexWord8 + n
                !column = s.pos.column + n
                !pos = s.pos { indexWord8, column }
              in
                sk () s { pos } ann ak
          else
            let
              !indexWord8 = s.pos.indexWord8 + n
              !column = s.pos.column + n
              !pos = s.pos { indexWord8, column }
            in
              loop (z && n == 0) s { input = s.input <> input, pos }
        else
          if z && n == 0
          then sk () s ann fk
          else
            let
              !indexWord8 = s.pos.indexWord8 + n
              !column = s.pos.column + n
              !pos = s.pos { indexWord8, column }
            in
              sk () s { pos } ann ak
  in
    loop True s

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \ s ann yk sk fk ak ->
  let
    loop z s =
      let
        !x = Unsafe.dropWord8 s.pos.indexWord8 s.input
        !y = Text.takeWhile f x
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
              then sk x s ann fk
              else
                let
                  !indexWord8 = s.pos.indexWord8 + n
                  !column = s.pos.column + n
                  !pos = s.pos { indexWord8, column }
                in
                  sk x s { pos } ann ak
          else
            let
              !indexWord8 = s.pos.indexWord8 + n
              !column = s.pos.column + n
              !pos = s.pos { indexWord8, column }
            in
              loop (y:z) s { input = s.input <> input, pos }
        else
          let
            !x = mconcat $ reverse (y:z)
          in
            if Text.null x
            then sk x s ann fk
            else
              let
                !indexWord8 = s.pos.indexWord8 + n
                !column = s.pos.column + n
                !pos = s.pos { indexWord8, column }
              in
                sk x s { pos } ann ak
  in
    loop [] s

char :: Char -> Parser ()
char x = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter y i = Unsafe.iter s.input s.pos.indexWord8
      in
        if y == x
        then
          let
            !indexWord8 = s.pos.indexWord8 + i
            !column = s.pos.column + 1
          in
            sk () s { pos = s.pos { indexWord8, column } } ann ak
        else fk s ann
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ann
      else loop s { input = s.input <> input }
    else loop s

tab :: Parser ()
tab = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.pos.indexWord8
      in
        if x == '\t'
        then
          let
            !indexWord8 = s.pos.indexWord8 + i
            !column = s.pos.column + 8
          in
            sk () s { pos = s.pos { indexWord8, column } } ann ak
        else fk s ann
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ann
      else loop s { input = s.input <> input }
    else loop s

newline :: Parser ()
newline = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.pos.indexWord8
      in
        if x == '\n'
        then
          let
            !indexWord8 = s.pos.indexWord8 + i
            !row = s.pos.row + 1
            !column = 0
            !pos = Pos { indexWord8, rowIndexWord8 = indexWord8, row, column }
          in sk () s { pos } ann ak
        else fk s ann
  in
    if Unsafe.lengthWord8 s.input == s.pos.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s ann
      else loop s { input = s.input <> input }
    else loop s

eof :: Parser ()
eof = Parser $ \ s ann yk sk fk _ak ->
  if Unsafe.lengthWord8 s.input == s.pos.indexWord8
  then yk $ \ input ->
    if Text.null input
    then sk () s ann fk
    else fk s { input = s.input <> input } ann
  else fk s ann
