{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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
  , Int
  , ($)
  , (+)
  , (.)
  , (/=)
  , (<)
  , (<>)
  , (==)
  , (||)
  , const
  , mconcat
  , mempty
  , reverse
  )

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
  , indexWord8 :: {-# UNPACK #-} !Int
  , rowIndexWord8 :: {-# UNPACK #-} !Int
  , row :: {-# UNPACK #-} !Int
  , column :: {-# UNPACK #-} !Int
  }

type Yield r = (Text -> r) -> r

type Succeed r a = a -> S -> [Text] -> Fail r -> r

type Fail r = Text -> Int -> Int -> [Text] -> r

data Result a
  = Yield (Text -> Result a)
  | Pure a
  | Empty {-# UNPACK #-} !Int {-# UNPACK #-} !Int [Text]

runParser :: Parser a -> Text -> Result a
runParser m input =
  let
    s = S { input, indexWord8 = 0, rowIndexWord8 = 0, row = 0, column = 0 }
  in
    unParser m s [] yk sk fk fk
  where
    yk = Yield
    sk x _s _ann _fk = Pure x
    fk _input = Empty

parse :: Parser a -> Text -> Either (Int, Int, [Text]) a
parse m input =
  let
    s = S { input, indexWord8 = 0, rowIndexWord8 = 0, row = 0, column = 0 }
  in
    unParser m s [] yk sk fk fk
  where
    yk f = f mempty
    sk x _s _ann _fk = Right x
    fk _input row column ann = Left (row, column, ann)

done :: Result a -> Either (Int, Int, [Text]) a
done = \ case
  Yield f -> done $ f mempty
  Pure x -> Right x
  Empty row column ann -> Left (row, column, ann)

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
    fk s.input s.row s.column ann
  x <|> y = Parser $ \ s ann yk sk fk ak ->
    unParser x s ann yk sk
    (\ input _row _column _ann -> unParser y s { input } ann yk sk fk ak)
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
              !y = Unsafe.dropWord8 s.indexWord8 s.input
            in
              if Unsafe.lengthWord8 y < n
              then yk $ \ input ->
                if Text.null input
                then fk s.input s.row s.column ann
                else loop s { input = s.input <> input }
              else
                if Unsafe.takeWord8 n y == x
                then
                  let
                    !indexWord8 = s.indexWord8 + n
                    !column = s.column + n
                  in
                    if n == 0
                    then sk () s { indexWord8, column } ann fk
                    else sk () s { indexWord8, column } ann ak
                else fk s.input s.row s.column ann
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
        !x = Text.takeWhile f $ Unsafe.takeWord8 s.indexWord8 s.input
      in yk $ \ input ->
        let
          !n = Unsafe.lengthWord8 x
          !indexWord8 = s.indexWord8 + n
          !column = s.column + n
        in
          if Text.null input
          then
            if z
            then sk () s { indexWord8, column } ann ak
            else sk () s { indexWord8, column } ann fk
          else loop (z || n /= 0) s
            { input = s.input <> input
            , indexWord8
            , column
            }
  in
    loop False s

takeWhile :: (Char -> Bool) -> Parser Text
takeWhile f = Parser $ \ s ann yk sk fk ak ->
  let
    loop z s =
      let
        !x = Text.takeWhile f $ Unsafe.takeWord8 s.indexWord8 s.input
      in yk $ \ input ->
        let
          !n = Unsafe.lengthWord8 x
          !indexWord8 = s.indexWord8 + n
          !column = s.column + n
        in
          if Text.null input
          then
            let
              !y = mconcat $ reverse (x:z)
            in
              if Text.null y
              then sk y s { indexWord8, column } ann fk
              else sk y s { indexWord8, column } ann ak
          else
            loop (x:z) s
              { input = s.input <> input
              , indexWord8
              , column
              }
  in
    loop [] s

char :: Char -> Parser ()
char x = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter y i = Unsafe.iter s.input s.indexWord8
      in
        if y == x
        then sk () s
          { indexWord8 = s.indexWord8 + i
          , column = s.column + 1
          } ann ak
        else fk s.input s.row s.column ann
  in
    if Unsafe.lengthWord8 s.input == s.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s.input s.row s.column ann
      else loop s { input = s.input <> input }
    else loop s

tab :: Parser ()
tab = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.indexWord8
      in
        if x == '\t'
        then sk () s
          { indexWord8 = s.indexWord8 + i
          , column = s.column + 8
          } ann ak
        else fk s.input s.row s.column ann
  in
    if Unsafe.lengthWord8 s.input == s.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s.input s.row s.column ann
      else loop s { input = s.input <> input }
    else loop s

newline :: Parser ()
newline = Parser $ \ s ann yk sk fk ak ->
  let
    loop s =
      let
        Unsafe.Iter x i = Unsafe.iter s.input s.indexWord8
      in
        if x == '\n'
        then sk () s
          { indexWord8 = s.indexWord8 + i
          , rowIndexWord8 = s.indexWord8 + i
          , row = s.row + 1
          , column = 0
          } ann ak
        else fk s.input s.row s.column ann
  in
    if Unsafe.lengthWord8 s.input == s.indexWord8
    then yk $ \ input ->
      if Text.null input
      then fk s.input s.row s.column ann
      else loop s { input = s.input <> input }
    else loop s

eof :: Parser ()
eof = Parser $ \ s ann yk sk fk _ak ->
  if Unsafe.lengthWord8 s.input == s.indexWord8
  then yk $ \ input ->
    if Text.null input
    then sk () s ann fk
    else fk (s.input <> input) s.row s.column ann
  else fk s.input s.row s.column ann
