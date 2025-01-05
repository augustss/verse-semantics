{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Pos
  ( Pos (..)
  , empty
  , add
  , prettyParseError
  ) where

import Data.Text.Unsafe qualified as Unsafe

import Text (Text)
import Text qualified as Text

import Prettyprinter
  ( Doc
  , Pretty (..)
  , (<+>)
  , annotate
  , colon
  , indent
  , line'
  )
import Prettyprinter.Render.Terminal

data Pos = Pos
  { indexWord8 :: {-# UNPACK #-} !Int
  , rowIndexWord8 :: {-# UNPACK #-} !Int
  , row :: {-# UNPACK #-} !Int
  , column :: {-# UNPACK #-} !Int
  } deriving Show

instance Pretty Pos where
  pretty Pos {..} = pretty row <> colon <> pretty column

empty :: Pos
empty = Pos
  { indexWord8 = 0
  , rowIndexWord8 = 0
  , row = 0
  , column = 0
  }

add :: Pos -> Int -> Char -> Pos
add !pos !i =
  let
    !indexWord8 = pos.indexWord8 + i
  in \ case
    '\n' ->
      let
        !row = pos.row + 1
        !column = 0
      in
        pos { indexWord8, rowIndexWord8 = indexWord8, row, column }
    '\t' ->
      let
        !column = pos.column + 8
      in
        pos { indexWord8, column }
    _ ->
      let
        !column = pos.column + 1
      in
        pos { indexWord8, column }

prettyParseError :: Text -> Pos -> [Text] -> Doc AnsiStyle
prettyParseError input pos ann =
  bolded ("Parse" <+> "error" <+> "at") <> line' <>
  indent 2 (prettyPos input pos ann)

prettyPos :: Text -> Pos -> [Text] -> Doc AnsiStyle
prettyPos input pos _ann =
  bolded (pretty pos <> colon) <> line' <>
  indent 2 (prettyPosText input pos)

prettyPosText :: Text -> Pos -> Doc AnsiStyle
prettyPosText input pos =
  pretty (Text.sliceWord8 pos.rowIndexWord8 pos.indexWord8 input) <>
  annotate
  (color Red)
  (pretty . Text.takeWhile (/= '\n') $ Unsafe.dropWord8 pos.indexWord8 input)

bolded :: Doc AnsiStyle -> Doc AnsiStyle
bolded =
  annotate bold
