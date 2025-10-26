{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
module Loc
  ( Loc (..)
  , L (..)
  , extract
  , unwrap
  , prettyStuckError
  ) where

import Data.Text.Unsafe qualified as Unsafe


import AnsiStyle
import Pos
import Pretty
import Text (Text)
import Text qualified

data Loc = Loc !Pos !Pos deriving Show

instance Semigroup Loc where
  Loc i _ <> Loc _ j = Loc i j

instance Pretty Loc where
  pretty (Loc i j) = pretty i <> pretty '-' <> pretty j

data L f = L !Loc !(f (L f))

deriving instance Show (f (L f)) => Show (L f)

instance PrettyPrec (f (L f)) => PrettyPrec (L f) where
  prettyPrec prec = prettyPrec prec . unwrap

extract :: L f -> Loc
extract (L x _) = x

unwrap :: L f -> f (L f)
unwrap (L _ x) = x

instance Pretty (f (L f)) => Pretty (L f) where
  pretty = pretty . unwrap

prettyStuckError :: Text -> [[Loc]] -> Doc AnsiStyle
prettyStuckError input = \ case
  [] -> bolded "Stuck"
  xs -> bolded ("Stuck" <+> "at") <> line' <> prettyStacks input xs

prettyStacks :: Text -> [[Loc]] -> Doc AnsiStyle
prettyStacks input xs =
  vcat . punctuate (line' <> bolded "and") $
  indent 2 . prettyStack input <$> xs

prettyStack :: Text -> [Loc] -> Doc AnsiStyle
prettyStack input =
  vcat . fmap (prettyLoc input)

prettyLoc :: Text -> Loc -> Doc AnsiStyle
prettyLoc input loc =
  bolded (pretty loc <> colon) <> line' <>
  indent 2 (prettyLocText input loc)

prettyLocText :: Text -> Loc -> Doc AnsiStyle
prettyLocText input (Loc i j) =
  pretty (Text.sliceWord8 i.rowIndexWord8 i.indexWord8 input) <>
  if i.rowIndexWord8 == j.rowIndexWord8 then
    annotate
    errorColor
    (pretty $ Text.sliceWord8 i.indexWord8 j.indexWord8 input) <>
    pretty (Text.takeWhile (/= '\n') $ Unsafe.dropWord8 j.indexWord8 input)
  else
    annotate
    errorColor
    (pretty . Text.takeWhile (/= '\n') $ Unsafe.dropWord8 i.indexWord8 input)
