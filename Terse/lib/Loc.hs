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

import Data.Functor
import Data.Text.Unsafe qualified as Unsafe

import Prettyprinter
import Prettyprinter.Render.Terminal

import Pos
import Text (Text)
import Text qualified as Text

data Loc = Loc !Pos !Pos deriving Show

instance Semigroup Loc where
  Loc i _ <> Loc _ j = Loc i j

instance Pretty Loc where
  pretty (Loc i j) = pretty i <> colon <> pretty j

data L f = L !Loc !(f (L f))

deriving instance Show (f (L f)) => Show (L f)

extract :: L f -> Loc
extract (L x _) = x

unwrap :: L f -> f (L f)
unwrap (L _ x) = x

instance Pretty (f (L f)) => Pretty (L f) where
  pretty = pretty . unwrap

prettyStuckError :: Text -> [[Loc]] -> Doc AnsiStyle
prettyStuckError xs =
  let
    prettyStacks xs =
      vcat . punctuate (line' <> bolded "and") $
      xs <&> \ x -> indent 2 $ prettyStack x
    prettyStack =
      vcat . fmap prettyLoc
    prettyLoc (Loc i j) =
      if i.row == j.row then
        bolded (prettyLocRowColumn i j <> colon) <> line' <>
        indent 2 (prettyLocText i j)
      else
        bolded (prettyLocRowColumn i j <> colon) <> line' <>
        indent 2 (annotate (color Red) $ dot <> dot <> dot)
    prettyLocText i j =
      pretty (Text.sliceWord8 i.rowIndexWord8 i.indexWord8 xs) <>
      annotate
      (color Red)
      (pretty $ Text.sliceWord8 i.indexWord8 j.indexWord8 xs) <>
      pretty (Text.takeWhile (/= '\n') $ Unsafe.dropWord8 j.indexWord8 xs)
    prettyLocRowColumn i j =
      pretty i <> pretty '-' <> pretty j
  in \ case
    [] -> bolded "Stuck"
    xs -> bolded ("Stuck" <+> "at") <> line' <> prettyStacks xs
  where
    bolded = annotate bold
