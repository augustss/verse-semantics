{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Loc
  ( Loc (..)
  , isSubsetOf
  , L (..)
  , extract
  , unwrap
  , prettyStuck
  ) where

import Control.Category ((>>>))

import Data.Functor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Prettyprinter
import Prettyprinter.Render.Terminal

import Text (Text)
import Text qualified

data Loc = Loc {-# UNPACK #-} !Int {-# UNPACK #-} !Int deriving Show

instance Semigroup Loc where
  Loc i _ <> Loc _ j = Loc i j

instance Pretty Loc where
  pretty (Loc i j) = pretty i <> colon <> pretty j

isSubsetOf :: Loc -> Loc -> Bool
isSubsetOf (Loc i j) (Loc i' j') = i' <= i && j <= j'

data L f = L !Loc !(f (L f))

deriving instance Show (f (L f)) => Show (L f)

extract :: L f -> Loc
extract (L x _) = x

unwrap :: L f -> f (L f)
unwrap (L _ x) = x

instance Pretty (f (L f)) => Pretty (L f) where
  pretty = pretty . unwrap

prettyStuck :: Text -> [[Loc]] -> Doc AnsiStyle
prettyStuck xs =
  let
    !xs' = rows xs
    prettyStacks xs =
      vcat . punctuate (line' <> bolded "and") $
      xs <&> \ x -> align (indent 2 $ prettyStack x)
    prettyStack =
      vcat . fmap prettyLoc
    prettyLoc (Loc i j) =
      let
        !x@(Loc i' _) = rowColumn i xs'
        !y@(Loc j' _) = rowColumn j xs'
      in
        if i' == j' then
          bolded (prettyLocRowColumn x y <> colon) <> line' <>
          indent 2 (prettyLocText i j)
        else
          bolded (prettyLocRowColumn x y <> colon) <> line' <>
          indent 2 (annotate (color Red) $ dot <> dot <> dot)
    prettyLocText i j =
      pretty (Text.slice (lineStart i xs') i xs) <>
      annotate (color Red) (pretty $ Text.slice i j xs) <>
      pretty (Text.slice j (lineEnd j xs') xs)
    prettyLocRowColumn x y =
      pretty x <> pretty '-' <> pretty y
  in \ case
    [] -> bolded "Stuck"
    xs -> bolded ("Stuck" <+> "at") <> line' <> prettyStacks xs
  where
    bolded = annotate bold

data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !Int ![(Int, Int)]

rows :: Text -> IntMap Int
rows = Text.foldl' f (S 1 1 mempty) >>> \ case
  S i j xs -> IntMap.fromDistinctAscList . reverse $ (i, j):xs
  where
    f (S i j xs) = \ case
      '\n' -> S (i + 1) (j + 1) ((i, j):xs)
      _ -> S (i + 1) j xs

rowColumn :: Int -> IntMap Int -> Loc
rowColumn x = IntMap.lookupLE x >>> \ case
  Nothing -> Loc 0 x
  Just (i, j) -> Loc j (x - i)

lineStart :: Int -> IntMap Int -> Int
lineStart x = IntMap.lookupLE x >>> \ case
  Nothing -> 0
  Just (x, _) -> x

lineEnd :: Int -> IntMap Int -> Int
lineEnd x = IntMap.lookupGT x >>> \ case
  Nothing -> error "lineEnd"
  Just (x, _) -> x - 1
