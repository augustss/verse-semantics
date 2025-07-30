module Language.Verse.Label
  ( Label
  , prettyLabel
  ) where

import Prettyprinter

type Label = Int

prettyLabel :: Label -> Doc ann
prettyLabel x = pretty $ x + maxBound + 1
