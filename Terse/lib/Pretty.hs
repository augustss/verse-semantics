{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Pretty
  ( module Prettyprinter
  , PrettyPrec (..)
  , prettyParens
  ) where

import Prettyprinter

import Fix

class Pretty a => PrettyPrec a where
  prettyPrec :: Int -> a -> Doc ann

instance PrettyPrec (f (Fix f)) => PrettyPrec (Fix f) where
  prettyPrec prec = prettyPrec prec . getFix

prettyParens :: Bool -> Doc ann -> Doc ann
prettyParens = \ case
  True -> parens
  False -> id
