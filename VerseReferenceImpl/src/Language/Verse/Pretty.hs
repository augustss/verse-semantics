{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Pretty
  ( MonadPretty (..)
  ) where

import Data.Fix

import Prettyprinter

class Monad m => MonadPretty a m where
  prettyM :: a -> m (Doc ann)

instance (Monad m, MonadPretty (f (Fix f)) m) => MonadPretty (Fix f) m where
  prettyM = prettyM . getFix
