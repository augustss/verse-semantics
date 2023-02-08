{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Pretty
  ( PrettyM (..)
  ) where

import Data.Fix

import Prettyprinter

class Monad m => PrettyM a m where
  prettyM :: a -> m (Doc ann)

instance (Monad m, PrettyM (f (Fix f)) m) => PrettyM (Fix f) m where
  prettyM = prettyM . getFix
