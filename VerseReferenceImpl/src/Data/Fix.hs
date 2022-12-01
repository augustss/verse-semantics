{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Fix
  ( Fix (..)
  ) where

import Prettyprinter

newtype Fix f = Fix { getFix :: f (Fix f) }

deriving instance Show (f (Fix f)) => Show (Fix f)

instance Pretty (f (Fix f)) => Pretty (Fix f) where
  pretty = pretty . getFix
