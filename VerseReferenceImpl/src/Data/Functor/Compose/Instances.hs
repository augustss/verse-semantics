{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Functor.Compose.Instances () where

import Data.Functor.Compose

import Prettyprinter

instance Pretty (f (g a)) => Pretty (Compose f g a) where
  pretty = pretty . getCompose
