{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Named
  ( Named (..)
  ) where

import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Var

import Data.Ref
import Data.Unifiable

import Language.Verse.Pretty
import Language.Verse.Val

data Named m a
  = Val a
  | Ref (Backtrack.Ref m (Var m (Val m))) deriving (Functor, Foldable, Traversable)

instance EqRef (Backtrack.Ref m) => Unifiable (Named m)

instance EqRef (Backtrack.Ref m) => Zippable (Named m) where
  zipMatch = curry $ \ case
    (Val x, Val y) -> Just [(x, y)]
    (Ref x, Ref y) | eqRef x y -> Just []
    _ -> Nothing

instance ( Backtrack.MonadRef m
         , MonadPretty a m
         , MonadVar m
         ) => MonadPretty (Named m a) m where
  prettyM = \ case
    Val x -> prettyM x
    Ref ref -> Backtrack.readRef ref >>= freeze >>= \ case
      Nothing -> pure "_"
      Just x -> prettyM x
