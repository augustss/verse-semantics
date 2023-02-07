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
  = Ref (Backtrack.Ref m (Var m (Val m)))
  | Val a deriving (Functor, Foldable, Traversable)

instance EqRef (Backtrack.Ref m) => Unifiable (Named m)

instance EqRef (Backtrack.Ref m) => Zippable (Named m) where
  zipMatch = curry $ \ case
    (Ref x, Ref y) | eqRef x y -> Just []
    (Val x, Val y) -> Just [(x, y)]
    _ -> Nothing

instance ( Backtrack.MonadRef m
         , MonadVar m
         , PrettyM a m
         ) => PrettyM (Named m a) m where
  prettyM = \ case
    Ref ref -> Backtrack.readRef ref >>= freeze >>= \ case
      Nothing -> pure "_"
      Just x -> prettyM x
    Val x -> prettyM x
