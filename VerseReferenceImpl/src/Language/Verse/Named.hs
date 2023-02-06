{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Named
  ( Named (..)
  , prettyM
  ) where

import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Var

import Data.Functor
import Data.Ref
import Data.Unifiable

import Language.Verse.Val

import Prettyprinter

data Named m a
  = Ref (Backtrack.Ref m (Var m (Val m)))
  | Val a deriving (Functor, Foldable, Traversable)

instance EqRef (Backtrack.Ref m) => Unifiable (Named m)

instance EqRef (Backtrack.Ref m) => Zippable (Named m) where
  zipMatch = curry $ \ case
    (Ref x, Ref y) | eqRef x y -> Just []
    (Val x, Val y) -> Just [(x, y)]
    _ -> Nothing

prettyM :: ( Backtrack.MonadRef m
           , MonadVar m
           , Pretty a
           ) => Named m a -> m (Doc ann)
prettyM = \ case
  Ref ref -> Backtrack.readRef ref >>= readVar <&> \ case
    Nothing -> "_"
    Just var -> "_"
  Val x -> pure $ pretty x
