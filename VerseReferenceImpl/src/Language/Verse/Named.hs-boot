{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Named
  ( Named
  , prettyM
  ) where

import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Var

import Data.Kind
import Data.Ref
import Data.Unifiable

import Prettyprinter

data Named (m :: Type -> Type) a

type role Named nominal representational

instance Functor (Named m)

instance Foldable (Named m)

instance Traversable (Named m)

instance EqRef (Backtrack.Ref m) => Unifiable (Named m)

instance EqRef (Backtrack.Ref m) => Zippable (Named m)

prettyM :: ( Backtrack.MonadRef m
           , MonadVar m
           , Pretty a
           ) => Named m a -> m (Doc ann)
