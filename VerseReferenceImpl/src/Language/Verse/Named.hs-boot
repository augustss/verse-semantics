{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Named
  ( Named
  ) where

import Control.Monad.Ref
import Control.Monad.Var

import Data.Kind
import Data.Ref
import Data.Unifiable

import Language.Verse.Pretty

data Named (m :: Type -> Type) a

type role Named nominal representational

instance Functor (Named m)

instance Foldable (Named m)

instance Traversable (Named m)

instance EqRef (Ref m) => Unifiable (Named m)

instance EqRef (Ref m) => Zippable (Named m)

instance ( MonadRef m
         , MonadPretty a m
         , MonadVar m
         ) => MonadPretty (Named m a) m
