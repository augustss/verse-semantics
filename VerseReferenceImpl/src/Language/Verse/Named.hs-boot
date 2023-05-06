{-# LANGUAGE TypeFamilies #-}
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

import Control.Monad.Var

import Data.Freshenable
import Data.Kind
import Data.Unifiable

import Language.Verse.Pretty

data Named (m :: Type -> Type) a

type role Named nominal representational

instance Functor (Named m)

instance Foldable (Named m)

instance Traversable (Named m)

instance EqVarRef (VarRef m) => Unifiable (Named m)

instance EqVarRef (VarRef m) => Zippable (Named m)

instance Freshenable (Named m a)

instance ( MonadPretty a m
         , MonadVarRef m
         ) => MonadPretty (Named m a) m
