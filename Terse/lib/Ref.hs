{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
module Ref
  ( Ref
  , World
  , MonadRef (..)
  , MonadWeakRef (..)
  ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as Strict

import Data.Kind
import Data.STRef

import GHC.IO
import GHC.Prim
import GHC.STRef
import GHC.Weak

type Ref m = STRef (World m)

type family World (m :: Type -> Type)

class Monad m => MonadRef m where
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

class MonadRef m => MonadWeakRef m where
  type WeakRef m :: Type -> Type
  newWeakRef :: Ref m a -> b -> m (WeakRef m b)
  readWeakRef :: WeakRef m a -> m (Maybe a)

type instance World IO = RealWorld

instance MonadRef IO where
  {-# INLINE newRef #-}
  newRef = stToIO . newRef
  {-# INLINE readRef #-}
  readRef = stToIO . readRef
  {-# INLINE writeRef #-}
  writeRef = (stToIO .) . writeRef

type instance World (ST s) = s

instance MonadRef (ST s) where
  {-# INLINE newRef #-}
  newRef = newSTRef
  {-# INLINE readRef #-}
  readRef = readSTRef
  {-# INLINE writeRef #-}
  writeRef = writeSTRef

instance MonadWeakRef IO where
  type WeakRef IO = Weak
  {-# INLINE newWeakRef #-}
  newWeakRef (STRef var) x = IO $ \ s ->
    case mkWeakNoFinalizer# var x s of
      (# s, x #) -> (# s, Weak x #)
  {-# INLINE readWeakRef #-}
  readWeakRef = deRefWeak

type instance World (Strict.StateT s m) = World m

instance MonadRef m => MonadRef (Strict.StateT s m) where
  {-# INLINE newRef #-}
  newRef = lift . newRef
  {-# INLINE readRef #-}
  readRef = lift . readRef
  {-# INLINE writeRef #-}
  writeRef = (lift .) . writeRef

instance MonadWeakRef m => MonadWeakRef (Strict.StateT s m) where
  type WeakRef (Strict.StateT s m) = WeakRef m
  {-# INLINE newWeakRef #-}
  newWeakRef = (lift .) . newWeakRef
  {-# INLINE readWeakRef #-}
  readWeakRef = lift . readWeakRef
