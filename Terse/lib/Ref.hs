{-# LANGUAGE TypeFamilies #-}
module Ref
  ( MonadRef (..)
  ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as Strict

import Data.Kind
import Data.IORef
import Data.STRef

class Monad m => MonadRef m where
  type Ref m :: Type -> Type
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

instance MonadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

instance MonadRef (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

instance MonadRef m => MonadRef (Strict.StateT s m) where
  type Ref (Strict.StateT s m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef
