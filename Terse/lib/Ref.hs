{-# LANGUAGE TypeFamilies #-}
module Ref
  ( MonadRef (..)
  , Ref
  , World
  ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as Strict

import Data.Kind
import Data.STRef

type Ref m = STRef (World m)

type family World (m :: Type -> Type)

class Monad m => MonadRef m where
  newRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

type instance World IO = RealWorld

instance MonadRef IO where
  newRef = stToIO . newRef
  readRef = stToIO . readRef
  writeRef = (stToIO .) . writeRef

type instance World (ST s) = s

instance MonadRef (ST s) where
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef

type instance World (Strict.StateT s m) = World m

instance MonadRef m => MonadRef (Strict.StateT s m) where
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef
