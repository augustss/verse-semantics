{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Ref
  ( MonadRef (..)
  , modifyRef'
  ) where

import Control.Monad.Reader
import Control.Monad.State.Strict qualified as Strict

import Data.IORef
import Data.Kind

class Monad m => MonadRef m where
  type Ref m :: Type -> Type
  type Ref m = RefDefault m

  newRef :: a -> m (Ref m a)
  default newRef :: (m ~ t n, MonadRefTrans t n) => a -> m (Ref m a)
  newRef = lift . newRef

  readRef :: Ref m a -> m a
  default readRef :: (m ~ t n, MonadRefTrans t n) => Ref m a -> m a
  readRef = lift . readRef

  writeRef :: Ref m a -> a -> m ()
  default writeRef :: (m ~ t n, MonadRefTrans t n) => Ref m a -> a -> m ()
  writeRef ref = lift . writeRef ref

type family RefDefault (m :: Type -> Type) :: Type -> Type where
  RefDefault (t n) = Ref n

type MonadRefTrans t n = (Ref (t n) ~ Ref n, MonadTrans t, MonadRef n)

instance MonadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef

modifyRef' :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef' r f = do
  x <- readRef r
  writeRef r $! f x

instance MonadRef m => MonadRef (ReaderT r m)

instance MonadRef m => MonadRef (Strict.StateT s m)
