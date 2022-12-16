{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Ref
  ( MonadRef (..)
  ) where

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans.Except

import Data.IORef
import Data.Kind
import Data.STRef

type family RefDefault (m :: Type -> Type) :: Type -> Type where
  RefDefault (t n) = Ref n

type MonadRefTrans t n = (Ref (t n) ~ Ref n, MonadTrans t, MonadRef n)

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
  writeRef x = lift . writeRef x

  modifyRef :: Ref m a -> (a -> a) -> m ()
  default modifyRef :: (m ~ t n, MonadRefTrans t n) => Ref m a -> (a -> a) -> m ()
  modifyRef x = lift . modifyRef x

instance MonadRef IO where
  type Ref IO = IORef
  newRef = newIORef
  readRef = readIORef
  writeRef = writeIORef
  modifyRef = modifyIORef

instance MonadRef (ST s) where
  type Ref (ST s) = STRef s
  newRef = newSTRef
  readRef = readSTRef
  writeRef = writeSTRef
  modifyRef = modifySTRef

instance MonadRef m => MonadRef (ExceptT e m)

instance MonadRef m => MonadRef (LogicT m)

instance MonadRef m => MonadRef (ReaderT r m)

instance MonadRef m => MonadRef (Lazy.StateT s m)

instance MonadRef m => MonadRef (Strict.StateT s m)
