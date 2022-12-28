{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Ref
  ( MonadRef (..)
  , modifyRef
  ) where

import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.CPS qualified as CPS

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
  writeRef ref = lift . writeRef ref

modifyRef :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef ref f = writeRef ref . f =<< readRef ref

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

instance MonadRef m => MonadRef (ExceptT e m)

instance MonadRef m => MonadRef (LogicT m)

instance MonadRef m => MonadRef (ReaderT r m)

instance MonadRef m => MonadRef (Lazy.StateT s m)

instance MonadRef m => MonadRef (Strict.StateT s m)

instance MonadRef m => MonadRef (CPS.WriterT w m)
