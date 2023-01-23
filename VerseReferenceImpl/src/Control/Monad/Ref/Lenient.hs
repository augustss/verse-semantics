{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Ref.Lenient
  ( MonadRef (..)
  , modifyRef
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Data.Kind

class Monad m => MonadRef m where
  type Ref m :: Type -> Type
  type Ref m = RefDefault m

  newRef :: a -> m (Ref m a)
  default newRef :: (m ~ t n, MonadRefTrans t n) => a -> m (Ref m a)
  newRef = lift . newRef

  readRef :: Ref m a -> (a -> m ()) -> m ()

  writeRef :: Ref m a -> a -> m ()
  default writeRef :: (m ~ t n, MonadRefTrans t n) => Ref m a -> a -> m ()
  writeRef ref = lift . writeRef ref

type family RefDefault (m :: Type -> Type) :: Type -> Type where
  RefDefault (t n) = Ref n

type MonadRefTrans t n = (Ref (t n) ~ Ref n, MonadTrans t, MonadRef n)

modifyRef :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef ref f = readRef ref $ writeRef ref . f

instance MonadRef m => MonadRef (ReaderT r m) where
  readRef ref f = ReaderT $ \ r -> readRef ref $ \ x -> runReaderT (f x) r
