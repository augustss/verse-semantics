{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Ref.Backtrack
  ( MonadRef (..)
  , modifyRef
  ) where

import Control.Monad.Trans.Class

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

  backtrack :: m ()
  default backtrack :: (m ~ t n, MonadRefTrans t n) => m ()
  backtrack = lift backtrack

type family RefDefault (m :: Type -> Type) :: Type -> Type where
  RefDefault (t n) = Ref n

type MonadRefTrans t n = (Ref (t n) ~ Ref n, MonadTrans t, MonadRef n)

modifyRef :: MonadRef m => Ref m a -> (a -> a) -> m ()
modifyRef ref f = writeRef ref . f =<< readRef ref
