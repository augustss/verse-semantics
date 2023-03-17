{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Var
  ( MonadVar (..)
  ) where

import Control.Monad.RST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.CPS qualified as CPS

import Data.Fix
import Data.Kind

class Monad m => MonadVar m where
  type Var m :: (Type -> Type) -> Type
  type Var m = VarDefault m

  freshVar :: m (Var m f)
  default freshVar :: (m ~ t n, MonadVarTrans t n) => m (Var m f)
  freshVar = lift freshVar

  newVar :: f (Var m f) -> m (Var m f)
  default newVar :: (m ~ t n, MonadVarTrans t n) => f (Var m f) -> m (Var m f)
  newVar = lift . newVar

  readVar :: Var m f -> m (Maybe (f (Var m f)))
  default readVar :: (m ~ t n, MonadVarTrans t n) => Var m f -> m (Maybe (f (Var m f)))
  readVar = lift . readVar

  freezeVar :: Traversable f => Var m f -> m (Maybe (Fix f))
  default freezeVar :: ( m ~ t n
                       , MonadVarTrans t n
                       , Traversable f
                       ) => Var m f -> m (Maybe (Fix f))
  freezeVar = lift . freezeVar

  freshenVar :: Traversable f => Var m f -> m (Var m f)
  default freshenVar :: ( m ~ t n
                        , MonadVarTrans t n
                        , Traversable f
                        ) => Var m f -> m (Var m f)
  freshenVar = lift . freshenVar

type family VarDefault (m :: Type -> Type) :: (Type -> Type) -> Type where
  VarDefault (t n) = Var n

type MonadVarTrans t n = (Var (t n) ~ Var n, MonadTrans t, MonadVar n)

instance MonadVar m => MonadVar (MaybeT m)

instance MonadVar m => MonadVar (ReaderT r m)

instance MonadVar m => MonadVar (RST r s m)

instance MonadVar m => MonadVar (CPS.WriterT w m)
