{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Var
  ( MonadVar (..)
  , MonadVarRef (..)
  , EqVarRef (..)
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

class MonadVar m => MonadVarRef m where
  type VarRef m :: (Type -> Type) -> Type
  type VarRef m = VarRefDefault m

  newVarRef :: Traversable f => Var m f -> m (VarRef m f)
  default newVarRef :: ( m ~ t n
                       , MonadVarRefTrans t n
                       , Traversable f
                       ) => Var m f -> m (VarRef m f)
  newVarRef = lift . newVarRef

  readVarRef :: VarRef m f -> m (Var m f)
  default readVarRef :: (m ~ t n, MonadVarRefTrans t n) => VarRef m f -> m (Var m f)
  readVarRef = lift . readVarRef

  writeVarRef :: Traversable f => VarRef m f -> Var m f -> m ()
  default writeVarRef :: ( m ~ t n
                         , MonadVarRefTrans t n
                         , Traversable f
                         ) => VarRef m f -> Var m f -> m ()
  writeVarRef ref = lift . writeVarRef ref

type family VarRefDefault (m :: Type -> Type) :: (Type -> Type) -> Type where
  VarRefDefault (t n) = VarRef n

type MonadVarRefTrans t n =
  ( Var (t n) ~ Var n
  , VarRef (t n) ~ VarRef n
  , MonadTrans t
  , MonadVarRef n
  )

instance MonadVarRef m => MonadVarRef (MaybeT m)

instance MonadVarRef m => MonadVarRef (ReaderT r m)

instance MonadVarRef m => MonadVarRef (RST r s m)

instance MonadVarRef m => MonadVarRef (CPS.WriterT w m)

class EqVarRef (r :: (Type -> Type) -> Type) where
  eqVarRef :: r f -> r f -> Bool
