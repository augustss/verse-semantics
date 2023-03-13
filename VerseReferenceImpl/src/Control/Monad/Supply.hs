{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Supply
  ( MonadSupply (..)
  , SupplyT
  , runSupplyT
  , Supply
  , runSupply
  ) where

import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Class
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Writer.CPS qualified as CPS

import Data.Functor.Identity

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  default supply :: (m ~ t n, MonadTrans t, MonadSupply s n) => m s
  supply = lift supply

instance MonadSupply s m => MonadSupply s (LogicT m)

instance MonadSupply s m => MonadSupply s (ReaderT r m)

instance MonadSupply s m => MonadSupply s (Lazy.StateT s' m)

instance MonadSupply s m => MonadSupply s (Strict.StateT s' m)

instance MonadSupply s m => MonadSupply s (CPS.WriterT w m)

newtype SupplyT s m a = SupplyT
  { unSupplyT :: Strict.StateT s m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             , MonadTrans
             , MonadIO
             , MonadRef
             )

type Supply s = SupplyT s Identity

deriving instance MonadError e m => MonadError e (SupplyT s m)

deriving instance MonadFix m => MonadFix (SupplyT s m)

instance (Enum s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ state $ \ s -> (s, succ s)

runSupplyT :: (Bounded s, Monad m) => SupplyT s m a -> m a
runSupplyT = flip Strict.evalStateT minBound . unSupplyT

runSupply :: Bounded s => Supply s a -> a
runSupply = runIdentity . runSupplyT
