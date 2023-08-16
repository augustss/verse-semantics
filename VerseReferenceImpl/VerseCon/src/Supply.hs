{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Supply
  ( MonadSupply (..)
  , SupplyT
  , runSupplyT
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.State.Strict qualified as Strict

import Ref

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  default supply :: (m ~ t n, MonadTrans t, MonadSupply s n) => m s
  supply = lift supply

instance MonadSupply s m => MonadSupply s (Strict.StateT s' m)

newtype SupplyT s m a = SupplyT
  { unSupplyT :: Strict.StateT s m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             , MonadTrans
             )

instance MonadRef m => MonadRef (SupplyT s m)

runSupplyT :: (Bounded s, Monad m) => SupplyT s m a -> m a
runSupplyT = flip Strict.evalStateT minBound . unSupplyT

instance (Enum s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT . state $ \ s -> let s' = succ s in s' `seq` (s, s')

instance MonadSupply s m => MonadSupply s (ReaderT r m)
