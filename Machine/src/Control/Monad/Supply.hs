module Control.Monad.Supply
  ( module Control.Monad.Supply.Class
  , SupplyT
  , runSupplyT
  , Supply
  , runSupply
  ) where

import Control.Monad.State.Strict
import Control.Monad.Supply.Class

import Data.Functor.Identity

newtype SupplyT s m a = SupplyT
  { unSupplyT :: StateT s m a
  } deriving ( Functor
             , Applicative
             , Monad
             )

runSupplyT :: (Bounded s, Monad m) => SupplyT s m a -> m a
runSupplyT = flip evalStateT minBound . unSupplyT

type Supply s = SupplyT s Identity

runSupply :: Bounded s => Supply s a -> a
runSupply = runIdentity . runSupplyT

instance (Enum s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ state $ \ s -> let s' = succ s in s' `seq` (s', s')
