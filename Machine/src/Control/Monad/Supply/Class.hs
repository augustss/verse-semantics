{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Supply.Class
  ( MonadSupply (..)
  ) where

import Control.Monad.Except

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  default supply :: (MonadTrans t, MonadSupply s n, t n ~ m) => m s
  supply = lift supply

instance MonadSupply s m => MonadSupply s (ExceptT e m)
