{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.Stuck
  ( MonadStuck (..)
  ) where

import Control.Monad.Trans.Class

class Monad m => MonadStuck m where
  stuck :: m a
  default stuck :: (m ~ t n, MonadTrans t, MonadStuck n) => m a
  stuck = lift stuck
