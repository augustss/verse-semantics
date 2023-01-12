{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Control.Monad.Stuck
  ( MonadStuck (..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict qualified as Strict

class Monad m => MonadStuck m where
  stuck :: m a
  default stuck :: (m ~ t n, MonadTrans t, MonadStuck n) => m a
  stuck = lift stuck

instance MonadStuck m => MonadStuck (Strict.StateT s m)
