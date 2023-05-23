{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Logic.State.Class
  ( MonadLogicState (..)
  ) where

import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

class MonadState s m => MonadLogicState s m where
  msplit' :: m a -> s -> m (Maybe (a, s, m a))

instance MonadLogicState s m => MonadLogicState s (ReaderT r m) where
  msplit' m s = ReaderT $ \ r -> msplit' (runReaderT m r) s >>= \ case
    Nothing -> pure Nothing
    Just (x, s, m) -> pure $ Just (x, s, lift m)
