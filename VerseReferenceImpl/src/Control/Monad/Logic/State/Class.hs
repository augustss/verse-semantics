{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Logic.State.Class
  ( MonadLogicState (..)
  ) where

import Control.Monad.State.Class

class MonadState s m => MonadLogicState s m where
  msplit' :: m a -> s -> m (Maybe (a, s, m a))
