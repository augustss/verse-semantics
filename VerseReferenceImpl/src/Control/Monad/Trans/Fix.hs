{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Trans.Fix
  ( FixT
  , runFixT
  , Fix
  , runFix
  ) where

import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Writer

import Data.Functor.Identity

newtype FixT s m a = FixT { getFixT :: s -> s -> m (a, s) }

runFixT :: (MonadFix m, Monoid s) => FixT s m a -> m (a, s)
runFixT m = mfix $ \ ~(_, r) -> getFixT m r mempty

type Fix s = FixT s Identity

runFix :: Monoid s => Fix s a -> (a, s)
runFix = runIdentity . runFixT

instance Functor m => Functor (FixT s m) where
  fmap f m = FixT $ \ r w -> fmap (\ (x, w) -> (f x, w)) (getFixT m r w)

instance Monad m => Applicative (FixT s m) where
  pure x = FixT $ \ _ w -> pure (x, w)
  f <*> x = FixT $ \ r w -> do
    (f, w) <- getFixT f r w
    (x, w) <- getFixT x r w
    pure (f x, w)

instance Monad m => Monad (FixT s m) where
  m >>= f = FixT $ \ r w -> do
    (x, w) <- getFixT m r w
    getFixT (f x) r w

instance Monad m => MonadReader s (FixT s m) where
  ask = FixT $ \ r w -> pure (r, w)
  local f m = FixT $ \ r w -> getFixT m (f r) w

instance (Monoid s, Monad m) => MonadWriter s (FixT s m) where
  tell w = FixT $ \ _ w' ->
    let w'' = w' <> w in w'' `seq` pure ((), w'')
  listen m = FixT $ \ r w -> do
    (x, w') <- getFixT m r mempty
    let w'' = w <> w'
    w'' `seq` pure ((x, w'), w'')
  pass m = FixT $ \ r w -> do
    ((x, f), w') <- getFixT m r mempty
    let w'' = w <> f w'
    w'' `seq` pure (x, w'')
