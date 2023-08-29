{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.RS
  ( RST (..)
  , evalRST
  , execRST
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Abort
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Logic.Class
import Control.Monad.Reader.Class
import Control.Monad.Ref
import Control.Monad.State.Class
import Control.Monad.Supply
import Control.Monad.Trans.Class

import Data.Functor

newtype RST r s m a = RST { runRST :: r -> s -> m (a, s) }

evalRST :: Functor m => RST r s m a -> r -> s -> m a
evalRST x r = fmap fst . runRST x r

execRST :: Functor m => RST r s m a -> r -> s -> m s
execRST x r = fmap snd . runRST x r

instance Functor m => Functor (RST r s m) where
  fmap f m = RST $ \ r s ->
    (\ (a, s) -> (f a, s)) <$> runRST m r s

instance Monad m => Applicative (RST r s m) where
  pure a = RST $ \ _ s ->
    pure (a, s)
  RST f <*> RST x = RST $ \ r s ->
    f r s >>= \ (f, s) -> x r s <&> \ (x, s) -> (f x, s)

instance (Alternative m, Monad m) => Alternative (RST r s m) where
  empty = lift empty
  x <|> y = RST $ \ r s -> runRST x r s <|> runRST y r s

instance (Alternative m, Monad m) => MonadPlus (RST r s m) where
  mzero = empty
  mplus = (<|>)

instance Monad m => Monad (RST r s m) where
  x >>= f = RST $ \ r s ->
    runRST x r s >>= \ (x, s) -> runRST (f x) r s

instance MonadFail m => MonadFail (RST r s m) where
  fail = lift . fail

instance MonadError e m => MonadError e (RST r s m) where
  throwError = lift . throwError
  x `catchError` f = RST $ \ r s ->
    runRST x r s `catchError` \ e -> runRST (f e) r s

instance MonadLogic m => MonadLogic (RST r s m) where
  msplit m = RST $ \ r s -> msplit (runRST m r s) >>= \ case
    Nothing -> pure (Nothing, s)
    Just ((a, s'), m) -> pure (Just (a, RST $ \ _ _ -> m), s')

instance Monad m => MonadReader r (RST r s m) where
  ask = RST $ \ r s -> pure (r, s)
  local f x = RST $ \ r -> runRST x (f r)
  reader f = RST $ \ r s -> pure (f r, s)

instance Monad m => MonadState s (RST r s m) where
  get = RST $ \ _ s -> pure (s, s)
  put s = RST $ \ _ _ -> pure ((), s)
  state f = RST $ \ _ s -> pure $ f s

instance MonadTrans (RST r s) where
  lift m = RST $ \ _ s -> (, s) <$> m

instance MonadFix m => MonadFix (RST r s m) where
  mfix f = RST $ \ r s -> mfix $ \ ~(x, _) -> runRST (f x) r s

instance MonadIO m => MonadIO (RST r s m) where
  liftIO m = RST $ \ _ s -> (, s) <$> liftIO m

instance MonadAbort e m => MonadAbort e (RST r s m)

instance MonadRef m => MonadRef (RST r s m)

instance MonadSupply s m => MonadSupply s (RST r s' m)
