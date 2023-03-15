{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Verse.Class
  ( MonadVerse (..)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RST
import Control.Monad.Unify
import Control.Monad.Var

import Data.Functor.Identity
import Data.Proxy

class (MonadUnify m, MonadRef m) => MonadVerse m where
  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  split :: (Traversable t, Traversable f) =>
           m (t (Var m f)) ->
           (Maybe (t (Var m f), m (t (Var m f))) -> m ()) ->
           m ()

  ifte' :: (Traversable t, Traversable f) =>
           m (t (Var m f)) ->
           (t (Var m f) -> m ()) ->
           m () ->
           m ()
  ifte' m f n = split m $ \ case
    Just (x, _) -> f x
    Nothing -> n

  once' :: Traversable f => m (Var m f) -> (Var m f -> m ()) -> m ()
  once' m f = ifte' (Identity <$> m) (f . runIdentity) empty

  lnot' :: m a -> m ()
  lnot' m = ifte' (proxy <$ m) (const empty) (pure ())
    where
      proxy = Proxy :: Proxy (Var m Proxy)

  for' :: (Traversable t, Traversable f) =>
          m (t (Var m f)) ->
          (t (Var m f) -> m (Var m f)) ->
          ([Var m f] -> m ()) ->
          m ()
  for' m f g = split m $ \ case
    Just (x, m) -> f x >>= \ y -> for' m f $ \ ys -> g $ y : ys
    Nothing -> g []

  all' :: Traversable f => m (Var m f) -> ([Var m f] -> m ()) -> m ()
  all' m = for' (Identity <$> m) (pure . runIdentity)

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ flip runReaderT r . f . fmap (fmap lift)

instance MonadVerse m => MonadVerse (RST r s m) where
  whenBound x f = RST $ \ r s -> do
    whenBound x $ \ x -> evalRST (f x) r s
    pure ((), s)
  split m f = RST $ \ r s -> do
    split (evalRST m r s) $ \ case
      Nothing -> evalRST (f Nothing) r s
      Just (x, m) -> evalRST (f $ Just (x, lift m)) r s
    pure ((), s)
