{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Verse.Class
  ( MonadVerse (..)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Unify
import Control.Monad.Var

class (MonadUnify m, MonadRef m) => MonadVerse m where
  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  split :: (Traversable t, Traversable f) =>
           m (t (Var m f)) ->
           (Maybe (t (Var m f), m (t (Var m f))) -> m ()) ->
           m ()

  once' :: (Traversable t, Traversable f) =>
           m (t (Var m f)) ->
           (t (Var m f) -> m ()) ->
           m ()
  once' m f = ifte' m f empty

  lnot' :: (Traversable t, Traversable f) => m (t (Var m f)) -> m ()
  lnot' m = ifte' m (const empty) (pure ())

  ifte' :: (Traversable t, Traversable f) =>
           m (t (Var m f)) ->
           (t (Var m f) -> m ()) ->
           m () ->
           m ()
  ifte' m f n = split m $ \ case
    Just (x, _) -> f x
    Nothing -> n

  for' :: (Traversable t, Traversable f) =>
          m (t (Var m f)) ->
          (t (Var m f) -> m (Var m f)) ->
          ([Var m f] -> m ()) ->
          m ()
  for' m f g = split m $ \ case
    Just (x, m) -> f x >>= \ y -> for' m f $ \ ys -> g $ y : ys
    Nothing -> g []

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ flip runReaderT r . f . fmap (fmap lift)
