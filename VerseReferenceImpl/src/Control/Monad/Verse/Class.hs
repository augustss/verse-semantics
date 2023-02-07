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
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Unify
import Control.Monad.Var

import Data.Kind
import Data.Unifiable

class (MonadUnify m, Backtrack.MonadRef m, Unifiable (World m)) => MonadVerse m where
  type World m :: Type -> Type
  type World m = WorldDefault m

  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  getWorld :: m (Var m (World m))
  default getWorld :: (m ~ t n, MonadVerseTrans t n) => m (Var m (World m))
  getWorld = lift getWorld

  putWorld :: Var m (World m) -> m ()
  default putWorld :: (m ~ t n, MonadVerseTrans t n) => Var m (World m) -> m ()
  putWorld = lift . putWorld

  split :: m a -> (Maybe (a, m a) -> m ()) -> m ()

  once' :: m a -> (a -> m ()) -> m ()
  once' m f = ifte' m f empty

  lnot' :: m a -> m ()
  lnot' m = ifte' m (const empty) (pure ())

  ifte' :: m a -> (a -> m ()) -> m () -> m ()
  ifte' m f n = split m $ \ case
    Just (x, _) -> f x
    Nothing -> n

  for' :: m a -> (a -> m b) -> ([b] -> m ()) -> m ()
  for' m f g = split m $ \ case
    Just (x, m) -> f x >>= \ y -> for' m f $ \ ys -> g $ y : ys
    Nothing -> g []

type family WorldDefault (m :: Type -> Type) :: Type -> Type where
  WorldDefault (t n) = World n

type MonadVerseTrans t n =
  ( Var (t n) ~ Var n
  , World (t n) ~ World n
  , MonadTrans t
  , MonadVerse n
  )

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ flip runReaderT r . f . fmap (fmap lift)
