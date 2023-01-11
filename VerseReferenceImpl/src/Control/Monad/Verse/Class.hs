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
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Trans.Verse (VerseT)
import Control.Monad.Trans.Verse qualified as Verse
import Control.Monad.Var

import Data.Fix
import Data.Ref
import Data.Unifiable

type MonadVerseTrans t n = (Var (t n) ~ Var n, MonadTrans t, MonadVerse n)

class (Alternative m, MonadVar m) => MonadVerse m where
  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  split :: m a -> (Maybe (a, m a) -> m ()) -> m ()

  once' :: m a -> (a -> m ()) -> m ()

  lnot' :: m a -> m ()

  ifte' :: m a -> (a -> m ()) -> m () -> m ()

  all' :: Traversable f => m (Var m f) -> ([Var m f] -> m ()) -> m ()

  for' :: Traversable f => m a -> (a -> m (Var m f)) -> ([Var m f] -> m ()) -> m ()

  unify :: Unifiable f => Var m f -> Var m f -> m ()
  default unify :: ( m ~ t n
                   , MonadVerseTrans t n
                   , Unifiable f
                   ) => Var m f -> Var m f -> m ()
  unify x y = lift $ unify x y

  freeze :: Traversable f => Var m f -> m (Maybe (Fix f))
  default freeze :: ( m ~ t n
                    , MonadVerseTrans t n
                    , Traversable f
                    ) => Var m f -> m (Maybe (Fix f))
  freeze = lift . freeze

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Verse.Label m
         , EqRef (Ref m)
         ) => MonadVerse (VerseT m) where
  whenBound = Verse.whenBound
  split = Verse.split
  once' = Verse.once'
  lnot' = Verse.lnot'
  ifte' = Verse.ifte'
  all' = Verse.all'
  for' = Verse.for'
  unify = Verse.unify
  freeze = Verse.freeze

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ flip runReaderT r . f . fmap (fmap lift)
  once' m f = ReaderT $ \ r ->
    once' (runReaderT m r) $ flip runReaderT r . f
  lnot' m = ReaderT $ lnot' . runReaderT m
  ifte' p t e = ReaderT $ \ r ->
    ifte' (runReaderT p r) (flip runReaderT r . t) (runReaderT e r)
  all' m f = ReaderT $ \ r ->
    all' (runReaderT m r) $ flip runReaderT r . f
  for' m f g = ReaderT $ \ r ->
    for' (runReaderT m r) (flip runReaderT r . f) (flip runReaderT r . g)
