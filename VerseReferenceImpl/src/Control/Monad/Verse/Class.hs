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
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Lenient qualified as Lenient
import Control.Monad.Supply
import Control.Monad.Trans.Verse (VerseT)
import Control.Monad.Trans.Verse qualified as Verse
import Control.Monad.Var

import Data.Fix
import Data.Ref
import Data.Unifiable

class (MonadPlus m, MonadVar m, Lenient.MonadRef m) => MonadVerse m where
  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  unify :: Unifiable f => Var m f -> Var m f -> m ()
  default unify :: ( m ~ t n
                   , MonadVerseTrans t n
                   , Unifiable f
                   ) => Var m f -> Var m f -> m ()
  unify x y = lift $ unify x y

  freeze :: Traversable f => Var m f -> m (Maybe (Fix f))
  freeze = freezeBy id

  freezeBy :: Traversable g => (forall a . f a -> g a) -> Var m f -> m (Maybe (Fix g))
  default freezeBy :: ( m ~ t n
                      , MonadVerseTrans t n
                      , Traversable g
                      ) => (forall a . f a -> g a) -> Var m f -> m (Maybe (Fix g))
  freezeBy f = lift . freezeBy f

  freshen :: Traversable f => Var m f -> m (Var m f)
  default freshen :: ( m ~ t n
                     , MonadVerseTrans t n
                     , Traversable f
                     ) => Var m f -> m (Var m f)
  freshen = lift . freshen

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

type MonadVerseTrans t n = (Var (t n) ~ Var n, MonadTrans t, MonadVerse n)

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Verse.Label m
         , EqRef (Ref m)
         ) => MonadVerse (VerseT m) where
  whenBound = Verse.whenBound
  unify = Verse.unify
  freezeBy = Verse.freezeBy
  freshen = Verse.freshen
  split = Verse.split

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ flip runReaderT r . f . fmap (fmap lift)
