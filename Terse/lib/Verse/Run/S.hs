{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Verse.Run.S
  ( S
  , Heap
  , freshS
  , newS
  , unifyS
  , readS
  , readChoiceFree
  , readStoreFree
  , unifyChoiceFree
  , unifyStoreFree
  , freshHeap
  , newHeap
  , readHeap
  , unifyHeap
  ) where

import Control.Monad.Reader

import Ref

import Verse.Monad

data S m = S
  { choiceFree :: !(Var m ())
  , storeFree :: !(Var m ())
  }

type Heap m = Var m ()

freshS :: MonadRef m => VerseT m (S m)
{-# INLINE freshS #-}
freshS = do
  choiceFree <- freshVar
  storeFree <- freshVar
  pure S {..}

newS :: VerseT m (S m)
{-# INLINE newS #-}
newS = do
  choiceFree <- newVar ()
  storeFree <- newVar ()
  pure S {..}

unifyS :: MonadWeakRef m => S m -> S m -> VerseT m ()
{-# INLINE unifyS #-}
unifyS s1 s2 = unifyChoiceFree s1 s2 *> unifyStoreFree s1 s2

readS :: MonadWeakRef m => S m -> VerseT m ()
{-# INLINE readS #-}
readS s = readChoiceFree s *> readStoreFree s

readChoiceFree :: MonadWeakRef m => S m -> VerseT m ()
{-# INLINE readChoiceFree #-}
readChoiceFree = readVar . (.choiceFree)

readStoreFree :: MonadWeakRef m => S m -> VerseT m ()
{-# INLINE readStoreFree #-}
readStoreFree = readVar . (.storeFree)

unifyChoiceFree :: MonadWeakRef m => S m -> S m -> VerseT m ()
{-# INLINE unifyChoiceFree #-}
unifyChoiceFree s1 s2 = unifyVar s1.choiceFree s2.choiceFree

unifyStoreFree :: MonadWeakRef m => S m -> S m -> VerseT m ()
{-# INLINE unifyStoreFree #-}
unifyStoreFree s1 s2 = unifyVar s1.storeFree s2.storeFree

freshHeap :: MonadRef m => VerseT m (Heap m)
{-# INLINE freshHeap #-}
freshHeap = freshVar

newHeap :: MonadWeakRef m => S m -> VerseT m (Heap m)
{-# INLINABLE newHeap #-}
newHeap s1 = do
  heap <- freshHeap
  fork $ do
    readStoreFree s1
    unifyHeap heap =<< ask
  pure heap

readHeap :: MonadWeakRef m => VerseT m ()
{-# INLINE readHeap #-}
readHeap = readVar =<< ask

unifyHeap :: MonadWeakRef m => Heap m -> Heap m -> VerseT m ()
{-# INLINE unifyHeap #-}
unifyHeap = unifyVar
