module Verse.Run.Heap
  ( Heap
  , freshHeap
  , newHeap
  , readHeap
  , unifyHeap
  ) where

import Control.Monad.Reader

import Ref

import Verse.Monad

type Heap m = Var m ()

freshHeap :: MonadRef m => VerseT m (Heap m)
{-# INLINE freshHeap #-}
freshHeap = freshVar

newHeap :: MonadWeakRef m => Var m () -> VerseT m (Heap m)
{-# INLINABLE newHeap #-}
newHeap s = fork1 $ readVar s *> ask

readHeap :: MonadWeakRef m => VerseT m ()
{-# INLINE readHeap #-}
readHeap = readVar =<< ask

unifyHeap :: MonadWeakRef m => Heap m -> Heap m -> VerseT m ()
{-# INLINE unifyHeap #-}
unifyHeap = unifyVar
