{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Verse.Run
  ( S
  , Heap
  , freshS
  , newS
  , unifyS
  , readChoiceFree
  , readStoreFree
  , unifyChoiceFree
  , unifyStoreFree
  , freshHeap
  , newHeap
  , readHeap
  , unifyHeap
  , plus
  , minus
  , less
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Ref

import Verse.Monad
import Verse.Run.Val (newInteger, readInteger)
import Verse.Run.Val qualified as Val

data S m = S
  { choiceFree :: !(Var m ())
  , storeFree :: !(Var m ())
  }

type Heap m = Var m ()

freshS :: MonadRef m => VerseT m (S m)
freshS = do
  choiceFree <- freshVar
  storeFree <- freshVar
  pure S {..}

newS :: MonadRef m => VerseT m (S m)
newS = do
  choiceFree <- newVar ()
  storeFree <- newVar ()
  pure S {..}

unifyS :: MonadRef m => S m -> S m -> VerseT m ()
unifyS s1 s2 = unifyChoiceFree s1 s2 *> unifyStoreFree s1 s2

readChoiceFree :: MonadRef m => S m -> VerseT m ()
readChoiceFree = readVar . (.choiceFree)

readStoreFree :: MonadRef m => S m -> VerseT m ()
readStoreFree = readVar . (.storeFree)

unifyChoiceFree :: MonadRef m => S m -> S m -> VerseT m ()
unifyChoiceFree s1 s2 = unifyVar s1.choiceFree s2.choiceFree

unifyStoreFree :: MonadRef m => S m -> S m -> VerseT m ()
unifyStoreFree s1 s2 = unifyVar s1.storeFree s2.storeFree

freshHeap :: MonadRef m => VerseT m (Heap m)
freshHeap = freshVar

newHeap :: MonadRef m => S m -> VerseT m (Heap m)
newHeap s1 = do
  heap <- freshHeap
  fork $ do
    readStoreFree s1
    unifyHeap heap =<< ask
  pure heap

readHeap :: MonadRef m => VerseT m ()
readHeap = readVar =<< ask

unifyHeap :: MonadRef m => Heap m -> Heap m -> VerseT m ()
unifyHeap = unifyVar

plus
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
plus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $! x1 + x2

minus
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
minus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $! x1 - x2

less
  :: MonadRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
less s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  guard $! x1 < x2
  pure var1
