{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Verse.Run
  ( module Verse.Run.S
  , app
  , alloc
  , read
  , write
  , getLine
  , readInt
  , print
  , plus
  , plus'
  , minus
  , minus'
  , less
  , less'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Function
import Data.Functor
import Data.List
import Data.Ord
import Data.Traversable
import Data.Tuple

import Prettyprinter

import System.IO qualified as IO

import Text.Read (reads)

import Ref

import Verse.Monad
import Verse.Run.S
import Verse.Run.Val qualified as Val

import Prelude (Num (..), ($!))

app
  :: MonadWeakRef m
  => Val.Var m -> S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE app #-}
app var1 s1 s2 var2 = Val.readVar var1 >>= \ case
  Val.Int _ -> stuck
  Val.Char _ -> stuck
  Val.Ptr _ -> stuck
  Val.Lam x f -> f x s1 s2 var2
  Val.Tup vars -> do
    readChoiceFree s1
    var <- asum $ zip [0 ..] vars <&> \ (i, var1) -> do
      Val.unifyVar var2 <=< Val.newVar $ Val.Int i
      pure var1
    unifyS s1 s2
    pure var

alloc
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE alloc #-}
alloc s1 s2 x = do
  unifyChoiceFree s1 s2
  readStoreFree s1
  readHeap
  var <- Val.newVar . Val.Ptr =<< newVarsRef x
  unifyStoreFree s1 s2
  pure var

read
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE read #-}
read s1 s2 x = Val.readVar x >>= \ case
  Val.Ptr x -> do
    unifyChoiceFree s1 s2
    readStoreFree s1
    readHeap
    var <- readVarsRef x
    unifyStoreFree s1 s2
    pure var
  _ -> stuck

write
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE write #-}
write s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  Val.readVar x1 >>= \ case
    Val.Ptr x1 -> do
      unifyChoiceFree s1 s2
      readStoreFree s1
      readHeap
      writeVarsRef x1 x2
      unifyStoreFree s1 s2
      Val.newTup []
    _ -> stuck

getLine
  :: (MonadIO m, MonadWeakRef m)
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE getLine #-}
getLine s1 s2 x = do
  one $ (Val.unifyVar x =<< Val.newTup []) <|> stuck
  unifyChoiceFree s1 s2
  readStoreFree s1
  readHeap
  var <- Val.newString =<< liftIO IO.getLine
  unifyStoreFree s1 s2
  pure var

readInt
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE readInt #-}
readInt s1 s2 x = do
  x <- one $ Val.readString x <|> stuck
  unifyS s1 s2
  Val.newTup <=<
    traverse (uncurry Val.newPair <=< Val.newInt *** Val.newString) $
    reads x

print
  :: (MonadIO m, MonadWeakRef m)
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE print #-}
print s1 s2 x = do
  unifyChoiceFree s1 s2
  readStoreFree s1
  readHeap
  liftIO . IO.print . pretty =<< Val.freeze x
  unifyStoreFree s1 s2
  Val.newTup []

plus
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE plus #-}
plus s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  plus' s1 s2 x1 x2

plus'
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE plus' #-}
plus' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  unifyS s1 s2
  Val.newInt $! x1 + x2

minus
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE minus #-}
minus s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  minus' s1 s2 x1 x2

minus'
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE minus' #-}
minus' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  unifyS s1 s2
  Val.newInt $! x1 - x2

less
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE less #-}
less s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  less' s1 s2 x1 x2

less'
  :: MonadWeakRef m
  => S m -> S m -> Val.Var m -> Val.Var m -> VerseT m (Val.Var m)
{-# INLINABLE less' #-}
less' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  unifyS s1 s2
  guard $! x1 < x2
  pure var1

infixr 3 ***
(***) :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
{-# INLINE (***) #-}
(f *** g) (x, y) = (,) <$> f x <*> g y
