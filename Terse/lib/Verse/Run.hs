{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Verse.Run
  ( module Verse.Run.Heap
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
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Data.Tuple
import Data.Vector qualified as Vector

import GHC.Exts (IsList (..))

import Prettyprinter

import System.IO qualified as IO

import Text.Read (reads)

import Ref

import Verse.Monad
import Verse.Run.Heap
import Verse.Run.Val qualified as Val

import Prelude (Num (..), ($!), (&&), fromIntegral)

app
  :: MonadWeakRef m
  => Val.Var m -> Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE app #-}
app var1 s1 s2 var2 = Val.readVar var1 >>= \ case
  Val.Int _ -> stuck
  Val.Char _ -> stuck
  Val.Ptr _ -> stuck
  Val.Lam x f -> f x s1 s2 var2
  Val.Tup vars -> do
    readVar s1
    Val.readVar' var2 >>= \ case
      Just x -> case x of
        Val.Int x | 0 <= x && x <= fromIntegral (Vector.length vars) ->
          pure (s1, s2, Vector.unsafeIndex vars $ fromIntegral x)
        _ -> empty
      Nothing ->
        fmap (s1, s2, ) . asum $ zip [0 ..] (toList vars) <&> \ (i, var1) -> do
          Val.unifyVar var2 <=< Val.newVar $ Val.Int i
          pure var1

alloc
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE alloc #-}
alloc s1 s2 x = do
  readVar s2
  readHeap
  fmap (s1, s2, ) . Val.newVar . Val.Ptr =<< newVarsRef x

read
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE read #-}
read s1 s2 x = Val.readVar x >>= \ case
  Val.Ptr x -> do
    readVar s2
    readHeap
    (s1, s2, ) <$> readVarsRef x
  _ -> stuck

write
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE write #-}
write s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  Val.readVar x1 >>= \ case
    Val.Ptr x1 -> do
      readVar s2
      readHeap
      writeVarsRef x1 x2
      (s1, s2, ) <$> Val.newTup mempty
    _ -> stuck

getLine
  :: (MonadIO m, MonadWeakRef m)
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE getLine #-}
getLine s1 s2 x = do
  one $ (Val.unifyVar x =<< Val.newTup mempty) <|> stuck
  readVar s2
  readHeap
  fmap (s1, s2, ) . Val.newString =<< liftIO IO.getLine

readInt
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE readInt #-}
readInt s1 s2 x = do
  x <- one $ Val.readString x <|> stuck
  fmap (s1, s2, ) . Val.newTup . fromList <=<
    traverse (uncurry Val.newPair <=< Val.newInt *** Val.newString) $
    reads x

print
  :: (MonadIO m, MonadWeakRef m)
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE print #-}
print s1 s2 x = do
  readVar s2
  readHeap
  liftIO . IO.print . pretty =<< Val.freeze x
  (s1, s2, ) <$> Val.newTup mempty

plus
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE plus #-}
plus s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  plus' s1 s2 x1 x2

plus'
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE plus' #-}
plus' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  fmap (s1, s2, ) . Val.newInt $! x1 + x2

minus
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE minus #-}
minus s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  minus' s1 s2 x1 x2

minus'
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE minus' #-}
minus' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  fmap (s1, s2, ) . Val.newInt $! x1 - x2

less
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE less #-}
less s1 s2 x = do
  (x1, x2) <- one $ Val.readPair x <|> stuck
  less' s1 s2 x1 x2

less'
  :: MonadWeakRef m
  => Var m () -> Var m () -> Val.Var m
  -> Val.Var m -> VerseT m (Var m (), Var m (), Val.Var m)
{-# INLINABLE less' #-}
less' s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> Val.readInt var1 <*> Val.readInt var2 <|> stuck
  guard $! x1 < x2
  pure (s1, s2, var1)

infixr 3 ***
(***) :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
{-# INLINE (***) #-}
(f *** g) (x, y) = (,) <$> f x <*> g y
