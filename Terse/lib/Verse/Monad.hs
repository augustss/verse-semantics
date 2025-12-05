{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module Verse.Monad
  ( VerseT
  , runVerseT
  , fork
  , yield
  , all'
  , one
  , Stream (..)
  , split
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Data.Functor

import PromptControl

newtype VerseT m a = VerseT
  { unVerseT
    :: forall b c . PromptTag m b
    -> (m c -> m b)
    -> PromptTag m c
    -> m c
    -> m (a, m c)
  }

runVerseT :: MonadPromptControl m => VerseT m a -> m (Maybe [a])
runVerseT m = do
  yt <- newPromptTag
  prompt yt $ Just <$> do
    ft <- newPromptTag
    prompt ft $ uncurry (fmap . (:)) =<< unVerseT m yt yk ft fk
  where
    yk = const $ pure Nothing
    fk = pure []

fork :: MonadPromptControl m => VerseT m () -> VerseT m ()
fork m = VerseT $ \ _yt _yk fk ft -> do
  yt <- newPromptTag
  prompt yt $ unVerseT m yt yk fk ft
  where
    yk fk = pure ((), fk)

yield
  :: MonadPromptControl m
  => ((VerseT m a -> VerseT m ()) -> VerseT m ()) -> VerseT m a
yield f = VerseT $ \ yt yk ft fk ->
  control yt $ \ k ->
    let
      unVerseT' m = unVerseT m yt yk ft fk
      k' m = VerseT $ \ _yt _yk _ft fk -> k (unVerseT' m) $> ((), fk)
    in
      unVerseT' (f k') *> yk fk

one :: MonadPromptControl m => VerseT m a -> VerseT m a
one = split >=> \ case
  Done -> empty
  Step x _ -> pure x

all' :: MonadPromptControl m => VerseT m a -> VerseT m [a]
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> fmap (x:) . loop =<< m

data Stream m a = Done | Step a (VerseT m (Stream m a))

split :: MonadPromptControl m => VerseT m a -> VerseT m (Stream m a)
split m = VerseT $ \ yt yk _ft fk -> do
  ft <- newPromptTag
  fmap (, fk) . prompt ft $
    uncurry Step . fmap lift <$> unVerseT m yt (const $ yk fk) ft fk'
  where
    fk' = pure Done

instance Functor m => Functor (VerseT m) where
  fmap f x = VerseT $ \ yt yk ft fk ->
    unVerseT x yt yk ft fk <&> \ (x, fk) -> (f x, fk)

instance Monad m => Applicative (VerseT m) where
  pure x = VerseT $ \ _yt _yk _ft fk -> pure (x, fk)
  f <*> x = VerseT $ \ yt yk ft fk -> do
    (f, fk) <- unVerseT f yt yk ft fk
    (x, fk) <- unVerseT x yt yk ft fk
    pure (f x, fk)

instance Monad m => Monad (VerseT m) where
  x >>= f = VerseT $ \ yt yk ft fk ->
    unVerseT x yt yk ft fk >>= \ (x, fk) ->
    unVerseT (f x) yt yk ft fk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _yt _yk _ft fk ->
    m <&> \ x -> (x, fk)

instance MonadPromptControl m => Alternative (VerseT m) where
  empty = VerseT $ \ _yt _yk ft fk ->
    control0 ft $ const fk
  x <|> y = VerseT $ \ yt yk ft fk ->
    control ft $ \ k -> k . unVerseT x yt yk ft . k $ unVerseT y yt yk ft fk
