{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples #-}
module Verse.Monad
  ( Verse
  , runVerse
  , fork
  , yield
  , all'
  , one
  , split
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import GHC.Exts
import GHC.IO (IO (..))

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO $ \ s -> case newPromptTag# s of
  (# s, x #) -> (# s, PromptTag x #)

prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag x) (IO f) = IO $ prompt# x f

control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag x) f = IO $ control0# x $ \ k s ->
  case f $ \ (IO f) -> IO $ k f of
    IO f -> f s

newtype Verse a = Verse
  { unVerse
    :: Level
    -> forall b . PromptTag b
    -> IO b
    -> IntMap (PromptTag ())
    -> forall c . PromptTag c
    -> (c -> IO c -> IO c)
    -> IO c
    -> IO a
  }

newtype Level = Level Int deriving (Eq, Ord, Num)

newtype Count = Count Int deriving (Eq, Num)

runVerse :: Verse a -> IO (Maybe [a])
runVerse m = do
  yt <- newPromptTag
  prompt yt $ Just <$> do
    ft <- newPromptTag
    prompt ft $ (:[]) <$>
      unVerse m level yt yk yts ft sk fk
  where
    level = 1
    yk = pure Nothing
    yts = mempty
    sk = fmap . (<>)
    fk = pure mempty

fork :: Verse () -> Verse ()
fork m = Verse $ \ level@(Level i) yt yk yts ft sk fk -> do
  yt' <- newPromptTag
  prompt yt' $ unVerse m level yt yk (IntMap.insert i yt' yts) ft sk fk

yield :: Level -> ((Verse a -> Verse ()) -> Verse ()) -> Verse a
yield (Level i) f = Verse $ \ level yt yk yts ft sk fk ->
  let
    unVerse' m = unVerse m level yt yk yts ft sk fk
  in
    case IntMap.lookupLE i yts of
      Nothing -> control0 yt $ \ k ->
        unVerse' (f $ \ m -> liftIO . void . prompt yt . k $ unVerse' m) *> yk
      Just (_, yt) -> control0 yt $ \ k ->
        unVerse' $ f $ \ m -> liftIO . prompt yt . k $ unVerse' m

data Stream a = Done | Step a (Verse (Stream a))

singleton :: a -> Stream a
singleton x = Step x $ pure Done

all' :: Verse a -> Verse [a]
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> fmap (x:) . loop =<< m

one :: Verse a -> Verse a
one = split >=> \ case
  Done -> empty
  Step x _m -> pure x

split :: Verse a -> Verse (Stream a)
split m = Verse $ \ level yt yts yk _ft _sk _fk -> do
  let !level' = level + 1
  ft' <- newPromptTag
  prompt ft' $ singleton <$> unVerse m level' yt yts yk ft' sk' fk'
  where
    sk' x m = case x of
      Done -> m
      Step x n -> pure . Step x $ n >>= liftIO . (`sk'` m)
    fk' =
      pure Done

instance Functor Verse where
  fmap f x = Verse $ \ level yt yts yk ft sk ->
    fmap f . unVerse x level yt yts yk ft sk

instance Applicative Verse where
  pure x = Verse $ \ _level _yt _yts _yk _ft _sk _fk ->
    pure x
  f <*> x = Verse $ \ level yt yts yk ft sk fk ->
    unVerse f level yt yts yk ft sk fk <*>
    unVerse x level yt yts yk ft sk fk

instance Alternative Verse where
  empty = Verse $ \ _level _yt _yts _yk ft _sk fk ->
    control0 ft $ const fk
  x <|> y = Verse $ \ level yt yts yk ft sk fk -> control0 ft $ \ k -> do
    x <- prompt ft . k $ unVerse x level yt yts yk ft sk fk
    sk x (prompt ft . k $ unVerse y level yt yts yk ft sk fk)

instance Monad Verse where
  x >>= f = Verse $ \ level yt yk yts ft sk fk ->
    unVerse x level yt yk yts ft sk fk >>= \ x ->
    unVerse (f x) level yt yk yts ft sk fk

instance MonadPlus Verse

instance MonadIO Verse where
  liftIO x = Verse $ \ _ _ _ _ _ _ _ -> x
