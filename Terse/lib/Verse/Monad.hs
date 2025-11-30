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
  , split
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

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
    :: forall b . PromptTag b
    -> IO b
    -> forall c . PromptTag c
    -> (c -> IO c -> IO c)
    -> IO c
    -> IO a
  }

newtype Count = Count Int deriving (Eq, Num)

runVerse :: Verse a -> IO (Maybe [a])
runVerse m = do
  yt <- newPromptTag
  prompt yt $ Just <$> do
    ft <- newPromptTag
    prompt ft $ (:[]) <$>
      unVerse m yt (pure Nothing) ft (fmap . (<>)) (pure mempty)

fork :: Verse () -> Verse ()
fork m = Verse $ \ _ _ ft sk fk -> do
  yt <- newPromptTag
  prompt yt $ unVerse m yt (pure ()) ft sk fk

yield :: ((Verse a -> Verse ()) -> Verse ()) -> Verse a
yield f = Verse $ \ yt yk ft sk fk ->
  control0 yt $ \ k ->
    unVerse
    (f $ \ m -> liftIO . void . prompt yt . k $ unVerse m yt yk ft sk fk)
    yt yk ft sk fk *>
    yk

data Stream a = Done | Step a (Verse (Stream a))

singleton :: a -> Stream a
singleton x = Step x $ pure Done

all' :: Verse a -> Verse [a]
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> fmap (x:) . loop =<< m

split :: Verse a -> Verse (Stream a)
split m = Verse $ \ yt yk _ft _sk _fk -> do
  ft' <- newPromptTag
  prompt ft' $ singleton <$> unVerse m yt yk ft' sk' fk'
  where
    sk' x m = case x of
      Done -> m
      Step x n -> pure . Step x $ n >>= liftIO . (`sk'` m)
    fk' =
      pure Done

instance Functor Verse where
  fmap f x = Verse $ \ yt yk ft sk -> fmap f . unVerse x yt yk ft sk

instance Applicative Verse where
  pure x = Verse $ \ _ _ _ _ _ ->
    pure x
  f <*> x = Verse $ \ yt yk ft sk fk ->
    unVerse f yt yk ft sk fk <*> unVerse x yt yk ft sk fk

instance Alternative Verse where
  empty = Verse $ \ _ _ ft _ fk ->
    control0 ft $ const fk
  x <|> y = Verse $ \ yt yk ft sk fk -> control0 ft $ \ k -> do
    x <- prompt ft . k $ unVerse x yt yk ft sk fk
    sk x (prompt ft . k $ unVerse y yt yk ft sk fk)

instance Monad Verse where
  x >>= f = Verse $ \ yt yk ft sk fk ->
    unVerse x yt yk ft sk fk >>= \ x -> unVerse (f x) yt yk ft sk fk

instance MonadPlus Verse

instance MonadIO Verse where
  liftIO x = Verse $ \ _ _ _ _ _ -> x
