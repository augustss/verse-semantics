{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Monad.Verse.Class
  ( MonadVerse (..)
  ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.RST
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Unify
import Control.Monad.Var

import Data.Freshenable

class (MonadUnify m, MonadVarRef m) => MonadVerse m where
  whenBound :: Var m f -> (f (Var m f) -> m ()) -> m ()

  split :: Freshenable f =>
           m (f (Var m)) ->
           (Maybe (f (Var m), m (f (Var m))) -> m ()) ->
           m ()

  ifte' :: Freshenable f => m (f (Var m)) -> (f (Var m) -> m ()) -> m () -> m ()
  ifte' m f n = split m $ \ case
    Just (x, _) -> f x
    Nothing -> n

  once' :: Freshenable f => m (f (Var m)) -> (f (Var m) -> m ()) -> m ()
  once' m f = ifte' m f empty

  lnot' :: m a -> m ()
  lnot' m = ifte' (Unit1 <$ m) (const empty) (pure ())

  for' :: (Freshenable f, Freshenable g) =>
          m (f (Var m)) ->
          (f (Var m) -> m (g (Var m))) ->
          ([g (Var m)] -> m ()) ->
          m ()
  for' m f g = split m $ \ case
    Just (x, m) -> f x >>= \ y -> for' m f $ \ ys -> g $ y : ys
    Nothing -> g []

  all' :: Freshenable f => m (f (Var m)) -> ([f (Var m)] -> m ()) -> m ()
  all' m f = split m $ \ case
    Just (x, m) -> all' m $ \ xs -> f $ x : xs
    Nothing -> f []

instance MonadVerse m => MonadVerse (ReaderT r m) where
  whenBound x f = ReaderT $ \ r ->
    whenBound x $ flip runReaderT r . f
  split m f = ReaderT $ \ r ->
    split (runReaderT m r) $ \ x -> runReaderT (f $ fmap lift <$> x) r

instance MonadVerse m => MonadVerse (RST r s m) where
  whenBound x f = RST $ \ r s -> do
    whenBound x $ \ x -> evalRST (f x) r s
    pure ((), s)
  split m f = RST $ \ r s -> do
    split (evalRST m r s) $ \ x ->
      evalRST (f $ fmap lift <$> x) r s
    pure ((), s)

instance (Monoid w, MonadVerse m) => MonadVerse (CPS.WriterT w m) where
  whenBound x f = lift $ whenBound x $ \ x ->
    () <$ CPS.runWriterT (f x)
  split m f = CPS.writerT $ do
    split (fst <$> CPS.runWriterT m) $ \ x ->
      fst <$> (CPS.runWriterT (f $ fmap lift <$> x))
    pure ((), mempty)
