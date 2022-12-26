{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ref.Logic
  ( RefLogicT
  , runRefLogicT
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Logic
import Control.Monad.Ref
import Control.Monad.Trans.Class

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (WordStateT m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             , MonadLogic
             )

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = flip evalWordStateT minBound . observeAllT . unRefLogicT

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT empty
  x <|> y = modify' (+ 1) *> RefLogicT (unRefLogicT x <|> unRefLogicT y)

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance MonadRef m => MonadRef (RefLogicT m) where
  type Ref (RefLogicT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    writeRef ref x
    r <- get
    sk () $ do
      r' <- get
      writeRef ref y *> fk <* when (r /= r') (writeRef ref x)

modify' :: Monad m => (Word -> Word) -> RefLogicT m ()
modify' = RefLogicT . lift . modify

newtype WordStateT m a = WordStateT
  { runWordStateT :: Word -> m (S a)
  }

evalWordStateT :: Functor m => WordStateT m a -> Word -> m a
evalWordStateT m s = extract <$> runWordStateT m s

instance Functor m => Functor (WordStateT m) where
  fmap f m = WordStateT $ fmap (fmap f) . runWordStateT m

instance Monad m => Applicative (WordStateT m) where
  pure x = WordStateT $ \ s -> pure $ S x s
  f <*> x = WordStateT $ \ s -> do
    S f s <- runWordStateT f s
    S x s <- runWordStateT x s
    pure $! S (f x) s

instance Monad m => Monad (WordStateT m) where
  x >>= f = WordStateT $ \ s -> do
    S x s <- runWordStateT x s
    runWordStateT (f x) s

instance MonadTrans WordStateT where
  lift m = WordStateT $ \ s -> flip S s <$> m

instance MonadRef m => MonadRef (WordStateT m) where
  type Ref (WordStateT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

get :: Applicative m => WordStateT m Word
get = WordStateT $ \ s -> pure $! S s s

modify :: Applicative m => (Word -> Word) -> WordStateT m ()
modify f = WordStateT $ \ s -> pure $! S () (f s)

data S a = S a !Word

extract :: S a -> a
extract (S x _) = x

instance Functor S where
  fmap f (S x y) = S (f x) y
