{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Supply
  ( MonadSupply (..)
  , SupplyT
  , runSupplyT
  , Supply
  , runSupply
  , IntSupplyT
  , runIntSupplyT
  ) where

import Control.Comonad
import Control.Comonad.Env.Class
import Control.Monad.Except
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Class
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Writer.CPS qualified as CPS

import Data.Functor.Identity

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  default supply :: (m ~ t n, MonadTrans t, MonadSupply s n) => m s
  supply = lift supply

instance MonadSupply s m => MonadSupply s (ReaderT r m)

instance MonadSupply s m => MonadSupply s (Lazy.StateT s' m)

instance MonadSupply s m => MonadSupply s (Strict.StateT s' m)

instance MonadSupply s m => MonadSupply s (CPS.WriterT w m)

newtype SupplyT s m a = SupplyT
  { unSupplyT :: Strict.StateT s m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             , MonadTrans
             , MonadIO
             , MonadRef
             )

type Supply s = SupplyT s Identity

deriving instance MonadError e m => MonadError e (SupplyT s m)

deriving instance MonadFix m => MonadFix (SupplyT s m)

instance (Enum s, Monad m) => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ state $ \ s -> (s, succ s)

runSupplyT :: (Bounded s, Monad m) => SupplyT s m a -> m a
runSupplyT = flip Strict.evalStateT minBound . unSupplyT

runSupply :: Bounded s => Supply s a -> a
runSupply = runIdentity . runSupplyT

newtype IntSupplyT m a = IntSupplyT
  { unIntSupplyT :: Int -> m (IntEnv a)
  }

runIntSupplyT :: Functor m => IntSupplyT m a -> m a
runIntSupplyT = fmap extract . flip unIntSupplyT 0

instance Functor m => Functor (IntSupplyT m) where
  fmap f m = IntSupplyT $ fmap (fmap f) . unIntSupplyT m

instance Monad m => Applicative (IntSupplyT m) where
  pure x = IntSupplyT $ pure . flip IntEnv x
  f <*> x = IntSupplyT $ \ s -> do
    IntEnv s f <- unIntSupplyT f s
    IntEnv s x <- unIntSupplyT x s
    pure $ IntEnv s (f x)

instance Monad m => Monad (IntSupplyT m) where
  m >>= k = IntSupplyT $ \ s -> do
    IntEnv s x <- unIntSupplyT m s
    unIntSupplyT (k x) s

instance MonadFix m => MonadFix (IntSupplyT m) where
  mfix f = IntSupplyT $ \ s -> mfix $ \ ~(IntEnv _ x) -> unIntSupplyT (f x) s

instance MonadTrans IntSupplyT where
  lift m = IntSupplyT $ flip fmap m . IntEnv

instance MonadRef m => MonadRef (IntSupplyT m) where
  type Ref (IntSupplyT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref = lift . writeRef ref

instance Monad m => MonadSupply Int (IntSupplyT m) where
  supply = IntSupplyT $ \ s -> pure $! IntEnv (s + 1) s

data IntEnv a = IntEnv {-# UNPACK #-} !Int a

instance Functor IntEnv where
  fmap f (IntEnv x y) = IntEnv x (f y)

instance Comonad IntEnv where
  extract (IntEnv _ x) = x
  duplicate y@(IntEnv x _) = IntEnv x y

instance ComonadEnv Int IntEnv where
  ask (IntEnv x _) = x
