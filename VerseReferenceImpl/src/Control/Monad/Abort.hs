{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Abort
  ( MonadAbort (..)
  , liftEither
  ) where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS qualified as CPS

class Monad m => MonadAbort e m | m -> e where
  abort :: e -> m a
  default abort :: (m ~ t n, MonadTrans t, MonadAbort e n) => e -> m a
  abort = lift . abort

instance MonadAbort e (Either e) where
  abort = Left

instance Monad m => MonadAbort e (ExceptT e m) where
  abort = ExceptT . pure . Left

instance MonadAbort e m => MonadAbort e (Strict.StateT s m)

instance MonadAbort e m => MonadAbort e (SupplyT s m)

instance MonadAbort e m => MonadAbort e (CPS.WriterT w m)

liftEither :: MonadAbort e m => Either e a -> m a
liftEither = \ case
  Left e -> abort e
  Right x -> pure x
