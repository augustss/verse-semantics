{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Wrong
  ( MonadWrong (..)
  , liftEither
  ) where

import Control.Monad.Except (ExceptT (..))
import Control.Monad.Reader
import Control.Monad.State.Strict qualified as Strict
import Control.Monad.Supply
import Control.Monad.Trans.Writer.CPS qualified as CPS

class Monad m => MonadWrong e m | m -> e where
  wrong :: e -> m a
  default wrong :: (m ~ t n, MonadTrans t, MonadWrong e n) => e -> m a
  wrong = lift . wrong

instance MonadWrong e (Either e) where
  wrong = Left

instance Monad m => MonadWrong e (ExceptT e m) where
  wrong = ExceptT . pure . Left

instance MonadWrong e m => MonadWrong e (ReaderT r m)

instance MonadWrong e m => MonadWrong e (Strict.StateT s m)

instance MonadWrong e m => MonadWrong e (SupplyT s m)

instance MonadWrong e m => MonadWrong e (CPS.WriterT w m)

liftEither :: MonadWrong e m => Either e a -> m a
liftEither = \ case
  Left e -> wrong e
  Right x -> pure x
