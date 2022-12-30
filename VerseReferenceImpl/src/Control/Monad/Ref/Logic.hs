{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ref.Logic
  ( module Control.Monad.Logic.Class
  , RefLogicT
  , runRefLogicT
  ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Supply

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (StateT (Ap m) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             )

deriving instance MonadError e m => MonadError e (RefLogicT m)

deriving instance MonadSupply s m => MonadSupply s (RefLogicT m)

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = evalWriterT . observeAllT . unRefLogicT

data Ap f = Ap !Bool (f ())

emptyAp :: Applicative f => Ap f
emptyAp = Ap True $ pure ()

runAp :: Ap f -> f ()
runAp (Ap _ x) = x

runWriterT :: Applicative f => StateT (Ap f) m a -> m (a, Ap f)
runWriterT = flip runStateT emptyAp

evalWriterT :: (Applicative f, Monad m) => StateT (Ap f) m a -> m a
evalWriterT = flip evalStateT emptyAp

tellAp :: (Applicative f, Applicative m) => f () -> StateT (Ap f) m ()
tellAp s = StateT $ \ case
  (Ap True s') -> pure ((), Ap True $ s *> s')
  s -> pure ((), s)

tellEmpty :: Applicative m => StateT (Ap f) m ()
tellEmpty = StateT $ \ case
  Ap True s -> pure ((), Ap False s)
  s -> pure ((), s)

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => MonadFail (RefLogicT m) where
  fail _ = empty

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk ->
    tellEmpty *> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $ unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, fk') <- lift . runWriterT . fmap (fmap (fmap RefLogicT)) . msplit' $ unRefLogicT m
    sk x $ lift (runAp fk') *> fk

msplit' :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
msplit' m = unLogicT m sk' $ pure Nothing
  where
    sk' x fk = pure $ Just (x, lift fk >>= reflect)

instance MonadRef m => MonadRef (RefLogicT m) where
  type Ref (RefLogicT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    writeRef ref x
    sk () $ do
      writeRef ref y
      tellAp $ writeRef ref x
      fk
