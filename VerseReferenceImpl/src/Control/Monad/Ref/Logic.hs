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

import Data.Coerce

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

data Ap m = Ap !Word (StateT (Ap m) m ())

emptyAp :: Monad m => Ap m
emptyAp = Ap 0 $ pure ()

runWriterT :: Monad m => StateT (Ap m) m a -> m (a, Ap m)
runWriterT = flip runStateT emptyAp

evalWriterT :: Monad m => StateT (Ap m) m a -> m a
evalWriterT = flip evalStateT emptyAp

tellAp :: Monad m => StateT (Ap m) m () -> StateT (Ap m) m ()
tellAp s = StateT $ \ case
  (Ap 0 s') -> pure ((), Ap 0 $ s *> s')
  s -> pure ((), s)

tellLeft :: Applicative m => StateT (Ap m) m ()
tellLeft = StateT $ \ (Ap i s) ->
  pure ((), Ap (i + 1) s)

tellRight :: Applicative m => StateT (Ap m) m ()
tellRight = StateT $ \ case
  Ap i s | i /= 0 -> pure ((), Ap (i - 1) s)
  s -> pure ((), s)

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => MonadFail (RefLogicT m) where
  fail _ = empty

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk -> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk -> do
    tellLeft
    unLogicT (unRefLogicT x) sk $ do
      tellRight
      unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, Ap _ fk') <- lift . runWriterT . fmap (fmap (fmap coerce)) . msplit' $ coerce m
    tellAp fk'
    sk x $ fk' *> fk

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
    loop x y
    sk () $ loop y x *> fk
    where
      loop x y = do
        writeRef ref x
        tellAp $ loop y x
