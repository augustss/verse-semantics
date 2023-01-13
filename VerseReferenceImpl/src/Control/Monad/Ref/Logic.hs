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

newtype Ap m = Ap (StateT (Ap m) m ())

emptyAp :: Monad m => Ap m
emptyAp = Ap $ pure ()

runWriterT :: Monad m => StateT (Ap m) m a -> m (a, Ap m)
runWriterT = flip runStateT emptyAp

evalWriterT :: Monad m => StateT (Ap m) m a -> m a
evalWriterT = flip evalStateT emptyAp

tellAp :: Monad m => StateT (Ap m) m () -> StateT (Ap m) m ()
tellAp s = StateT $ \ (Ap s') -> pure ((), Ap $ s *> s')

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => MonadFail (RefLogicT m) where
  fail _ = empty

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk -> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $ unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, Ap fk') <- lift . runWriterT . fmap (fmap (fmap coerce)) . msplit' $ coerce m
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
    write x y
    sk () $ write y x *> fk
    where
      write x y = do
        writeRef ref x
        tellAp $ writeRef ref y
