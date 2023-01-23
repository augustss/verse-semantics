{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Ref.Logic
  ( module Control.Monad.Logic.Class
  , RefLogicT
  , runRefLogicT
  ) where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Control.Monad.Ref
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Coerce

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (StateT (S m) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             )

deriving instance MonadError e m => MonadError e (RefLogicT m)

deriving instance MonadSupply s m => MonadSupply s (RefLogicT m)

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = evalWriterT . observeAllT . unRefLogicT

data S m = S !Bool !(Ap m)

emptyS :: Monad m => S m
emptyS = S False emptyAp

newtype Ap m = Ap (StateT (S m) m ())

emptyAp :: Monad m => Ap m
emptyAp = Ap $ pure ()

runWriterT :: Monad m => StateT (S m) m a -> m (a, S m)
runWriterT = flip runStateT emptyS

evalWriterT :: Monad m => StateT (S m) m a -> m a
evalWriterT = flip evalStateT emptyS

tellAp :: Monad m => StateT (S m) m () -> StateT (S m) m ()
tellAp s = StateT $ \ (S p (Ap s')) -> pure ((), S p . Ap $ s *> s')

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => MonadFail (RefLogicT m) where
  fail _ = empty

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk ->
    modify (\ (S _ s) -> S True s) *> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $
    modify (\ (S _ s) -> S False s) *> unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, S _ (Ap fk')) <- lift . runWriterT . fmap (fmap (fmap coerce)) . msplit' $ coerce m
    tellAp fk'
    sk x $ fk' *> fk

msplit' :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
msplit' m = unLogicT m sk' $ pure Nothing
  where
    sk' x fk = pure $ Just (x, lift fk >>= reflect)

instance MonadRef m => MonadRef (RefLogicT m) where
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    write x y
    sk () $ write y x *> fk
    where
      write x y = do
        writeRef ref x
        tellAp $ writeRef ref y

instance MonadRef m => Backtrack.MonadRef (RefLogicT m) where
  type Ref (RefLogicT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    write x y
    sk () $ get >>= \ (S p _) -> when p (write y x) >> fk
    where
      write x y = do
        writeRef ref x
        tellAp $ get >>= \ (S p _) -> when p (writeRef ref y)
  backtrack = RefLogicT . lift . modify $ \ (S _ s) -> S True s
