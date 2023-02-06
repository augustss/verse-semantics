{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Control.Monad.IO.Class
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Control.Monad.Ref
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.State.Class
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Supply
import Control.Monad.Trans.Class

import Data.Coerce

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (StateT m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             )

deriving instance MonadError e m => MonadError e (RefLogicT m)

deriving instance MonadSupply s m => MonadSupply s (RefLogicT m)

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = evalStateT . observeAllT . unRefLogicT

type StateT m = Strict.StateT (S m) m

data S m = S
  { failed :: !Bool
  , forward :: !(StateT m ())
  , backward :: !(StateT m ())
  }

emptyS :: Monad m => S m
emptyS = S { failed, forward, backward }
  where
    failed = False
    forward = pure ()
    backward = pure ()

runStateT :: Monad m => StateT m a -> m (a, S m)
runStateT = flip Strict.runStateT emptyS

evalStateT :: Monad m => StateT m a -> m a
evalStateT = flip Strict.evalStateT emptyS

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => MonadFail (RefLogicT m) where
  fail _ = empty

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk ->
    putFailed True *> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $
    getFailed >>= \ case
      False -> unLogicT (unRefLogicT y) sk $ putFailed False *> fk
      True -> putFailed False *> unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, s) <- lift (runStateT . fmap coerceSplit . msplit' $ coerce m)
    s.forward
    sk x $ s.backward *> fk

coerceSplit :: Maybe (a, LogicT (StateT m) a) -> Maybe (a, RefLogicT m a)
coerceSplit = coerce

msplit' :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
msplit' m = unLogicT m sk' $ pure Nothing
  where
    sk' x fk = pure $ Just (x, lift fk >>= reflect)

instance MonadRef m => MonadRef (RefLogicT m) where
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    writeRef ref x *> loop x y
    sk () $ writeRef ref y *> loop y x *> fk
    where
      loop x y = do
        tellForward $ loop x y
        tellBackward $ writeRef ref y *> loop y x

instance MonadRef m => Backtrack.MonadRef (RefLogicT m) where
  type Ref (RefLogicT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    writeRef ref x *> loop x y
    sk () $ whenFailed (writeRef ref y *> loop y x) *> fk
    where
      loop x y = do
        tellForward $ loop x y
        tellBackward $ whenFailed $ writeRef ref y *> loop y x
  backtrack = RefLogicT . lift $ putFailed True

whenFailed :: Monad m => StateT m () -> StateT m ()
whenFailed m = getFailed >>= \ case
  False -> pure ()
  True -> m

getFailed :: Monad m => StateT m Bool
getFailed = gets failed

putFailed :: Monad m => Bool -> StateT m ()
putFailed failed = modify $ \ s -> s { failed }

tellForward :: Monad m => StateT m () -> StateT m ()
tellForward m = modify $ \ s -> s { forward = s.forward *> m }

tellBackward :: Monad m => StateT m () -> StateT m ()
tellBackward m = modify $ \ s -> s { backward = m *> s.backward }
