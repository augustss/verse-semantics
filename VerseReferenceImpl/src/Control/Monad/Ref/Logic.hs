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

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (StateT (m ()) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             )

deriving instance MonadError e m => MonadError e (RefLogicT m)

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = flip evalStateT (pure ()) . observeAllT . unRefLogicT

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk ->
    put (pure ()) *> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $ unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, fk') <- runStateT' (msplit' m)
    sk x $ lift fk' *> fk
    where
      runStateT' =
        lift . flip runStateT (pure ())
      msplit' m =
        unLogicT (unRefLogicT m) sk' $ pure Nothing
        where
          sk' x fk = pure $ Just (x, RefLogicT (lift fk) >>= reflect)

instance MonadRef m => MonadRef (RefLogicT m) where
  type Ref (RefLogicT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref x = RefLogicT $ LogicT $ \ sk fk -> do
    y <- readRef ref
    writeRef ref x
    sk () $ do
      writeRef ref y
      modify (writeRef ref x *>)
      fk
