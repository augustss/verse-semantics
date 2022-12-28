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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS

newtype RefLogicT m a = RefLogicT
  { unRefLogicT :: LogicT (WriterT (Ap m) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFail
             )

deriving instance MonadError e m => MonadError e (RefLogicT m)

runRefLogicT :: Monad m => RefLogicT m a -> m [a]
runRefLogicT = evalWriterT . observeAllT . unRefLogicT

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

data Ap f
  = Ap (f ())
  | Zero

runAp :: Applicative f => Ap f -> f ()
runAp = \ case
  Ap x -> x
  Zero -> pure ()

instance Applicative f => Semigroup (Ap f) where
  (<>) = curry $ \ case
    (Zero, _) -> Zero
    (_, Zero) -> Zero
    (Ap x, Ap y) -> Ap $ y *> x

instance Applicative f => Monoid (Ap f) where
  mempty = Ap $ pure ()

instance MonadTrans RefLogicT where
  lift = RefLogicT . lift . lift

instance Monad m => Alternative (RefLogicT m) where
  empty = RefLogicT $ LogicT $ \ _ fk ->
    tell Zero *> fk
  x <|> y = RefLogicT $ LogicT $ \ sk fk ->
    unLogicT (unRefLogicT x) sk $ unLogicT (unRefLogicT y) sk fk

instance Monad m => MonadLogic (RefLogicT m) where
  msplit m = RefLogicT $ LogicT $ \ sk fk -> do
    (x, fk') <- lift . runWriterT . fmap (fmap (fmap RefLogicT)) . msplit' $ unRefLogicT m
    sk x $ lift (runAp fk') *> fk

msplit' :: Monad m => LogicT m a -> m (Maybe (a, LogicT m a))
msplit' m =
  unLogicT m sk' $ pure Nothing
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
      tell . Ap $ writeRef ref x
      fk
