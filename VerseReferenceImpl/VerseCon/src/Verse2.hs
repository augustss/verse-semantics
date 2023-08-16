{-# LANGUAGE LambdaCase #-}
module Verse2
  ( VerseT
  , runVerseT
  , Var
  , freshVar
  , newVar
  , readVar
  , writeVar
  , fork
  ) where

import Control.Applicative
import Control.Monad.Trans.Class

import Ref

newtype VerseT m a = VerseT
  { unVerseT :: forall r . (a -> m r -> m r) -> m r -> Yield m r -> m r
  }

newtype Yield m r = Yield
  { unYield :: forall a . Var m a -> (a -> VerseT m ()) -> (a -> m r -> m r) -> m r -> m r
  }

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ sk -> unVerseT m $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ sk fk _ -> sk x fk
  f <*> x = VerseT $ \ sk fk yk -> unVerseT f (\ f fk -> unVerseT x (sk . f) fk yk) fk yk

instance Alternative (VerseT m) where
  empty = VerseT $ \ _ fk _ -> fk
  x <|> y = VerseT $ \ sk fk yk -> unVerseT x sk (unVerseT y sk fk yk) yk

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ sk fk yk -> unVerseT x (\ x fk -> unVerseT (f x) sk fk yk) fk yk

instance MonadTrans VerseT where
  lift m = VerseT $ \ sk fk _ -> m >>= \ x -> sk x fk

newtype Var m a = Var
  { unVar :: Ref m (VarState m a)
  }

data VarState m a
  = Val a
  | Susp (a -> VerseT m ())

runVerseT :: Monad m => VerseT m a -> m (Maybe [a])
runVerseT m = unVerseT m sk fk yk
  where
    sk x fk = fmap (x:) <$> fk
    fk = pure $ Just []
    yk = Yield $ \ _ _ _ _ -> pure Nothing

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> VerseT m a
readVar v = VerseT $ \ sk fk yk -> readRef (unVar v) >>= \ case
  Val x -> sk x fk
  Susp k -> unYield yk v k sk fk

writeVar :: MonadRef m => Var m a -> a -> VerseT m ()
writeVar v x = VerseT $ \ sk fk yk -> readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> unVerseT (k x) sk fk yk

fork :: MonadRef m => VerseT m () -> VerseT m ()
fork m = VerseT $ \ sk fk yk ->
  unVerseT m (\ () fk -> pure . Just $ lift fk >>= reflect_) (pure Nothing) yk' >>= \ case
    Just m -> sk () $ unVerseT m sk fk yk
    Nothing -> fk
  where
    yk' = Yield $ \ v k sk fk -> do
      writeRef (unVar v) . Susp $ \ x -> k x >> lift (sk x fk) >>= reflect_
      pure $ Just empty

reflect_ :: Alternative m => Maybe (m ()) -> m ()
reflect_ = \ case
  Just m -> pure () <|> m
  Nothing -> empty
