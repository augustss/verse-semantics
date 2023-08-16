{-# LANGUAGE LambdaCase #-}
module Verse1
  ( VerseT
  , runVerseT
  , Var
  , freshVar
  , newVar
  , readVar
  , writeVar
  , fork
  ) where

import Control.Monad.Trans.Class

import Ref

newtype VerseT m a = VerseT
  { unVerseT :: forall r . (a -> m r) -> m r -> m r
  }

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ sk fk -> unVerseT m (sk . f) fk

instance Applicative (VerseT m) where
  pure x = VerseT $ \ sk _ -> sk x
  f <*> x = VerseT $ \ sk fk -> unVerseT f (\ f -> unVerseT x (sk . f) fk) fk

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ sk fk -> unVerseT x (\ x -> unVerseT (f x) sk fk) fk

instance MonadTrans VerseT where
  lift m = VerseT $ \ sk _ -> m >>= sk

newtype Var m a = Var
  { unVar :: Ref m (VarState m a)
  }

data VarState m a
  = Val a
  | Susp (a -> m ())

runVerseT :: Applicative m => VerseT m a -> m (Maybe a)
runVerseT m = unVerseT m (pure . Just) (pure Nothing)

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> VerseT m a
readVar v = VerseT $ \ sk fk -> readRef (unVar v) >>= \ case
  Val x -> sk x
  Susp k -> writeRef (unVar v) (Susp $ \ x -> k x <* sk x) *> fk

writeVar :: MonadRef m => Var m a -> a -> VerseT m ()
writeVar v x = VerseT $ \ sk _ -> readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> k x *> sk ()

fork :: Applicative m => VerseT m () -> VerseT m ()
fork m = VerseT $ \ sk _ -> unVerseT m (const $ pure ()) (pure ()) *> sk ()
