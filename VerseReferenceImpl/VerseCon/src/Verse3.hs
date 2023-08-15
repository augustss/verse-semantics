{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Verse3
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

import Data.Functor

import Ref

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield m r -> Logic r m a
  }

type Logic r m a = (a -> m r -> m r) -> m r -> m r

type Success r m a = a -> m r -> m r

newtype Yield m r = Yield
  { unYield :: forall a . ((a -> VerseT m ()) -> m (m ())) -> Logic r m a
  }

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ yk sk -> unVerseT m yk $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ sk fk -> sk x fk
  f <*> x = VerseT $ \ yk sk fk -> unVerseT f yk (\ f fk -> unVerseT x yk (sk . f) fk) fk

instance Alternative (VerseT m) where
  empty = VerseT $ \ _ _ fk -> fk
  x <|> y = VerseT $ \ yk sk fk -> unVerseT x yk sk $ unVerseT y yk sk fk

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk sk fk -> unVerseT x yk (\ x fk -> unVerseT (f x) yk sk fk) fk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ sk fk -> m >>= \ x -> sk x fk

instance MonadRef m => MonadRef (VerseT m) where
  type Ref (VerseT m) = Ref m

  newRef = lift . newRef

  readRef = lift . readRef

  writeRef r x = VerseT $ \ _ sk fk -> do
    y <- readRef r
    writeRef r x
    sk () $ writeRef r y *> fk

newtype Var m a = Var
  { unVar :: Ref m (Contents m a)
  }

data Contents m a
  = Val a
  | Susp (a -> VerseT m ())

runVerseT :: Monad m => VerseT m a -> m (Maybe [a])
runVerseT m = unVerseT m yk sk fk
  where
    yk = Yield $ \ _ _ _ -> pure Nothing
    sk x fk = fmap (x:) <$> fk
    fk = pure $ Just []

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> VerseT m a
readVar v = VerseT $ \ yk sk fk -> readRef (unVar v) >>= \ case
  Val x -> sk x fk
  x@(Susp k) -> rotate (unYield yk) sk fk $ \ k' ->
    writeRef (unVar v) (Susp $ \ x -> k x *> k' x) $>
    writeRef (unVar v) x
  where
    rotate f x y z = f z x y

writeVar :: MonadRef m => Var m a -> a -> VerseT m ()
writeVar v x = readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> k x

fork :: MonadRef m => VerseT m () -> VerseT m ()
fork m = VerseT $ \ yk sk fk ->
  unVerseT m yk' sk' fk' >>= \ case
    Just m -> sk () $ unVerseT m yk sk fk
    Nothing -> fk
  where
    yk' = Yield $ \ k sk fk ->
      k (\ x -> lift (sk x fk) >>= reflect_) <&> \ fk ->
      Just $ lift fk *> empty
    sk' () fk = pure . Just $ lift fk >>= reflect_
    fk' = pure Nothing

reflect_ :: Alternative m => Maybe (m ()) -> m ()
reflect_ = \ case
  Just m -> pure () <|> m
  Nothing -> empty
