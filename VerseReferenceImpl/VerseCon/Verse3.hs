{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
module Par4
  ( ParT
  , runParT
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

newtype ParT m a = ParT
  { unParT :: forall r . Yield m r -> Logic r m a
  }

type Logic r m a = (a -> m r -> m r) -> m r -> m r

type Success r m a = a -> m r -> m r

newtype Yield m r = Yield
  { unYield :: forall a . ((a -> ParT m ()) -> m (m ())) -> Logic r m a
  }

instance Functor (ParT m) where
  fmap f m = ParT $ \ yk sk -> unParT m yk $ sk . f

instance Applicative (ParT m) where
  pure x = ParT $ \ _ sk fk -> sk x fk
  f <*> x = ParT $ \ yk sk fk -> unParT f yk (\ f fk -> unParT x yk (sk . f) fk) fk

instance Alternative (ParT m) where
  empty = ParT $ \ _ _ fk -> fk
  x <|> y = ParT $ \ yk sk fk -> unParT x yk sk $ unParT y yk sk fk

instance Monad (ParT m) where
  x >>= f = ParT $ \ yk sk fk -> unParT x yk (\ x fk -> unParT (f x) yk sk fk) fk

instance MonadTrans ParT where
  lift m = ParT $ \ _ sk fk -> m >>= \ x -> sk x fk

instance MonadRef m => MonadRef (ParT m) where
  type Ref (ParT m) = Ref m

  newRef = lift . newRef

  readRef = lift . readRef

  writeRef r x = ParT $ \ _ sk fk -> do
    y <- readRef r
    writeRef r x
    sk () $ writeRef r y *> fk

newtype Var m a = Var
  { unVar :: Ref m (Contents m a)
  }

data Contents m a
  = Val a
  | Susp (a -> ParT m ())

runParT :: Monad m => ParT m a -> m (Maybe [a])
runParT m = unParT m yk sk fk
  where
    yk = Yield $ \ _ _ _ -> pure Nothing
    sk x fk = fmap (x:) <$> fk
    fk = pure $ Just []

freshVar :: MonadRef m => ParT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> ParT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> ParT m a
readVar v = ParT $ \ yk sk fk -> readRef (unVar v) >>= \ case
  Val x -> sk x fk
  x@(Susp k) -> rotate (unYield yk) sk fk $ \ k' ->
    writeRef (unVar v) (Susp $ \ x -> k x *> k' x) $>
    writeRef (unVar v) x
  where
    rotate f x y z = f z x y

writeVar :: MonadRef m => Var m a -> a -> ParT m ()
writeVar v x = readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> k x

fork :: MonadRef m => ParT m () -> ParT m ()
fork m = ParT $ \ yk sk fk ->
  unParT m yk' sk' fk' >>= \ case
    Just m -> sk () $ unParT m yk sk fk
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
