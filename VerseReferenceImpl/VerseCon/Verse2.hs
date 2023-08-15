{-# LANGUAGE LambdaCase #-}
import Control.Applicative
import Control.Monad.Trans.Class

import Ref

newtype ParT m a = ParT
  { unParT :: forall r . (a -> m r -> m r) -> m r -> Yield m r -> m r
  }

newtype Yield m r = Yield
  { unYield :: forall a . Var m a -> (a -> ParT m ()) -> (a -> m r -> m r) -> m r -> m r
  }

instance Functor (ParT m) where
  fmap f m = ParT $ \ sk -> unParT m $ sk . f

instance Applicative (ParT m) where
  pure x = ParT $ \ sk fk _ -> sk x fk
  f <*> x = ParT $ \ sk fk yk -> unParT f (\ f fk -> unParT x (sk . f) fk yk) fk yk

instance Alternative (ParT m) where
  empty = ParT $ \ _ fk _ -> fk
  x <|> y = ParT $ \ sk fk yk -> unParT x sk (unParT y sk fk yk) yk

instance Monad (ParT m) where
  x >>= f = ParT $ \ sk fk yk -> unParT x (\ x fk -> unParT (f x) sk fk yk) fk yk

instance MonadTrans ParT where
  lift m = ParT $ \ sk fk _ -> m >>= \ x -> sk x fk

newtype Var m a = Var
  { unVar :: Ref m (Contents m a)
  }

data Contents m a
  = Val a
  | Susp (a -> ParT m ())

runParT :: Monad m => ParT m a -> m (Maybe [a])
runParT m = unParT m sk fk yk
  where
    sk x fk = fmap (x:) <$> fk
    fk = pure $ Just []
    yk = Yield $ \ _ _ _ _ -> pure Nothing

freshVar :: MonadRef m => ParT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> ParT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> ParT m a
readVar v = ParT $ \ sk fk yk -> readRef (unVar v) >>= \ case
  Val x -> sk x fk
  Susp k -> unYield yk v k sk fk

writeVar :: MonadRef m => Var m a -> a -> ParT m ()
writeVar v x = ParT $ \ sk fk yk -> readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> unParT (k x) sk fk yk

fork :: MonadRef m => ParT m () -> ParT m ()
fork m = ParT $ \ sk fk yk ->
  unParT m (\ () fk -> pure . Just $ lift fk >>= reflect_) (pure Nothing) yk' >>= \ case
    Just m -> sk () $ unParT m sk fk yk
    Nothing -> fk
  where
    yk' = Yield $ \ v k sk fk -> do
      writeRef (unVar v) . Susp $ \ x -> k x >> lift (sk x fk) >>= reflect_
      pure $ Just empty

reflect_ :: Alternative m => Maybe (m ()) -> m ()
reflect_ = \ case
  Just m -> pure () <|> m
  Nothing -> empty
