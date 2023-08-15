{-# LANGUAGE LambdaCase #-}
import Control.Monad.Trans.Class

import Ref

newtype ParT m a = ParT
  { unParT :: forall r . (a -> m r) -> m r -> m r
  }

instance Functor (ParT m) where
  fmap f m = ParT $ \ sk fk -> unParT m (sk . f) fk

instance Applicative (ParT m) where
  pure x = ParT $ \ sk _ -> sk x
  f <*> x = ParT $ \ sk fk -> unParT f (\ f -> unParT x (\ x -> sk $ f x) fk) fk

instance Monad (ParT m) where
  x >>= f = ParT $ \ sk fk -> unParT x (\ x -> unParT (f x) sk fk) fk

instance MonadTrans ParT where
  lift m = ParT $ \ sk _ -> m >>= sk

newtype Var m a = Var
  { unVar :: Ref m (Contents m a)
  }

data Contents m a
  = Val a
  | Susp (a -> m ())

runParT :: Applicative m => ParT m a -> m (Maybe a)
runParT m = unParT m (pure . Just) (pure Nothing)

freshVar :: MonadRef m => ParT m (Var m a)
freshVar = lift . fmap Var . newRef . Susp . const $ pure ()

newVar :: MonadRef m => a -> ParT m (Var m a)
newVar = lift . fmap Var . newRef . Val

readVar :: MonadRef m => Var m a -> ParT m a
readVar v = ParT $ \ sk fk -> readRef (unVar v) >>= \ case
  Val x -> sk x
  Susp k -> writeRef (unVar v) (Susp $ \ x -> k x <* sk x) *> fk

writeVar :: MonadRef m => Var m a -> a -> ParT m ()
writeVar v x = ParT $ \ sk _ -> readRef (unVar v) >>= \ case
  Val _ -> error "writeVar"
  Susp k -> writeRef (unVar v) (Val x) *> k x *> sk ()

fork :: Applicative m => ParT m () -> ParT m ()
fork m = ParT $ \ sk _ -> unParT m (const $ pure ()) (pure ()) *> sk ()

yield :: ParT m a
yield = ParT $ \ _ fk -> fk
