{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Logic.State
  ( module Control.Monad.Logic.State.Class
  , LogicStateT
  , evalLogicStateT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Logic.State.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class

newtype LogicStateT s m a = LogicStateT
  { unLogicStateT :: forall r .
    (a -> m r -> (s -> m r) -> s -> m r) ->
    m r -> (s -> m r) -> s -> m r
  }

evalLogicStateT :: Applicative m =>
                   LogicStateT s m a -> (a -> m r -> m r) -> m r -> s -> m r
evalLogicStateT m sk fk s = unLogicStateT m sk' fk (const fk) s
  where
    sk' x fk _ _ = sk x fk

instance Functor (LogicStateT s m) where
  fmap f m = LogicStateT $ \ sk ->
    unLogicStateT m (sk . f)

instance Applicative (LogicStateT s m) where
  pure x = LogicStateT $ \ sk ->
    sk x
  f <*> x = LogicStateT $ \ sk ->
    unLogicStateT f (\ f -> unLogicStateT x (sk . f))

instance Alternative (LogicStateT s m) where
  empty = LogicStateT $ \ _ fk _ _ ->
    fk
  x <|> y = LogicStateT $ \ sk fk fk' s ->
    let f = unLogicStateT y sk fk fk'
    in unLogicStateT x sk (f s) f s

instance Monad (LogicStateT s m) where
  m >>= f = LogicStateT $ \ sk ->
    unLogicStateT m (\ x -> unLogicStateT (f x) sk)

instance MonadFail (LogicStateT s m) where
  fail _ = empty

instance MonadPlus (LogicStateT s m)

instance MonadTrans (LogicStateT s) where
  lift m = LogicStateT $ \ sk fk fk' s ->
    m >>= \ x -> sk x fk fk' s

instance MonadState s (LogicStateT s m) where
  get = LogicStateT $ \ sk fk fk' s ->
    sk s fk fk' s
  put s = LogicStateT $ \ sk fk fk' _ ->
    sk () fk fk' s

instance Monad m => MonadLogicState s (LogicStateT s m) where
  msplit' m s = LogicStateT $ \ sk fk fk' s' -> do
    x <- unLogicStateT m sk_x fk_x fk_x' s
    sk x fk fk' s'
    where
      sk_x x _ fk' s = pure $ Just (x, s, lift' fk')
      fk_x = pure Nothing
      fk_x' = const fk_x

lift' :: Monad m => (s -> m (Maybe (a, s, LogicStateT s m a))) -> LogicStateT s m a
lift' f = LogicStateT $ \ sk fk fk' s -> f s >>= \ case
  Nothing -> fk
  Just (x, s', m) ->
    let f = unLogicStateT m sk fk fk'
    in sk x (f s) f s'

instance MonadError e m => MonadError e (LogicStateT s m) where
  throwError = lift . throwError
  catchError m f = LogicStateT $ \ sk fk fk' s ->
    let handle m = m `catchError` \ e -> unLogicStateT (f e) sk fk fk' s
    in handle $ unLogicStateT m (\ x fk fk' -> sk x (handle fk) (handle . fk')) fk fk' s
