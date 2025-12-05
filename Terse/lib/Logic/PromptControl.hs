{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module Logic.PromptControl
  ( LogicT
  , runLogicT
  , one
  , all'
  , Stream (..)
  , split
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class

import Data.Functor

import PromptControl

newtype LogicT m a = LogicT
  { unLogicT :: forall r . PromptTag m r -> m r -> m (a, m r)
  }

runLogicT :: MonadPromptControl m => LogicT m a -> m [a]
runLogicT m = do
  ft <- newPromptTag
  prompt ft $ uncurry (fmap . (:)) =<< unLogicT m ft (pure [])

one :: MonadPromptControl m => LogicT m a -> LogicT m a
one = split >=> \ case
  Done -> empty
  Step x _ -> pure x

all' :: MonadPromptControl m => LogicT m a -> LogicT m [a]
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> fmap (x:) . loop =<< m

data Stream m a = Done | Step a (LogicT m (Stream m a))

split :: MonadPromptControl m => LogicT m a -> LogicT m (Stream m a)
split m = lift $ do
  ft <- newPromptTag
  prompt ft $ uncurry Step . fmap lift <$> unLogicT m ft (pure Done)

instance Functor m => Functor (LogicT m) where
  fmap f x = LogicT $ \ ft fk ->
    unLogicT x ft fk <&> \ (x, fk) -> (f x, fk)

instance Monad m => Applicative (LogicT m) where
  pure x = LogicT $ \ _ fk -> pure (x, fk)
  f <*> x = LogicT $ \ ft fk -> do
    (f, fk) <- unLogicT f ft fk
    (x, fk) <- unLogicT x ft fk
    pure (f x, fk)

instance Monad m => Monad (LogicT m) where
  x >>= f = LogicT $ \ ft fk ->
    unLogicT x ft fk >>= \ (x, fk) ->
    unLogicT (f x) ft fk

instance MonadTrans LogicT where
  lift m = LogicT $ \ _ fk ->
    m <&> \ x -> (x, fk)

instance MonadPromptControl m => Alternative (LogicT m) where
  empty = LogicT $ \ ft fk ->
    control0 ft $ const fk
  x <|> y = LogicT $ \ ft fk ->
    control ft $ \ k -> k (unLogicT x ft . k $ unLogicT y ft fk)
