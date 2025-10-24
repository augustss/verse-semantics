{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Verse.CPS.Monad
  ( VerseT
  , runVerseT
  , split
  , fork
  , stuck
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.HashMap.Strict qualified as Strict (HashMap)
import Data.Kind
import Data.Monoid (Ap (..), Sum (..))

import GHC.Exts (Any)

import Unsafe.Coerce (unsafeCoerce)

import Fix
import IntMap (IntMap, (!))
import IntMap qualified
import Ref

newtype VerseT (m :: Type -> Type) a = VerseT
  { unVerseT
    :: forall r . R
    -> S
    -> Env m
    -> Mem m
    -> Yield r m
    -> Succeed r m a
    -> m r
  }

newtype R = R { level :: Level }

type Level = Sum Int

newtype S = S { count :: Int }

succS :: S -> S
succS !s = s { count = s.count + 1 }

predS :: S -> S
predS !s = s { count = s.count - 1 }

type Env m = Var m ()

newtype Mem m = Mem
  { label :: Label
  }

type Label = Int

newtype Yield r m = Yield
  { unYield
    :: forall a . Level
    -> S
    -> Mem m
    -> Handler m a
    -> m r
  }

type Handler m a = (VerseT m a -> VerseT m ()) -> VerseT m ()

type Succeed r m a = S -> Mem m -> a -> m r

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ r s env mem ->
    unVerseT m r s env mem yk $ \ s mem -> sk s mem . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _r s _env mem _yk sk ->
    sk s mem x
  f <*> x = VerseT $ \ r s env mem yk sk ->
    unVerseT f r s env mem yk $ \ s mem f ->
    unVerseT x r s env mem yk $ \ s mem x ->
    sk s mem $ f x

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ r s env mem yk sk ->
    unVerseT x r s env mem yk $ \ s mem x ->
    unVerseT (f x) r s env mem yk sk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _r s _env mem _yk sk ->
    m >>= \ x -> sk s mem x

instance MonadIO m => MonadIO (VerseT m) where
  liftIO = lift . liftIO

instance MonadReader (Var m ()) (VerseT m) where
  ask = VerseT $ \ _r s env mem _yk sk ->
    sk s mem env
  local f m = VerseT $ \ r s env mem yk sk ->
    unVerseT m r s (f env) mem yk sk
  reader f = VerseT $ \ _r s env mem _yk sk ->
    sk s mem $ f env

instance MonadState s m => MonadState s (VerseT m) where
  get = lift get
  put = lift . put
  state = lift . state

yield :: Level -> Handler m a -> VerseT m a
yield i f = VerseT $ \ _r s _env mem yk ->
  unYield yk i s mem f

getLevel :: VerseT m Level
getLevel = VerseT $ \ r s _env mem _yk sk ->
  sk s mem r.level

putS :: S -> VerseT m ()
putS s = VerseT $ \ _r _s _env mem _yk sk ->
  sk s mem ()

modifyS :: (S -> S) -> VerseT m ()
modifyS f = VerseT $ \ _r s _env mem _yk sk ->
  let
    !s' = f s
  in
    sk s' mem ()

putMem :: Mem m -> VerseT m ()
putMem mem = VerseT $ \ _r s _env _mem _yk sk ->
  sk s mem ()

putLabel :: Label -> VerseT m ()
putLabel label = VerseT $ \ _r s _env Mem {} _yk sk ->
  sk s Mem {..} ()

runVerseT :: (MonadRef m, Vars a m) => VerseT m a -> m (Maybe a)
runVerseT m = do
  (env, label) <- newVar' () 0
  let
    sk s Mem {..} x
      | s.count == 0 = runFindT (findVars x) level label >>= \ case
          Nothing -> pure Nothing
          Just (x, label) -> pure $ Just x
      | otherwise = pure Nothing
  unVerseT m r s env Mem {..} yk sk
  where
    r = R {..}
    level = 0
    s = S { count = 0 }
    yk = Yield $ \ _i _f _s _mem _s _sk ->
      pure Nothing
    newVar' binding label =
      let
        !label' = label + 1
      in
        fmap ((, label') . Var) . newRef $ Bound MkBound {..}

split :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m (Stream m a)
split m = split' m S { count = 0 }

split'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> S -> VerseT m (Stream m a)
split' m s = splitS m s >>= \ case
  YieldS i s mem f f_s -> do
    putLabel mem.label
    level <- getLevel
    if i > level then
      stuck
    else
      yield i $ \ k ->
      f $ \ m -> k $ split' (m >>= \ x -> alt (f_s x) m_f m_e) s
  SucceedS s Mem {..} x m_f _m_e ->
    if s.count == 0 then do
      level <- getLevel
      lift (runFindT (findVars (heap, x)) level label) >>= \ case
        Nothing ->
          stuck
        Just ((heap, x), label) -> do
          putLabel label
          putHeap heap
          pure . Step x $ split m_f
    else do
      putLabel label
      stuck
  FailS label -> do
    putLabel label
    pure Done

data Stream m a = Done | Step a (VerseT m (Stream m a))

splitS :: Monad m => VerseT m a -> Int -> Heap m -> VerseT m (Split m a)
splitS m !count = VerseT $ \ r s env mem _yk sk ->
  let
    !r' = R { level = r.level <> 1 }
    !s' = S { count }
    !mem' = Mem { label = mem.label, heap }
  in
    unVerseT m r' s' mem' yieldS succeedS >>= \ x ->
    sk s mem x

yieldS :: Monad m => Yield (Split m a) m
yieldS = Yield $ \ i s mem f sk ->
  pure $
  YieldS i s mem f
  (liftS sk >=> reflect)

succeedS :: Monad m => Succeed (Split m a) m a
succeedS s mem x fk ek =
  pure $
  SucceedS s mem x

reflect :: Split m a -> VerseT m a
reflect = \ case
  YieldS i s mem f f_s ->
    putS s *>
    putMem mem *>
    yield i $ \ k -> f $ \ m -> k $ m >>= f_s
  SucceedS s mem x -> do
    putS s
    putMem mem
    pure x

fork :: Monad m => VerseT m () -> VerseT m ()
fork m = forkS m >>= reflectF

forkS :: Monad m => VerseT m () -> VerseT m (Split m ())
forkS m = VerseT $ \ r s env mem _yk sk fk ek ->
  unVerseT m r s env mem yieldF succeedF >>= \ x ->
  sk s mem x

liftS :: Monad m => Succeed (Split m a) m b -> b -> VerseT m (Split m a)
liftS f x = VerseT $ \ _r s _env mem _yk sk ->
  f s mem x >>= \ x -> sk s mem x

data Split m a
  = forall b .
    YieldS
    {-# UNPACK #-} !Level
    !(Handler m b)
    {-# UNPACK #-} !S
    !(Mem m)
    !(b -> VerseT m a)
  | SucceedS
    {-# UNPACK #-} !S
    !(Mem m)
    !a

stuck :: VerseT m a
stuck = VerseT $ \ r s _env mem yk ->
  unYield yk r.level (const $ pure ()) s mem
