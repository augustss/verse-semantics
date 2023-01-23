{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.Verse
  ( VerseT
  , runVerseT
  , Label
  , whenBound
  , split
  , unify
  , freshen
  , freezeBy
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad ((>=>), join, unless, when)
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Backtrack (backtrack)
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Ref.Lenient qualified as Lenient
import Control.Monad.Ref.Logic
import Control.Monad.RST
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.Var (MonadVar, freshVar)
import Control.Monad.Var qualified as Var

import Data.Fix
import Data.Foldable (for_)
import Data.Functor
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.Ref
import Data.Traversable (for)
import Data.Unifiable

newtype VerseT m a = VerseT
  { unVerseT :: RST R (S m) (RefLogicT m) a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadFail
             , MonadIO
             )

deriving instance MonadError e m => MonadError e (VerseT m)

deriving instance MonadSupply s m => MonadSupply s (VerseT m)

instance MonadTrans VerseT where
  lift = VerseT . lift . lift

instance ( MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => MonadVar (VerseT m) where
  type Var (VerseT m) = Var m

  freshVar =
    fmap Var . newRef' . Repr 1 =<<
    Unbound <$> newRef' (const $ pure ()) <*> askLevel

  newVar = lift . newVar'

  readVar var = findVar var <&> \ case
    (_, _, Bound x _) -> Just x
    _ -> Nothing

instance MonadRef m => Lenient.MonadRef (VerseT m) where
  type Ref (VerseT m) = Ref m

  newRef = VerseT . lift . Backtrack.newRef

  readRef ref f = do
    world <- getWorld
    world' <- freshWorld
    putWorld world'
    whenRealWorld world $ do
      f =<< VerseT (lift $ Backtrack.readRef ref)
      resolveWorld world'

  writeRef ref x = do
    world <- getWorld
    world' <- freshWorld
    putWorld world'
    whenRealWorld world $ do
      VerseT . lift $ Backtrack.writeRef ref x
      resolveWorld world'

runVerseT :: MonadRef m => VerseT m a -> m [a]
runVerseT m = runRefLogicT $ do
  world <- newWorld'
  evalRST (unVerseT m) R { level } S { promises, world }
  where
    level = minBound
    promises = []

newtype Var m f = Var { unVar :: Set m (VarState m f) }

data VarState m f
  = Unbound !(Ref m (f (Var m f) -> VerseT m ())) !Level
  | Bound !(f (Var m f)) !Label

type Label = Int

newVar' :: (MonadRef m, MonadSupply Label m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newRef . Repr 1 . Bound x =<< supply

whenBound :: MonadRef m => Var m f -> (f (Var m f) -> VerseT m ()) -> VerseT m ()
whenBound var_x f = do
  (_, _, repr_x) <- findVar var_x
  case repr_x of
    Unbound f_x _ -> do
      f' <- toListener f
      lift $ modifyRef f_x $ flip (liftA2 (*>)) f'
    Bound val_x _ ->
      f val_x
  where
    toListener f = do
      r <- ask'
      promise <- freshPromise
      modifyPromises (promise:)
      p <- newRef' False
      writeRef' p True
      pure $ \ val_x -> whenM (readRef' p) $ resolvePromise promise r $ f val_x

unify :: ( MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         , Unifiable f
         ) => Var m f -> Var m f -> VerseT m ()
unify var_x var_y = do
  x@(set_x, _, repr_x) <- findVar var_x
  y@(set_y, _, repr_y) <- findVar var_y
  unless (eqRef set_x set_y) $ case (repr_x, repr_y) of
    (Unbound f_x level_x, Unbound f_y level_y) -> do
      level <- askLevel
      case (level_x == level, level_y == level) of
        (True, True) -> do
          f <- newRef' =<< liftA2 (*>) <$> readRef' f_x <*> readRef' f_y
          union' (\ _ _ -> Unbound f level_x) x y
        (True, False) -> do
          link set_y set_x
          f_y' <- toListener =<< readRef' f_y
          lift $ modifyRef f_x $ flip (liftA2 (*>)) f_y'
        (False, True) -> do
          link set_x set_y
          f_x' <- toListener =<< readRef' f_x
          lift $ modifyRef f_y $ flip (liftA2 (*>)) f_x'
        (False, False) ->
          whenBound var_x $ \ val_x ->
          whenBound var_y $ \ val_y ->
          unify' val_x val_y
    (Unbound f_x level_x, Bound val_y _) -> do
      level <- askLevel
      if level_x == level then do
        union y x
        ($ val_y) =<< readRef' f_x
      else
        whenBound var_x $ \ val_x ->
          unify' val_x val_y
    (Bound val_x _, Unbound f_y level_y) -> do
      level <- askLevel
      if level_y == level then do
        union x y
        ($ val_x) =<< readRef' f_y
      else
        whenBound var_y $ \ val_y ->
          unify' val_x val_y
    (Bound val_x _, Bound val_y _) ->
      unify' val_x val_y
  where
    toListener f = do
      p <- newRef' False
      writeRef' p True
      pure $ whenM (readRef' p) . f

unify' :: ( MonadRef m
          , MonadSupply Label m
          , EqRef (Ref m)
          , Unifiable f
          ) => f (Var m f) -> f (Var m f)  -> VerseT m ()
unify' val_x val_y = zipMatchM val_x val_y >>= \ case
  Nothing -> empty
  Just val_z -> for_ val_z $ uncurry unify

freshen :: ( MonadFix m
           , MonadRef m
           , MonadSupply Label m
           , Traversable f
           ) => Var m f -> VerseT m (Var m f)
freshen = lift . flip evalStateT mempty . freshen'

freshen' :: ( MonadFix m
            , MonadRef m
            , MonadSupply Label m
            , Traversable f
            ) => Var m f -> StateT (IntMap (Var m f)) m (Var m f)
freshen' var = lift (find' $ unVar var) >>= \ case
  (_, _, Bound val i) -> gets (IntMap.lookup i) >>= \ case
    Nothing -> mfix $ \ var -> do
      modify $ IntMap.insert i var
      lift . newVar' =<< for val freshen'
    Just val -> pure val
  (set, _, Unbound {}) -> pure $ Var set

freezeBy :: (MonadFix m, MonadRef m, Traversable g) =>
            (forall a . f a -> g a) ->
            Var m f -> VerseT m (Maybe (Fix g))
freezeBy f = lift . freezeBy' f

freezeBy' :: (MonadFix m, MonadRef m, Traversable g) =>
             (forall a . f a -> g a) ->
             Var m f -> m (Maybe (Fix g))
freezeBy' f = runMaybeT . flip evalStateT mempty . freezeBy'' f

freezeBy'' :: (MonadFix m, MonadRef m, Traversable g) =>
              (forall a . f a -> g a) ->
              Var m f -> StateT (IntMap (Fix g)) (MaybeT m) (Fix g)
freezeBy'' f = unVar >>> find' >>> lift >>> lift >=> \ case
  (_, _, Bound val i) -> gets (IntMap.lookup i) >>= \ case
    Nothing -> mfix $ \ val' -> do
      modify $ IntMap.insert i val'
      Fix <$> traverse (freezeBy'' f) (f val)
    Just val' -> pure val'
  _ -> empty

split :: MonadRef m
      => VerseT m a -> (Maybe (a, VerseT m a) -> VerseT m ()) -> VerseT m ()
split m f = do
  r <- ask'
  world <- getWorld
  world' <- freshWorld
  split' r { level = r.level + 1 } m $ \ x -> f x *> resolveWorld world'
  putWorld world'

split' :: MonadRef m
       => R -> VerseT m a -> (Maybe (a, VerseT m a) -> VerseT m ()) -> VerseT m ()
split' r m f = do
  s <- getPromises
  putPromises []
  msplit' (local' (const r) m) >>= \ case
    Nothing -> do
      putPromises s
      f Nothing
    Just (x, m) -> do
      s' <- getPromises
      putPromises s
      splitPromises s' $ \ case
        False -> split' r (backtrack' *> m) f
        True -> f $ Just (x, m)

splitPromises :: MonadRef m
              => [Promise m] -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromises = splitPromises' . reverse

splitPromises' :: MonadRef m
               => [Promise m] -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromises' xs f = case xs of
  [] -> f True
  x:xs -> splitPromise x $ \ case
    False -> f False
    True -> splitPromises' xs f

splitPromise :: MonadRef m => Promise m -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromise = whenResolved

findVar :: MonadRef m => Var m f -> VerseT m (Found m (VarState m f))
findVar = find . unVar

type Set m a = Ref m (SetState m a)

data SetState m a
  = Repr !Word a
  | Link !(Set m a)

type Found m a = (Set m a, Word, a)

union :: ( MonadRef m
         , EqRef (Ref m)
         ) => Found m a -> Found m a -> VerseT m ()
union = union' const

union' :: ( MonadRef m
          , EqRef (Ref m)
          ) => (a -> a -> a) -> Found m a -> Found m a -> VerseT m ()
union' f (set_x, size_x, repr_x) (set_y, size_y, repr_y) = do
  if size_y > size_x then do
    writeRef' set_x $ Link set_y
    writeRef' set_y $ Repr (size_x + size_y) (f repr_x repr_y)
  else do
    writeRef' set_x $ Repr (size_x + size_y) (f repr_x repr_y)
    writeRef' set_y $ Link set_x

link :: MonadRef m => Set m a -> Set m a -> VerseT m ()
link set_x set_y = writeRef' set_y $ Link set_x

find :: MonadRef m => Set m a -> VerseT m (Found m a)
find = lift . find'

find' :: MonadRef m => Set m a -> m (Found m a)
find' set = readRef set >>= \ case
  Repr size repr -> pure (set, size, repr)
  Link set -> find' set

newtype World m = World (Ref m (WorldState m))

data WorldState m
  = PendingWorld !(Ref m (VerseT m ()))
  | RealWorld

freshWorld :: MonadRef m => VerseT m (World m)
freshWorld =
  VerseT . lift $
  fmap World . Backtrack.newRef . PendingWorld =<<
  lift (newRef $ pure ())

newWorld' :: MonadRef m => RefLogicT m (World m)
newWorld' = fmap World $ Backtrack.newRef RealWorld

resolveWorld :: MonadRef m => World m -> VerseT m ()
resolveWorld (World ref) = VerseT (lift $ Backtrack.readRef ref) >>= \ case
  PendingWorld m -> do
    VerseT . lift $ Backtrack.writeRef ref RealWorld
    join . lift $ readRef m
  RealWorld -> error "resolveWorld"

whenRealWorld :: MonadRef m => World m -> VerseT m () -> VerseT m ()
whenRealWorld (World ref_x) m = VerseT (lift $ Backtrack.readRef ref_x) >>= \ case
  PendingWorld m_x -> do
    m' <- toListener m
    lift $ modifyRef m_x (*> m')
  RealWorld -> m
  where
    toListener m = do
      r <- ask'
      promise <- freshPromise
      modifyPromises (promise:)
      p <- newRef' False
      writeRef' p True
      pure $ whenM (readRef' p) $ resolvePromise promise r m

newtype R = R
  { level :: Level
  }

type Level = Word

data S m = S
  { promises :: Promises m
  , world :: World m
  }

type Promises m = [Promise m]

newtype Promise m = Promise (Ref m (PromiseState m))

data PromiseState m
  = Pending [Listener m]
  | Resolved !Bool

data Listener m = Listener !(Ref m Bool) !(Bool -> VerseT m ())

freshPromise :: MonadRef m => VerseT m (Promise m)
freshPromise = fmap Promise . newRef' $ Pending []

resolvePromise :: MonadRef m => Promise m -> R -> VerseT m () -> VerseT m ()
resolvePromise (Promise ref) r m = readRef' ref >>= \ case
  Pending xs -> hasListeners xs >>= \ case
    False -> msplit' (local' (const r) m) >>= \ case
      Nothing -> empty
      Just ((), _) -> writeRef' ref (Resolved True)
    True -> split' r m $ \ case
      Nothing -> writeRef' ref (Resolved False) *> apListeners xs False
      Just ((), _) -> writeRef' ref (Resolved True) *> apListeners xs True
  Resolved _ -> error "resolve"

hasListeners :: MonadRef m => [Listener m] -> VerseT m Bool
hasListeners = \ case
  [] -> pure False
  Listener ref _:xs -> readRef' ref >>= \ case
    False -> hasListeners xs
    True -> pure True

apListeners :: MonadRef m => [Listener m] -> Bool -> VerseT m ()
apListeners xs x = case xs of
  [] -> pure ()
  Listener ref f:xs -> readRef' ref >>= \ case
    False -> apListeners xs x
    True -> f x *> apListeners xs x

whenResolved :: MonadRef m => Promise m -> (Bool -> VerseT m ()) -> VerseT m ()
whenResolved (Promise ref_x) f = readRef' ref_x >>= \ case
  Pending xs -> do
    f' <- toListener f
    lift . writeRef ref_x . Pending $ f' : xs
  Resolved x -> f x
  where
    toListener f = do
      r <- ask'
      promise <- freshPromise
      modifyPromises (promise:)
      p <- newRef' False
      writeRef' p True
      pure $ Listener p (resolvePromise promise r . f)

askLevel :: Monad m => VerseT m Level
askLevel = asks' level

ask' :: Monad m => VerseT m R
ask' = VerseT ask

asks' :: Monad m => (R -> a) -> VerseT m a
asks' = VerseT . asks

local' :: Monad m => (R -> R) -> VerseT m a -> VerseT m a
local' f = VerseT . local f . unVerseT

getPromises :: VerseT m (Promises m)
getPromises = VerseT $ gets promises

putPromises :: Promises m -> VerseT m ()
putPromises promises = VerseT . modify $ \ s -> s { promises }

modifyPromises :: (Promises m -> Promises m) -> VerseT m ()
modifyPromises f = VerseT . modify $ \ s -> s { promises = f s.promises }

getWorld :: VerseT m (World m)
getWorld = VerseT $ gets world

putWorld :: World m -> VerseT m ()
putWorld world = VerseT . modify $ \ s -> s { world }

localWorld :: (World m -> World m) -> VerseT m a -> VerseT m a
localWorld f m = do
  world <- getWorld
  putWorld $ f world
  x <- m
  putWorld world
  pure x

newRef' :: MonadRef m => a -> VerseT m (Ref m a)
newRef' = VerseT . lift . newRef

readRef' :: MonadRef m => Ref m a -> VerseT m a
readRef' = VerseT . lift . readRef

writeRef' :: MonadRef m => Ref m a -> a -> VerseT m ()
writeRef' ref = VerseT . lift . writeRef ref

msplit' :: Monad m => VerseT m a -> VerseT m (Maybe (a, VerseT m a))
msplit' = VerseT . fmap (fmap (fmap VerseT)) . msplit . unVerseT

backtrack' :: MonadRef m => VerseT m ()
backtrack' = VerseT $ lift backtrack

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
