{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Verse
  ( VerseT
  , runVerseT
  , Label
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Backtrack (backtrack)
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Ref.Logic
import Control.Monad.RST
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.Unify
import Control.Monad.Var (MonadVar, freshVar)
import Control.Monad.Var qualified as Var
import Control.Monad.Verse.Class ( MonadVerse
                                 , whenBound
                                 , freshWorld
                                 , getWorld
                                 , putWorld
                                 , unifyWorld
                                 )
import Control.Monad.Verse.Class qualified

import Data.Coerce
import Data.Fix
import Data.Foldable (for_)
import Data.Functor
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.Ref
import Data.Traversable
import Data.Unifiable

newtype VerseT m a = VerseT
  { unVerseT :: UnVerseT m a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadFail
             , MonadIO
             , MonadPlus
             , MonadRef
             , Backtrack.MonadRef
             )

type UnVerseT m = RST (R m) (S m) (RefLogicT m)

runVerseT :: (MonadRef m, MonadSupply Label m) => VerseT m a -> m [a]
runVerseT m = do
  world <- newRef =<< newWorld'
  runRefLogicT (evalRST (unVerseT m) R { level, world } S { promises })
  where
    level = minBound
    promises = []

deriving instance MonadError e m => MonadError e (VerseT m)

deriving instance MonadSupply s m => MonadSupply s (VerseT m)

instance MonadTrans VerseT where
  lift = VerseT . lift . lift

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => MonadVar (VerseT m) where
  type Var (VerseT m) = Var m

  freshVar =
    fmap Var . newSet =<<
    Unbound <$> lift (newRef . const $ pure ()) <*> askLevel

  newVar = lift . newVar'

  readVar var = find (unVar var) <&> \ case
    (_, _, Bound x _) -> Just x
    _ -> Nothing

  freshen = lift . flip evalStateT mempty . freshen'

  freeze = lift . freeze'

newVar' :: (MonadRef m, MonadSupply Label m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newSet' . Bound x =<< supply

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

freeze' :: ( MonadFix m
           , MonadRef m
           , Traversable f
           ) => Var m f -> m (Maybe (Fix f))
freeze' = runMaybeT . flip evalStateT mempty . freeze''

freeze'' :: ( MonadFix m
            , MonadRef m
            , Traversable f
            ) => Var m f -> StateT (IntMap (Fix f)) (MaybeT m) (Fix f)
freeze'' = unVar >>> find' >>> lift >>> lift >=> \ case
  (_, _, Bound val i) -> gets (IntMap.lookup i) >>= \ case
    Nothing -> mfix $ \ val' -> do
      modify $ IntMap.insert i val'
      Fix <$> traverse freeze'' val
    Just val' -> pure val'
  _ -> empty

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => MonadUnify (VerseT m) where
  unify var_x var_y = do
    x@(set_x, _, repr_x) <- find $ unVar var_x
    y@(set_y, _, repr_y) <- find $ unVar var_y
    unless (eqRef set_x set_y) $ case (repr_x, repr_y) of
      (Unbound f_x level_x, Unbound f_y level_y) -> do
        level <- askLevel
        case (level_x == level, level_y == level) of
          (True, True) -> do
            f_y <- toListener =<< lift (readRef f_y)
            lift $ modifyRef f_x $ flip (liftA2 (*>)) f_y
            union x y
          (True, False) -> do
            link set_y set_x
            f_x <- toListener =<< lift (readRef f_x)
            lift $ modifyRef f_y $ flip (liftA2 (*>)) f_x
          (False, True) -> do
            link set_x set_y
            f_y <- toListener =<< lift (readRef f_y)
            lift $ modifyRef f_x $ flip (liftA2 (*>)) f_y
          (False, False) ->
            whenBound var_x $ \ val_x ->
            whenBound var_y $ \ val_y ->
            unify' val_x val_y
      (Unbound f_x level_x, Bound val_y _) -> do
        level <- askLevel
        if level_x == level then do
          union y x
          ($ val_y) =<< readRef f_x
        else
          whenBound var_x $ \ val_x ->
            unify' val_x val_y
      (Bound val_x _, Unbound f_y level_y) -> do
        level <- askLevel
        if level_y == level then do
          union x y
          ($ val_x) =<< readRef f_y
        else
          whenBound var_y $ \ val_y ->
            unify' val_x val_y
      (Bound val_x _, Bound val_y _) ->
        unify' val_x val_y
    where
      toListener f = do
        p <- newRef False
        writeRef p True
        pure $ whenM (readRef p) . f

unify' :: ( MonadFix m
          , MonadRef m
          , MonadSupply Label m
          , EqRef (Ref m)
          , Unifiable f
          ) => f (Var m f) -> f (Var m f)  -> VerseT m ()
unify' val_x val_y = zipMatchM val_x val_y >>= \ case
  Nothing -> empty
  Just val_z -> for_ val_z $ uncurry unify

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => MonadVerse (VerseT m) where
  type World (VerseT m) = World m

  whenBound var_x f = do
    (_, _, repr_x) <- find $ unVar var_x
    case repr_x of
      Unbound f_x _ ->
        withListener f $
        lift . modifyRef f_x . flip (liftA2 (*>)) <=< toListener
      Bound val_x _ ->
        f val_x
    where
      toListener f = do
        p <- newRef False
        writeRef p True
        pure $ whenM (readRef p) . f

  freshWorld =
    fmap World . newSet . UnboundWorld =<<
    lift (newRef $ pure ())

  getWorld = Backtrack.readRef =<< asks' world

  putWorld x = flip Backtrack.writeRef x =<< asks' world

  unifyWorld world_x world_y = do
    x@(set_x, _, repr_x) <- find $ unWorld world_x
    y@(set_y, _, repr_y) <- find $ unWorld world_y
    unless (eqRef set_x set_y) $ case (repr_x, repr_y) of
      (UnboundWorld m_x, UnboundWorld m_y) -> do
        m_y <- toWorldListener =<< lift (readRef m_y)
        lift $ modifyRef m_x $ (*> m_y)
        unionWorld x y
      (UnboundWorld m_x, BoundWorld) -> do
        unionWorld y x
        join $ Backtrack.readRef m_x
      (BoundWorld, UnboundWorld m_y) -> do
        unionWorld x y
        join $ Backtrack.readRef m_y
      (BoundWorld, BoundWorld) ->
        pure ()

  whenWorldBound world_x m = do
    (_, _, repr_x) <- find $ unWorld world_x
    case repr_x of
      UnboundWorld m_x ->
        withWorldListener m $
        lift . modifyRef m_x . flip (*>) <=< toWorldListener
      BoundWorld -> m

  split m f = do
    r <- ask'
    split' r { level = r.level + 1 } m f

toWorldListener :: MonadRef m => VerseT m () -> VerseT m (VerseT m ())
toWorldListener m = do
  p <- Backtrack.newRef False
  Backtrack.writeRef p True
  pure $ whenM (Backtrack.readRef p) m

split' :: ( MonadFix m
          , MonadRef m
          , MonadSupply Label m
          , EqRef (Ref m)
          ) => R m -> VerseT m a -> (Maybe (a, VerseT m a) -> VerseT m ()) -> VerseT m ()
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

splitPromises :: ( MonadFix m
                 , MonadRef m
                 , MonadSupply Label m
                 , EqRef (Ref m)
                 ) => [Promise m] -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromises = splitPromises' . reverse

splitPromises' :: ( MonadFix m
                  , MonadRef m
                  , MonadSupply Label m
                  , EqRef (Ref m)
                  ) => [Promise m] -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromises' xs f = case xs of
  [] -> f True
  x:xs -> splitPromise x $ \ case
    False -> f False
    True -> splitPromises' xs f

splitPromise :: ( MonadFix m
                , MonadRef m
                , MonadSupply Label m
                , EqRef (Ref m)
                ) => Promise m -> (Bool -> VerseT m ()) -> VerseT m ()
splitPromise = whenResolved

newtype Var m f = Var { unVar :: Set m (VarState m f) }

data VarState m f
  = Unbound !(Ref m (f (Var m f) -> VerseT m ())) !Level
  | Bound !(f (Var m f)) !Label

type Label = Int

type Set m a = Ref m (SetState m a)

data SetState m a
  = Repr !Word a
  | Link !(Set m a)

newSet :: MonadRef m => a -> VerseT m (Set m a)
newSet = lift . newSet'

newSet' :: MonadRef m => a -> m (Set m a)
newSet' = newRef . Repr 1

type Found m a = (Set m a, Word, a)

union :: ( MonadRef m
         , EqRef (Ref m)
         ) => Found m a -> Found m a -> VerseT m ()
union = union' const

union' :: MonadRef m => (a -> a -> a) -> Found m a -> Found m a -> VerseT m ()
union' f (set_x, size_x, repr_x) (set_y, size_y, repr_y) = do
  if size_y > size_x then do
    writeRef set_x $ Link set_y
    writeRef set_y $ Repr (size_x + size_y) (f repr_x repr_y)
  else do
    writeRef set_x $ Repr (size_x + size_y) (f repr_x repr_y)
    writeRef set_y $ Link set_x

link :: MonadRef m => Set m a -> Set m a -> VerseT m ()
link set_x set_y = writeRef set_y $ Link set_x

find :: MonadRef m => Set m a -> VerseT m (Found m a)
find = lift . find'

find' :: MonadRef m => Set m a -> m (Found m a)
find' set = readRef set >>= \ case
  Repr size repr -> pure (set, size, repr)
  Link set -> find' set

newtype World m = World { unWorld :: Set m (WorldState m) }

data WorldState m
  = UnboundWorld !(Ref m (VerseT m ()))
  | BoundWorld

newWorld' :: MonadRef m => m (World m)
newWorld' = World <$> newSet' BoundWorld

unionWorld :: MonadRef m =>
              Found m (WorldState m) ->
              Found m (WorldState m) ->
              VerseT m ()
unionWorld (set_x, size_x, repr_x) (set_y, size_y, _) = do
  if size_y > size_x then do
    Backtrack.writeRef set_x $ Link set_y
    Backtrack.writeRef set_y $ Repr (size_x + size_y) repr_x
  else do
    Backtrack.writeRef set_x $ Repr (size_x + size_y) repr_x
    Backtrack.writeRef set_y $ Link set_x

data R m = R
  { level :: Level
  , world :: Ref m (World m)
  }

type Level = Word

newtype S m = S
  { promises :: Promises m
  }

type Promises m = [Promise m]

newtype Promise m = Promise (Ref m (PromiseState m))

data PromiseState m
  = Pending [Listener m]
  | Resolved !Bool

data Listener m = Listener !(Ref m Bool) !(Bool -> VerseT m ())

freshPromise :: MonadRef m => VerseT m (Promise m)
freshPromise = fmap Promise . newRef $ Pending []

resolvePromise :: ( MonadFix m
                  , MonadRef m
                  , MonadSupply Label m
                  , EqRef (Ref m)
                  ) => Promise m -> R m -> Ref m (VerseT m ()) -> VerseT m () -> VerseT m ()
resolvePromise (Promise ref) r ref_m m = readRef ref >>= \ case
  Pending xs -> hasListeners xs >>= \ case
    False -> msplit' (local' (const r) m) >>= \ case
      Nothing -> empty
      Just ((), m) -> do
        writeRef ref $ Resolved True
        lift $ writeRef ref_m m
    True -> split' r m $ \ case
      Nothing -> do
        writeRef ref $ Resolved False
        apListeners xs False
      Just ((), m) -> do
        writeRef ref $ Resolved True
        lift $ writeRef ref_m m
        apListeners xs True
  Resolved _ -> error "resolve"

hasListeners :: MonadRef m => [Listener m] -> VerseT m Bool
hasListeners = \ case
  [] -> pure False
  Listener ref _:xs -> readRef ref >>= \ case
    False -> hasListeners xs
    True -> pure True

apListeners :: MonadRef m => [Listener m] -> Bool -> VerseT m ()
apListeners xs x = case xs of
  [] -> pure ()
  Listener ref f:xs -> readRef ref >>= \ case
    False -> apListeners xs x
    True -> f x *> apListeners xs x

whenResolved :: ( MonadFix m
                , MonadRef m
                , MonadSupply Label m
                , EqRef (Ref m)
                ) => Promise m -> (Bool -> VerseT m ()) -> VerseT m ()
whenResolved (Promise ref_x) f = readRef ref_x >>= \ case
  Pending xs ->
    withListener f $
    lift . writeRef ref_x . Pending . (: xs) <=< toListener
  Resolved x -> f x
  where
    toListener f = do
      p <- newRef False
      writeRef p True
      pure $ Listener p f

withListener :: ( MonadFix m
                , MonadRef m
                , MonadSupply Label m
                , EqRef (Ref m)
              ) => (a -> VerseT m ()) -> ((a -> VerseT m ()) -> VerseT m b) -> VerseT m b
withListener f k = do
  r <- ask'
  promise <- freshPromise
  modifyPromises (promise:)
  world <- getWorld
  world' <- freshWorld
  putWorld world'
  ref_m <- lift $ newRef empty
  x <- k $ \ x -> resolvePromise promise r ref_m $ do
    world'' <- getWorld
    putWorld world
    f x
    unifyWorld world' =<< getWorld
    putWorld world''
  pure () <|> join (lift $ readRef ref_m)
  pure x

withWorldListener :: ( MonadFix m
                     , MonadRef m
                     , MonadSupply Label m
                     , EqRef (Ref m)
                     ) => VerseT m () -> (VerseT m () -> VerseT m b) -> VerseT m b
withWorldListener m k = withListener (\ _ -> m) (k . ($ ()))

askLevel :: Monad m => VerseT m Level
askLevel = asks' level

ask' :: Monad m => VerseT m (R m)
ask' = VerseT ask

asks' :: Monad m => (R m -> a) -> VerseT m a
asks' = VerseT . asks

local' :: Monad m => (R m -> R m) -> VerseT m a -> VerseT m a
local' f = VerseT . local f . unVerseT

getPromises :: VerseT m (Promises m)
getPromises = VerseT $ gets promises

putPromises :: Promises m -> VerseT m ()
putPromises promises = VerseT . modify $ \ s -> s { promises }

modifyPromises :: (Promises m -> Promises m) -> VerseT m ()
modifyPromises f = VerseT . modify $ \ s -> s { promises = f s.promises }

msplit' :: Monad m => VerseT m a -> VerseT m (Maybe (a, VerseT m a))
msplit' = coerceSplit . msplit . coerce

coerceSplit :: UnVerseT m (Maybe (a, UnVerseT m a)) ->
               VerseT m (Maybe (a, VerseT m a))
coerceSplit = coerce

backtrack' :: MonadRef m => VerseT m ()
backtrack' = VerseT $ lift backtrack

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
