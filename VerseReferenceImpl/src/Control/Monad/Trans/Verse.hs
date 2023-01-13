{-# LANGUAGE LambdaCase #-}
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
  , once'
  , lnot'
  , ifte'
  , all'
  , for'
  , unify
  , freshen
  , freeze
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad ((>=>), unless, when)
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Logic
import Control.Monad.RST
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.Var (MonadVar)
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
  { getVerseT :: RST R (S m) (RefLogicT m) a
  } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadFail
             , MonadIO
             , MonadRef
             )

deriving instance MonadError e m => MonadError e (VerseT m)

deriving instance MonadSupply s m => MonadSupply s (VerseT m)

instance MonadTrans VerseT where
  lift = VerseT . lift . lift

instance Monad m => MonadLogic (VerseT m) where
  msplit =
    VerseT .
    fmap (fmap (fmap VerseT)) .
    msplit .
    local (\ r -> r { level = r.level + 1 }) .
    getVerseT

instance ( MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => MonadVar (VerseT m) where
  type Var (VerseT m) = Var m

  freshVar =
    fmap Var . newRef . Repr 1 =<<
    Unbound <$> newRef (const $ pure ()) <*> askLevel

  newVar = lift . newVar'

  readVar var = findVar var <&> \ case
    (_, _, Bound x _) -> Just x
    _ -> Nothing

runVerseT :: MonadRef m => VerseT m a -> m [a]
runVerseT m = do
  let level = minBound
  runRefLogicT (evalRST (getVerseT m) R { level } [])

data VarState m f
  = Unbound !(Ref m (f (Var m f) -> VerseT m ())) !Level
  | Bound !(f (Var m f)) !Label

type Label = Int

newtype Var m f = Var { getVar :: Set m (VarState m f) }

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
      p <- newRef False
      writeRef p True
      pure $ \ val_x -> whenM (readRef p) $ resolve promise r $ f val_x

once' :: MonadRef m => VerseT m a -> (a -> VerseT m ()) -> VerseT m ()
once' m f = ifte' m f empty

lnot' :: MonadRef m => VerseT m a -> VerseT m ()
lnot' m = ifte' m (const empty) (pure ())

ifte' :: MonadRef m
      => VerseT m a -> (a -> VerseT m ()) -> VerseT m () -> VerseT m ()
ifte' m f n = split m $ \ case
  Nothing -> n
  Just (x, _) -> f x

all' :: ( MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , Traversable f
        ) => VerseT m (Var m f) -> ([Var m f] -> VerseT m ()) -> VerseT m ()
all' m f = split m $ \ case
  Nothing -> f []
  Just (var, m) -> freshen var >>= \ var ->  all' m $ \ vars -> f $ var : vars

for' :: ( MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , Traversable f
        ) => VerseT m a -> (a -> VerseT m (Var m f)) -> ([Var m f] -> VerseT m ()) -> VerseT m ()
for' m f g = split m $ \ case
  Nothing -> g []
  Just (x, m) -> f x >>= freshen >>= \ var -> for' m f $ \ vars -> g $ var : vars

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
          f <- newRef =<< liftA2 (*>) <$> readRef f_x <*> readRef f_y
          union' (\ _ _ -> Unbound f level_x) x y
        (True, False) -> do
          link set_y set_x
          f_y' <- toListener =<< readRef f_y
          lift $ modifyRef f_x $ flip (liftA2 (*>)) f_y'
        (False, True) -> do
          link set_x set_y
          f_x' <- toListener =<< readRef f_x
          lift $ modifyRef f_y $ flip (liftA2 (*>)) f_x'
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
freshen' var = lift (find' $ getVar var) >>= \ case
  (_, _, Bound val i) -> gets (IntMap.lookup i) >>= \ case
    Nothing -> mfix $ \ var -> do
      modify $ IntMap.insert i var
      lift . newVar' =<< for val freshen'
    Just val -> pure val
  (set, _, Unbound {}) -> pure $ Var set

freeze :: ( MonadFix m
          , MonadRef m
          , Traversable f
          ) => Var m f -> VerseT m (Maybe (Fix f))
freeze = lift . freeze'

freeze' :: ( MonadFix m
           , MonadRef m
           , Traversable f
           ) => Var m f -> m (Maybe (Fix f))
freeze' = runMaybeT . flip evalStateT mempty . freeze''

freeze'' :: ( MonadFix m
            , MonadRef m
            , Traversable f
            ) => Var m f -> StateT (IntMap (Fix f)) (MaybeT m) (Fix f)
freeze'' = getVar >>> find' >>> lift >>> lift >=> \ case
  (_, _, Bound val i) -> gets (IntMap.lookup i) >>= \ case
    Nothing -> mfix $ \ val' -> do
      modify $ IntMap.insert i val'
      Fix <$> for val freeze''
    Just val' -> pure val'
  _ -> empty

split :: MonadRef m
      => VerseT m a -> (Maybe (a, VerseT m a) -> VerseT m ()) -> VerseT m ()
split m f = do
  r <- ask'
  split' r { level = r.level + 1 } m f

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
        False -> split' r m f
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
splitPromise (Promise ref_x) f = readRef ref_x >>= \ case
  Pending xs -> do
    f' <- toListener f
    lift . writeRef ref_x . Pending $ f' : xs
  Resolved x -> f x
  where
    toListener f = do
      r <- ask'
      promise <- freshPromise
      modifyPromises (promise:)
      p <- newRef False
      writeRef p True
      pure $ Listener p (resolve promise r . f)

findVar :: MonadRef m => Var m f -> VerseT m (Found m (VarState m f))
findVar = find . getVar

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

newtype R = R
  { level :: Level
  }

type Level = Word

type S m = Promises m

type Promises m = [Promise m]

newtype Promise m = Promise (Ref m (PromiseState m))

data PromiseState m
  = Pending [Listener m]
  | Resolved !Bool

data Listener m = Listener !(Ref m Bool) !(Bool -> VerseT m ())

freshPromise :: MonadRef m => VerseT m (Promise m)
freshPromise = Promise <$> newRef (Pending [])

resolve :: MonadRef m => Promise m -> R -> VerseT m () -> VerseT m ()
resolve (Promise ref) r m = readRef ref >>= \ case
  Pending xs -> hasListeners xs >>= \ case
    False -> msplit' (local' (const r) m) >>= \ case
      Nothing -> empty
      Just ((), _) -> writeRef ref (Resolved True)
    True -> split' r m $ \ case
      Nothing -> writeRef ref (Resolved False) *> apListeners xs False
      Just ((), _) -> writeRef ref (Resolved True) *> apListeners xs True
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

askLevel :: Monad m => VerseT m Level
askLevel = asks' level

ask' :: Monad m => VerseT m R
ask' = VerseT ask

asks' :: Monad m => (R -> a) -> VerseT m a
asks' = VerseT . asks

local' :: Monad m => (R -> R) -> VerseT m a -> VerseT m a
local' f = VerseT . local f . getVerseT

getPromises :: VerseT m (Promises m)
getPromises = VerseT get

putPromises :: Promises m -> VerseT m ()
putPromises = VerseT . put

modifyPromises :: (Promises m -> Promises m) -> VerseT m ()
modifyPromises = VerseT . modify

msplit' :: Monad m => VerseT m a -> VerseT m (Maybe (a, VerseT m a))
msplit' = VerseT . fmap (fmap (fmap VerseT)) . msplit . getVerseT

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
