{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Trans.Verse
  ( VerseT
  , runVerseT
  , Var
  , freshVar
  , Label
  , newVar
  , whenBound
  , once
  , lnot
  , ifte
  , all'
  , for'
  , unify
  , freeze
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad ((>=>), when)
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Logic (LogicT, msplit, observeAllT)
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RST
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Maybe

import Data.Functor
import Data.Fix
import Data.Foldable (for_)
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as IntMap
import Data.Ref
import Data.Traversable (for)
import Data.Unifiable

import Data.Proxy

import Debug.Traceable

newtype VerseT m a = VerseT
  { getVerseT :: RST (R m) (S m) (LogicT m) a
  } deriving (Functor, Applicative, Monad)

deriving instance MonadError e m => MonadError e (VerseT m)

instance (MonadRef m, EqRef (Ref m)) => Alternative (VerseT m) where
  empty = VerseT empty
  m <|> n = plus (stop *> m) (backtrack *> n)

instance MonadTrans (VerseT) where
  lift = VerseT . lift . lift

instance MonadRef m => MonadRef (VerseT m)

instance MonadSupply s m => MonadSupply s (VerseT m)

runVerseT :: (MonadRef m, EqRef (Ref m)) => VerseT m a -> m [a]
runVerseT m = do
  let level = minBound
  cursor <- newRef []
  observeAllT (evalRST (getVerseT m) R { level, cursor } [])

data VarState m f
  = Unbound !(Ref m (f (Var m f) -> VerseT m ())) !Level
  | Bound !(f (Var m f)) !Label

instance Traceable (f (Var m f)) => Traceable (VarState m f) where
  debugs = \ case
    Unbound _ level ->
      debugs "(Unbound " .
      debugs level .
      debugs ")"
    Bound val _ ->
      debugs "(Bound " .
      debugs val .
      debugs ")"

type Label = Int

newtype Var m f = Var { getVar :: Set m (VarState m f) }

freshVar :: MonadRef m => VerseT m (Var m f)
freshVar = fmap Var . newRef . Repr 1 =<< Unbound <$> newRef f <*> askLevel
  where
    f = const $ pure ()

newVar :: (MonadRef m, MonadSupply Label m) => f (Var m f) -> VerseT m (Var m f)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Label m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newRef . Repr 1 =<< Bound x <$> supply

whenBound :: ( MonadRef m
             , EqRef (Ref m)
             ) => Var m f -> (f (Var m f) -> VerseT m ()) -> VerseT m ()
whenBound var_x f = do
  (_, _, repr_x) <- findVar var_x
  case repr_x of
    Unbound f_x _ -> do
      f' <- toListener f
      modifyRef f_x $ flip (liftA2 (*>)) f'
    Bound val_x _ ->
      f val_x
  where
    toListener f = do
      f' <- toListener' f
      p <- newRef False
      writeTrailed "EnableWhenBound" p True
      pure $ whenM (readRef p) . f'
    toListener' f = do
      promise <- freshPromise
      modifyPromises (promise:)
      pure $ resolve promise . f

once :: ( MonadRef m
        , EqRef (Ref m)
        ) => VerseT m a -> (a -> VerseT m ()) -> VerseT m ()
once m f = ifte m f empty

lnot :: ( MonadRef m
        , EqRef (Ref m)
        ) => VerseT m a -> VerseT m ()
lnot m = ifte m (const empty) (pure ())

ifte :: ( MonadRef m
        , EqRef (Ref m)
        ) => VerseT m a -> (a -> VerseT m ()) -> VerseT m () -> VerseT m ()
ifte m f n = do
  promise <- freshPromise
  modifyPromises (promise:)
  level <- (+ 1) <$> askLevel
  predCursor <- askCursor
  cursor <- newRef =<< readRef predCursor
  local' (const R { level, cursor }) $ split m $ \ case
    Nothing -> resolve promise n
    Just (x, _) -> resolve promise $ f x
  writeRef predCursor =<< readRef cursor

all' :: ( MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , EqRef (Ref m)
        , Traversable f
        ) => VerseT m (Var m f) -> ([Var m f] -> VerseT m ()) -> VerseT m ()
all' m f = do
  promise <- freshPromise
  modifyPromises (promise:)
  level <- (+ 1) <$> askLevel
  predCursor <- askCursor
  cursor <- newRef =<< readRef predCursor
  local' (const R { level, cursor }) $ loop promise m f
  writeRef predCursor =<< readRef cursor
  where
    loop promise m f = split m $ \ case
      Nothing -> resolve promise $ f []
      Just (var, m) -> do
        var <- freshen var
        loop promise m $ \ vars -> f $ var:vars

for' m f g = undefined

unify :: ( MonadRef m
         , EqRef (Ref m)
         , Unifiable f
         ) => Var m f -> Var m f -> VerseT m ()
unify var_x var_y = trace "unify" $ do
  (set_x, _, repr_x) <- findVar var_x
  (set_y, _, repr_y) <- findVar var_y
  when (not $ eqRef set_x set_y) $ case (repr_x, repr_y) of
    (Unbound f_x level_x, Unbound f_y level_y) -> do
      level <- askLevel
      if level_x == level && level_y == level then do
        f <- newRef =<< liftA2 (*>) <$> readRef f_x <*> readRef f_y
        union' (\ _ _ -> Unbound f level_x) set_x set_y
      else
        whenBound var_x $ \ val_x ->
          whenBound var_y $ \ val_y ->
            unify' val_x val_y
    (Unbound f_x level_x, Bound val_y _) -> do
      level <- askLevel
      if level_x == level then do
        union set_y set_x
        ($ val_y) =<< readRef f_x
      else
        whenBound var_x $ \ val_x ->
          unify' val_x val_y
    (Bound val_x _, Unbound f_y level_y) -> do
      level <- askLevel
      if level_y == level then do
        union set_x set_y
        ($ val_x) =<< readRef f_y
      else
        whenBound var_y $ \ val_y ->
          unify' val_x val_y
    (Bound val_x _, Bound val_y _) ->
      unify' val_x val_y

unify' :: ( MonadRef m
          , EqRef (Ref m)
          , Unifiable f
          ) => f (Var m f) -> f (Var m f)  -> VerseT m ()
unify' val_x val_y = case zipMatch val_x val_y of
  Nothing -> empty
  Just val_z -> for_ val_z $ uncurry $ unify

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
  _ -> pure var

freeze :: ( MonadFix m
          , MonadRef m
          , EqRef (Ref m)
          , Traversable f
          ) => Var m f -> VerseT m (Maybe (Fix f))
freeze var = do
  s <- getPromises
  putPromises []
  forcePromises s
  lift $ freeze' var

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

force :: (MonadRef m, EqRef (Ref m)) => VerseT m a -> VerseT m a
force m = trace "force" $ msplit' m >>= \ case
  Nothing -> empty
  Just (x, m) -> trace "force Just" $ do
    s <- getPromises
    putPromises []
    join' (forcePromises s $> x) (force m)

forcePromises :: (MonadRef m, EqRef (Ref m)) => [Promise m] -> VerseT m ()
forcePromises = forcePromises' . reverse

forcePromises' :: (MonadRef m, EqRef (Ref m)) => [Promise m] -> VerseT m ()
forcePromises' = \ case
  [] -> pure ()
  x:xs -> do
    getResolved x >>= \ case
      Nothing -> pure ()
      Just m -> force m
    forcePromises' xs

split :: ( MonadRef m
         , EqRef (Ref m)
         ) => VerseT m a -> (Maybe (a, VerseT m a) -> VerseT m ()) -> VerseT m ()
split m f = do
  s <- getPromises
  putPromises []
  msplit' m >>= \ case
    Nothing -> do
      putPromises s
      f Nothing
    Just (x, m) -> do
      s' <- getPromises
      putPromises s
      splitPromises s' $ \ case
        Nothing -> split m f
        Just m' -> f $ Just (x, join' (m' $> x) m)

splitPromises :: ( MonadRef m
                 , EqRef (Ref m)
                 ) => [Promise m] -> (Maybe (VerseT m ()) -> VerseT m ()) -> VerseT m ()
splitPromises = splitPromises' . reverse

splitPromises' :: ( MonadRef m
                  , EqRef (Ref m)
                  ) => [Promise m] -> (Maybe (VerseT m ()) -> VerseT m ()) -> VerseT m ()
splitPromises' xs f = case xs of
  [] -> f $ Just empty
  x:xs -> splitPromise x $ \ case
    Nothing -> f Nothing
    Just m -> splitPromises' xs $ \ case
      Nothing -> loop m xs f
      Just m' -> f $ Just $ join' m' m
  where
    loop m xs f = split m $ \ case
      Nothing -> f Nothing
      Just ((), m) -> splitPromises' xs $ \ case
        Nothing -> loop m xs f
        Just m' -> f $ Just $ join' m' m

splitPromise :: ( MonadRef m
                , EqRef (Ref m)
                ) => Promise m -> (Maybe (VerseT m ()) -> VerseT m ()) -> VerseT m ()
splitPromise (Promise x f_x) f = readRef x >>= \ case
  Nothing -> do
    f' <- toListener f
    modifyRef f_x $ (flip (liftA2 (*>))) f'
  Just m -> split m $ \ case
    Nothing -> f Nothing
    Just ((), m) -> f $ Just m
  where
    toListener f = do
      p <- newRef False
      writeTrailed "EnableResolve" p True
      f' <- toListener' f
      pure $ whenM (readRef p) . f'
    toListener' f = do
      r@R { cursor } <- ask'
      pure $ \ m -> do
        predCursor <- askCursor
        when (not $ eqRef cursor predCursor) $ do
          xs <- readRef cursor
          writeRef cursor =<< (Jump cursor xs:) <$> readRef predCursor
        () <- local' (const r) $ split m $ \ case
          Nothing -> f Nothing
          Just ((), m) -> f $ Just m
        writeRef predCursor =<< readRef cursor

findVar :: MonadRef m => Var m f -> VerseT m (Set m (VarState m f), Word, VarState m f)
findVar = find . getVar

join' :: Monad m => VerseT m a -> VerseT m a -> VerseT m a
join' m n = msplit' m >>= \ case
  Nothing -> n
  Just (x, m) -> plus (pure x) (join' m n)

plus :: VerseT m a -> VerseT m a -> VerseT m a
plus m n = VerseT $ getVerseT m <|> getVerseT n

type Set m a = Ref m (SetState m a)

data SetState m a
  = Repr !Word a
  | Link !(Set m a)

instance Traceable a => Traceable (SetState m a) where
  debugs = \ case
    Repr size repr ->
      debugs "(Repr " .
      debugs size .
      debugs " " .
      debugs repr .
      debugs ")"
    Link set ->
      debugs "(Link " .
      unsafeDebugsRef (Proxy :: Proxy m) set .
      debugs ")"

union :: ( MonadRef m
         , EqRef (Ref m)
         ) => Set m a -> Set m a -> VerseT m ()
union = union' const

union' :: ( MonadRef m
          , EqRef (Ref m)
          ) => (a -> a -> a) -> Set m a -> Set m a -> VerseT m ()
union' f set_x set_y = trace "union" $ do
  (set_x, size_x, repr_x) <- find set_x
  (set_y, size_y, repr_y) <- find set_y
  if size_y > size_x then do
    writeTrailed "Link" set_x $ Link set_y
    writeTrailed "Repr" set_y $ Repr (size_x + size_y) (f repr_x repr_y)
  else do
    writeTrailed "Repr" set_x $ Repr (size_x + size_y) (f repr_x repr_y)
    writeTrailed "Link" set_y $ Link set_x

find :: MonadRef m => Set m a -> VerseT m (Set m a, Word, a)
find = lift . find'

find' :: MonadRef m => Set m a -> m (Set m a, Word, a)
find' set = readRef set >>= \ case
  Repr size repr -> pure (set, size, repr)
  Link set -> find' set

data R m = R
  { level :: !Level
  , cursor :: !(Cursor m)
  }

type Level = Word

type Cursor m = Ref m [Insn m]

type S m = Promises m

type Promises m = [Promise m]

data Insn m
  = Forward !(Cmd m)
  | Backward !(Cmd m)
  | Stop !(Cursor m)
  | Jump !(Cursor m) [Insn m]

instance Traceable (Insn m) where
  debugs = \ case
    Forward x ->
      debugs "Forward (" .
      debugs x .
      debugs ")"
    Backward x ->
      debugs "Backward (" .
      debugs x .
      debugs ")"
    Stop x ->
      debugs "Stop " .
      debugsStableName x
    Jump x xs ->
      debugs "Jump " .
      debugsStableName x .
      debugs " " .
      debugs xs

data Cmd m = Cmd
  { name :: String
  , forward :: m ()
  , backward :: m ()
  }

instance Traceable (Cmd m) where
  debugs Cmd {..} =
    debugs "Cmd { name = " .
    debugs name .
    debugs ", forward = " .
    debugsStableName forward .
    debugs ", backward = " .
    debugsStableName backward .
    debugs "}"

data Promise m = Promise !(Ref m (PromiseState m)) !(Ref m (VerseT m () -> VerseT m ()))

type PromiseState m = Maybe (VerseT m ())

instance EqRef (Ref m) => Eq (Promise m) where
  Promise x _ == Promise y _ = eqRef x y

freshPromise :: MonadRef m => VerseT m (Promise m)
freshPromise = Promise <$> newRef Nothing <*> newRef (const $ pure ())

resolve :: (MonadRef m, EqRef (Ref m)) => Promise m -> VerseT m () -> VerseT m ()
resolve (Promise s f) m = trace "resolve" $ readRef s >>= \ case
  Nothing -> do
    writeTrailed "Resolve" s $ Just m
    ($ m) =<< readRef f
  _ -> error "resolve"

getResolved :: MonadRef m => Promise m -> VerseT m (Maybe (VerseT m ()))
getResolved (Promise s _) = readRef s

writeTrailed :: MonadRef m => String -> Ref m a -> a -> VerseT m ()
writeTrailed name ref x = do
  y <- readRef ref
  let
    forward = writeRef ref x
    backward = writeRef ref y
    cmd = Cmd { name, forward, backward }
  lift $ cmd.forward
  cursor <- askCursor
  modifyRef cursor (Backward cmd :)
  traceA =<< readRef cursor

stop :: MonadRef m => VerseT m ()
stop = trace "stop" $ do
  cursor <- askCursor
  modifyRef cursor (Stop cursor :)
  traceA =<< readRef cursor

backtrack :: (MonadRef m, EqRef (Ref m)) => VerseT m ()
backtrack = lift . backtrack' =<< askCursor

backtrack' :: (MonadRef m, EqRef (Ref m)) => Cursor m -> m ()
backtrack' cursor = trace "backtrack" $ do
  traceA $ debugsStableName cursor ""
  traceA =<< readRef cursor
  writeRef cursor =<< exec cursor =<< readRef cursor
  traceA =<< readRef cursor

exec :: (MonadRef m, EqRef (Ref m)) => Cursor m -> [Insn m] -> m [Insn m]
exec i = \ case
  [] -> pure []
  Forward x:xs -> do
    x.forward
    exec i xs
  Backward x:xs -> do
    x.backward
    exec i xs
  Stop j:xs
    | eqRef i j -> pure xs
    | otherwise -> exec i xs
  z@(Jump j ys:xs)
    | eqRef i j -> exec' i z ys
    | otherwise -> writeRef j ys *> exec i xs

exec' :: (Monad m, EqRef (Ref m)) => Cursor m -> [Insn m] -> [Insn m] -> m [Insn m]
exec' i z = \ case
  [] -> pure []
  Forward x:xs -> do
    x.forward
    exec' i (Backward x:z) xs
  Backward x:xs -> do
    x.backward
    exec' i (Forward x:z) xs
  Stop j:xs
    | eqRef i j -> pure $ Jump i xs : z
    | otherwise -> exec' i z xs
  Jump j ys:xs
    | eqRef i j -> exec' i z ys
    | otherwise -> exec' i z xs

askLevel :: Monad m => VerseT m Level
askLevel = asks' level

askCursor :: Monad m => VerseT m (Cursor m)
askCursor = asks' cursor

ask' :: Monad m => VerseT m (R m)
ask' = VerseT ask

asks' :: Monad m => (R m -> a) -> VerseT m a
asks' = VerseT . asks

local' :: Monad m => (R m -> R m) -> VerseT m a -> VerseT m a
local' f = VerseT . local f . getVerseT

getPromises :: VerseT m (Promises m)
getPromises = VerseT get

putPromises :: Promises m -> VerseT m ()
putPromises = VerseT . put

modifyPromises :: (Promises m -> Promises m) -> VerseT m ()
modifyPromises = VerseT . modify

msplit' :: Monad m => VerseT m a -> VerseT m (Maybe (a, VerseT m a))
msplit' = trace "msplit" . VerseT . fmap (fmap (fmap VerseT)) . msplit . getVerseT

whenM :: Monad m => m Bool -> m () -> m ()
whenM p m = p >>= flip when m
