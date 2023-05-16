{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Verse
  ( VerseT
  , Label
  , runVerseT
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RST
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Trans.Maybe
import Control.Monad.Unify
import Control.Monad.Var (EqVarRef, MonadVar, MonadVarRef, freshenVar, readVar)
import Control.Monad.Var qualified
import Control.Monad.Verse.Class

import Data.Bool
import Data.Either
import Data.Eq
import Data.Fix
import Data.Foldable
import Data.Traversable
import Data.Freshenable
import Data.Function
import Data.Functor
import Data.Int
import Data.IntMap.Internal qualified as LabelMap.Internal
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as LabelMap
import Data.Maybe
import Data.Monoid (mempty)
import Data.Ref
import Data.Tuple
import Data.Semigroup ((<>))
import Data.Unifiable
import Data.Word

import GHC.Exts (Any)

import Prelude (Num (..), ($!), error)

import Unsafe.Coerce qualified

newtype VerseT m a = VerseT
  { unVerseT :: RST SplitLabel (Listeners m) (LogicT (StateT (Heap m) (SupplyT Label m))) a
  } deriving ( Functor
             , Applicative
             , Monad
             )

type SplitLabel = Label

type Listeners m = LabelMap [Listener m Any]

data Listener m a = Listener
  !(Heap m)
  !(a -> VerseT m ())

data Heap m = Nil | Cons
  { label :: {-# UNPACK #-} !Label
  , listeners :: !(HeapListeners m)
  , copied :: !(Heap m)
  , commitRef :: !(Ref m (Commit m))
  , tail :: !(Heap m)
  }

data HeapListeners m = Empty | Listeners
  { kind :: !HeapKind
  , listenerLengthRef :: !(Ref m Word)
  , listenersRef :: !(Ref m (Listeners m))
  , heapListenerRef :: !(Ref m (Maybe (HeapListener m)))
  }

data HeapKind = Split | Choice

isSettled :: MonadRef m => Heap m -> VerseT m Bool
isSettled = \ case
  Cons { listeners = Listeners { listenerLengthRef } } ->
    lift $ (== 0) <$> readRef listenerLengthRef
  _ -> pure False

incrListenerLength :: MonadRef m => Heap m -> VerseT m ()
incrListenerLength = \ case
  Cons { listeners = Listeners { listenerLengthRef } } ->
    lift $ incr listenerLengthRef
  _ -> pure ()

popListeners :: MonadRef m => Label -> VerseT m (Maybe [Listener m Any])
popListeners i = getHeap >>= \ case
  Cons { listeners = Listeners { listenersRef } } -> do
    VerseT . modify $ LabelMap.delete i
    lift (readRef listenersRef) <&> lookupDelete i >>= \ case
      Just (xs, s) -> Just xs <$ lift (writeRef listenersRef s)
      Nothing -> pure Nothing
  _ -> VerseT . state $ \ s -> case lookupDelete i s of
    Just (xs, s) -> (Just xs, s)
    Nothing -> (Nothing, s)

addNewListener :: MonadRef m =>
                  Var m f ->
                  Label ->
                  (f (Var m f) -> VerseT m ()) ->
                  VerseT m ()
addNewListener _ i f = do
  h <- getHeap
  splitLabel <- askSplitLabel
  let f' = toAnyListener $ Listener h $ localSplitLabel (const splitLabel) . f
  addNewListener' h i f'
  VerseT . modify' $ insertListener i f'

addNewListener' :: MonadRef m => Heap m -> Label -> Listener m Any -> VerseT m ()
addNewListener' h i f = do
  incrListenerLength h
  modifyListeners' h $ insertListener i f

addListeners :: MonadRef m => Listeners m -> VerseT m ()
addListeners s = modifyListeners (flip appendListeners s)

addListeners' :: MonadRef m => Heap m -> Listeners m -> VerseT m ()
addListeners' h s = modifyListeners' h (flip appendListeners s)

addListenersFromTo :: MonadRef m => Heap m -> Heap m -> Listeners m -> VerseT m ()
addListenersFromTo h = \ case
  Cons { label } -> addListenersFromTo' h label
  _ -> addListenersFrom h

addListenersFromTo' :: MonadRef m => Heap m -> Label -> Listeners m -> VerseT m ()
addListenersFromTo' h i = case h of
  Cons { label, listeners = Listeners { kind }, tail } -> case label == i of
    False -> case kind of
      Split -> \ s -> addListeners' tail s *> addListenersFromTo' tail i s
      Choice -> addListenersFromTo' tail i
    True -> const $ pure ()
  _ -> const $ pure ()

addListenersFrom :: MonadRef m => Heap m -> Listeners m -> VerseT m ()
addListenersFrom = \ case
  Cons { listeners = Listeners { kind }, tail = h } -> case kind of
    Split -> \ s -> addListeners' h s *> addListenersFrom h s
    Choice -> addListenersFrom h
  _ -> const $ pure ()

moveListenersAt :: MonadRef m => Label -> Label -> VerseT m ()
moveListenersAt i j = modifyListeners $ insertDeleteListeners i j

getListeners' :: MonadRef m => Heap m -> VerseT m (Listeners m)
getListeners' = \ case
  Cons { listeners = Listeners { listenersRef } } -> lift $ readRef listenersRef
  _ -> VerseT get

putListeners :: MonadRef m => Listeners m -> VerseT m ()
putListeners s = getHeap >>= \ case
  Cons { listeners = Listeners { listenersRef } } -> do
    lift $ writeRef listenersRef s
    VerseT $ put s
  _ -> VerseT $ put s

modifyListeners :: MonadRef m => (Listeners m -> Listeners m) -> VerseT m ()
modifyListeners f = do
  getHeap >>= flip modifyListeners' f
  VerseT $ modify' f

modifyListeners' :: MonadRef m => Heap m -> (Listeners m -> Listeners m) -> VerseT m ()
modifyListeners' h f = case h of
  Cons { listeners = Listeners { listenersRef } } -> lift $ modifyRef' listenersRef f
  _ -> pure ()

insertDeleteListeners :: Label -> Label -> Listeners m -> Listeners m
insertDeleteListeners i j s = case lookupDelete j s of
  Just (xs, s) -> insertListeners i xs s
  Nothing -> s

data HeapListener m = HeapListener
  !(Heap m)
  !(VerseT m ())
  !(Heap m -> VerseT m () -> Bool -> VerseT m ())

putHeapListener :: (MonadFix m, MonadRef m) =>
                   Heap m ->
                   (Heap m -> VerseT m () -> Bool -> VerseT m ()) ->
                   VerseT m ()
putHeapListener = \ case
  Cons { listeners = Listeners { heapListenerRef } } -> \ f -> do
    splitLabel <- askSplitLabel
    h <- getHeap
    incrListenerLength h
    let f' h m = localSplitLabel (const splitLabel) . f h m
    lift . writeRef heapListenerRef . Just $ HeapListener h empty f'
  _ -> error "putHeapListener"

modifyHeapListener :: ( MonadFix m
                      , MonadRef m
                      ) => Heap m -> VerseT m () -> VerseT m ()
modifyHeapListener = \ case
  Cons { listeners = Listeners { heapListenerRef } } -> \ m -> lift $
    readRef heapListenerRef >>= \ case
      Just (HeapListener h m' f) ->
        writeRef heapListenerRef . Just $ HeapListener h (plus m m') f
      Nothing -> error "modifyHeapListener"
  _ -> error "modifyHeapListener"

type Label = Int

type LabelMap = IntMap

type Commit m = Heap m -> Heap m -> CommitT m ()

emptyCommit :: Monad m => Commit m
emptyCommit _ _ = pure ()

deriving instance MonadError e m => MonadError e (VerseT m)

runVerseT :: MonadRef m => VerseT m a -> m [a]
runVerseT m =
  runSupplyT .
  flip evalStateT Nil .
  observeAllT' $ do
    splitLabel <- supply
    evalRST (unVerseT m) splitLabel mempty

observeAllT' :: MonadRef m =>
                LogicT (StateT (Heap m) (SupplyT Label m)) a ->
                StateT (Heap m) (SupplyT Label m) [a]
observeAllT' m = unLogicT m sk fk
  where
    sk x fk = (lift . commit' =<< get) *> ((x:) <$> fk)
    fk = pure []

instance MonadTrans VerseT where
  lift = VerseT . lift . lift . lift . lift

instance (MonadFix m, MonadRef m) => Alternative (VerseT m) where
  empty = VerseT empty

  x <|> y = do
    h <- getHeap
    VerseT $ unVerseT (pushChoice h *> x) <|> unVerseT (popChoice h *> y)

instance (MonadFix m, MonadRef m) => MonadPlus (VerseT m)

instance MonadSupply s m => MonadSupply s (VerseT m)

instance (MonadFix m, MonadRef m) => MonadVar (VerseT m) where
  type Var (VerseT m) = Var m

  freshVar = lift . newVar'' =<< Unbound <$> VerseT supply <*> askSplitLabel

  newVar = VerseT . lift . lift . lift . newVar'

  readVar var = findVar var <&> \ case
    (_, Unbound {}) -> Nothing
    (_, Bound _ x) -> Just x

  freezeVar = runFreezeT . freezeVar'

  freshenVar = runFreshenT . freshenVar'

newtype Var m f = Var
  { unVar :: Ref m (RefState (SetState m f))
  }

data RefState a = RefState
  !(LabelMap a)
  !a

data SetState m f
  = Repr !(VarState m f)
  | Link (Var m f)

data VarState m f
  = Unbound {-# UNPACK #-} !Label {-# UNPACK #-} !SplitLabel
  | Bound {-# UNPACK #-} !Label !(f (Var m f))

newVar' :: MonadRef m => f (Var m f) -> SupplyT Label m (Var m f)
newVar' x = lift . newVar'' . flip Bound x =<< supply

newVar'' :: MonadRef m => VarState m f -> m (Var m f)
newVar'' = newVar''' . Repr

newVar''' :: MonadRef m => SetState m f -> m (Var m f)
newVar''' = fmap Var . newRef . RefState mempty

eqVar :: EqRef (Ref m) => Var m f -> Var m f -> Bool
eqVar = eqRef `on` unVar

type FoundVar m f = (Var m f, VarState m f)

findVar :: MonadRef m => Var m f -> VerseT m (FoundVar m f)
findVar var = lift . findVar' var =<< getHeap

findVar' :: MonadRef m => Var m f -> Heap m -> m (FoundVar m f)
findVar' var h = readSetState var h >>= \ case
  Repr x -> pure (var, x)
  Link var' -> findVar'' var var' h

findVar'' :: MonadRef m => Var m f -> Var m f -> Heap m -> m (FoundVar m f)
findVar'' var var' h = readSetState var' h >>= \ case
  Repr x -> pure (var', x)
  Link var'' -> do
    writeVar' var (Link var'') h
    findVar'' var' var'' h

readSetState :: MonadRef m => Var m f -> Heap m -> m (SetState m f)
readSetState var h = readRef (unVar var) <&> \ s -> find s h
  where
    find (RefState xs x) = \ case
      Nil -> x
      Cons { label, copied, tail } ->
        lookup' label xs `or'`
        lookupCopied xs copied `or'`
        lookup xs tail `or'`
        x

    lookup xs = \ case
      Nil -> Nothing
      Cons { label, copied, tail } ->
        lookup' label xs <|>
        lookupCopied xs copied <|>
        lookup xs tail

    lookup' i xs = case LabelMap.lookup i xs of
      x@(Just (Repr (Bound {}))) -> x
      x@(Just (Link _)) -> x
      _ -> Nothing

    lookupCopied xs = \ case
      Nil -> Nothing
      Cons { label, copied } ->
        lookup' label xs <|>
        lookupCopied xs copied

writeVar :: MonadRef m => Var m f -> SetState m f -> VerseT m ()
writeVar var x = lift . writeVar' var x =<< getHeap

writeVar' :: MonadRef m => Var m f -> SetState m f -> Heap m -> m ()
writeVar' var x = \ case
  Nil -> modifyRef' (unVar var) $ \ (RefState xs _) ->
    RefState xs x
  Cons { label } -> modifyRef' (unVar var) $ \ (RefState xs y) ->
    RefState (LabelMap.insert label x xs) y

readRefState :: MonadRef m => Ref m (RefState a) -> Heap m -> m a
readRefState ref h = readRef ref <&> \ s -> find s h
  where
    find (RefState xs x) = \ case
      Nil -> x
      Cons { label, copied, tail } ->
        LabelMap.lookup label xs `or'`
        lookupCopied xs copied `or'`
        lookup xs tail `or'`
        x

    lookup xs = \ case
      Nil -> Nothing
      Cons { label, copied, tail } ->
        LabelMap.lookup label xs <|>
        lookupCopied xs copied <|>
        lookup xs tail

    lookupCopied xs = \ case
      Nil -> Nothing
      Cons { label, copied } ->
        LabelMap.lookup label xs <|>
        lookupCopied xs copied

writeRefState :: MonadRef m => Ref m (RefState a) -> a -> Heap m -> m ()
writeRefState ref x = \ case
  Nil -> modifyRef' ref $ \ (RefState xs _) ->
    RefState xs x
  Cons { label } -> modifyRef' ref $ \ (RefState xs y) ->
    RefState (LabelMap.insert label x xs) y

type FreezeT f m = RST (Heap m) (LabelMap (Fix f)) (MaybeT m)

runFreezeT :: Monad m => FreezeT f m a -> VerseT m (Maybe a)
runFreezeT m = do
  h <- getHeap
  lift . runMaybeT $ evalRST m h mempty

freezeVar' :: ( MonadFix m
              , MonadRef m
              , Traversable f
              ) => Var m f -> FreezeT f m (Fix f)
freezeVar' var = do
  h <- ask
  lift (lift $ findVar' var h) >>= \ case
    (_, Unbound {}) -> empty
    (_, Bound i x) -> mfix $ \ x' ->
      state' (lookupInsert i x') >>= \ case
        Just x -> pure x
        Nothing -> Fix <$> traverse freezeVar' x

type FreshenT f m = RST (Heap m) (LabelMap (Var m f)) (SupplyT Label m)

runFreshenT :: Monad m => FreshenT f m a -> VerseT m a
runFreshenT m = do
  h <- getHeap
  VerseT . lift . lift . lift $ evalRST m h mempty

runFreshenT' :: Monad m => FreshenT f m a -> Heap m -> CommitT m a
runFreshenT' m h = evalRST m h mempty

freshenVar' :: ( MonadFix m
               , MonadRef m
               , Traversable f
               ) => Var m f -> FreshenT f m (Var m f)
freshenVar' var = ask >>= lift . lift . findVar' var >>= \ case
  (_, Bound i x) -> mfix $ \ x' ->
    state' (lookupInsert i x') >>= \ case
      Just x -> pure x
      Nothing -> lift . newVar' =<< traverse freshenVar' x
  (var, Unbound {}) -> pure var

newtype VarRef m f = VarRef { unVarRef :: Ref m (RefState (Var m f)) }

instance EqRef (Ref m) => EqVarRef (VarRef m) where
  eqVarRef = eqRef `on` unVarRef

instance (MonadFix m, MonadRef m) => MonadVarRef (VerseT m) where
  type VarRef (VerseT m) = VarRef m

  newVarRef x = do
    h <- getHeap
    lift $ do
      ref <- fmap VarRef . newRef $ RefState mempty x
      addWriteCommit' ref h
      pure ref

  readVarRef ref = lift . readRefState (unVarRef ref) =<< getHeap

  writeVarRef ref x = do
    h <- getHeap
    lift $ do
      writeRefState (unVarRef ref) x h
      addWriteCommit' ref h

addWriteCommit' :: ( MonadFix m
                   , MonadRef m
                   , Traversable f
                   ) => VarRef m f -> Heap m -> m ()
addWriteCommit' ref = addCommit' $ \ h h' -> do
  x <- lift $ readRefState (unVarRef ref) h
  x' <- runFreshenT' (freshenVar' x) h
  lift $ writeRefState (unVarRef ref) x' h'

instance (MonadFix m, MonadRef m, EqRef (Ref m)) => MonadUnify (VerseT m) where
  unify var_x var_y = do
    (var_x, repr_x) <- findVar var_x
    (var_y, repr_y) <- findVar var_y
    unless (eqVar var_x var_y) $ case (repr_x, repr_y) of
      (Unbound i_x splitLabel_x, Unbound i_y splitLabel_y) -> do
        splitLabel <- askSplitLabel
        case (splitLabel_x == splitLabel, splitLabel_y == splitLabel) of
          (False, False) -> do
            addNewListener var_x i_x $ \ val_x ->
              readVar var_y >>= \ case
                Just val_y -> check val_x val_y
                Nothing -> pure ()
            addNewListener var_y i_y $ \ val_y ->
              readVar var_x >>= \ case
                Just val_x -> check val_x val_y
                Nothing -> pure ()
            unionVars var_x var_y
          (False, True) -> do
            moveListenersAt i_x i_y
            unionVars var_x var_y
          (True, False) -> do
            moveListenersAt i_y i_x
            unionVars var_y var_x
          (True, True) -> do
            moveListenersAt i_x i_y
            unionVars var_x var_y
      (Unbound i_x splitLabel_x, Bound _ val_y) -> do
        unionVars var_y var_x
        notifyListeners i_x val_y
        splitLabel <- askSplitLabel
        when (splitLabel_x /= splitLabel) $
          addNewListener var_x i_x $ \ val_x ->
            check val_x val_y
      (Bound _ val_x, Unbound i_y splitLabel_y) -> do
        unionVars var_x var_y
        notifyListeners i_y val_x
        splitLabel <- askSplitLabel
        when (splitLabel_y /= splitLabel) $
          addNewListener var_y i_y $ \ val_y ->
            check val_x val_y
      (Bound _ val_x, Bound _ val_y) ->
        check val_x val_y

check :: ( MonadFix m
         , MonadRef m
         , EqRef (Ref m)
         , Unifiable f
         ) => f (Var m f) -> f (Var m f) -> VerseT m ()
check val_x val_y = zipMatchM val_x val_y >>= \ case
  Nothing -> empty
  Just val_z -> for_ val_z $ uncurry unify

instance (MonadFix m, MonadRef m, EqRef (Ref m)) => MonadVerse (VerseT m) where
  whenBound var f = findVar var >>= \ case
    (_, Unbound i _) -> addNewListener var i f
    (_, Bound _ x) -> f x

  split m = split' $ do
    splitLabel <- VerseT supply
    localSplitLabel (const splitLabel) $ do
      pushSplit
      m

split' :: (MonadFix m, MonadRef m, Freshenable a, Elem a ~ Var m) =>
          VerseT m a ->
          (Maybe (a, VerseT m a) -> VerseT m ()) ->
          VerseT m ()
split' m f = do
  h <- getHeap
  msplit' m mempty >>= \ case
    Just (x, s, m) -> do
      h' <- getHeap
      isSettled h' >>= \ case
        True -> do
          x <- freshen freshenVar x
          commit h'
          putHeap h
          addListeners s
          f $ Just (x, putHeap h' *> m)
        False -> do
          putHeap h
          addListeners s
          putHeapListener h' $ \ h' m' -> \ case
            True -> do
              h <- getHeap
              putHeap h'
              x' <- freshen freshenVar x
              commit h'
              putHeap h
              f $ Just (x', putHeap h' *> (plus (m' $> x) m))
            False -> split' (putHeap h' *> (plus (m' $> x) m)) f
    Nothing -> do
      putHeap h
      f Nothing

notifyListeners :: (MonadFix m, MonadRef m) => Label -> f (Var m f) -> VerseT m ()
notifyListeners i x = popListeners i >>= \ case
  Just xs -> for_ xs $ toListener >>> flip apListener x
  Nothing -> pure ()

notifyHeap :: (MonadFix m, MonadRef m) => Heap m -> Bool -> VerseT m ()
notifyHeap = \ case
  h@Cons { listeners = Listeners { listenerLengthRef, heapListenerRef } } -> \ case
    True -> lift (readRef heapListenerRef) >>= \ case
      Just f -> do
        n <- lift $ readRef listenerLengthRef
        case n of
          0 -> pure ()
          1 -> do
            lift $ writeRef listenerLengthRef 0
            apHeapListener f h True
          _ -> lift . writeRef listenerLengthRef $ n - 1
      Nothing -> do
        n <- lift $ readRef listenerLengthRef
        case n of
          0 -> pure ()
          _ -> lift . writeRef listenerLengthRef $ n - 1
    False -> lift (readRef heapListenerRef) >>= \ case
      Just f -> do
        n <- lift $ readRef listenerLengthRef
        case n of
          0 -> pure ()
          _ -> do
            lift $ writeRef listenerLengthRef 0
            apHeapListener f h False
      Nothing -> empty
  _ -> \ case
    True -> pure ()
    False -> empty

apListener :: ( MonadFix m
              , MonadRef m
              ) => Listener m a -> a -> VerseT m ()
apListener (Listener h f) = resume h . f

apHeapListener :: ( MonadFix m
                  , MonadRef m
                  ) => HeapListener m -> Heap m -> Bool -> VerseT m ()
apHeapListener (HeapListener h m f) h' = resume h . f h' m

resume :: ( MonadFix m
          , MonadRef m
          ) => Heap m -> VerseT m () -> VerseT m ()
resume h m = case h of
  Cons { listeners = Listeners { listenerLengthRef, heapListenerRef } } ->
    lift (readRef listenerLengthRef) >>= \ case
      0 -> pure ()
      _ -> lift (readRef heapListenerRef) >>= \ case
        Nothing -> do
          notifyHeap h True
          m
        Just _ -> do
          h' <- getHeap
          msplit' (putHeap h *> m) mempty >>= \ case
            Just ((), s, m) -> do
              h <- getHeap
              addListenersFromTo h h' s
              VerseT . modify $ flip appendListeners s
              modifyHeapListener h m
              putHeap h'
              notifyHeap h True
            Nothing -> do
              putHeap h'
              notifyHeap h False
  _ -> m


msplit' :: Monad m =>
           VerseT m a ->
           Listeners m ->
           VerseT m (Maybe (a, Listeners m, VerseT m a))
msplit' m s = VerseT . RST $ \ r s' ->
  runRST (msplit $ unVerseT m) r s <&> \ case
    (Nothing, _) -> (Nothing, s')
    (Just (x, m), s) -> (Just (x, s, VerseT m), s')

appendListeners :: Listeners m -> Listeners m -> Listeners m
appendListeners = LabelMap.unionWith (<>)

insertListener :: Label -> Listener m a -> Listeners m -> Listeners m
insertListener i = insertListeners i . (:[]) . toAnyListener

insertListeners :: Label -> [Listener m Any] -> Listeners m -> Listeners m
insertListeners = LabelMap.insertWith (flip (<>))

toAnyListener :: Listener m a -> Listener m Any
toAnyListener = Unsafe.Coerce.unsafeCoerce

toListener :: Listener m Any -> Listener m (f (Var m f))
toListener = Unsafe.Coerce.unsafeCoerce

unionVars :: MonadRef m => Var m f -> Var m f -> VerseT m ()
unionVars var_x var_y = writeVar var_y $ Link var_x

commit :: MonadRef m => Heap m -> VerseT m ()
commit = runCommitT . commit'

type CommitT m = SupplyT Label m

runCommitT :: Monad m => CommitT m a -> VerseT m a
runCommitT = VerseT . lift . lift . lift

commit' :: MonadRef m => Heap m -> CommitT m ()
commit' = commit'' emptyCommit

commit'' :: MonadRef m => Commit m -> Heap m -> CommitT m ()
commit'' f' = \ case
  h@Cons { listeners, commitRef, tail = h' } -> do
    f <- readRef commitRef
    let f'' h h' = f h h' *> f' h h'
    f'' h h'
    case listeners of
      Listeners { kind = Split } -> lift $ addCommit' f'' h'
      _ -> commit'' f'' h'
  _ -> pure ()

addCommit' :: MonadRef m => Commit m -> Heap m -> m ()
addCommit' f = \ case
  Cons { commitRef } -> modifyRef' commitRef $ \ f' h h' -> f' h h' *> f h h'
  _ -> pure ()

pushSplit :: MonadRef m => VerseT m ()
pushSplit = do
  label <- VerseT supply
  h <- getHeap
  commitRef <- lift $ newRef emptyCommit
  listenerLengthRef <- lift $ newRef 0
  listenersRef <- lift $ newRef mempty
  heapListenerRef <- lift $ newRef Nothing
  putHeap Cons { listeners = Listeners { kind = Split, .. }, copied = Nil, tail = h, .. }

pushChoice :: (MonadFix m, MonadRef m) => Heap m -> VerseT m ()
pushChoice h = do
  s <- VerseT get
  (h', s') <- VerseT . lift . lift . lift . mfix $ \ ~(h', _) ->
    runCopyT' ((,) <$> pushChoice' h <*> copyListeners s) $ toCopied h h'
  putHeap h'
  VerseT $ put s'

pushChoice' :: (MonadFix m, MonadRef m) => Heap m -> CopyT m (Heap m)
pushChoice' h = case h of
  Cons { listeners = Listeners {..} } -> do
    label <- supply
    commitRef <- newRef emptyCommit
    listenerLengthRef <- newRef =<< readRef listenerLengthRef
    listenersRef <- newRef =<< copyListeners =<< readRef listenersRef
    heapListenerRef <- newRef =<< traverse copyHeapListener =<< readRef heapListenerRef
    pure Cons { listeners = Listeners { kind = Choice, .. }, copied = Nil, tail = h, .. }
  _ -> do
    label <- supply
    commitRef <- newRef emptyCommit
    pure Cons { listeners = Empty, copied = Nil, tail = h, .. }

popChoice :: (MonadFix m, MonadRef m) => Heap m -> VerseT m ()
popChoice h = do
  h' <- stateHeap $ \ case
    Cons { listeners = Listeners { kind = Choice}, tail } -> (tail, tail)
    Cons { listeners = Empty, tail } -> (tail, tail)
    _ -> error "popChoice"
  putListeners =<< flip runCopyT (toCopied h h') . copyListeners =<< getListeners' h'

toCopied :: Heap m -> Heap m -> Copied m
toCopied = \ case
  Cons { label } -> flip Copied Nil . LabelMap.singleton label
  Nil -> Copied mempty

type CopyT m = StateT (Copied m) (SupplyT Label m)

data Copied m = Copied !(LabelMap (Heap m)) (Heap m)

runCopyT :: Monad m => CopyT m a -> Copied m -> VerseT m a
runCopyT m = VerseT . lift . lift . lift . runCopyT' m

runCopyT' :: Monad m => CopyT m a -> Copied m -> SupplyT Label m a
runCopyT' = evalStateT

copyListeners :: (MonadFix m, MonadRef m) => Listeners m -> CopyT m (Listeners m)
copyListeners = traverse (traverse copyListener)

copyListener :: (MonadFix m, MonadRef m) => Listener m a -> CopyT m (Listener m a)
copyListener (Listener h f) = copyHeap h <&> \ h -> Listener h f

copyHeapListener :: (MonadFix m, MonadRef m) => HeapListener m -> CopyT m (HeapListener m)
copyHeapListener (HeapListener h m f) = copyHeap h <&> \ h -> HeapListener h m f

copyHeap :: (MonadFix m, MonadRef m) => Heap m -> CopyT m (Heap m)
copyHeap = \ case
  xs@Cons {..} -> mfix $ \ xs' -> state' (lookupInsertCopied label xs') >>= \ case
    Nothing -> do
      label <- supply
      listeners <- copyHeapListeners listeners
      commitRef <- newRef =<< readRef commitRef
      tail <- copyHeap tail
      pure Cons { copied = xs, .. }
    Just xs' -> pure xs'
  Nil -> gets $ \ (Copied _ xs') -> xs'

copyHeapListeners :: ( MonadFix m
                     , MonadRef m
                     ) => HeapListeners m -> CopyT m (HeapListeners m)
copyHeapListeners = \ case
  Listeners {..} -> do
    listenerLengthRef <- newRef =<< readRef listenerLengthRef
    listenersRef <- newRef =<< copyListeners =<< readRef listenersRef
    heapListenerRef <- newRef =<< traverse copyHeapListener =<< readRef heapListenerRef
    pure $ Listeners {..}
  Empty -> pure Empty

lookupInsertCopied :: Label -> Heap m -> Copied m -> Either (Heap m) (Copied m)
lookupInsertCopied i x (Copied xss xs) = flip Copied xs <$> lookupInsert i x xss

plus :: VerseT m a -> VerseT m a -> VerseT m a
plus x y = VerseT $ unVerseT x <|> unVerseT y

askSplitLabel :: VerseT m SplitLabel
askSplitLabel = VerseT ask

localSplitLabel :: (SplitLabel -> SplitLabel) -> VerseT m a -> VerseT m a
localSplitLabel f = VerseT . local f . unVerseT

getHeap :: Monad m => VerseT m (Heap m)
getHeap = VerseT . lift $ get

putHeap :: Monad m => Heap m -> VerseT m ()
putHeap = VerseT . lift . put

stateHeap :: Monad m => (Heap m -> (a, Heap m)) -> VerseT m a
stateHeap = VerseT . lift . state

insert' :: LabelMap.Key -> a -> LabelMap a -> Maybe (LabelMap a)
insert' !k0 x0 t0 = loop k0 x0 t0
  where
    loop k x = \ case
      t@(LabelMap.Internal.Bin p m l r)
        | LabelMap.Internal.nomatch k p m ->
          Just $! LabelMap.Internal.link k (LabelMap.singleton k x) p t
        | LabelMap.Internal.zero k m -> case loop k x l of
          Just l -> Just $! LabelMap.Internal.Bin p m l r
          Nothing -> Nothing
        | otherwise -> case loop k x r of
          Just r -> Just $! LabelMap.Internal.Bin p m l r
          Nothing -> Nothing
      t@(LabelMap.Internal.Tip k' _)
        | k == k' -> Nothing
        | otherwise ->
          Just $! LabelMap.Internal.link k (LabelMap.singleton k x) k' t
      LabelMap.Internal.Nil -> Just $! LabelMap.singleton k x

lookupInsert :: LabelMap.Key -> a -> LabelMap a -> Either a (LabelMap a)
lookupInsert !k0 x0 t0 = loop k0 x0 t0
  where
    loop k x = \ case
      t@(LabelMap.Internal.Bin p m l r)
        | LabelMap.Internal.nomatch k p m ->
          Right $! LabelMap.Internal.link k (LabelMap.singleton k x) p t
        | LabelMap.Internal.zero k m -> case loop k x l of
          Right l -> Right $! LabelMap.Internal.Bin p m l r
          l@(Left _) -> l
        | otherwise -> case loop k x r of
          Right r -> Right $! LabelMap.Internal.Bin p m l r
          r@(Left _) -> r
      t@(LabelMap.Internal.Tip k' y)
        | k == k' -> Left y
        | otherwise ->
          Right $! LabelMap.Internal.link k (LabelMap.singleton k x) k' t
      LabelMap.Internal.Nil -> Right $! LabelMap.singleton k x

lookupDelete :: Label -> LabelMap a -> Maybe (a, LabelMap a)
lookupDelete !k0 t0 = toMaybe $ loop k0 t0
  where
    loop k = \ case
      LabelMap.Internal.Bin p m l r
        | LabelMap.Internal.nomatch k p m -> Lacks
        | LabelMap.Internal.zero k m -> case loop k l of
            Lacks -> Lacks
            Had x l' -> Had x $ LabelMap.Internal.binCheckLeft p m l' r
        | otherwise -> case loop k r of
            Lacks -> Lacks
            Had x r' -> Had x $ LabelMap.Internal.binCheckRight p m l r'
      LabelMap.Internal.Tip k' x
        | k == k' -> Had x LabelMap.Internal.Nil
        | otherwise -> Lacks
      LabelMap.Internal.Nil -> Lacks

toMaybe :: LookupDelete a -> Maybe (a, LabelMap a)
toMaybe = \ case
  Lacks -> Nothing
  Had x y -> Just (x, y)

data LookupDelete a
  = Lacks
  | Had !a !(LabelMap a)

or' :: Maybe a -> a -> a
or' = flip fromMaybe
infixr 3 `or'`

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

incr :: (MonadRef m, Num a) => Ref m a -> m ()
incr = flip modifyRef' (+ 1)
