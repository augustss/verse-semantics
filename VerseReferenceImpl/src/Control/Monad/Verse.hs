{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
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
import Data.IntMap.Lazy qualified as LabelMap.Lazy
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as LabelMap
import Data.Maybe
import Data.Monoid (mempty)
import Data.Ref
import Data.Tuple
import Data.Semigroup ((<>))
import Data.Unifiable
import Data.Word

import GHC.Exts (Any)

import Prelude (Num (..), ($!), error)

import Text.Show

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
  , copied :: !(Heap m)
  , kind :: !HeapKind
  , commitRef :: !(Ref m (Commit m))
  , listenerLengthRef :: !(Ref m Word)
  , listenersRef :: !(Ref m (IntMap [Listener m Any]))
  , heapListenerRef :: !(Ref m (Maybe (Heap m, Heap m -> Bool -> VerseT m ())))
  , tail :: !(Heap m)
  }

incrListenerLength :: MonadRef m => Heap m -> VerseT m ()
incrListenerLength = \ case
  Cons { listenerLengthRef } -> lift $ incr listenerLengthRef
  Nil -> pure ()

getListenerLength :: MonadRef m => Heap m -> VerseT m Word
getListenerLength = \ case
  Cons { listenerLengthRef } -> lift $ readRef listenerLengthRef
  Nil -> pure 0

putListenerLength :: MonadRef m => Heap m -> Word -> VerseT m ()
putListenerLength h n = case h of
  Cons { listenerLengthRef } -> lift $ writeRef listenerLengthRef n
  Nil -> pure ()

popListeners :: MonadRef m => Label -> VerseT m (Maybe [Listener m Any])
popListeners i = popListeners' i =<< getHeap

popListeners' :: MonadRef m => Label -> Heap m -> VerseT m (Maybe [Listener m Any])
popListeners' i = \ case
  Cons { listenersRef } -> do
    VerseT . modify $ LabelMap.delete i
    lift (readRef listenersRef) <&> deleteLookup i >>= \ case
      Just (xs, s) -> Just xs <$ lift (writeRef listenersRef s)
      Nothing -> pure Nothing
  Nil -> VerseT . state $ \ s -> case deleteLookup i s of
    Just (xs, s) -> (Just xs, s)
    Nothing -> (Nothing, s)

getListeners' :: MonadRef m => Heap m -> VerseT m (Listeners m)
getListeners' = \ case
  Cons { listenersRef } -> lift $ readRef listenersRef
  Nil -> VerseT get

putListeners' :: MonadRef m => Heap m -> Listeners m -> VerseT m ()
putListeners' = \ case
  Cons { listenersRef } -> \ s -> do
    lift $ writeRef listenersRef s
    VerseT $ put s
  Nil -> VerseT . put

addNewListener' :: MonadRef m => Label -> Listener m Any -> Heap m -> VerseT m ()
addNewListener' i s = \ case
  Cons { listenerLengthRef, listenersRef } -> do
    lift $ incr listenerLengthRef
    lift . modifyRef' listenersRef $ insertListener i s
    VerseT . modify $ insertListener i s
  Nil -> VerseT . modify $ insertListener i s

addListeners' :: MonadRef m => Heap m -> Listeners m -> VerseT m ()
addListeners' = \ case
  Cons { listenersRef } -> \ s -> do
    lift . modifyRef' listenersRef $ flip appendListeners s
    VerseT . modify $ flip appendListeners s
  Nil -> VerseT . modify . flip appendListeners

addListenersFromTo :: MonadRef m => Heap m -> Heap m -> Listeners m -> VerseT m ()
addListenersFromTo h = \ case
  Cons { label } -> addListenersFromTo' h label
  Nil -> addListenersFrom h

addListenersFromTo' :: MonadRef m => Heap m -> Label -> Listeners m -> VerseT m ()
addListenersFromTo' h i = case h of
  Cons { label, kind, tail } -> case label == i of
    False -> case kind of
      Split -> addListeners' tail *> addListenersFromTo' tail i
      Choice -> addListenersFromTo' tail i
    True -> VerseT . modify . flip appendListeners
  Nil -> VerseT . modify . flip appendListeners

moveListenersAt :: MonadRef m => Label -> Label -> VerseT m ()
moveListenersAt i j = moveListenersAt' i j =<< getHeap

moveListenersAt' :: MonadRef m => Label -> Label -> Heap m -> VerseT m ()
moveListenersAt' i j = \ case
  Cons { listenersRef } -> do
    VerseT . modify $ insertDeleteListeners i j
    lift . modifyRef' listenersRef $ insertDeleteListeners i j
  Nil -> VerseT . modify $ insertDeleteListeners i j

insertDeleteListeners :: Label -> Label -> Listeners m -> Listeners m
insertDeleteListeners i j s = case deleteLookup j s of
  Just (xs, s) -> insertListeners i xs s
  Nothing -> s

addListenersFrom :: MonadRef m => Heap m -> Listeners m -> VerseT m ()
addListenersFrom h = case h of
  Cons { kind, tail = h } -> case kind of
    Split -> addListeners' h *> addListenersFrom h
    Choice -> addListenersFrom h
  Nil -> VerseT . modify . flip appendListeners

getHeapListener :: MonadRef m => Heap m -> VerseT m (Maybe (Heap m -> Bool -> VerseT m ()))
getHeapListener = \ case
  Cons { heapListenerRef } -> lift (readRef heapListenerRef) <&> \ case
    Just (h, f) -> Just $ \ h' -> localHeap (const h) . f h'
    Nothing -> Nothing
  Nil -> pure Nothing

putHeapListener :: MonadRef m => Heap m -> (Heap m -> Bool -> VerseT m ()) -> VerseT m ()
putHeapListener = \ case
  Cons { heapListenerRef } -> \ f -> do
    splitLabel <- askSplitLabel
    h <- getHeap
    let f' h = localSplitLabel (const splitLabel) . f h
    lift . writeRef heapListenerRef $ Just (h, f')
  Nil -> const $ pure ()

data HeapKind = Split | Choice deriving Show

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

    lookupCopied xs = \ case
      Nil -> Nothing
      Cons { label, copied } -> lookup' label xs <|> lookupCopied xs copied

    lookup' i xs = case LabelMap.lookup i xs of
      x@(Just (Repr (Bound {}))) -> x
      x@(Just (Link _)) -> x
      _ -> Nothing

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

split' :: (MonadFix m, MonadRef m, Freshenable a, Elem a ~ Var m f, Traversable f) =>
          VerseT m a ->
          (Maybe (a, VerseT m a) -> VerseT m ()) ->
          VerseT m ()
split' m f = do
  h <- getHeap
  msplit' m mempty >>= \ case
    Just (x, s, m) -> do
      h' <- getHeap
      addListeners' h s
      getListenerLength h' >>= \ case
        0 -> do
          x <- freshen freshenVar x
          commit h'
          putHeap h
          f $ Just (x, putHeap h' *> m)
        _ -> do
          incrListenerLength h
          putHeap h
          putHeapListener h' $ \ h' -> \ case
            True -> do
              h <- getHeap
              putHeap h'
              x <- freshen freshenVar x
              commit h'
              putHeap h
              f $ Just (x, putHeap h' *> m)
              notifyHeap h True
            False -> do
              split' (putHeap h' *> m) f
              h <- getHeap
              notifyHeap h True
    Nothing -> do
      putHeap h
      f Nothing

notifyListeners :: (MonadFix m, MonadRef m) => Label -> f (Var m f) -> VerseT m ()
notifyListeners i x = popListeners i >>= \ case
  Just xs -> for_ xs $ toListener >>> \ (Listener h f) -> do
    h' <- getHeap
    s <- getEmptyListeners' h'
    msplit' (putHeap h *> f x) s >>= \ case
      Just ((), s, _) -> do
        h <- getHeap
        putHeap h'
        addListenersFromTo h h' s
        notifyHeap h True
      Nothing -> do
        putHeap h'
        notifyHeap h False
  Nothing -> pure ()

getEmptyListeners' :: Heap m -> VerseT m (Listeners m)
getEmptyListeners' = \ case
  Cons {} -> pure mempty
  Nil -> VerseT get

notifyHeap :: (MonadFix m, MonadRef m) => Heap m -> Bool -> VerseT m ()
notifyHeap h = \ case
  True -> getHeapListener h >>= \ case
    Just f -> do
      n <- getListenerLength h
      case n of
        0 -> pure ()
        1 -> do
          putListenerLength h 0
          f h True
        _ -> putListenerLength h $ n - 1
    Nothing -> do
      n <- getListenerLength h
      case n of
        0 -> pure ()
        _ -> putListenerLength h $ n - 1
  False -> getHeapListener h >>= \ case
    Just f -> do
      n <- getListenerLength h
      case n of
        0 -> pure ()
        _ -> do
          putListenerLength h 0
          f h False
    Nothing -> do
      n <- getListenerLength h
      case n of
        0 -> pure ()
        _ -> do
          putListenerLength h $ n - 1
          empty

msplit' :: Monad m =>
           VerseT m a ->
           Listeners m ->
           VerseT m (Maybe (a, Listeners m, VerseT m a))
msplit' m s = VerseT . RST $ \ r s' ->
  runRST (msplit $ unVerseT m) r s <&> \ case
    (Nothing, _) -> (Nothing, s')
    (Just (x, m), s) -> (Just (x, s, VerseT m), s')

addNewListener :: MonadRef m =>
                  Var m f ->
                  Label ->
                  (f (Var m f) -> VerseT m ()) ->
                  VerseT m ()
addNewListener _ i f = do
  h <- getHeap
  splitLabel <- askSplitLabel
  let f' = toAnyListener $ Listener h $ localSplitLabel (const splitLabel) . f
  addNewListener' i f' h

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
  h@Cons { kind, commitRef, tail = h' } -> do
    f <- readRef commitRef
    let f'' h h' = f h h' *> f' h h'
    f'' h h'
    case kind of
      Split -> lift $ addCommit' f'' h'
      Choice -> commit'' f'' h'
  Nil -> pure ()

pushSplit :: MonadRef m => VerseT m ()
pushSplit = do
  i <- VerseT supply
  h <- getHeap
  r_commit <- lift $ newRef emptyCommit
  r_length <- lift $ newRef 0
  r_listeners <- lift $ newRef mempty
  r_listener <- lift $ newRef Nothing
  putHeap $ Cons i Nil Split r_commit r_length r_listeners r_listener h

pushChoice :: (MonadFix m, MonadRef m) => Heap m -> VerseT m ()
pushChoice h = do
  label <- VerseT supply
  commitRef <- lift $ newRef emptyCommit
  listenerLengthRef <- lift . newRef =<< getListenerLength h
  listenersRef <- lift $ newRef mempty
  heapListenerRef <- lift $ newRef Nothing
  let h' = Cons { copied = Nil, kind = Choice, tail = h, .. }
  putListeners' h' =<< flip runCopyT (toCopied h h') . copyListeners =<< getListeners' h
  putHeap h'

popChoice :: (MonadFix m, MonadRef m) => Heap m -> VerseT m ()
popChoice h = do
  h' <- stateHeap $ \ case
    Cons _ _ Choice _ _ _ _ h' -> (h', h')
    _ -> error "popChoice"
  putListeners' h' =<< flip runCopyT (toCopied h h') . copyListeners =<< getListeners' h'

toCopied :: Heap m -> Heap m -> Copied m
toCopied = \ case
  Cons { label } -> flip Copied Nil . LabelMap.singleton label
  Nil -> Copied mempty

type CopyT m = StateT (Copied m) (SupplyT Label m)

data Copied m = Copied !(LabelMap (Heap m)) !(Heap m)

runCopyT :: Monad m => CopyT m a -> Copied m -> VerseT m a
runCopyT m = VerseT . lift . lift . lift . evalStateT m

copyListeners :: (MonadFix m, MonadRef m) => Listeners m -> CopyT m (Listeners m)
copyListeners = traverse (traverse copyListener)

copyListener :: (MonadFix m, MonadRef m) => Listener m a -> CopyT m (Listener m a)
copyListener (Listener h f) = flip Listener f <$> copyHeap h

copyHeap :: (MonadFix m, MonadRef m) => Heap m -> CopyT m (Heap m)
copyHeap = \ case
  xs@Cons {..} -> mfix $ \ xs' -> state' (lookupInsertCopied label xs') >>= \ case
    Nothing -> do
      label <- supply
      commitRef <- newRef =<< readRef commitRef
      listenerLengthRef <- newRef =<< readRef listenerLengthRef
      listenersRef <- newRef =<< copyListeners =<< readRef listenersRef
      heapListenerRef <- newRef =<< traverse (firstM copyHeap) =<< readRef heapListenerRef
      tail <- copyHeap tail
      pure $ Cons { copied = xs, .. }
    Just xs' -> pure xs'
  Nil -> gets $ \ (Copied _ xs) -> xs

lookupInsertCopied :: Label -> Heap m -> Copied m -> Either (Heap m) (Copied m)
lookupInsertCopied i x (Copied xss xs) = flip Copied xs <$> lookupInsert i x xss

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

localHeap :: Monad m => (Heap m -> Heap m) -> VerseT m a -> VerseT m a
localHeap f m = do
  h <- getHeap
  putHeap (f h) *> m <* putHeap h

addCommit' :: MonadRef m => Commit m -> Heap m -> m ()
addCommit' f = \ case
  Cons { commitRef } -> modifyRef' commitRef $ \ f' h h' -> f' h h' *> f h h'
  Nil -> pure ()

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
          Right $! LabelMap.Internal.link k (LabelMap.Lazy.singleton k x) p t
        | LabelMap.Internal.zero k m -> case loop k x l of
          Right l -> Right $! LabelMap.Internal.Bin p m l r
          l@(Left _) -> l
        | otherwise -> case loop k x r of
          Right r -> Right $! LabelMap.Internal.Bin p m l r
          r@(Left _) -> r
      t@(LabelMap.Internal.Tip k' y)
        | k == k' -> Left y
        | otherwise ->
          Right $! LabelMap.Internal.link k (LabelMap.Lazy.singleton k x) k' t
      LabelMap.Internal.Nil -> Right $! LabelMap.Lazy.singleton k x

deleteLookup :: Label -> LabelMap a -> Maybe (a, LabelMap a)
deleteLookup !k0 t0 = toMaybe $ loop k0 t0
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

toMaybe :: DeleteLookup a -> Maybe (a, LabelMap a)
toMaybe = \ case
  Lacks -> Nothing
  Had x y -> Just (x, y)

data DeleteLookup a
  = Lacks
  | Had !a !(LabelMap a)

or' :: Maybe a -> a -> a
or' = flip fromMaybe
infixr 3 `or'`

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

firstM :: Functor f => (a -> f c) -> (a, b) -> f (c, b)
firstM f (x, y) = f x <&> (, y)

incr :: (MonadRef m, Num a) => Ref m a -> m ()
incr = flip modifyRef' (+ 1)
