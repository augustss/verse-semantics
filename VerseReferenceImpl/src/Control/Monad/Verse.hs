{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.Var (MonadVar, freshenVar, readVar)
import Control.Monad.Var qualified
import Control.Monad.Verse.Class

import Data.Fix
import Data.Foldable
import Data.Freshenable
import Data.Function
import Data.Functor
import Data.IntMap.Internal qualified as LabelMap.Internal
import Data.IntMap.Lazy qualified as LabelMap.Lazy
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as LabelMap
import Data.Maybe
import Data.Ref
import Data.Unifiable

import GHC.Exts (Any)

import Unsafe.Coerce qualified

newtype VerseT m a = VerseT
  { unVerseT :: RST Env (Susps m) (LogicT (StateT (Heaps m) (SupplyT Label m))) a
  } deriving ( Functor
             , Applicative
             , Monad
             )

deriving instance MonadError e m => MonadError e (VerseT m)

runVerseT :: MonadRef m => VerseT m a -> m [a]
runVerseT m =
  runSupplyT .
  flip evalStateT emptyHeaps .
  observeAllT' $ do
    splitLabel <- supply
    evalRST (unVerseT m) Env { splitLabel } emptySusps

observeAllT' :: MonadRef m =>
                LogicT (StateT (Heaps m) (SupplyT Label m)) a ->
                StateT (Heaps m) (SupplyT Label m) [a]
observeAllT' m = unLogicT m sk fk
  where
    sk x fk = (lift . lift . commit' =<< gets heap) *> ((x:) <$> fk)
    fk = pure []

emptyHeaps :: Heaps m
emptyHeaps = Heaps { heap = Nil }

emptySusps :: Susps m
emptySusps = Susps { promises = mempty, listeners = mempty }

instance MonadTrans VerseT where
  lift = VerseT . lift . lift . lift . lift

instance MonadRef m => Alternative (VerseT m) where
  empty = VerseT empty

  x <|> y = do
    h <- getHeap
    VerseT $ unVerseT (pushChoice h *> x) <|> unVerseT (popChoice h *> y)

instance MonadRef m => MonadPlus (VerseT m)

instance MonadSupply s m => MonadSupply s (VerseT m)

newtype TRef m a = TRef { unTRef :: Ref m (RefState a) }

instance EqRef (Ref m) => EqRef (TRef m) where
  eqRef = eqRef `on` unTRef

instance MonadRef m => MonadRef (VerseT m) where
  type Ref (VerseT m) = TRef m

  newRef = fmap TRef . lift . newRef . RefState mempty

  readRef ref = lift . readRefState (unTRef ref) =<< getHeap

  writeRef ref x = do
    lift . writeRefState (unTRef ref) x =<< getHeap
    addCommit $ writeRefState (unTRef ref) x

newVar' :: MonadRef m => f (Var m f) -> SupplyT Label m (Var m f)
newVar' x = lift . fmap Var . newLRef' . Repr . flip Bound x =<< supply

instance (MonadFix m, MonadRef m) => MonadVar (VerseT m) where
  type Var (VerseT m) = Var m

  freshVar =
    fmap Var . newLRef . Repr =<< Unbound <$> VerseT supply <*> askSplitLabel

  newVar = VerseT . lift . lift . lift . newVar'

  readVar var = findVar var <&> \ case
    (_, Unbound _ _) -> Nothing
    (_, Bound _ x) -> Just x

  freezeVar = runFreezeT . freezeVar'

  freshenVar = runFreshenT . freshenVar'

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
    (_, Unbound _ _) -> empty
    (_, Bound i x) -> mfix $ \ x' ->
      gets (lookupInsert i x') >>= \ case
        Left x -> pure x
        Right s -> put s *> (Fix <$> traverse freezeVar' x)

type FreshenT f m = RST (Heap m) (LabelMap (Var m f)) (SupplyT Label m)

runFreshenT :: Monad m => FreshenT f m a -> VerseT m a
runFreshenT m = do
  h <- getHeap
  VerseT . lift . lift . lift $ evalRST m h mempty

freshenVar' :: ( MonadFix m
               , MonadRef m
               , Traversable f
               ) => Var m f -> FreshenT f m (Var m f)
freshenVar' var = do
  h <- ask
  lift (lift $ findVar' var h) >>= \ case
    (_, Bound i x) -> mfix $ \ x' ->
      gets (lookupInsert i x') >>= \ case
        Left x -> pure x
        Right s -> put s *> (lift . newVar' =<< traverse freshenVar' x)
    (var, Unbound _ _) -> pure var

instance (MonadFix m, MonadRef m, EqRef (Ref m)) => MonadUnify (VerseT m) where
  unify var_x var_y = do
    (var_x, repr_x) <- findVar var_x
    (var_y, repr_y) <- findVar var_y
    unless (eqVar var_x var_y) $ case (repr_x, repr_y) of
      (Unbound i_x splitLabel_x, Unbound i_y splitLabel_y) -> do
        splitLabel <- askSplitLabel
        case (splitLabel_x == splitLabel, splitLabel_y == splitLabel) of
          (False, False) -> do
            addBoundListener var_x i_x $ \ val_x ->
              readVar var_y >>= \ case
                Just val_y -> check val_x val_y
                Nothing -> pure ()
            addBoundListener var_y i_y $ \ val_y ->
              readVar var_x >>= \ case
                Just val_x -> check val_x val_y
                Nothing -> pure ()
            unionVars var_x var_y
          (False, True) -> do
            addListenersAt i_x =<< getListenersAt i_y
            unionVars var_x var_y
          (True, False) -> do
            addListenersAt i_y =<< getListenersAt i_x
            unionVars var_y var_x
          (True, True) -> do
            addListenersAt i_x =<< getListenersAt i_y
            unionVars var_x var_y
      (Unbound i_x splitLabel_x, Bound _ val_y) -> do
        unionVars var_y var_x
        notifyBoundListeners i_x val_y
        splitLabel <- askSplitLabel
        when (splitLabel_x /= splitLabel) $
          addBoundListener var_x i_x $ \ val_x ->
            check val_x val_y
      (Bound _ val_x, Unbound i_y splitLabel_y) -> do
        unionVars var_x var_y
        notifyBoundListeners i_y val_x
        splitLabel <- askSplitLabel
        when (splitLabel_y /= splitLabel) $
          addBoundListener var_y i_y $ \ val_y ->
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
    (_, Unbound i _) -> addBoundListener var i f
    (_, Bound _ x) -> f x

  split m = split' $ do
    splitLabel <- VerseT supply
    localSplitLabel (const splitLabel) $ do
      pushSplit
      m

newtype Env = Env
  { splitLabel :: SplitLabel
  }

type SplitLabel = Label

data Susps m = Susps
  { promises :: !(Promises m)
  , listeners :: !(Listeners m)
  }

type Promises m = [Promise m]

type Listeners m = LabelMap [Listener m Any]

newtype Heaps m = Heaps
  { heap :: (Heap m)
  }

type Commit m = Heap m -> m ()

data Listener m a = Listener
  !(Heap m)
  !(Heap m)
  !(Promise m)
  !(Callback m a)

type Callback m a = Heap m -> a -> VerseT m ()

data Promise m = Promise
  {-# UNPACK #-} !Label
  !(LRef m (Maybe Bool))

data Heap m
  = Nil
  | Cons {-# UNPACK #-} !Label !(Heap m) !HeapKind !(Ref m (Commit m)) !(Heap m)

data HeapKind = Split | Choice deriving Show

type Label = Int

type LabelMap = IntMap

split' :: (MonadFix m, MonadRef m, Freshenable f) =>
          VerseT m (f (Var m)) ->
          (Maybe (f (Var m), VerseT m (f (Var m))) -> VerseT m ()) ->
          VerseT m ()
split' m f = do
  h <- getHeap
  msplit' m >>= \ case
    Just (x, s, m) -> do
      h' <- getHeap
      putHeap h
      for_ s.promises addListenable
      addListeners s.listeners
      splitPromises s.promises h' $ \ h' -> \ case
        True -> do
          h <- getHeap
          putHeap h'
          x <- freshen freshenVar x
          commit h'
          putHeap h
          f $ Just (x, putHeap h' *> m)
        False -> split' (putHeap h' *> m) f
    Nothing -> do
      putHeap h
      f Nothing

msplit' :: Monad m => VerseT m a -> VerseT m (Maybe (a, Susps m, VerseT m a))
msplit' m = VerseT . RST $ \ r s ->
  runRST (msplit $ unVerseT m) r emptySusps <&> \ case
    (Nothing, _) -> (Nothing, s)
    (Just (x, m), s') -> (Just (x, s', VerseT m), s)

split'' :: MonadRef m =>
           VerseT m a ->
           (Maybe (a, VerseT m a) -> VerseT m ()) ->
           VerseT m ()
split'' m f = do
  h <- getHeap
  msplit'' m >>= \ case
    Just (x, s, m) -> do
      h' <- getHeap
      putHeap h
      for_ s.promises addListenable
      putListeners s.listeners
      splitPromises s.promises h' $ \ h' -> \ case
        True -> f $ Just (x, putHeap h' *> m)
        False -> split'' (putHeap h' *> m) f
    Nothing -> putHeap h *> f Nothing

msplit'' :: Monad m => VerseT m a -> VerseT m (Maybe (a, Susps m, VerseT m a))
msplit'' m = VerseT . RST $ \ r s ->
  runRST (msplit $ unVerseT m) r s { promises = mempty } <&> \ case
    (Nothing, _) -> (Nothing, s)
    (Just (x, m), s') -> (Just (x, s', VerseT m), s)

splitPromises :: MonadRef m => Promises m -> Heap m -> Callback m Bool -> VerseT m ()
splitPromises = splitPromises' . reverse

splitPromises' :: MonadRef m => Promises m -> Heap m -> Callback m Bool -> VerseT m ()
splitPromises' xs h f = case xs of
  [] -> f h True
  x:xs -> whenResolved x h $ \ h -> \ case
    True -> splitPromises' xs h f
    False -> f h False

freshPromise :: MonadRef m => VerseT m (Promise m)
freshPromise = do
  p <- Promise <$> VerseT supply <*> newLRef Nothing
  modifyPromises (p:)
  pure p

whenResolved :: MonadRef m => Promise m -> Heap m -> Callback m Bool -> VerseT m ()
whenResolved (Promise i ref) h f = lift (readLRef' ref h) >>= \ case
  Nothing -> addResolveListener i h f
  Just x -> f h x

resolve :: MonadRef m => Promise m -> Bool -> VerseT m ()
resolve (Promise i ref) x = readLRef ref >>= \ case
  Nothing -> do
    writeLRef ref $ Just x
    notifyResolveListeners i x
  Just _ -> error "resolve"

addListenable :: Monad m => Promise m -> VerseT m ()
addListenable (Promise i _) = modifyListeners $ LabelMap.insert i []

notifyResolveListeners :: MonadRef m => Label -> Bool -> VerseT m ()
notifyResolveListeners i x = stateListeners (deleteLookup i) >>= \ case
  Nothing -> guard x
  Just xs -> for_ xs $ toResolveListener >>> \ (Listener h h' p f) ->
    split'' (putHeap h *> f h' x) $ \ case
      Nothing -> resolve p False
      Just ((), _) -> resolve p True

notifyBoundListeners :: MonadRef m => Label -> f (Var m f) -> VerseT m ()
notifyBoundListeners i x = stateListeners (deleteLookup i) >>= \ case
  Just xs -> for_ xs $ toBoundListener >>> \ (Listener h h' p f) ->
    split'' (putHeap h *> f h' x) $ \ case
      Nothing -> resolve p False
      Just ((), _) -> resolve p True
  Nothing -> pure ()

addResolveListener :: MonadRef m =>
                      Label ->
                      Heap m ->
                      Callback m Bool ->
                      VerseT m ()
addResolveListener = addListener

addBoundListener :: MonadRef m =>
                    Var m f ->
                    Label ->
                    (f (Var m f) -> VerseT m ()) ->
                    VerseT m ()
addBoundListener _ i f = do
  h <- getHeap
  addListener i h $ \ _ x -> f x

addListener :: MonadRef m => Label -> Heap m -> Callback m a -> VerseT m ()
addListener i h f = do
  h' <- getHeap
  p <- freshPromise
  splitLabel <- askSplitLabel
  modifyListeners . insertListener i $ Listener h' h p $ \ h x ->
    localSplitLabel (const splitLabel) $ f h x

appendListeners :: Listeners m -> Listeners m -> Listeners m
appendListeners = LabelMap.unionWith (<>)

insertListener :: Label -> Listener m a -> Listeners m -> Listeners m
insertListener i = insertListeners i . (:[]) . toAnyListener

insertListeners :: Label -> [Listener m Any] -> Listeners m -> Listeners m
insertListeners = LabelMap.insertWith (flip (<>))

toAnyListener :: Listener m a -> Listener m Any
toAnyListener = Unsafe.Coerce.unsafeCoerce

toResolveListener :: Listener m Any -> Listener m Bool
toResolveListener = Unsafe.Coerce.unsafeCoerce

toBoundListener :: Listener m Any -> Listener m (f (Var m f))
toBoundListener = Unsafe.Coerce.unsafeCoerce

newtype Var m f = Var
  { unVar :: LRef m (SetState m f)
  }

data SetState m f
  = Repr !(VarState m f)
  | Link (Var m f)

data VarState m f
  = Unbound {-# UNPACK #-} !Label {-# UNPACK #-} !SplitLabel
  | Bound {-# UNPACK #-} !Label !(f (Var m f))

eqVar :: EqRef (Ref m) => Var m f -> Var m f -> Bool
eqVar = eqRef `on` unVar

unionVars :: MonadRef m => Var m f -> Var m f -> VerseT m ()
unionVars var_x var_y = writeLRef (unVar var_y) $ Link var_x

type FoundVar m f = (Var m f, VarState m f)

findVar :: MonadRef m => Var m f -> VerseT m (FoundVar m f)
findVar var = lift . findVar' var =<< getHeap

findVar' :: MonadRef m => Var m f -> Heap m -> m (FoundVar m f)
findVar' var h = readSetState (unVar var) h >>= \ case
  Repr x -> pure (var, x)
  Link var' -> findVar'' var var' h

findVar'' :: MonadRef m => Var m f -> Var m f -> Heap m -> m (FoundVar m f)
findVar'' var var' h = readSetState (unVar var') h >>= \ case
  Repr x -> pure (var', x)
  Link var'' -> do
    writeLRef' (unVar var) (Link var'') h
    findVar'' var' var'' h

readSetState :: MonadRef m => LRef m (SetState m f) -> Heap m -> m (SetState m f)
readSetState ref h = readRef (unLRef ref) <&> \ s -> find s h
  where
    find (RefState xs x) = \ case
      Nil -> x
      Cons i h _ _ h' ->
        lookup' i xs `or'`
        lookupCopied xs h `or'`
        lookup xs h' `or'`
        x

    lookup xs = \ case
      Nil -> Nothing
      Cons i h _ _ h' ->
        lookup' i xs <|>
        lookupCopied xs h <|>
        lookup xs h'

    lookupCopied xs = \ case
      Nil -> Nothing
      Cons i h _ _ _ -> lookup' i xs <|> lookupCopied xs h

    lookup' i xs = case LabelMap.lookup i xs of
      x@(Just (Repr (Bound _ _))) -> x
      x@(Just (Link _)) -> x
      _ -> Nothing

newtype LRef m a = LRef
  { unLRef :: Ref m (RefState a)
  }

instance EqRef (Ref m) => EqRef (LRef m) where
  eqRef = eqRef `on` unLRef

newLRef :: MonadRef m => a -> VerseT m (LRef m a)
newLRef = lift . newLRef'

newLRef' :: MonadRef m => a -> m (LRef m a)
newLRef' = fmap LRef . newRef . RefState mempty

readLRef :: MonadRef m => LRef m a -> VerseT m a
readLRef ref = lift . readLRef' ref =<< getHeap

readLRef' :: MonadRef m => LRef m a -> Heap m -> m a
readLRef' = readRefState . unLRef

writeLRef :: MonadRef m => LRef m a -> a -> VerseT m ()
writeLRef ref x = lift . writeLRef' ref x =<< getHeap

writeLRef' :: MonadRef m => LRef m a -> a -> Heap m -> m ()
writeLRef' ref x = \ case
  Nil -> modifyRef (unLRef ref) $ \ (RefState xs _) ->
    RefState xs x
  Cons i _ _ _ _ -> modifyRef (unLRef ref) $ \ (RefState xs y) ->
    RefState (LabelMap.insert i x xs) y

data RefState a = RefState
  !(LabelMap a)
  !a deriving Show

readRefState :: MonadRef m => Ref m (RefState a) -> Heap m -> m a
readRefState ref h = readRef ref <&> \ s -> find s h
  where
    find (RefState xs x) = \ case
      Nil -> x
      Cons i h _ _ h' ->
        LabelMap.lookup i xs `or'`
        lookupCopied xs h `or'`
        lookup xs h' `or'`
        x

    lookup xs = \ case
      Nil -> Nothing
      Cons i h _ _ h' ->
        LabelMap.lookup i xs <|>
        lookupCopied xs h <|>
        lookup xs h'

    lookupCopied xs = \ case
      Nil -> Nothing
      Cons i h _ _ _ -> LabelMap.lookup i xs <|> lookupCopied xs h

writeRefState :: MonadRef m => Ref m (RefState a) -> a -> Heap m -> m ()
writeRefState ref x = \ case
  Nil -> modifyRef ref $ \ (RefState xs _) ->
    RefState xs x
  Cons i _ _ _ _ -> modifyRef ref $ \ (RefState xs y) ->
    RefState (LabelMap.insert i x xs) y

commit :: MonadRef m => Heap m -> VerseT m ()
commit = lift . commit'

commit' :: MonadRef m => Heap m -> m ()
commit' = commit'' (const $ pure ())

commit'' :: MonadRef m => Commit m -> Heap m -> m ()
commit'' f' = \ case
  Cons _ _ x ref_f h' -> do
    f <- readRef ref_f
    writeRef ref_f . const $ pure ()
    let f'' x = f x *> f' x
    f'' h'
    case x of
      Split -> addCommit' f'' h'
      Choice -> commit'' f'' h'
  Nil -> pure ()

pushSplit :: MonadRef m => VerseT m ()
pushSplit = do
  i <- VerseT supply
  h <- getHeap
  ref_f <- lift . newRef . const $ pure ()
  putHeap $ Cons i Nil Split ref_f h

pushChoice :: MonadRef m => Heap m -> VerseT m ()
pushChoice h = do
  i <- VerseT supply
  ref_f <- lift . newRef . const $ pure ()
  let h' = Cons i Nil Choice ref_f h
  putHeap h'
  putListeners =<< flip runCopyT (toCopied h h') . copyListeners =<< getListeners

popChoice :: MonadRef m => Heap m -> VerseT m ()
popChoice h = do
  h' <- stateHeap $ \ case
    Cons _ _ Choice _ h' -> (h', h')
    _ -> error "popChoice"
  putListeners =<< flip runCopyT (toCopied h h') . copyListeners =<< getListeners

toCopied :: Heap m -> Heap m -> Copied m
toCopied = \ case
  Cons i _ _ _ _ -> flip Copied Nil . LabelMap.singleton i
  Nil -> Copied mempty

type CopyT m = StateT (Copied m) (SupplyT Label m)

data Copied m = Copied !(LabelMap (Heap m)) !(Heap m)

runCopyT :: Monad m => CopyT m a -> Copied m -> VerseT m a
runCopyT m = VerseT . lift . lift . lift . evalStateT m

copyListeners :: MonadRef m => Listeners m -> CopyT m (Listeners m)
copyListeners = traverse (traverse copyListener)

copyListener :: MonadRef m => Listener m a -> CopyT m (Listener m a)
copyListener (Listener h h' p f) =
  (\ h h' -> Listener h h' p f) <$> copyHeap h <*> copyHeap h'

copyHeap :: MonadRef m => Heap m -> CopyT m (Heap m)
copyHeap = \ case
  Nil -> gets $ \ (Copied _ xs) -> xs
  xs@(Cons i _ x ref_f ys) -> gets (lookupCopied i) >>= \ case
    Nothing -> do
      ys' <- copyHeap ys
      i' <- supply
      ref_f' <- lift . lift $ newRef =<< readRef ref_f
      let xs' = Cons i' xs x ref_f' ys'
      modify $ insertCopied i xs'
      pure xs'
    Just xs -> pure xs

lookupCopied :: Label -> Copied m -> Maybe (Heap m)
lookupCopied i (Copied xss _) = LabelMap.lookup i xss

insertCopied :: Label -> Heap m -> Copied m -> Copied m
insertCopied i xs (Copied xss ys) = Copied (LabelMap.insert i xs xss) ys

askSplitLabel :: VerseT m SplitLabel
askSplitLabel = VerseT $ asks splitLabel

localSplitLabel :: (SplitLabel -> SplitLabel) -> VerseT m a -> VerseT m a
localSplitLabel f =
  VerseT . local (\ r -> r { splitLabel = f r.splitLabel }) . unVerseT

modifyPromises :: (Promises m -> Promises m) -> VerseT m ()
modifyPromises f = VerseT . modify $ \ s -> s { promises = f s.promises }

getListenersAt :: Monad m => Label -> VerseT m [Listener m Any]
getListenersAt i = VerseT . gets $ fromMaybe [] . LabelMap.lookup i . listeners

addListeners :: Monad m => Listeners m -> VerseT m ()
addListeners = modifyListeners . flip appendListeners

addListenersAt :: Monad m => Label -> [Listener m Any] -> VerseT m ()
addListenersAt i xs = modifyListeners $ insertListeners i xs

getListeners :: Monad m => VerseT m (Listeners m)
getListeners = VerseT $ gets listeners

putListeners :: Monad m => Listeners m -> VerseT m ()
putListeners listeners = VerseT . modify $ \ s -> s { listeners }

modifyListeners :: Monad m => (Listeners m -> Listeners m) -> VerseT m ()
modifyListeners f = VerseT . modify $ \ s -> s { listeners = f s.listeners }

stateListeners :: Monad m => (Listeners m -> (a, Listeners m)) -> VerseT m a
stateListeners f = VerseT . state $ \ s ->
  f s.listeners <&> \ listeners -> s { listeners }

getHeap :: Monad m => VerseT m (Heap m)
getHeap = VerseT . lift $ gets heap

putHeap :: Monad m => Heap m -> VerseT m ()
putHeap heap = modifyHeaps $ \ s -> s { heap }

stateHeap :: Monad m => (Heap m -> (a, Heap m)) -> VerseT m a
stateHeap f = VerseT . lift . state $ \ s ->
  f s.heap <&> \ heap -> s { heap }

addCommit :: MonadRef m => Commit m -> VerseT m ()
addCommit f = lift . addCommit' f =<< getHeap

addCommit' :: MonadRef m => Commit m -> Heap m -> m ()
addCommit' f = \ case
  Cons _ _ _ ref_f _ -> modifyRef ref_f $ \ f' h -> f' h *> f h
  Nil -> pure ()

modifyHeaps :: Monad m => (Heaps m -> Heaps m) -> VerseT m ()
modifyHeaps = VerseT . lift . modify

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

deleteLookup :: Label -> LabelMap a -> (Maybe a, LabelMap a)
deleteLookup !k0 t0 = toTuple $ loop k0 t0
  where
    loop k = \ case
      t@(LabelMap.Internal.Bin p m l r)
        | LabelMap.Internal.nomatch k p m ->
          Nothing :*: t
        | LabelMap.Internal.zero k m ->
          let x :*: l' = loop k l in x :*: LabelMap.Internal.binCheckLeft p m l' r
        | otherwise ->
          let x :*: r' = loop k r in x :*: LabelMap.Internal.binCheckRight p m l r'
      t@(LabelMap.Internal.Tip k' x)
        | k == k' -> Just x :*: LabelMap.Internal.Nil
        | otherwise -> Nothing :*: t
      LabelMap.Internal.Nil -> Nothing :*: LabelMap.Internal.Nil

data Sum a b = !a :*: !b

toTuple :: Sum a b -> (a, b)
toTuple (x :*: y) = (x, y)

or' :: Maybe a -> a -> a
or' = flip fromMaybe
infixr 3 `or'`
