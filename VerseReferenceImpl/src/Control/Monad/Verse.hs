{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Verse
  ( VerseT
  , runVerseT
  , yield
  , IVar
  , freshIVar
  , newIVar
  , readIVar
  , writeIVar
  , Var
  , freshVar
  , newVar
  , readVar
  , Match (..)
  , unify
  , unifyEq
  , VarRef
  , newVarRef
  , readVarRef
  , writeVarRef
  , fork
  , join
  , one
  , if'
  , all
  , for
  , verify
  , decide
  , succeeds
  , fails
  , decides
  , assume
  , FreezeT
  , runFreezeT
  , Freezable (..)
  , freeze'
  , FreshenT
  , Freshenable (..)
  ) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad (Monad (..), (=<<), (>=>), guard, sequence_, when)
import Control.Monad.Abort (MonadAbort)
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RS
import Control.Monad.Supply

import Data.Bool
import Data.Either
import Data.Eq
import Data.Fix
import Data.Foldable (foldlM, traverse_)
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.HashMap.Strict qualified as Strict (HashMap)
import Data.Int
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Lazy.Extras qualified as IntMap.Lazy
import Data.IntSet (IntSet)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable (Traversable, traverse)
import Data.Tuple

import GHC.Exts qualified

import Prelude (Num (..), ($!), error, reverse, seq, subtract)

import Unsafe.Coerce (unsafeCoerce)

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield r m -> Abort r m -> Logic r m a
  }

newtype Yield r m = Yield
  { unYield :: forall a . AddSusp m a -> Logic r m a
  }

type AddSusp m a = Susp m a -> m (Fail () m)

type Logic r m a = Succeed r m a -> Fail r m -> Empty r m -> Env m -> m r

type Succeed r m a = a -> Fail r m -> Empty r m -> S m -> m r

type Fail r m = R -> m r

type Empty r m = R -> m r

type Abort r m = m r

data Env m = Env
  { heap :: !Heap
  , abstractHeap :: !Heap
  , heaps :: !(IntMap Heap)
  , store :: !(Store m)
  , processes :: !(Processes m)
  , splitDepth :: {-# UNPACK #-} !Int
  , suspCount :: !(HRef m Int)
  , decisions :: !(HRef m Decisions)
  , result :: !(Maybe (IVar m ()))
  }

data R = R
  { heap :: !Heap
  , abstractHeap :: !Heap
  , heaps :: !(IntMap Heap)
  }

data S m = S
  { heap :: !Heap
  , abstractHeap :: !Heap
  , heaps :: !(IntMap Heap)
  , store :: Store m
  , processes :: Processes m
  }

toS :: Env m -> S m
toS Env {..} = S {..}

toR :: Env m -> R
toR Env {..} = R {..}

type Store m = IntMap (StoreElem m)

data StoreElem m = forall a . Freshenable a m => StoreElem !(VarRef m a)

type Processes m = [Process m]

data Process m = forall a . Freshenable a m => Process
  { heap :: {-# UNPACK #-} !Int
  , abstractHeap :: !Bool
  , heaps :: !IntSet
  , store :: !(Store m)
  , processes :: !(Processes m)
  , splitDepth :: {-# UNPACK #-} !Int
  , suspCount :: !(HRef m Int)
  , decisions :: !(HRef m Decisions)
  , left :: !(HRef m (Maybe a))
  , rightFail :: !(VerseT m ())
  , rightEmpty :: !(VerseT m ())
  , result :: !(IVar m (Split (Heap, IntMap Heap, a, VerseT m ())))
  }

data Heap = Root | Child
  { label :: {-# UNPACK #-} !Int
  , tail :: !Heap
  , right :: !Heap
  , pred :: !Heap
  }

eqHeap :: Heap -> Heap -> Bool
eqHeap = curry $ \ case
  (Root, Root) -> True
  (Child { label = x }, Child { label = y }) -> x == y
  _ -> False

type Susp m a = a -> VerseT m ()

emptySusp :: Susp m a
emptySusp = const $ pure ()

newtype IVar m a = IVar
  { unIVar :: Ref m (HeapMap (IVarState m a))
  }

newtype Var m a = Var
  { unVar :: Ref m (HeapMap (VarState m a))
  }

data VarRef m a = VarRef
  { label :: {-# UNPACK #-} !Int
  , unVarRef :: !(Ref m (HeapMap (Var m a)))
  }

instance EqRef (Ref m) => Eq (VarRef m a) where
  (==) = eqRef `on` unVarRef

data HeapMap a = HeapMap
  { root :: !a
  , child :: !(IntMap a)
  }

data IVarState m a
  = Val !a
  | Susp !(Susp m a)

data VarState m a
  = Link !(Var m a)
  | Repr !(Repr m a)

data Repr m a
  = Bound !a {-# UNPACK #-} !Int
  | Unbound {-# UNPACK #-} !Int !(Susp m a) {-# UNPACK #-} !Int

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ yk ak sk -> unVerseT m yk ak $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ _ sk fk ek Env {..} ->
    sk x fk ek S {..}
  f <*> x = VerseT $ \ yk ak sk fk ek r@Env {..} ->
    let sk' f fk ek S {..} = unVerseT x yk ak (sk . f) fk ek Env {..}
    in unVerseT f yk ak sk' fk ek r

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         ) => Alternative (VerseT m) where
  empty = VerseT $ \ _ _ _ _ ek Env {..} -> ek R {..}
  x <|> y = VerseT $ \ yk ak sk fk ek Env {..} -> do
    let
      fk' R {..} = unVerseT y yk ak sk fk fk Env { heaps = revert heaps heap, .. }
      ek' R {..} = unVerseT y yk ak sk fk ek Env { heaps = revert heaps heap, .. }
    heaps <- copy heaps heap
    unVerseT x yk ak sk fk' ek' Env {..}

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk ak sk fk ek r@Env {..} ->
    let sk' x fk ek S {..} = unVerseT (f x) yk ak sk fk ek Env {..}
    in unVerseT x yk ak sk' fk ek r

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ _ sk fk ek Env {..} ->
    m >>= \ x -> sk x fk ek S {..}

instance MonadAbort e m => MonadAbort e (VerseT m)

instance MonadSupply s m => MonadSupply s (VerseT m)

runVerseT
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m a -> m (Maybe [a])
runVerseT m = do
  label <- supply
  let heap = Child { label, tail = abstractHeap, right = Root, pred = Root }
  suspCount <- newHRef' 0
  decisions <- newHRef' emptyDecisions
  let
    sk x fk _ S {..} = readHRef' suspCount heap >>= \ case
      0 -> do
        freshenStore store FreshenEnv {..}
        fmap (x:) <$> fk R {..}
      _ -> pure Nothing
  unVerseT m yk ak sk fk fk Env {..}
  where
    abstractHeap = Root
    heaps = mempty
    splitDepth = 1
    store = mempty
    processes = mempty
    result = Nothing
    yk = Yield $ \ _ _ _ _ _ -> pure Nothing
    ak = pure $ Just []
    fk _ = pure $ Just []

yield :: Applicative m => VerseT m a
yield = VerseT $ \ yk _ sk fk ek r -> unYield yk addSusp sk fk ek r
  where
    addSusp = const . pure . const $ pure ()

freshIVar :: MonadRef m => VerseT m (IVar m a)
freshIVar = lift freshIVar'

freshIVar' :: MonadRef m => m (IVar m a)
freshIVar' = fmap IVar . newRef . singleton $ Susp emptySusp

newIVar :: MonadRef m => a -> VerseT m (IVar m a)
newIVar = lift . newIVar'

newIVar' :: MonadRef m => a -> m (IVar m a)
newIVar' = fmap IVar . newRef . singleton . Val

readIVar :: MonadRef m => IVar m a -> VerseT m a
readIVar v = VerseT $ \ yk _ sk fk ek Env {..} ->
  readRef (unIVar v) <&> lookupIVarState heap >>= \ case
    Val x -> sk x fk ek S {..}
    x@(Susp k) -> rotate (unYield yk) sk fk ek Env {..} $ \ k' ->
      put' (unIVar v) heap (Susp $ \ x -> k' x *> k x) $> \ R {..} ->
      put' (unIVar v) heap x
  where
    rotate f x1 x2 x3 x4 x5 = f x5 x1 x2 x3 x4

writeIVar
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => IVar m a -> a -> VerseT m ()
writeIVar v x = readIVarState v >>= \ case
  Val _ -> error "writeIVar"
  y@(Susp k) -> do
    r <- ask'
    liftAlt $
      put' (unIVar v) r.heap (Val x) $> \ r ->
      put' (unIVar v) r.heap y
    k x
    resumeProcesses $ writeLocalIVar v x

writeLocalIVar
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => IVar m a -> a -> VerseT m ()
writeLocalIVar v x = readLocalIVarState v >>= \ case
  Just (Val _) -> error "writeIVar"
  Just y@(Susp k) -> do
    r <- ask'
    liftAlt $
      put' (unIVar v) r.heap (Val x) $> \ r ->
      put' (unIVar v) r.heap y
    k x
    resumeProcesses $ writeLocalIVar v x
  Nothing -> resumeProcesses $ writeLocalIVar v x

readIVarState :: MonadRef m => IVar m a -> VerseT m (IVarState m a)
readIVarState v = liftSucceed $ \ r ->
  readRef (unIVar v) <&> lookupIVarState r.heap

readLocalIVarState :: MonadRef m => IVar m a -> VerseT m (Maybe (IVarState m a))
readLocalIVarState v = liftSucceed $ \ r ->
  readRef (unIVar v) <&> lookupLocal r.heap

freshVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m a)
freshVar = liftSucceed $ \ r -> freshVar' r.splitDepth

freshVar' :: (MonadRef m, MonadSupply Int m) => Int -> m (Var m f)
freshVar' n = fmap Var . newRef . singleton . Repr . Unbound n emptySusp =<< supply

newVar :: (MonadRef m, MonadSupply Int m) => a -> VerseT m (Var m a)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Int m) => a -> m (Var m a)
newVar' x = fmap Var . newRef . singleton . Repr . Bound x =<< supply

readVar :: MonadRef m => Var m a -> VerseT m a
readVar v = readVarState v >>= \ case
  Link v -> readVar v
  Repr (Bound x _) -> pure x
  x@(Repr (Unbound n k i)) -> VerseT $ \ yk _ sk fk ek r ->
    rotate (unYield yk) sk fk ek r $ \ k' ->
    put' (unVar v) r.heap (Repr $ Unbound n (\ x -> k x *> k' x) i) $> \ r ->
    put' (unVar v) r.heap x
  where
    rotate f x1 x2 x3 x4 x5 = f x5 x1 x2 x3 x4

data Match = SEQ | LE | GE

unify
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Var m a -> VerseT m ()
unify f v_x v_y = (,) <$> findRepr v_x <*> findRepr v_y >>= \ case
  (Found v_x (Bound x _), Found v_y (Unbound n_y k_y _)) -> do
    r <- ask'
    when (n_y < r.splitDepth) incrSuspCount
    writeLink v_y v_x
    k_y x
    resumeProcesses $ subst f v_x v_y
  (Found v_x (Unbound n_x k_x _), Found v_y (Bound y _)) -> do
    r <- ask'
    when (n_x < r.splitDepth) incrSuspCount
    writeLink v_x v_y
    k_x y
    resumeProcesses $ subst f v_y v_x
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound n_y k_y i_y)) -> do
    r <- ask'
    case compare' n_x i_x n_y i_y of
      EQ -> pure ()
      LT -> do
        when (n_y < r.splitDepth) incrSuspCount
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeProcesses $ subst f v_x v_y
      GT -> do
        when (n_x < r.splitDepth) incrSuspCount
        writeLink v_x v_y
        writeRepr v_y $ Unbound n_y (\ x -> k_x x *> k_y x) i_y
        resumeProcesses $ subst f v_y v_x
  (Found v_x repr_x@(Bound x i_x), Found v_y repr_y@(Bound y i_y)) ->
    when (i_x /= i_y) $ f x y >>= \ case
      (SEQ, m) -> do
        writeRepr v_y repr_x
        m
      (LE, m) -> do
        decide
        writeAbstractRepr v_y repr_x
        m
      (GE, m) -> do
        decide
        writeAbstractRepr v_x repr_y
        m

subst
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Var m a -> VerseT m ()
subst f v_x v_y = findLocalRepr v_y >>= \ case
  Nothing -> resumeProcesses $ subst f v_x v_y
  Just y -> subst' f v_x y

subst'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Found m a -> VerseT m ()
subst' f v_x y = findRepr v_x <&> (, y) >>= \ case
  (Found v_x (Bound x _), Found v_y (Unbound _ k_y _)) -> do
    writeLink v_y v_x
    k_y x
    resumeProcesses $ subst f v_x v_y
  (Found _ (Unbound _ k_x _), Found v_y (Bound y _)) -> do
    writeLink v_x v_y
    k_x y
    resumeProcesses $ subst f v_y v_x
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound n_y k_y i_y)) -> do
    case compare' n_x i_x n_y i_y of
      EQ -> decrSuspCount
      LT -> do
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeProcesses $ subst f v_x v_y
      GT -> do
        writeLink v_x v_y
        writeRepr v_y $ Unbound n_y (\ x -> k_x x *> k_y x) i_y
        resumeProcesses $ subst f v_y v_x
  (Found v_x repr_x@(Bound x i_x), Found v_y repr_y@(Bound y i_y)) -> do
    when (i_x /= i_y) $ f x y >>= \ case
      (SEQ, m) -> do
        writeRepr v_y repr_x
        m
      (LE, m) -> do
        decide
        writeAbstractRepr v_y repr_x
        m
      (GE, m) -> do
        decide
        writeAbstractRepr v_x repr_y
        m
    decrSuspCount

unifyEq
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Eq a)
  => Var m a -> Var m a -> VerseT m ()
unifyEq = unify $ \ x y -> guard (x == y) $> (SEQ, pure ())

compare' :: Int -> Int -> Int -> Int -> Ordering
compare' n_x i_x n_y i_y = compare (n_x, i_x) (n_y, i_y)

writeLink :: MonadRef m => Var m f -> Var m f -> VerseT m ()
writeLink v_x v_y = do
  x <- readVarState v_x
  r <- ask'
  liftAlt $
    put' (unVar v_x) r.heap (Link v_y) $> \ r ->
    put' (unVar v_x) r.heap x

writeRepr :: MonadRef m => Var m f -> Repr m f -> VerseT m ()
writeRepr v x = do
  y <- readVarState v
  r <- ask'
  liftAlt $
    put' (unVar v) r.heap (Repr x) $> \ r ->
    put' (unVar v) r.heap y

writeAbstractRepr :: MonadRef m => Var m f -> Repr m f -> VerseT m ()
writeAbstractRepr v x = do
  k <- asks' (.abstractHeap)
  lift . put' (unVar v) k $ Repr x

readRepr' :: MonadRef m => Var m f -> Heap -> m (Repr m f)
readRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> readRepr' v h
  Repr x -> pure x

data Found m a = Found !(Var m a) !(Repr m a)

findRepr :: MonadRef m => Var m a -> VerseT m (Found m a)
findRepr v = liftSucceed $ \ r -> findRepr' v r.heap

findRepr' :: MonadRef m => Var m a -> Heap -> m (Found m a)
findRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> findRepr' v h
  Repr x -> pure $ Found v x

findLocalRepr :: MonadRef m => Var m a -> VerseT m (Maybe (Found m a))
findLocalRepr v = liftSucceed $ \ r -> loop v r.heap
  where
    loop v h = readRef (unVar v) <&> lookupLocal h >>= \ case
      Nothing -> pure Nothing
      Just (Link v) -> Just <$> findRepr' v h
      Just (Repr x) -> pure . Just $ Found v x

readVarState :: MonadRef m => Var m a -> VerseT m (VarState m a)
readVarState v = liftSucceed $ \ r ->
  readRef (unVar v) <&> lookupVarState r.heap

resumeProcesses
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m ()
resumeProcesses m = do
  processes <- asks' (.processes)
  (xs, n) <- resumeAll processes m
  putProcesses xs
  n

resumeAll
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => [Process m] -> VerseT m ()
  -> VerseT m ([Process m], VerseT m ())
resumeAll xs m = fmap sequence_ . partitionEithers <$> traverse (flip resume m) xs

resume
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Process m -> VerseT m ()
  -> VerseT m (Either (Process m) (VerseT m ()))
resume process@Process {..} m = do
  heap <- asks' (.heaps) <&> (! heap)
  abstractHeap <- asks' $ if abstractHeap then (.heap) else (.abstractHeap)
  heaps <- asks' (.heaps) <&> (`intersection'` heaps)
  lift (msplit_ m Env { result = Nothing, .. }) >>= \ case
    Fail -> resume' process m
    Abort -> pure . Right $ writeIVar result Abort
    Succeed (S {..}, rightFail', rightEmpty') -> do
      lift ((0 ==) <$> readHRef' suspCount heap `andM` readHRef' left heap) >>= \ case
        Just x -> lift $ do
          let r = FreshenEnv {..}
          (commit, x) <- runFreshenT ((,) <$> commitStore store <*> freshen x) r
          let m' = rightFail' <|> fork rightFail *> m
          pure . Right $ commit *> writeIVar result (Succeed (heap, heaps, x, m'))
        Nothing -> do
          heaps <- modifyHeaps (heaps <>) $> IntMap.keysSet heaps
          pure . Left $ process
            { heaps
            , store
            , processes
            , rightFail = rightFail' <|> fork rightFail *> m
            , rightEmpty = rightEmpty' <|> fork rightEmpty *> m
            }

resume'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Process m -> VerseT m ()
  -> VerseT m (Either (Process m) (VerseT m ()))
resume' process@Process {..} m = do
  heap <- asks' (.heaps) <&> (! heap)
  abstractHeap <- asks' $ if abstractHeap then (.heap) else (.abstractHeap)
  heaps <- asks' (.heaps) <&> (`intersection'` heaps)
  lift (msplit_ (fork rightEmpty *> m) Env { result = Nothing, .. }) >>= \ case
    Fail -> pure . Right $ writeIVar result Fail
    Abort -> pure . Right $ writeIVar result Abort
    Succeed (S {..}, rightFail, rightEmpty) -> do
      lift ((0 ==) <$> readHRef' suspCount heap `andM` readHRef' left heap) >>= \ case
        Just x -> lift $ do
          let r = FreshenEnv {..}
          (commit, x) <- runFreshenT ((,) <$> commitStore store <*> freshen x) r
          pure $ Right $ commit *> writeIVar result (Succeed (heap, heaps, x, rightFail))
        Nothing -> do
          heaps <- modifyHeaps (heaps <>) $> IntMap.keysSet heaps
          pure . Left $ process
            { heaps
            , store
            , processes
            , rightFail
            , rightEmpty
            }

newVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => Var m a -> VerseT m (VarRef m a)
newVarRef x = do
  ref <- lift $ VarRef <$> supply <*> newRef (singleton x)
  modifyStore' . IntMap.insert ref.label $ StoreElem ref
  pure ref

readVarRef :: MonadRef m => VarRef m a -> VerseT m (Var m a)
readVarRef ref = do
  r <- ask'
  lift $ readVarRef' ref r.heap

readVarRef' :: MonadRef m => VarRef m a -> Heap -> m (Var m a)
readVarRef' ref = get' (unVarRef ref)

writeVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VarRef m a -> Var m a -> VerseT m ()
writeVarRef ref x = do
  r <- ask'
  y <- lift $ get' (unVarRef ref) r.heap
  liftEmpty $
    put' (unVarRef ref) r.heap x $> \ r ->
    put' (unVarRef ref) r.heap y
  modifyStore' . IntMap.insert ref.label $ StoreElem ref

fork
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m ()
fork m = liftSucceed (unVerseT m yield abort succeed fail fail) >>= reflect_
  where
    yield = Yield $ \ addSusp sk fk ek r@Env {..} -> do
      incr' suspCount heap
      removeSusp <- addSusp $ \ x -> do
        decr suspCount
        whenJust result $ \ result ->
          whenM (readHRef suspCount <&> (== 0)) $
            writeIVar result ()
        liftSucceed (toS >>> sk x fail fail) >>= reflect_
      let
        f r@R {..} = do
          removeSusp r
          decr' suspCount heap
      pure $ Succeed
        ( toS r
        , liftSucceed (toR >>> \ r -> f r *> fk r) >>= reflect_
        , liftSucceed (toR >>> \ r -> f r *> ek r) >>= reflect_
        )
    succeed () fk ek r = pure $ Succeed
      ( r
      , liftSucceed (toR >>> fk) >>= reflect_
      , liftSucceed (toR >>> ek) >>= reflect_
      )
    fail _ = pure Fail
    abort = pure Abort

join
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m (IVar m ())
join m = do
  n <- newHRef 1
  x <- freshIVar
  local' (\ Env {..} -> Env { suspCount = n, result = Just x, .. }) m
  decr n
  whenM (readHRef n <&> (== 0)) $ writeIVar x ()
  pure x

one
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m a)
one m = do
  v <- freshIVar
  fork $ do
    ref <- newHRef Nothing
    split ref (m >>= writeHRef ref . Just) >>= readIVar >>= \ case
      Fail -> empty
      Abort -> abort
      Succeed (_, _, x, _) -> writeIVar v x
  pure v

if'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> (a -> VerseT m b) -> VerseT m b -> VerseT m (IVar m b)
if' p t e = do
  v <- freshIVar
  fork $ do
    ref <- newHRef Nothing
    split ref (p >>= writeHRef ref . Just) >>= readIVar >>= \ case
      Fail -> e >>= writeIVar v
      Abort -> abort
      Succeed (_, _, x, _) -> t x >>= writeIVar v
  pure v

all
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (IVar m [a])
all m = do
  v <- freshIVar
  fork $ do
    heap <- newChildHeap
    splitDepth <- asks' $ (+ 1) . (.splitDepth)
    suspCount <- newHRef 0
    ref <- newHRef Nothing
    let
      loop heap heaps m xs = do
        abstractHeap <- asks' (.abstractHeap)
        decisions <- asks' (.decisions)
        split' Env {..} False ref m >>= readIVar >>= \ case
          Fail -> pure $ reverse xs
          Abort -> abort
          Succeed (heap, heaps, x, m) -> loop heap heaps m $ x:xs
    loop heap heaps (m >>= writeHRef ref . Just) [] >>= writeIVar v
  pure v
  where
    heaps = mempty
    store = mempty
    processes = mempty
    result = Nothing

for
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> (a -> VerseT m b) -> VerseT m (IVar m [b])
for m f = do
  v <- freshIVar
  fork $ do
    heap <- newChildHeap
    splitDepth <- asks' $ (+ 1) . (.splitDepth)
    suspCount <- newHRef 0
    ref <- newHRef Nothing
    let
      loop heap heaps m xs = do
        abstractHeap <- asks' (.abstractHeap)
        decisions <- asks' (.decisions)
        split' Env {..} False ref m >>= readIVar >>= \ case
          Fail -> pure $ reverse xs
          Abort -> abort
          Succeed (heap, heaps, x, m) -> do
            heap <- supply >>= \ i -> modifyHeaps (IntMap.insert i heap) $> i
            heaps <- modifyHeaps (heaps <>) $> IntMap.keysSet heaps
            xs <- f x <&> (:xs)
            heap <- asks' (.heaps) <&> (! heap)
            heaps <- asks' (.heaps) <&> (`intersection'` heaps)
            duplicateStore heap
            loop heap heaps m xs
    loop heap heaps (m >>= writeHRef ref . Just) [] >>= writeIVar v
  pure v
  where
    heaps = mempty
    store = mempty
    processes = mempty
    result = Nothing

verify
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m (IVar m ())
verify m = do
  v <- freshIVar
  fork $ loop emptyDecisions m >>= writeIVar v
  pure v
  where
    loop decisions m =
      verifyAll decisions m >>=
      readIVar <&> succ >>= \ case
        Nothing -> pure ()
        Just decisions -> loop decisions m

verifyAll
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Decisions -> VerseT m () -> VerseT m (IVar m Decisions)
verifyAll decisions m = do
  v <- freshIVar
  fork $ do
    decisions <- newHRef decisions
    heap <- newChildHeap
    splitDepth <- asks' $ (+ 1) . (.splitDepth)
    suspCount <- newHRef 0
    ref <- newHRef Nothing
    let
      loop heap heaps m = do
        abstractHeap <- asks' (.heap)
        split' Env {..} True ref m >>= readIVar >>= \ case
          Fail -> readHRef decisions
          Abort -> readHRef decisions
          Succeed (heap, heaps, (), m) -> loop heap heaps m
        where
          store = mempty
          processes = mempty
          result = Nothing
    loop heap mempty (m >> writeHRef ref (Just ())) >>= writeIVar v
  pure v

decide :: (MonadFix m, MonadRef m, MonadSupply Int m) => VerseT m ()
decide = do
  Env {..} <- ask'
  stateAbstractHRef' decisions uncons >>= guard

succeeds
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m (Maybe a))
succeeds m = do
  v <- freshIVar
  fork $ do
    heap <- newChildHeap
    abstractHeap <- asks' (.abstractHeap)
    splitDepth <- asks' $ (+ 1) . (.splitDepth)
    suspCount <- newHRef 0
    decisions <- asks' (.decisions)
    ref <- newHRef Nothing
    split' Env {..} False ref (m >>= writeHRef ref . Just) >>= readIVar >>= \ case
      Fail -> writeIVar v Nothing
      Abort -> abort
      Succeed (heap, heaps, x, m) -> do
        abstractHeap <- asks' (.abstractHeap)
        split' Env {..} False ref m >>= readIVar >>= \ case
          Fail -> writeIVar v $ Just x
          Abort -> abort
          Succeed _ -> writeIVar v Nothing
  pure v
  where
    heaps = mempty
    store = mempty
    processes = mempty
    result = Nothing

fails
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m a -> VerseT m (IVar m ())
fails m = do
  v <- freshIVar
  fork $ do
    ref <- newHRef Nothing
    split ref (m >> writeHRef ref (Just ())) >>= readIVar >>= \ case
      Fail -> empty
      Abort -> abort
      Succeed _ -> writeIVar v ()
  pure v

decides
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m (Maybe a))
decides m = do
  v <- freshIVar
  fork $ do
    heap <- newChildHeap
    abstractHeap <- asks' (.abstractHeap)
    splitDepth <- asks' $ (+ 1) . (.splitDepth)
    suspCount <- newHRef 0
    decisions <- asks' (.decisions)
    ref <- newHRef Nothing
    split' Env {..} False ref (m >>= writeHRef ref . Just) >>= readIVar >>= \ case
      Fail -> empty
      Abort -> abort
      Succeed (heap, heaps, x, m) -> do
        abstractHeap <- asks' (.abstractHeap)
        split' Env {..} False ref m >>= readIVar >>= \ case
          Fail -> writeIVar v $ Just x
          Abort -> abort
          Succeed _ -> writeIVar v Nothing
  pure v
  where
    heaps = mempty
    store = mempty
    processes = mempty
    result = Nothing

assume
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (IVar m a)
assume m = do
  v <- freshIVar
  fork $ do
    ref <- newHRef Nothing
    split ref (m >>= writeHRef ref . Just) >>= readIVar >>= \ case
      Fail -> abort
      Abort -> abort
      Succeed (_, _, x, _) -> writeIVar v x
  pure v

abort :: VerseT m a
abort = VerseT $ \ _ ak _ _ _ _ -> ak

split
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => HRef m (Maybe a)
  -> VerseT m ()
  -> VerseT m (IVar m (Split (Heap, IntMap Heap, a, VerseT m ())))
split left m = do
  heap <- newChildHeap
  abstractHeap <- asks' (.abstractHeap)
  splitDepth <- asks' $ (+ 1) . (.splitDepth)
  suspCount <- newHRef 0
  decisions <- asks' (.decisions)
  split' Env {..} False left m
  where
    heaps = mempty
    store = mempty
    processes = mempty
    result = Nothing

split'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => Env m
  -> Bool
  -> HRef m (Maybe a)
  -> VerseT m ()
  -> VerseT m (IVar m (Split (Heap, IntMap Heap, a, VerseT m ())))
split' r@Env { abstractHeap = _, ..} abstractHeap left m = do
  lift (msplit_ m r) >>= \ case
    Fail -> newIVar Fail
    Abort -> newIVar Abort
    Succeed (S { heaps, store, processes }, rightFail, rightEmpty) ->
      lift ((0 ==) <$> readHRef' suspCount heap `andM` readHRef' left heap) >>= \ case
        Just x -> do
          let r = FreshenEnv {..}
          (commit, x) <- lift $ runFreshenT ((,) <$> commitStore store <*> freshen x) r
          commit *> newIVar (Succeed (heap, heaps, x, rightFail))
        Nothing -> do
          heap <- supply >>= \ i -> modifyHeaps (IntMap.insert i heap) $> i
          heaps <- modifyHeaps (heaps <>) $> IntMap.keysSet heaps
          result <- freshIVar
          modifyProcesses (Process {..}:)
          pure result

newtype FreezeT m a = FreezeT
  { unFreezeT :: RST Heap (IntMap GHC.Exts.Any) m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             )

runFreezeT :: Monad m => FreezeT m a -> VerseT m a
runFreezeT m = do
  r <- ask'
  lift $ evalRST (unFreezeT m) r.heap mempty

instance MonadTrans FreezeT where
  lift = FreezeT . lift

class Monad m => Freezable a b m | a -> b where
  freeze :: a -> FreezeT m b

instance Monad m => Freezable Int Int m where
  freeze = pure

instance ( Freezable a b m
         , Freezable c d m
         ) => Freezable (Const a c) (Const b d) m where
  freeze = fmap Const . freeze . getConst

instance Freezable a b m => Freezable [a] [b] m where
  freeze = traverse freeze

instance Freezable a b m => Freezable (Maybe a) (Maybe b) m where
  freeze = traverse freeze

instance ( Monad m
         , Freezable (f (Fix f)) (f' (Fix f')) m
         ) => Freezable (Fix f) (Fix f') m where
  freeze = fmap Fix . freeze . getFix

instance ( Monad m
         , Freezable (f (g a)) (f' (g' a')) m
         ) => Freezable (Compose f g a) (Compose f' g' a') m where
  freeze = fmap Compose . freeze . getCompose

instance ( MonadFix m
         , MonadRef m
         , Freezable a b m
         ) => Freezable (Var m a) (Maybe b) m where
  freeze v = FreezeT ask >>= lift . readRepr' v >>= \ case
    Unbound {} -> pure Nothing
    Bound x i ->
      mfix $
      FreezeT .
      state' .
      IntMap.Lazy.lookupInsert i .
      unsafeCoerce >=> \ case
        Just x' -> pure $ unsafeCoerce x'
        Nothing -> Just <$> freeze x

instance ( MonadFix m
         , MonadRef m
         , Freezable a b m
         ) => Freezable (VarRef m a) (Maybe b) m where
  freeze ref = freeze =<< lift . readVarRef' ref =<< FreezeT ask

freeze' :: (Monad m, Freezable a b m) => a -> VerseT m b
freeze' = runFreezeT . freeze

newtype FreshenT m a = FreshenT
  { unFreshenT :: RST FreshenEnv (IntMap GHC.Exts.Any) m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadFix
             )

data FreshenEnv = FreshenEnv
  { heap :: !Heap
  , splitDepth :: {-# UNPACK #-} !Int
  }

runFreshenT :: Monad m => FreshenT m a -> FreshenEnv -> m a
runFreshenT m r = evalRST (unFreshenT m) r mempty

instance MonadTrans FreshenT where
  lift = FreshenT . lift

instance MonadRef m => MonadRef (FreshenT m)

class Monad m => Freshenable a m where
  freshen :: a -> FreshenT m a

instance Monad m => Freshenable () m where
  freshen = pure

instance ( Freshenable a m
         , Freshenable b m
         ) => Freshenable (a, b) m where
  freshen (x, y) = (,) <$> freshen x <*> freshen y

instance Monad m => Freshenable Int m where
  freshen = pure

instance Freshenable a m => Freshenable [a] m where
  freshen = traverse freshen

instance Freshenable v m => Freshenable (Strict.HashMap k v) m where
  freshen = traverse freshen

instance ( Monad m
         , Freshenable (f (g a)) m
         ) => Freshenable (Compose f g a) m where
  freshen = fmap Compose . freshen . getCompose

instance ( Monad m
         , Freshenable (f (Fix f)) m
         ) => Freshenable (Fix f) m where
  freshen = fmap Fix . freshen . getFix

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Freshenable a m
         ) => Freshenable (Var m a) m where
  freshen v = do
    FreshenEnv {..} <- FreshenT ask
    lift (findRepr' v heap) >>= \ case
      Found _ (Bound x i) -> mfix $ \ x' ->
        FreshenT (state' . IntMap.Lazy.lookupInsert i $ unsafeCoerce x') >>= \ case
          Just x' -> pure $ unsafeCoerce x'
          Nothing -> lift . newVar' =<< freshen x
      Found v (Unbound n _ i) -> mfix $ \ x ->
        FreshenT (state' . IntMap.Lazy.lookupInsert i $ unsafeCoerce x) >>= \ case
          Just x -> pure $ unsafeCoerce x
          Nothing ->
            if n == splitDepth
            then lift . freshVar' $ splitDepth - 1
            else pure v

msplit_
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> Env m -> m (Split (S m, VerseT m (), VerseT m ()))
msplit_ m = unVerseT m yield abort succeed fail fail
  where
    yield = Yield $ \ addSusp sk fk ek r -> do
      removeSusp <- addSusp $ \ x ->
        liftSucceed (toS >>> sk x fail fail) >>= reflect_
      pure $ Succeed
        ( toS r
        , liftSucceed (toR >>> \ r -> removeSusp r *> fk r) >>= reflect_
        , liftSucceed (toR >>> \ r -> removeSusp r *> ek r) >>= reflect_
        )
    succeed () fk ek r = pure $ Succeed
      ( r
      , liftSucceed (toR >>> fk) >>= reflect_
      , liftSucceed (toR >>> ek) >>= reflect_
      )
    fail _ = pure Fail
    abort = pure Abort

newtype HRef m a = HRef
  { unHRef :: Ref m (HeapMap a)
  }

newHRef :: MonadRef m => a -> VerseT m (HRef m a)
newHRef = lift . newHRef'

newHRef' :: MonadRef m => a -> m (HRef m a)
newHRef' = fmap HRef . newRef . singleton

readHRef :: MonadRef m => HRef m a -> VerseT m a
readHRef ref = lift . readHRef' ref =<< asks' (.heap)

readHRef' :: MonadRef m => HRef m a -> Heap -> m a
readHRef' ref = getLocal (unHRef ref)

writeHRef :: MonadRef m => HRef m a -> a -> VerseT m ()
writeHRef ref = liftAlt' . writeHRef' ref
  where
    liftAlt' f = liftAlt . fmap (. (.heap)) . f =<< asks' (.heap)

writeHRef' :: MonadRef m => HRef m a -> a -> Heap -> m (Heap -> m ())
writeHRef' ref x h =
  readHRef' ref h >>= \ y ->
  put' (unHRef ref) h x $> \ h ->
  put' (unHRef ref) h y

modifyHRef' :: MonadRef m => HRef m a -> (a -> a) -> VerseT m ()
modifyHRef' ref f = do
  x <- f <$> readHRef ref
  x `seq` writeHRef ref x

stateAbstractHRef' :: MonadRef m => HRef m s -> (s -> (a, s)) -> VerseT m a
stateAbstractHRef' ref f = asks' (.abstractHeap) >>= \ k -> lift $ do
  (x, s) <- f <$> getLocal (unHRef ref) k
  s `seq` put' (unHRef ref) k s
  pure x

incrSuspCount :: MonadRef m => VerseT m ()
incrSuspCount = do
  Env {..} <- ask'
  incr suspCount

incr :: (MonadRef m, Num a) => HRef m a -> VerseT m ()
incr = flip modifyHRef' (+ 1)

incr' :: (MonadRef m, Num a) => HRef m a -> Heap -> m ()
incr' ref k = do
  x <- getLocal (unHRef ref) k
  put' (unHRef ref) k $! x + 1

decrSuspCount :: (MonadFix m, MonadRef m, MonadSupply Int m) => VerseT m ()
decrSuspCount = do
  Env {..} <- ask'
  decr suspCount
  whenJust result $ \ result ->
    whenM (readHRef suspCount <&> (== 0)) $
      writeIVar result ()

decr :: (MonadRef m, Num a) => HRef m a -> VerseT m ()
decr = flip modifyHRef' $ subtract 1

decr' :: (MonadRef m, Num a) => HRef m a -> Heap -> m ()
decr' ref k = do
  x <- getLocal (unHRef ref) k
  put' (unHRef ref) k $! x - 1

freshenStore
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Store m
  -> FreshenEnv
  -> m ()
freshenStore = runFreshenT . traverse_ freshenStoreElem

freshenStoreElem
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => StoreElem m
  -> FreshenT m ()
freshenStoreElem (StoreElem ref) = do
  FreshenEnv {..} <- FreshenT ask
  x <- freshen =<< lift (readVarRef' ref heap)
  lift $ put' (unVarRef ref) heap x

commitStore
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Store m
  -> FreshenT m (VerseT m ())
commitStore store = foldlM f (pure ()) store
  where
    f z x = (z *>) <$> commitStoreElem x

commitStoreElem
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => StoreElem m
  -> FreshenT m (VerseT m ())
commitStoreElem (StoreElem ref) = do
  FreshenEnv {..} <- FreshenT ask
  x <- freshen =<< lift (get' (unVarRef ref) heap)
  lift $ put' (unVarRef ref) heap x
  pure $ writeVarRef ref x

duplicateStore :: MonadRef m => Heap -> VerseT m ()
duplicateStore h = traverse_ f =<< asks' (.store)
  where
    f (StoreElem ref) = lift . put' (unVarRef ref) h =<< readVarRef ref

copy
  :: (MonadFix m, MonadSupply Int m, Traversable f)
  => f Heap -> Heap -> m (f Heap)
copy = runCopyT . copyHeaps

type CopyT m = RST Heap (IntMap Heap) m

runCopyT :: Monad m => CopyT m a -> Heap -> m a
runCopyT m h = evalRST m h mempty

copyHeaps
  :: (MonadFix m, MonadSupply Int m, Traversable f)
  => f Heap -> CopyT m (f Heap)
copyHeaps = traverse copyHeap

copyHeap
  :: (MonadFix m, MonadSupply Int m)
  => Heap -> CopyT m Heap
copyHeap = \ case
  Root -> pure Root
  h@Child {..} ->
    mfix $ state' . IntMap.Lazy.lookupInsert label >=> \ case
      Just h -> pure h
      Nothing -> do
        label <- supply
        ask <&> eqHeap tail >>= \ case
          True -> pure $ Child { label, tail, right = h, pred = h }
          False -> do
            tail <- copyHeap tail
            right <- copyHeap right
            pure $ Child { label, tail, right, pred = h }

revert :: Traversable f => f Heap -> Heap -> f Heap
revert = runRevert . revertHeaps

type Revert = Reader Heap

runRevert :: Revert a -> Heap -> a
runRevert = runReader

revertHeaps :: Traversable f => f Heap -> Revert (f Heap)
revertHeaps = traverse revertHeap

revertHeap :: Heap -> Revert Heap
revertHeap = \ case
  Root -> pure Root
  Child {..} -> ask <&> eqHeap tail >>= \ case
    True -> pure right
    False -> pure pred

newChildHeap :: MonadSupply Int m => VerseT m Heap
newChildHeap = do
  label <- supply
  tail <- ask' <&> (.heap)
  pure $ Child { right = Root, pred = Root, .. }

reflect_
  :: (Applicative m, MonadFix m, MonadRef m, MonadSupply Int m)
  => Split (S m, VerseT m (), VerseT m ())
  -> VerseT m ()
reflect_ = \ case
  Fail -> empty
  Abort -> abort
  Succeed (r, m, n) -> alts r m n

liftSucceed :: Monad m => (Env m -> m a) -> VerseT m a
liftSucceed f = VerseT $ \ _ _ sk fk ek r@Env {..} -> f r >>= \ x -> sk x fk ek S {..}

liftAlt :: Monad m => m (Env m -> m ()) -> VerseT m ()
liftAlt m = VerseT $ \ _ _ sk fk ek Env {..} ->
  m >>= \ f ->
  sk () (\ r@R {..} -> f Env {..} *> fk r) (\ r@R {..} -> f Env {..} *> ek r) S {..}

liftEmpty :: Monad m => m (R -> m ()) -> VerseT m ()
liftEmpty m = VerseT $ \ _ _ sk fk ek Env {..} ->
  m >>= \ f -> sk () fk (\ r -> f r *> ek r) S {..}

alts
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => S m -> VerseT m () -> VerseT m () -> VerseT m ()
alts r x y = VerseT $ \ yk ak sk fk ek Env {..} -> do
  let
    fk' R {..} = unVerseT x yk ak sk fk fk Env {..}
    ek' R {..} = unVerseT y yk ak sk fk ek Env {..}
  sk () fk' ek' r

modifyHeaps :: (IntMap Heap -> IntMap Heap) -> VerseT m ()
modifyHeaps f = VerseT $ \ _ _ sk fk ek Env {..} ->
  sk () fk ek S { heaps = f heaps, .. }

modifyStore' :: (Store m -> Store m) -> VerseT m ()
modifyStore' f = VerseT $ \ _ _ sk fk ek Env {..} ->
  sk () fk ek S { store = f store, .. }

putProcesses :: Processes m -> VerseT m ()
putProcesses xs = VerseT $ \ _ _ sk fk ek Env {..} ->
  sk () fk ek S { processes = xs, .. }

modifyProcesses :: (Processes m -> Processes m) -> VerseT m ()
modifyProcesses f = VerseT $ \ _ _ sk fk ek Env {..} ->
  sk () fk ek S { processes = f processes, .. }

ask' :: VerseT m (Env m)
ask' = VerseT $ \ _ _ sk fk ek r@Env {..} -> sk r fk ek S {..}

asks' :: (Env m -> a) -> VerseT m a
asks' f = VerseT $ \ _ _ sk fk ek r@Env {..} -> sk (f r) fk ek S {..}

local' :: (Env m -> Env m) -> VerseT m a -> VerseT m a
local' f m = VerseT $ \ yk ak sk fk ek -> unVerseT m yk ak sk fk ek . f

lookupIVarState :: Heap -> HeapMap (IVarState m a) -> IVarState m a
lookupIVarState k xs@HeapMap {..} = case k of
  Root -> root
  Child {..} ->
    IntMap.lookup label child `or`
    lookupPred pred child `or`
    case lookup tail xs of
      Susp _ -> Susp emptySusp
      x -> x

lookupVarState :: Heap -> HeapMap (VarState m a) -> VarState m a
lookupVarState k xs@HeapMap {..} = case k of
  Root -> root
  Child {..} ->
    IntMap.lookup label child `or`
    lookupPred pred child `or`
    case lookup tail xs of
      Repr (Unbound n _ i) -> Repr $ Unbound n emptySusp i
      x -> x

get' :: MonadRef m => Ref m (HeapMap a) -> Heap -> m a
get' ref k = readRef ref <&> lookup k

getLocal :: MonadRef m => Ref m (HeapMap a) -> Heap -> m a
getLocal ref k = readRef ref <&> \ xs -> fromMaybe xs.root $ lookupLocal k xs

put' :: MonadRef m => Ref m (HeapMap a) -> Heap -> a -> m ()
put' ref k = modifyRef' ref . insert k

singleton :: a -> HeapMap a
singleton root = HeapMap { child = mempty, .. }

lookup :: Heap -> HeapMap a -> a
lookup k xs@HeapMap {..} = case k of
  Root -> root
  Child {..} ->
    IntMap.lookup label child `or`
    lookupPred pred child `or`
    lookup tail xs

lookupLocal :: Heap -> HeapMap a -> Maybe a
lookupLocal k HeapMap {..} = case k of
  Root -> Just root
  Child {..} ->
    IntMap.lookup label child <|>
    lookupPred pred child

lookupPred :: Heap -> IntMap a -> Maybe a
lookupPred k xs = case k of
  Root -> Nothing
  Child {..} ->
    IntMap.lookup label xs <|>
    lookupPred pred xs

insert :: Heap -> a -> HeapMap a -> HeapMap a
insert k v HeapMap {..} = case k of
  Root -> HeapMap { root = v, .. }
  Child {..} -> HeapMap { child = IntMap.insert label v child, .. }

or :: Maybe a -> a -> a
or = flip fromMaybe
infixr 3 `or`

andM :: Monad m => m Bool -> m (Maybe a) -> m (Maybe a)
m `andM` n = m >>= \ case
  False -> pure Nothing
  True -> n
infixr 3 `andM`

whenM :: Monad m => m Bool -> m () -> m ()
whenM m n = m >>= \ case
  True -> n
  False -> pure ()

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust x f = maybe (pure ()) f x

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

intersection' :: IntMap a -> IntSet -> IntMap a
intersection' x = IntMap.intersection x . IntMap.fromSet (const ())

data Split a = Fail | Abort | Succeed a

data Decisions = Decisions [Bool] [Bool]

emptyDecisions :: Decisions
emptyDecisions = Decisions [] []

uncons :: Decisions -> (Bool, Decisions)
uncons = \ case
  Decisions [] ys -> (True, Decisions [] (True:ys))
  Decisions (x:xs) ys -> (x, Decisions xs (x:ys))

succ :: Decisions -> Maybe Decisions
succ (Decisions _ ys) = loop ys
  where
    loop = \ case
      [] -> Nothing
      True:xs -> Just $ Decisions (reverse $ False:xs) []
      False:xs -> loop xs
