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
  , IVar
  , freshIVar
  , newIVar
  , readIVar
  , writeIVar
  , Var
  , freshVar
  , newVar
  , readVar
  , unify
  , VarRef
  , newVarRef
  , readVarRef
  , writeVarRef
  , fork
  , one
  , if'
  , all
  , for
  , verify
  , decide
  , succeeds
  , decides
  , assume
  , FreezeT
  , runFreezeT
  , Frozen (..)
  , Freezable (..)
  , freeze'
  , FreshenT
  , Freshenable (..)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Abort (MonadAbort)
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RS
import Control.Monad.RWS.CPS
import Control.Monad.Supply

import Data.Bool
import Data.Either
import Data.Eq
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.HashMap.Strict qualified as Strict (HashMap)
import Data.Int
import Data.IntMap.Internal qualified as IntMap.Internal
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Lazy qualified as IntMap.Lazy
import Data.Match
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Traversable (Traversable, traverse)
import Data.Tuple

import GHC.Exts qualified

import Prelude (Num (..), ($!), error, reverse, subtract)

import Prettyprinter

import Text.Show

import Unsafe.Coerce (unsafeCoerce)

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield r m -> Abort r m -> Logic r m a
  }

newtype Yield r m = Yield
  { unYield :: forall a . AddSusp m a -> Logic r m a
  }

type AddSusp m a = Susp m a -> m (Fail () m)

type Logic r m a = Succeed r m a -> Fail r m -> Empty r m -> Env m -> m r

type Succeed r m a = a -> Fail r m -> Empty r m -> Env m -> m r

type Fail r m = Env m -> m r

type Empty r m = Env m -> m r

type Abort r m = m r

data Env m = Env
  { heap :: !HeapKey
  , decisions :: !(Ref m Decisions)
  , children :: !(Ref m (Processes m))
  , suspCount :: !(Ref m Int)
  , commit :: !(Ref m (Commit m))
  , splitDepth :: {-# UNPACK #-} !Int
  }

type Processes m = [Process m]

data Process m = forall a . Freshenable a m => Process
  { env :: !(Env m)
  , left :: !(HeapRef m (Maybe a))
  , right :: !(Ref m (VerseT m (), VerseT m ()))
  , result :: !(IVar m (Split (HeapKey, Ref m Decisions, a, VerseT m ())))
  }

data Heap = Heap
  { label :: {-# UNPACK #-} !Int
  , tail :: !HeapKey
  , pred :: !(Maybe Heap)
  , abstract :: !Bool
  }

type Susp m a = a -> VerseT m ()

emptySusp :: Susp m a
emptySusp = const $ pure ()

type Commit m = FreshenT m ()

newtype IVar m a = IVar
  { unIVar :: Ref m (HeapMap (IVarState m a))
  }

newtype Var m f = Var
  { unVar :: Ref m (HeapMap (VarState m f))
  }

newtype VarRef m f = VarRef
  { unVarRef :: Ref m (HeapMap (Var m f))
  }

instance EqRef (Ref m) => Eq (VarRef m f) where
  (==) = eqRef `on` unVarRef

data HeapMap a = HeapMap !a !(IntMap a)

type HeapKey = Maybe Heap

data IVarState m a
  = Val !a
  | Susp !(Susp m a)

data VarState m f
  = Link !(Var m f)
  | Repr !(Repr m f)

data Repr m f
  = Bound
    !(f (Var m f))
    {-# UNPACK #-} !Int
  | Unbound
    {-# UNPACK #-} !Int
    !(Susp m (f (Var m f)))
    {-# UNPACK #-} !Int

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ yk ak sk -> unVerseT m yk ak $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ _ sk -> sk x
  f <*> x = VerseT $ \ yk ak sk -> unVerseT f yk ak $ \ f -> unVerseT x yk ak $ sk . f

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         ) => Alternative (VerseT m) where
  empty = VerseT $ \ _ _ _ _ ek r -> ek r
  x <|> y = VerseT $ \ yk ak sk fk ek r -> do
    xs <- readRef r.children
    writeRef r.children =<< runCopyT (copyProcesses xs) r
    let
      f r = writeRef r.children xs
      fk' r = f r *> unVerseT y yk ak sk fk fk r
      ek' r = f r *> unVerseT y yk ak sk fk ek r
    unVerseT x yk ak sk fk' ek' r

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk ak sk -> unVerseT x yk ak $ \ x -> unVerseT (f x) yk ak sk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ _ sk fk ek r -> m >>= \ x -> sk x fk ek r

instance MonadAbort e m => MonadAbort e (VerseT m)

instance MonadSupply s m => MonadSupply s (VerseT m)

runVerseT :: MonadRef m => VerseT m a -> m (Maybe [a])
runVerseT m = do
  children <- newRef mempty
  suspCount <- newRef 0
  commit <- newRef $ pure ()
  decisions <- newRef emptyDecisions
  unVerseT m yk ak sk fk fk Env {..}
  where
    yk = Yield $ \ _ _ _ _ _ -> pure Nothing
    ak = pure $ Just []
    heap = Nothing
    splitDepth = 0
    sk x fk _ r = readRef r.suspCount >>= \ case
      0 -> do
        commit <- readRef r.commit
        runFreshenT' commit r.heap splitDepth
        fmap (x:) <$> fk r
      _ -> pure Nothing
    fk _ = pure $ Just []

freshIVar :: MonadRef m => VerseT m (IVar m a)
freshIVar = lift freshIVar'

freshIVar' :: MonadRef m => m (IVar m a)
freshIVar' = fmap IVar . newRef . singleton $ Susp emptySusp

newIVar :: MonadRef m => a -> VerseT m (IVar m a)
newIVar = lift . newIVar'

newIVar' :: MonadRef m => a -> m (IVar m a)
newIVar' = fmap IVar . newRef . singleton . Val

readIVar :: MonadRef m => IVar m a -> VerseT m a
readIVar v = VerseT $ \ yk _ sk fk ek r ->
  readRef (unIVar v) <&> lookupIVarState r.heap >>= \ case
    Val x -> sk x fk ek r
    x@(Susp k) -> rotate (unYield yk) sk fk ek r $ \ k' ->
      put' (unIVar v) r.heap (Susp $ \ x -> k' x *> k x) $> \ r ->
      put' (unIVar v) r.heap x
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
    resumeChildren $ writeLocalIVar v x
    k x

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
    resumeChildren $ writeLocalIVar v x
    k x
  Nothing -> resumeChildren $ writeLocalIVar v x

readIVarState :: MonadRef m => IVar m a -> VerseT m (IVarState m a)
readIVarState v = liftSucceed $ \ r ->
  readRef (unIVar v) <&> lookupIVarState r.heap

readLocalIVarState :: MonadRef m => IVar m a -> VerseT m (Maybe (IVarState m a))
readLocalIVarState v = liftSucceed $ \ r ->
  readRef (unIVar v) <&> lookupLocal r.heap

freshVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m f)
freshVar = liftSucceed $ \ r -> freshVar' r.splitDepth

freshVar' :: (MonadRef m, MonadSupply Int m) => Int -> m (Var m f)
freshVar' n = fmap Var . newRef . singleton . Repr . Unbound n emptySusp =<< supply

newVar :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> VerseT m (Var m f)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newRef . singleton . Repr . Bound x =<< supply

readVar :: MonadRef m => Var m f -> VerseT m (f (Var m f))
readVar v = readVarState v >>= \ case
  Link v -> readVar v
  Repr (Bound x _) -> pure x
  x@(Repr (Unbound n k i)) -> VerseT $ \ yk _ sk fk ek r ->
    rotate (unYield yk) sk fk ek r $ \ k' ->
    put' (unVar v) r.heap (Repr $ Unbound n (\ x -> k x *> k' x) i) $> \ r ->
    put' (unVar v) r.heap x
  where
    rotate f x1 x2 x3 x4 x5 = f x5 x1 x2 x3 x4

unify
  :: (MonadFix m, MonadRef m, MonadSupply Int m, RowMatchable f)
  => Var m f -> Var m f -> VerseT m ()
unify v_x v_y = (,) <$> findRepr v_x <*> findRepr v_y >>= \ case
  (Found v_x (Bound x _), Found v_y (Unbound n_y k_y _)) -> do
    r <- ask'
    when (n_y < r.splitDepth) incrSuspCount
    writeLink v_y v_x
    k_y x
    resumeChildren $ subst v_x v_y
  (Found v_x (Unbound n_x k_x _), Found v_y (Bound y _)) -> do
    r <- ask'
    when (n_x < r.splitDepth) incrSuspCount
    writeLink v_x v_y
    k_x y
    resumeChildren $ subst v_y v_x
  (Found v_x repr_x@(Bound x i_x), Found v_y repr_y@(Bound y i_y)) ->
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y repr_x
        for_ z $ uncurry unify
      LE z -> do
        decide
        writeAbstractRepr v_y repr_x
        for_ z $ \ (v_x, y) -> do
          v_y <- newVar y
          unify v_x v_y
      GE z -> do
        decide
        writeAbstractRepr v_x repr_y
        for_ z $ \ (x, v_y) -> do
          v_x <- newVar x
          unify v_x v_y
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y repr_x
        unifyUncons f_x v_xs f_y v_ys
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound n_y k_y i_y)) -> do
    r <- ask'
    case compare' n_x i_x n_y i_y of
      EQ -> pure ()
      LT -> do
        when (n_y < r.splitDepth) incrSuspCount
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeChildren $ subst v_x v_y
      GT -> do
        when (n_x < r.splitDepth) incrSuspCount
        writeLink v_x v_y
        writeRepr v_y $ Unbound n_y (\ x -> k_x x *> k_y x) i_y
        resumeChildren $ subst v_y v_x

subst
  :: (MonadFix m, MonadRef m, MonadSupply Int m, RowMatchable f)
  => Var m f -> Var m f -> VerseT m ()
subst v_x v_y = findLocalRepr v_y >>= \ case
  Nothing -> resumeChildren $ subst v_x v_y
  Just y -> subst' v_x y

subst'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, RowMatchable f)
  => Var m f -> Found m f -> VerseT m ()
subst' v_x y = findRepr v_x <&> (, y) >>= \ case
  (Found v_x (Bound x _), Found v_y (Unbound _ k_y _)) -> do
    writeLink v_y v_x
    k_y x
    resumeChildren $ subst v_x v_y
  (Found _ (Unbound _ k_x _), Found v_y (Bound y _)) -> do
    writeLink v_x v_y
    k_x y
    resumeChildren $ subst v_y v_x
  (Found v_x repr_x@(Bound x i_x), Found v_y repr_y@(Bound y i_y)) -> do
    decrSuspCount
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y repr_x
        for_ z $ uncurry unify
      LE z -> do
        decide
        writeAbstractRepr v_y repr_x
        for_ z $ \ (v_x, y) -> do
          v_y <- newVar y
          unify v_x v_y
      GE z -> do
        decide
        writeAbstractRepr v_x repr_y
        for_ z $ \ (x, v_y) -> do
          v_x <- newVar x
          unify v_x v_y
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y repr_x
        unifyUncons f_x v_xs f_y v_ys
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound n_y k_y i_y)) ->
    case compare' n_x i_x n_y i_y of
      EQ -> decrSuspCount
      LT -> do
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeChildren $ subst v_x v_y
      GT -> do
        writeLink v_x v_y
        writeRepr v_y $ Unbound n_y (\ x -> k_x x *> k_y x) i_y
        resumeChildren $ subst v_y v_x

unifyUncons
  :: (MonadFix m, MonadRef m, MonadSupply Int m, RowMatchable f)
  => (Var m f -> f (Var m f)) -> Var m f
  -> (Var m f -> f (Var m f)) -> Var m f
  -> VerseT m ()
unifyUncons f_x v_xs f_y v_ys = do
  v_zs <- freshVar
  v_xs' <- newVar $ f_x v_zs
  unify v_xs' v_ys
  v_ys' <- newVar $ f_y v_zs
  unify v_xs v_ys'

decide :: (MonadFix m, MonadRef m, MonadSupply Int m) => VerseT m ()
decide = do
  r <- ask'
  lift (stateRef' r.decisions uncons) >>= guard

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
  k <- abstractHeap . (.heap) <$> ask'
  lift . put' (unVar v) k $ Repr x

readRepr' :: MonadRef m => Var m f -> HeapKey -> m (Repr m f)
readRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> readRepr' v h
  Repr x -> pure x

data Found m f = Found !(Var m f) !(Repr m f)

findRepr :: MonadRef m => Var m f -> VerseT m (Found m f)
findRepr v = liftSucceed $ \ r -> findRepr' v r.heap

findRepr' :: MonadRef m => Var m f -> HeapKey -> m (Found m f)
findRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> findRepr' v h
  Repr x -> pure $ Found v x

findLocalRepr :: MonadRef m => Var m f -> VerseT m (Maybe (Found m f))
findLocalRepr v = liftSucceed $ \ r -> loop v r.heap
  where
    loop v h = readRef (unVar v) <&> lookupLocal h >>= \ case
      Nothing -> pure Nothing
      Just (Link v) -> Just <$> findRepr' v h
      Just (Repr x) -> pure . Just $ Found v x

readVarState :: MonadRef m => Var m f -> VerseT m (VarState m f)
readVarState v = liftSucceed $ \ r ->
  readRef (unVar v) <&> lookupVarState r.heap

resumeChildren
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m ()
resumeChildren m = do
  r <- ask'
  (xs, n) <- flip resumeAll m =<< lift (readRef r.children)
  lift $ writeRef r.children xs
  n

resumeAll
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => [Process m] -> VerseT m ()
  -> VerseT m ([Process m], VerseT m ())
resumeAll xs m = fmap sequence_ <$> partitionMaybeM (flip resume m) xs

resume
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Process m -> VerseT m ()
  -> VerseT m (Maybe (VerseT m ()))
resume p@Process { env = Env {..}, .. } m = lift (msplit_ m Env {..}) >>= \ case
  Fail -> resume' p m
  Abort -> pure $ Just $ writeIVar result Abort
  Succeed (m_fail', m_empty') -> do
    (m_fail, m_empty) <- lift $ readRef right
    lift ((0 ==) <$> readRef suspCount `andM` readHeapRef' left heap) >>= \ case
      Nothing -> lift $ do
        writeRef right (m_fail' <|> fork m_fail *> m, m_empty' <|> fork m_empty *> m)
        pure Nothing
      Just x -> do
        x <- runFreshenT (join (readRef commit) *> freshen x) heap
        let m' = m_fail' <|> fork m_fail *> m
        pure $ Just $ writeIVar result $ Succeed (heap, decisions, x, m')

resume'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Process m -> VerseT m ()
  -> VerseT m (Maybe (VerseT m ()))
resume' Process { env = Env {..}, .. } m = do
  (_, m_empty) <- lift $ readRef right
  lift (msplit_ (fork m_empty *> m) Env {..}) >>= \ case
    Fail -> pure . Just $ writeIVar result Fail
    Abort -> pure . Just $ writeIVar result Abort
    Succeed m@(m_fail, _) -> do
      lift ((0 ==) <$> readRef suspCount `andM` readHeapRef' left heap) >>= \ case
        Nothing -> lift $ do
          writeRef right m
          pure Nothing
        Just x -> do
          x <- runFreshenT (join (readRef commit) *> freshen x) heap
          pure $ Just $ writeIVar result $ Succeed (heap, decisions, x, m_fail)

newVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
  => Var m f -> VerseT m (VarRef m f)
newVarRef x = do
  r <- ask'
  ref <- lift . fmap VarRef . newRef $ singleton x
  lift . modifyRef' r.commit $ \ commit -> do
    commit
    (h, h', _) <- FreshenT ask
    x <- freshen =<< get' (unVarRef ref) h
    put' (unVarRef ref) h x *> put' (unVarRef ref) h' x
  pure ref

readVarRef :: MonadRef m => VarRef m f -> VerseT m (Var m f)
readVarRef ref = do
  r <- ask'
  lift $ readVarRef' ref r.heap

readVarRef' :: MonadRef m => VarRef m f -> HeapKey -> m (Var m f)
readVarRef' ref = get' (unVarRef ref)

writeVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
  => VarRef m f -> Var m f -> VerseT m ()
writeVarRef ref x = do
  r <- ask'
  y <- lift $ get' (unVarRef ref) r.heap
  liftEmpty $
    put' (unVarRef ref) r.heap x $> \ r ->
    put' (unVarRef ref) r.heap y
  commit <- lift $ readRef r.commit
  let
    commit' = do
      commit
      (h, h', _) <- FreshenT ask
      x <- freshen =<< get' (unVarRef ref) h
      put' (unVarRef ref) h x *> put' (unVarRef ref) h' x
  liftEmpty $
    writeRef r.commit commit' $> \ r ->
    writeRef r.commit commit

fork
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m ()
fork m = liftSucceed (unVerseT m yield abort succeed fail fail) >>= reflect_
  where
    yield = Yield $ \ addSusp sk fk ek r -> do
      incr r.suspCount
      removeSusp <- addSusp $ \ x -> do
        decrSuspCount
        liftSucceed (sk x fail fail) >>= reflect_
      let
        f r = do
          removeSusp r
          decr r.suspCount
      pure $ Succeed
        ( liftSucceed (\ r -> f r *> fk r) >>= reflect_
        , liftSucceed (\ r -> f r *> ek r) >>= reflect_
        )
    succeed () fk ek _ = pure $ Succeed
      ( liftSucceed fk >>= reflect_
      , liftSucceed ek >>= reflect_
      )
    fail _ = pure Fail
    abort = pure Abort

one
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m a)
one m = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    split h ref (m >>= writeHeapRef ref . Just) >>= readIVar >>= \ case
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
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    split h ref (p >>= writeHeapRef ref . Just) >>= readIVar >>= \ case
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
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    loop h ref (m >>= writeHeapRef ref . Just) [] >>= writeIVar v
  pure v
  where
    loop h ref m xs = split h ref m >>= readIVar >>= \ case
      Fail -> pure $ reverse xs
      Abort -> abort
      Succeed (h, _, x, m) -> loop h ref m $ x:xs

for
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> (a -> VerseT m b) -> VerseT m (IVar m [b])
for m f = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    loop h ref (m >>= writeHeapRef ref . Just) f [] >>= writeIVar v
  pure v
  where
    loop h ref m f xs = split h ref m >>= readIVar >>= \ case
      Fail -> pure $ reverse xs
      Abort -> abort
      Succeed (h, _, x, m) -> loop h ref m f . (:xs) =<< f x

verify
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> VerseT m (IVar m ())
verify m = do
  v <- freshIVar
  fork $ loop emptyDecisions m >>= writeIVar v
  pure v
  where
    loop decisions m = verifyAll decisions m >>= readIVar <&> succ >>= \ case
      Nothing -> pure ()
      Just decisions -> loop decisions m

verifyAll
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Decisions -> VerseT m () -> VerseT m (IVar m Decisions)
verifyAll decisions m = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newAbstractHeap
    decisions <- lift $ newRef decisions
    ref <- newHeapRef $ Just ()
    loop h decisions ref m >>= writeIVar v
  pure v
  where
    loop h decisions ref m = split' h decisions ref m >>= readIVar >>= \ case
      Fail -> lift $ readRef decisions
      Abort -> lift $ readRef decisions
      Succeed (h, decisions, (), m) -> loop h decisions ref m

succeeds
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m (Maybe a))
succeeds m = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    split h ref (m >>= writeHeapRef ref . Just) >>= readIVar >>= \ case
      Fail -> writeIVar v Nothing
      Abort -> abort
      Succeed (h, _, x, m) -> split h ref m >>= readIVar >>= \ case
        Fail -> writeIVar v $ Just x
        Abort -> abort
        Succeed _ -> writeIVar v Nothing
  pure v

decides
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a -> VerseT m (IVar m (Maybe a))
decides m = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    split h ref (m >>= writeHeapRef ref . Just) >>= readIVar >>= \ case
      Fail -> empty
      Abort -> abort
      Succeed (h, _, x, m) -> split h ref m >>= readIVar >>= \ case
        Fail -> writeIVar v $ Just x
        Abort -> abort
        Succeed _ -> writeIVar v Nothing
  pure v

assume
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (IVar m a)
assume m = do
  v <- freshIVar
  fork $ do
    h <- Just <$> newHeap
    ref <- newHeapRef Nothing
    split h ref (m >>= writeHeapRef ref . Just) >>= readIVar >>= \ case
      Fail -> abort
      Abort -> abort
      Succeed (_, _, x, _) -> writeIVar v x
  pure v

abort :: VerseT m a
abort = VerseT $ \ _ ak _ _ _ _ -> ak

split
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => Maybe Heap
  -> HeapRef m (Maybe a)
  -> VerseT m ()
  -> VerseT m (IVar m (Split (HeapKey, Ref m Decisions, a, VerseT m ())))
split heap left m = do
  r <- ask'
  split' heap r.decisions left m

split'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => HeapKey
  -> Ref m Decisions
  -> HeapRef m (Maybe a)
  -> VerseT m ()
  -> VerseT m (IVar m (Split (HeapKey, Ref m Decisions, a, VerseT m ())))
split' heap decisions left m = do
  r <- ask'
  children <- lift $ newRef mempty
  suspCount <- lift $ newRef 0
  commit <- lift . newRef $ pure ()
  let
    splitDepth = r.splitDepth + 1
    env = Env {..}
  lift (msplit_ m env) >>= \ case
    Fail -> newIVar Fail
    Abort -> newIVar Abort
    Succeed m@(m_fail, _) ->
      lift ((0 ==) <$> readRef suspCount `andM` readHeapRef' left heap) >>= \ case
        Just x -> do
          x <- runFreshenT (join (readRef commit) *> freshen x) heap
          newIVar $ Succeed (heap, decisions, x, m_fail)
        Nothing -> do
          result <- freshIVar
          right <- lift $ newRef m
          lift $ modifyRef' r.children (Process {..}:)
          pure result

newtype FreezeT m a = FreezeT
  { unFreezeT :: RST HeapKey (IntMap GHC.Exts.Any) m a
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

data Frozen f
  = Unknown
  | Known (f (Frozen f))

deriving instance Show (f (Frozen f)) => Show (Frozen f)

deriving instance Eq (f (Frozen f)) => Eq (Frozen f)

instance Pretty (f (Frozen f)) => Pretty (Frozen f) where
  pretty = \ case
    Unknown -> "_"
    Known x -> pretty x

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

instance ( MonadFix m
         , MonadRef m
         , Freezable (f (Var m f)) (g (Frozen g)) m
         ) => Freezable (Var m f) (Frozen g) m where
  freeze v = FreezeT ask >>= lift . readRepr' v >>= \ case
    Unbound {} -> pure Unknown
    Bound x i -> mfix $ FreezeT . state' . lookupInsert i . unsafeCoerce >=> \ case
      Just x' -> pure $ unsafeCoerce x'
      Nothing -> Known <$> freeze x

instance ( MonadFix m
         , MonadRef m
         , Freezable (f (Var m f)) (g (Frozen g)) m
         ) => Freezable (VarRef m f) (Frozen g) m where
  freeze ref = freeze =<< lift . readVarRef' ref =<< FreezeT ask

freeze' :: (Monad m, Freezable a b m) => a -> VerseT m b
freeze' = runFreezeT . freeze

newtype FreshenT m a = FreshenT
  { unFreshenT :: RWST (HeapKey, HeapKey, Int) (Rollback m) (IntMap GHC.Exts.Any) m a
  } deriving ( Functor
             , Applicative
             , Monad
             )

newtype Rollback m = Rollback { getRollback :: Env m -> m () }

instance Applicative m => Semigroup (Rollback m) where
  f <> g = Rollback $ \ r -> getRollback f r *> getRollback g r

instance Applicative m => Monoid (Rollback m) where
  mempty = Rollback . const $ pure ()

runFreshenT :: Monad m => FreshenT m a -> HeapKey -> VerseT m a
runFreshenT m heap = do
  r <- ask'
  (x, w) <- lift $ evalRWST (unFreshenT m) (heap, r.heap, r.splitDepth) mempty
  addEmpty $ getRollback w
  pure x

runFreshenT' :: Monad m => FreshenT m () -> HeapKey -> Int -> m ()
runFreshenT' m h splitDepth =
  fst <$> evalRWST (unFreshenT m) (h, h, splitDepth) mempty

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

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Traversable f
         ) => Freshenable (Var m f) m where
  freshen = FreshenT . loop
    where
      loop v = do
        (h, _, splitDepth) <- ask
        lift (findRepr' v h) >>= \ case
          Found _ (Bound x i) -> mfix $ \ x' ->
            state' (lookupInsert i $ unsafeCoerce x') >>= \ case
              Just x' -> pure $ unsafeCoerce x'
              Nothing -> lift . newVar' =<< traverse loop x
          Found v (Unbound n _ i) -> mfix $ \ x' ->
            state' (lookupInsert i $ unsafeCoerce x') >>= \ case
              Just x' -> pure $ unsafeCoerce x'
              Nothing -> case n > splitDepth of
                False -> pure v
                True -> lift $ freshVar' splitDepth

msplit_
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m () -> Env m -> m (Split (VerseT m (), VerseT m ()))
msplit_ m = unVerseT m yield abort succeed fail fail
  where
    yield = Yield $ \ addSusp sk fk ek _ -> do
      removeSusp <- addSusp (\ x -> liftSucceed (sk x fail fail) >>= reflect_)
      pure $ Succeed
        ( liftSucceed (\ r -> removeSusp r *> fk r) >>= reflect_
        , liftSucceed (\ r -> removeSusp r *> ek r) >>= reflect_
        )
    succeed () fk ek _ = pure $ Succeed
      ( liftSucceed fk >>= reflect_
      , liftSucceed ek >>= reflect_
      )
    fail _ = pure Fail
    abort = pure Abort

newtype HeapRef m a = HeapRef
  { unHeapRef :: Ref m (HeapMap a)
  }

newHeapRef :: MonadRef m => a -> VerseT m (HeapRef m a)
newHeapRef = lift . newHeapRef'

newHeapRef' :: MonadRef m => a -> m (HeapRef m a)
newHeapRef' = fmap HeapRef . newRef . singleton

readHeapRef' :: MonadRef m => HeapRef m a -> HeapKey -> m a
readHeapRef' ref = get' (unHeapRef ref)

writeHeapRef :: MonadRef m => HeapRef m a -> a -> VerseT m ()
writeHeapRef ref x =
  liftAlt .
  fmap (. (.heap)) .
  writeHeapRef' ref x .
  (.heap) =<< ask'

writeHeapRef' :: MonadRef m => HeapRef m a -> a -> HeapKey -> m (HeapKey -> m ())
writeHeapRef' ref x h =
  readHeapRef' ref h >>= \ y ->
  put' (unHeapRef ref) h x $> \ h ->
  put' (unHeapRef ref) h y

newHeap :: MonadSupply Int m => VerseT m Heap
newHeap = newHeap' False

newAbstractHeap :: MonadSupply Int m => VerseT m Heap
newAbstractHeap = newHeap' True

newHeap' :: MonadSupply Int m => Bool -> VerseT m Heap
newHeap' abstract = do
  label <- lift supply
  tail <- ask' <&> (.heap)
  pure $ Heap { label, tail, pred = Nothing, abstract }

abstractHeap :: HeapKey -> HeapKey
abstractHeap = \ case
  Nothing -> Nothing
  k@(Just h)
    | h.abstract -> k
    | otherwise -> abstractHeap h.tail

isAbstract :: HeapKey -> Bool
isAbstract = \ case
  Nothing -> True
  Just h -> h.abstract

incrSuspCount :: MonadRef m => VerseT m ()
incrSuspCount = do
  r <- ask'
  liftAlt $
    incr r.suspCount $> \ r ->
    decr r.suspCount

decrSuspCount :: MonadRef m => VerseT m ()
decrSuspCount = do
  r <- ask'
  liftAlt $
    decr r.suspCount $> \ r ->
    incr r.suspCount

ask' :: VerseT m (Env m)
ask' = VerseT $ \ _ _ sk fk ek r -> sk r fk ek r

liftSucceed :: Monad m => (Env m -> m a) -> VerseT m a
liftSucceed f = VerseT $ \ _ _ sk fk ek r -> f r >>= \ x -> sk x fk ek r

liftAlt :: Monad m => m (Env m -> m ()) -> VerseT m ()
liftAlt m = VerseT $ \ _ _ sk fk ek r ->
  m >>= \ f -> sk () (\ r -> f r *> fk r) (\ r -> f r *> ek r) r

liftEmpty :: Monad m => m (Env m -> m ()) -> VerseT m ()
liftEmpty m = VerseT $ \ _ _ sk fk ek r ->
  m >>= \ f -> sk () fk (\ r -> f r *> ek r) r

alts
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VerseT m a -> VerseT m a -> VerseT m a -> VerseT m a
alts x y z = VerseT $ \ yk ak sk fk ek r -> do
  xs <- readRef r.children
  writeRef r.children =<< runCopyT (copyProcesses xs) r
  let
    f r = writeRef r.children xs
    fk' r = f r *> unVerseT y yk ak sk fk fk r
    ek' r = f r *> unVerseT z yk ak sk fk ek r
  unVerseT x yk ak sk fk' ek' r

addEmpty :: Applicative m => (Env m -> m ()) -> VerseT m ()
addEmpty f = VerseT $ \ _ _ sk fk ek ->
  sk () fk (\ r -> f r *> ek r)

type CopyT m = ReaderT (HeapKey, Ref m Decisions) m

runCopyT :: CopyT m a -> Env m -> m a
runCopyT m r = runReaderT m (r.heap, r.decisions)

copyProcesses
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Processes m
  -> CopyT m (Processes m)
copyProcesses = traverse copyProcess

copyProcess
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Process m
  -> CopyT m (Process m)
copyProcess Process {..} = do
  env <- copyEnv env
  right <- newRef =<< readRef right
  pure Process {..}

copyEnv
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Env m
  -> CopyT m (Env m)
copyEnv Env {..} = do
  heap <- traverse copyHeap heap
  decisions <- if isAbstract heap then newRef =<< readRef decisions else snd <$> ask
  let r = (heap, decisions)
  children <- newRef =<< local (const r) . copyProcesses =<< readRef children
  suspCount <- newRef =<< readRef suspCount
  commit <- newRef =<< readRef commit
  pure $ Env {..}

copyHeap :: (MonadFix m, MonadSupply Int m) => Heap -> CopyT m Heap
copyHeap pred = do
  label <- supply
  tail <- fst <$> ask
  let abstract = pred.abstract
  pure $ Heap { pred = Just pred, .. }

lookupIVarState :: HeapKey -> HeapMap (IVarState m a) -> IVarState m a
lookupIVarState k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    case lookup k.tail xs of
      Susp _ -> Susp emptySusp
      x -> x

lookupVarState :: HeapKey -> HeapMap (VarState m a) -> VarState m a
lookupVarState k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    case lookup k.tail xs of
      Repr (Unbound n _ i) -> Repr $ Unbound n emptySusp i
      x -> x

lookupLocal :: HeapKey -> HeapMap a -> Maybe a
lookupLocal k (HeapMap y ys) = case k of
  Nothing -> Just y
  Just k -> IntMap.lookup k.label ys <|> lookupPred k.pred ys

get' :: MonadRef m => Ref m (HeapMap a) -> HeapKey -> m a
get' ref k = lookup k <$> readRef ref

put' :: MonadRef m => Ref m (HeapMap a) -> HeapKey -> a -> m ()
put' ref k = modifyRef' ref . insert k

singleton :: a -> HeapMap a
singleton = flip HeapMap mempty

lookup :: HeapKey -> HeapMap a -> a
lookup k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    lookup k.tail xs

lookupPred :: HeapKey -> IntMap a -> Maybe a
lookupPred k xs = k >>= \ k ->
  IntMap.lookup k.label xs <|> lookupPred k.pred xs

insert :: HeapKey -> a -> HeapMap a -> HeapMap a
insert k v (HeapMap x xs) = case k of
  Nothing -> HeapMap v xs
  Just k -> HeapMap x $ IntMap.insert k.label v xs

reflect_
  :: (Applicative m, MonadFix m, MonadRef m, MonadSupply Int m)
  => Split (VerseT m (), VerseT m ())
  -> VerseT m ()
reflect_ = \ case
  Fail -> empty
  Abort -> abort
  Succeed (m, n) -> alts (pure ()) m n

incr :: (MonadRef m, Num a) => Ref m a -> m ()
incr = flip modifyRef' (+ 1)

decr :: (MonadRef m, Num a) => Ref m a -> m ()
decr = flip modifyRef' $ subtract 1

or :: Maybe a -> a -> a
or = flip fromMaybe
infixr 3 `or`

partitionMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m ([a], [b])
partitionMaybeM f = loop f [] []
  where
    loop f xs ys = \ case
      [] -> pure (xs, ys)
      z:zs -> f z >>= \ case
        Nothing -> loop f (z:xs) ys zs
        Just z -> loop f xs (z:ys) zs

andM :: Monad m => m Bool -> m (Maybe a) -> m (Maybe a)
m `andM` n = m >>= \ case
  False -> pure Nothing
  True -> n
infixr 3 `andM`

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

lookupInsert :: IntMap.Key -> a -> IntMap a -> Either a (IntMap a)
lookupInsert !k0 x0 t0 = loop k0 x0 t0
  where
    loop k x = \ case
      t@(IntMap.Internal.Bin p m l r)
        | IntMap.Internal.nomatch k p m ->
          Right $! IntMap.Internal.link k (IntMap.Lazy.singleton k x) p t
        | IntMap.Internal.zero k m -> case loop k x l of
          Right l -> Right $! IntMap.Internal.Bin p m l r
          l@(Left _) -> l
        | otherwise -> case loop k x r of
          Right r -> Right $! IntMap.Internal.Bin p m l r
          r@(Left _) -> r
      t@(IntMap.Internal.Tip k' y)
        | k == k' -> Left y
        | otherwise ->
          Right $! IntMap.Internal.link k (IntMap.Lazy.singleton k x) k' t
      IntMap.Internal.Nil -> Right $! IntMap.Lazy.singleton k x

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
