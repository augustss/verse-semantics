{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , Frozen (..)
  , freezeVar
  , VarRef
  , newVarRef
  , readVarRef
  , writeVarRef
  , FreshenT
  , Freshenable (..)
  , fork
  , one
  , if'
  , all
  , for
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Bool
import Data.Either
import Data.Eq
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.Int
import Data.IntMap.Internal qualified as IntMap.Internal
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Lazy qualified as IntMap.Lazy
import Data.Match
import Data.Maybe
import Data.Monoid (mempty)
import Data.Ord
import Data.Traversable (Traversable, traverse)
import Data.Tuple

import GHC.Exts (Any)

import Prelude (Num (..), ($!), error, reverse, subtract)

import Text.Show

import Unsafe.Coerce (unsafeCoerce)

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield r m -> Logic r m a
  }

newtype Yield r m = Yield
  { unYield :: forall a . AddSusp m a -> Logic r m a
  }

type AddSusp m a = Susp m a -> m (Fail () m)

type Logic r m a = Succeed r m a -> Result r m

type Succeed r m a = a -> Result r m

type Result r m = Fail r m -> Empty r m -> Rollback m -> Env m -> m r

type Fail r m = Env m -> m r

type Empty r m = Env m -> m r

type Rollback m = Env m -> m ()

type Susp m a = a -> VerseT m ()

data Env m = Env
  { heap :: !(Maybe Heap)
  , children :: !(Ref m (Processes m))
  , suspCount :: !(Ref m Int)
  , commit :: !(Ref m (Commit m))
  , splitDepth :: {-# UNPACK #-} !Int
  }

type Processes m = [Process m]

data Process m = forall a . Freshenable a m => Process
  { heap :: !Heap
  , children :: !(Ref m (Processes m))
  , suspCount :: !(Ref m Int)
  , commit :: !(Ref m (Commit m))
  , splitDepth :: {-# UNPACK #-} !Int
  , left :: !(HeapRef m (Maybe a))
  , right :: !(Ref m (VerseT m ()))
  , result :: !(IVar m (Maybe (Heap, a, VerseT m ())))
  }

data Heap = Heap
  { label :: {-# UNPACK #-} !Int
  , tail :: !(Maybe Heap)
  , pred :: !(Maybe Heap)
  } deriving Show

type Commit m = FreshenT m ()

newtype HeapRef m a = HeapRef
  { unHeapRef :: Ref m (HeapMap a)
  }

newtype IVar m a = IVar
  { unIVar :: Ref m (HeapMap (IVarState m a))
  }

newtype Var m f = Var
  { unVar :: Ref m (HeapMap (VarState m f))
  }

newtype VarRef m f = VarRef
  { unVarRef :: Ref m (Var m f)
  }

data HeapMap a = HeapMap !a !(IntMap a)

type HeapKey = Maybe Heap

data IVarState m a
  = Val !a
  | Susp !(Susp m a)

data VarState m f
  = Link !(Var m f)
  | Repr !(Repr m f)

data Repr m f
  = Bound !(f (Var m f)) {-# UNPACK #-} !Int
  | Unbound {-# UNPACK #-} !Int !(Susp m (f (Var m f))) {-# UNPACK #-} !Int

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ yk sk -> unVerseT m yk $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ sk -> sk x
  f <*> x = VerseT $ \ yk sk -> unVerseT f yk $ \ f -> unVerseT x yk $ sk . f

instance (MonadRef m, MonadSupply Int m) => Alternative (VerseT m) where
  empty = VerseT $ \ _ _ _ ek _ r -> ek r
  x <|> y = VerseT $ \ yk sk fk ek rk r -> do
    xs <- readRef r.children
    writeRef r.children =<< runReaderT (copyProcesses xs) r.heap
    let
      f r = writeRef r.children xs
      fk' r = f r *> unVerseT y yk sk fk fk (const $ pure ()) r
      ek' r = f r *> unVerseT y yk sk fk ek rk r
    unVerseT x yk sk fk' ek' rk r

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk sk -> unVerseT x yk $ \ x -> unVerseT (f x) yk sk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ sk fk ek rk r -> m >>= \ x -> sk x fk ek rk r

runVerseT :: MonadRef m => VerseT m a -> m (Maybe [a])
runVerseT m = do
  children <- newRef mempty
  suspCount <- newRef 0
  commit <- newRef $ pure ()
  unVerseT m yk sk fk fk (const $ pure ()) Env {..}
  where
    yk = Yield $ \ _ _ _ _ _ _ -> pure Nothing
    heap = Nothing
    splitDepth = 0
    sk x fk _ _ r = readRef r.suspCount >>= \ case
      0 -> fmap (x:) <$> fk r
      _ -> pure Nothing
    fk _ = pure $ Just []

freshIVar :: MonadRef m => VerseT m (IVar m a)
freshIVar = lift freshIVar'

freshIVar' :: MonadRef m => m (IVar m a)
freshIVar' = fmap IVar . newRef . singleton . Susp . const $ pure ()

newIVar :: MonadRef m => a -> VerseT m (IVar m a)
newIVar = lift . newIVar'

newIVar' :: MonadRef m => a -> m (IVar m a)
newIVar' = fmap IVar . newRef . singleton . Val

readIVar :: MonadRef m => IVar m a -> VerseT m a
readIVar v = VerseT $ \ yk sk fk ek rk r ->
  readRef (unIVar v) <&> lookupIVarState r.heap >>= \ case
    Val x -> sk x fk ek rk r
    x@(Susp k) -> rotate (unYield yk) sk fk ek rk r $ \ k' ->
      put' (unIVar v) r.heap (Susp $ \ x -> k x *> k' x) $> \ r ->
      put' (unIVar v) r.heap x
  where
    rotate f x1 x2 x3 x4 x5 x6 = f x6 x1 x2 x3 x4 x5

writeIVar :: (MonadRef m, MonadSupply Int m) => IVar m a -> a -> VerseT m ()
writeIVar v x = readIVarState v >>= \ case
  Val _ -> error "writeIVar"
  y@(Susp k) -> do
    lift' (\ r -> put' (unIVar v) r.heap (Val x)) (\ r -> put' (unIVar v) r.heap y)
    resumeChildren $ writeLocalIVar v x
    k x

writeLocalIVar :: (MonadRef m, MonadSupply Int m) => IVar m a -> a -> VerseT m ()
writeLocalIVar v x = readLocalIVarState v >>= \ case
  Just (Val _) -> error "writeIVar"
  Just y@(Susp k) -> do
    lift' (\ r -> put' (unIVar v) r.heap (Val x)) (\ r -> put' (unIVar v) r.heap y)
    resumeChildren $ writeLocalIVar v x
    k x
  Nothing -> resumeChildren $ writeLocalIVar v x

readIVarState :: MonadRef m => IVar m a -> VerseT m (IVarState m a)
readIVarState v = liftSuccess $ \ r ->
  readRef (unIVar v) <&> lookupIVarState r.heap

readLocalIVarState :: MonadRef m => IVar m a -> VerseT m (Maybe (IVarState m a))
readLocalIVarState v = liftSuccess $ \ r ->
  readRef (unIVar v) <&> lookupLocalState r.heap

freshVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m f)
freshVar = liftSuccess $ \ r ->
  fmap Var . newRef . singleton . Repr . Unbound r.splitDepth (const $ pure ()) =<< supply

newVar :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> VerseT m (Var m f)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newRef . singleton . Repr . Bound x =<< supply

readVar :: MonadRef m => Var m f -> VerseT m (f (Var m f))
readVar v = readVarState v >>= \ case
  Link v -> readVar v
  Repr (Bound x _) -> pure x
  x@(Repr (Unbound n k i)) -> VerseT $ \ yk sk fk ek rk r ->
    rotate (unYield yk) sk fk ek rk r $ \ k' ->
    put' (unVar v) r.heap (Repr $ Unbound n (\ x -> k x *> k' x) i) $> \ r ->
    put' (unVar v) r.heap x
  where
    rotate f x1 x2 x3 x4 x5 x6 = f x6 x1 x2 x3 x4 x5

unify :: ( MonadRef m
         , MonadSupply Int m
         , RowMatchable f
         ) => Var m f -> Var m f -> VerseT m ()
unify v_x v_y = (,) <$> findRepr v_x <*> findRepr v_y >>= \ case
  (Found v_x (Unbound n_x k_x _), Found v_y r_y@(Bound y _)) -> do
    r <- ask'
    when (n_x < r.splitDepth) incrSuspCount
    writeRepr v_x r_y
    k_x y
    resumeChildren $ subst v_y v_x
  (Found v_x r_x@(Bound x _), Found v_y (Unbound n_y k_y _)) -> do
    r <- ask'
    when (n_y < r.splitDepth) incrSuspCount
    writeRepr v_y r_x
    k_y x
    resumeChildren $ subst v_x v_y
  (Found _ r_x@(Bound x i_x), Found v_y (Bound y i_y)) ->
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y r_x
        for_ z $ uncurry unify
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y r_x
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

subst :: ( MonadRef m
         , MonadSupply Int m
         , RowMatchable f
         ) => Var m f -> Var m f -> VerseT m ()
subst v_x v_y = findLocalRepr v_y >>= \ case
  Nothing -> resumeChildren $ subst v_x v_y
  Just y -> subst' v_x y

subst' :: ( MonadRef m
          , MonadSupply Int m
          , RowMatchable f
          ) => Var m f -> Found m f -> VerseT m ()
subst' v_x y = findRepr v_x <&> (, y) >>= \ case
  (Found _ (Unbound _ k_x _), Found v_y r_y@(Bound y _)) -> do
    writeRepr v_x r_y
    k_x y
    resumeChildren $ subst v_y v_x
  (Found v_x r_x@(Bound x _), Found v_y (Unbound _ k_y _)) -> do
    writeRepr v_y r_x
    k_y x
    resumeChildren $ subst v_x v_y
  (Found _ r_x@(Bound x i_x), Found v_y (Bound y i_y)) -> do
    decrSuspCount
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y r_x
        for_ z $ uncurry unify
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y r_x
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

unifyUncons :: (MonadRef m, MonadSupply Int m, RowMatchable f)
            => (Var m f -> f (Var m f)) -> Var m f
            -> (Var m f -> f (Var m f)) -> Var m f
            -> VerseT m ()
unifyUncons f_x v_xs f_y v_ys = do
  v_zs <- freshVar
  v_xs' <- newVar $ f_x v_zs
  unify v_xs' v_ys
  v_ys' <- newVar $ f_y v_zs
  unify v_xs v_ys'

compare' :: Int -> Int -> Int -> Int -> Ordering
compare' n_x i_x n_y i_y = compare (n_x, i_x) (n_y, i_y)

data Frozen f
  = Unknown
  | Known (f (Frozen f))

deriving instance Show (f (Frozen f)) => Show (Frozen f)

freezeVar :: ( MonadFix m
             , MonadRef m
             , Traversable f
             ) => Var m f -> VerseT m (Frozen f)
freezeVar v = do
  r <- ask'
  lift $ runFreezeT (freezeVar' v) r.heap

type FreezeT f m = ReaderT HeapKey (StateT (IntMap (Frozen f)) m)

runFreezeT :: Monad m => FreezeT f m a -> Maybe Heap -> m a
runFreezeT m = flip evalStateT mempty . runReaderT m

freezeVar' :: ( MonadFix m
              , MonadRef m
              , Traversable f
              ) => Var m f -> FreezeT f m (Frozen f)
freezeVar' v = ask >>= lift . lift . readRepr' v >>= \ case
  Unbound {} -> pure Unknown
  Bound x i -> mfix $ \ x' -> state' (lookupInsert i x') >>= \ case
    Just x' -> pure x'
    Nothing -> Known <$> traverse freezeVar' x

writeLink :: MonadRef m => Var m f -> Var m f -> VerseT m ()
writeLink v_x v_y = do
  x <- readVarState v_x
  lift'
    (\ r -> put' (unVar v_x) r.heap $ Link v_y)
    (\ r -> put' (unVar v_x) r.heap x)

writeRepr :: MonadRef m => Var m f -> Repr m f -> VerseT m ()
writeRepr v x = do
  y <- readVarState v
  lift'
    (\ r -> put' (unVar v) r.heap $ Repr x)
    (\ r -> put' (unVar v) r.heap y)

readRepr' :: MonadRef m => Var m f -> HeapKey -> m (Repr m f)
readRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> readRepr' v h
  Repr x -> pure x

data Found m f = Found !(Var m f) !(Repr m f)

findRepr :: MonadRef m => Var m f -> VerseT m (Found m f)
findRepr v = liftSuccess $ \ r -> findRepr' v r.heap

findRepr' :: MonadRef m => Var m f -> HeapKey -> m (Found m f)
findRepr' v h = readRef (unVar v) <&> lookupVarState h >>= \ case
  Link v -> findRepr' v h
  Repr x -> pure $ Found v x

findLocalRepr :: MonadRef m => Var m f -> VerseT m (Maybe (Found m f))
findLocalRepr v = liftSuccess $ \ r -> loop v r.heap
  where
    loop v h = readRef (unVar v) <&> lookupLocalState h >>= \ case
      Nothing -> pure Nothing
      Just (Link v) -> loop v h
      Just (Repr x) -> pure . Just $ Found v x

readVarState :: MonadRef m => Var m f -> VerseT m (VarState m f)
readVarState v = liftSuccess $ \ r ->
  readRef (unVar v) <&> lookupVarState r.heap

resumeChildren :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
resumeChildren m = do
  r <- ask'
  (xs, n) <- flip resumeAll m =<< lift (readRef r.children)
  lift $ writeRef r.children xs
  n

resumeAll :: (MonadRef m, MonadSupply Int m)
          => [Process m] -> VerseT m ()
          -> VerseT m ([Process m], VerseT m ())
resumeAll xs m = fmap sequence_ . partitionEithers <$> traverse (flip resume m) xs

resume :: (MonadRef m, MonadSupply Int m)
       => Process m -> VerseT m ()
       -> VerseT m (Either (Process m) (VerseT m ()))
resume p@Process {..} m = lift (msplit_ m Env { heap = Just heap, .. }) >>= \ case
  Nothing -> resume' p m
  Just (m', rk') -> VerseT $ \ _ sk fk ek rk r -> do
    m' <- (m' <|>) . (*> m) . fork <$> readRef right
    p <- (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
      Nothing -> writeRef right m' $> Left p
      Just x ->
        Right . writeIVar result . Just . (heap,, m') <$>
        runFreshenT (readRef commit *> freshen x) (Just heap)
    sk p fk (\ r -> rk' r *> ek r) (\ r -> rk' r *> rk r) r

resume' :: (MonadRef m, MonadSupply Int m)
        => Process m -> VerseT m ()
        -> VerseT m (Either (Process m) (VerseT m ()))
resume' p@Process {..} m = do
  m' <- lift $ readRef right
  lift (msplit_ (fork m' *> m) Env { heap = Just heap, .. }) >>= \ case
    Nothing -> pure . Right $ writeIVar result Nothing
    Just (m', rk') -> VerseT $ \ _ sk fk ek rk r -> do
      p <- (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
        Nothing -> writeRef right m' $> Left p
        Just x ->
          Right . writeIVar result . Just . (heap,, m') <$>
          runFreshenT (readRef commit *> freshen x) (Just heap)
      sk p fk (\ r -> rk' r *> ek r) (rk' *> rk) r

newVarRef :: MonadRef m => Var m f -> VerseT m (VarRef m f)
newVarRef = lift . fmap VarRef . newRef

readVarRef :: MonadRef m => VarRef m f -> VerseT m (Var m f)
readVarRef = lift . readRef . unVarRef

writeVarRef :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
            => VarRef m f -> Var m f -> VerseT m ()
writeVarRef (VarRef ref) x = VerseT $ \ _ sk fk ek rk r -> do
  y <- readRef ref
  writeRef ref x
  ck <- readRef r.commit
  writeRef r.commit $ ck >> readRef ref >>= freshen >>= writeRef ref
  let rk' r = writeRef ref y *> writeRef r.commit ck
  sk () fk (\ r -> rk' r *> ek r) (\ r -> rk' r *> rk r) r

fork :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
fork m = liftSuccess (unVerseT m yk sk fk fk rk) >>= reflect_
  where
    yk = Yield $ \ addSusp sk fk _ rk r -> do
      incr r.suspCount
      removeSusp <- addSusp $ \ x -> do
        decrSuspCount
        liftSuccess (sk x fk fk (const $ pure ())) >>= reflect_
      pure $ Just $ (, rk) $ liftFail $ \ r -> do
        removeSusp r
        decr r.suspCount
    sk () fk _ rk _ = pure . Just . (, rk) $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing
    rk _ = pure ()

one :: ( MonadRef m
       , MonadSupply Int m
       , Freshenable a m
       ) => VerseT m a -> VerseT m (IVar m a)
one m = do
  v <- freshIVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    split h r (m >>= writeHeapRef r . Just) >>= readIVar >>= \ case
      Nothing -> empty
      Just (_, x, _) -> writeIVar v x
  pure v

if' :: ( MonadRef m
       , MonadSupply Int m
       , Freshenable a m
       ) => VerseT m a -> (a -> VerseT m b) -> VerseT m b -> VerseT m (IVar m b)
if' p t e = do
  v <- freshIVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    split h r (p >>= writeHeapRef r . Just) >>= readIVar >>= \ case
      Nothing -> e >>= writeIVar v
      Just (_, x, _) -> t x >>= writeIVar v
  pure v

all :: ( MonadRef m
       , MonadSupply Int m
       , Freshenable a m
       ) => VerseT m a -> VerseT m (IVar m [a])
all m = do
  v <- freshIVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    loop h r (m >>= writeHeapRef r . Just) [] >>= writeIVar v
  pure v
  where
    loop h r m xs = split h r m >>= readIVar >>= \ case
      Nothing -> pure $ reverse xs
      Just (h, x, m) -> loop h r m $ x:xs

for :: ( MonadRef m
       , MonadSupply Int m
       , Freshenable a m
       ) => VerseT m a -> (a -> VerseT m b) -> VerseT m (IVar m [b])
for m f = do
  v <- freshIVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    loop h r (m >>= writeHeapRef r . Just) f [] >>= writeIVar v
  pure v
  where
    loop h r m f xs = split h r m >>= readIVar >>= \ case
      Nothing -> pure $ reverse xs
      Just (h, x, m) -> loop h r m f . (:xs) =<< f x

split :: (MonadRef m, MonadSupply Int m, Freshenable a m)
      => Heap -> HeapRef m (Maybe a) -> VerseT m ()
      -> VerseT m (IVar m (Maybe (Heap, a, VerseT m ())))
split heap left m = VerseT $ \ _ sk fk ek rk r -> do
  children <- newRef mempty
  suspCount <- newRef 0
  commit <- newRef $ pure ()
  let splitDepth = r.splitDepth + 1
  msplit_ m Env { heap = Just heap, .. } >>= \ case
    Nothing -> do
      v <- newIVar' Nothing
      sk v fk ek rk r
    Just (m, rk') ->
      (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
        Just x ->
          runFreshenT (join (readRef commit) *> freshen x) (Just heap) >>=
          newIVar' . Just . (heap,, m) >>= \ v ->
          sk v fk (\ r -> rk' r *> ek r) (rk' *> rk) r
        Nothing -> do
          result <- freshIVar'
          right <- newRef m
          modifyRef' r.children (Process {..}:)
          sk result fk (\ r -> rk' r *> ek r) (rk' *> rk) r

class Monad m => Freshenable a m where
  freshen :: a -> FreshenT m a

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Traversable f
         ) => Freshenable (Var m f) m where
  freshen = FreshenT . loop
    where
      loop v = ask >>= lift . lift . findRepr' v >>= \ case
        Found v Unbound {} -> pure v
        Found _ (Bound x i) -> mfix $ \ x' ->
          state' (lookupInsert i $ unsafeCoerce x') >>= \ case
            Just x' -> pure $ unsafeCoerce x'
            Nothing -> lift . lift . newVar' =<< traverse loop x

newtype FreshenT m a = FreshenT
  { unFreshenT :: ReaderT HeapKey (StateT (IntMap Any) m) a
  } deriving ( Functor
             , Applicative
             , Monad
             )

instance MonadTrans FreshenT where
  lift = FreshenT . lift . lift

instance MonadRef m => MonadRef (FreshenT m)

runFreshenT :: Monad m => FreshenT m a -> Maybe Heap -> m a
runFreshenT m = flip evalStateT mempty . runReaderT (unFreshenT m)

msplit_ :: (MonadRef m, MonadSupply Int m)
        => VerseT m () -> Env m -> m (Maybe (VerseT m (), Rollback m))
msplit_ m = unVerseT m yk sk fk fk rk
  where
    yk = Yield $ \ k sk _ fk rk _ ->
      Just . (, rk) . liftFail <$>
      k (\ x -> liftSuccess (sk x fk fk (const $ pure ())) >>= reflect_)
    sk () fk _ rk _ = pure . Just . (, rk) $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing
    rk _ = pure ()

newHeapRef :: MonadRef m => a -> VerseT m (HeapRef m a)
newHeapRef = lift . fmap HeapRef . newRef . singleton

readHeapRef' :: MonadRef m => HeapRef m a -> Heap -> m a
readHeapRef' r h = lookup (Just h) <$> readRef (unHeapRef r)

writeHeapRef :: MonadRef m => HeapRef m a -> a -> VerseT m ()
writeHeapRef ref x = do
  y <- liftSuccess $ \ r -> lookup r.heap <$> readRef (unHeapRef ref)
  lift'
    (\ r -> put' (unHeapRef ref) r.heap x)
    (\ r -> put' (unHeapRef ref) r.heap y)

newHeap :: MonadSupply Int m => VerseT m Heap
newHeap =
  (\ label r -> Heap { label, tail = r.heap, pred = Nothing }) <$>
  lift supply <*>
  ask'

incrSuspCount :: MonadRef m => VerseT m ()
incrSuspCount = lift' (\ r -> incr r.suspCount) (\ r -> decr r.suspCount)

decrSuspCount :: MonadRef m => VerseT m ()
decrSuspCount = lift' (\ r -> decr r.suspCount) (\ r -> incr r.suspCount)

ask' :: VerseT m (Env m)
ask' = VerseT $ \ _ sk fk ek rk r -> sk r fk ek rk r

liftSuccess :: Monad m => (Env m -> m a) -> VerseT m a
liftSuccess f = VerseT $ \ _ sk fk ek rk r -> f r >>= \ x -> sk x fk ek rk r

liftFail :: Applicative m => (Env m -> m ()) -> VerseT m ()
liftFail f = VerseT $ \ _ _ fk _ _ r -> f r *> fk r

lift' :: Monad m => (Env m -> m a) -> (Env m -> m ()) -> VerseT m a
lift' m n = VerseT $ \ _ sk fk ek rk r -> do
  x <- m r
  sk x (\ r -> n r *> fk r) (\ r -> n r *> ek r) rk r

type CopyT = ReaderT (Maybe Heap)

copyProcesses :: (MonadRef m, MonadSupply Int m)
              => Processes m
              -> CopyT m (Processes m)
copyProcesses = traverse copyProcess

copyProcess :: (MonadRef m, MonadSupply Int m)
            => Process m
            -> CopyT m (Process m)
copyProcess Process {..} = do
  heap <- copyHeap heap
  children <- newRef =<< local (const $ Just heap) . copyProcesses =<< readRef children
  suspCount <- newRef =<< readRef suspCount
  commit <- newRef =<< readRef commit
  right <- newRef =<< readRef right
  pure Process {..}

copyHeap :: MonadSupply Int m => Heap -> CopyT m Heap
copyHeap pred = (\ label tail -> Heap { pred = Just pred, .. }) <$> supply <*> ask

lookupIVarState :: HeapKey -> HeapMap (IVarState m a) -> IVarState m a
lookupIVarState k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    case lookup k.tail xs of
      Susp _ -> Susp (const $ pure ())
      x -> x

lookupVarState :: HeapKey -> HeapMap (VarState m a) -> VarState m a
lookupVarState k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    case lookup k.tail xs of
      Repr (Unbound n _ i) -> Repr $ Unbound n (const $ pure ()) i
      x -> x

lookupLocalState :: HeapKey -> HeapMap a -> Maybe a
lookupLocalState k (HeapMap y ys) = case k of
  Nothing -> Just y
  Just k ->
    IntMap.lookup k.label ys <|>
    lookupPred k.pred ys

put' :: MonadRef m => Ref m (HeapMap a) -> HeapKey -> a -> m ()
put' r k = modifyRef' r . insert k

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
lookupPred k xs = k >>= \ k -> IntMap.lookup k.label xs <|> lookupPred k.pred xs

insert :: HeapKey -> a -> HeapMap a -> HeapMap a
insert k v (HeapMap x xs) = case k of
  Nothing -> HeapMap v xs
  Just k -> HeapMap x $ IntMap.insert k.label v xs

reflect_ :: Applicative m
         => Maybe (VerseT m (), Rollback m)
         -> VerseT m ()
reflect_ x = VerseT $ \ yk sk fk ek rk r -> case x of
  Nothing -> ek r
  Just (m, rk') -> sk ()
    (\ r -> unVerseT m yk sk fk fk (const $ pure ()) r)
    (\ r -> unVerseT m yk sk fk (\ r -> rk' r *> ek r) (\ r -> rk' r *> rk r) r)
    (\ r -> rk' r *> rk r)
    r

incr :: (MonadRef m, Num a) => Ref m a -> m ()
incr = flip modifyRef' (+ 1)

decr :: (MonadRef m, Num a) => Ref m a -> m ()
decr = flip modifyRef' $ subtract 1

or :: Maybe a -> a -> a
or = flip fromMaybe
infixr 3 `or`

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
