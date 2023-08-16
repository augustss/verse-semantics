{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Verse6
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
import Control.Monad.State.Strict

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
import Data.Maybe
import Data.Monoid (mempty)
import Data.Ord
import Data.Traversable (Traversable, traverse)
import Data.Tuple

import GHC.Exts (Any)

import Prelude (Num (..), ($!), error, reverse, subtract)

import Text.Show

import Unsafe.Coerce (unsafeCoerce)

import Match
import Ref
import Supply

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield r m -> Logic r m a
  }

newtype Yield r m = Yield
  { unYield :: forall a . AddSusp m a -> Logic r m a
  }

type AddSusp m a = Susp m a -> m (Fail () m)

type Logic r m a = Env m -> Succeed r m a -> Fail r m -> Empty r m -> Rollback m -> m r

type Succeed r m a = a -> Env m -> Fail r m -> Empty r m -> Rollback m -> m r

type Fail r m = Env m -> m r

type Empty r m = Env m -> m r

type Rollback m = m ()

type Susp m a = a -> VerseT m ()

data Env m = Env
  { heap :: !(Maybe Heap)
  , children :: !(Ref m (Processes m))
  , length :: !(Ref m Int)
  , depth :: {-# UNPACK #-} !Int
  }

type Processes m = [Process m]

data Process m = forall a . Freshenable a m => Process
  { heap :: !Heap
  , children :: !(Ref m (Processes m))
  , length :: !(Ref m Int)
  , depth :: {-# UNPACK #-} !Int
  , left :: !(HeapRef m (Maybe a))
  , right :: !(Ref m (VerseT m ()))
  , result :: !(IVar m (Maybe (Heap, a, VerseT m ())))
  }

data Heap = Heap
  { label :: {-# UNPACK #-} !Int
  , tail :: !(Maybe Heap)
  , pred :: !(Maybe Heap)
  } deriving Show

newtype HeapRef m a = HeapRef
  { unHeapRef :: Ref m (HeapMap a)
  }

newtype IVar m a = IVar
  { unIVar :: Ref m (HeapMap (IVarState m a))
  }

newtype Var m f = Var
  { unVar :: Ref m (HeapMap (VarState m f))
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
  fmap f m = VerseT $ \ yk r sk -> unVerseT m yk r $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ r sk fk ek rk -> sk x r fk ek rk
  f <*> x = VerseT $ \ yk r sk -> unVerseT f yk r $ \ f r -> unVerseT x yk r $ sk . f

instance (MonadRef m, MonadSupply Int m) => Alternative (VerseT m) where
  empty = VerseT $ \ _ r _ _ ek _ -> ek r
  x <|> y = VerseT $ \ yk r sk fk ek rk -> do
    xs <- readRef r.children
    writeRef r.children =<< runReaderT (copyProcesses xs) r.heap
    let f r = writeRef r.children xs
    unVerseT x yk r
      sk
      (\ r -> f r *> unVerseT y yk r sk fk fk (pure ()))
      (\ r -> f r *> unVerseT y yk r sk fk ek rk)
      rk

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk r sk fk ek rk -> unVerseT x yk r
    (\ x r -> unVerseT (f x) yk r sk)
    fk
    ek
    rk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ r sk fk ek rk -> m >>= \ x -> sk x r fk ek rk

runVerseT :: MonadRef m => VerseT m a -> m (Maybe [a])
runVerseT m = do
  children <- newRef mempty
  length <- newRef 0
  unVerseT m yk Env {..} sk fk fk $ pure ()
  where
    yk = Yield $ \ _ _ _ _ _ _ -> pure Nothing
    heap = Nothing
    depth = 0
    sk x r fk _ _ = readRef r.length >>= \ case
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
readIVar v = VerseT $ \ yk r sk fk ek rk ->
  readRef (unIVar v) <&> lookupIVarState r.heap >>= \ case
    Val x -> sk x r fk ek rk
    x@(Susp k) -> rotate (unYield yk) r sk fk ek rk $ \ k' ->
      put' (unIVar v) r.heap (Susp $ \ x -> k x *> k' x) $> \ r ->
      put' (unIVar v) r.heap x
  where
    rotate f x1 x2 x3 x4 x5 x6 = f x6 x1 x2 x3 x4 x5

writeIVar :: (MonadRef m, MonadSupply Int m) => IVar m a -> a -> VerseT m ()
writeIVar v x = readIVarState v >>= \ case
  Val _ -> error "writeIVar"
  y@(Susp k) -> do
    lift' (\ r -> put' (unIVar v) r.heap (Val x)) (\ r -> put' (unIVar v) r.heap y)
    resumeChildren $ writeIVar' v x
    k x

writeIVar' :: (MonadRef m, MonadSupply Int m) => IVar m a -> a -> VerseT m ()
writeIVar' v x = readLocalIVarState v >>= \ case
  Just (Val _) -> error "writeIVar"
  Just y@(Susp k) -> do
    lift' (\ r -> put' (unIVar v) r.heap (Val x)) (\ r -> put' (unIVar v) r.heap y)
    resumeChildren $ writeIVar' v x
    k x
  Nothing -> resumeChildren $ writeIVar' v x

readIVarState :: MonadRef m => IVar m a -> VerseT m (IVarState m a)
readIVarState v = liftSuccess $ \ r ->
  readRef (unIVar v) <&> lookupIVarState r.heap

readLocalIVarState :: MonadRef m => IVar m a -> VerseT m (Maybe (IVarState m a))
readLocalIVarState v = liftSuccess $ \ r ->
  readRef (unIVar v) <&> lookupLocalState r.heap

freshVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m f)
freshVar = liftSuccess $ \ r ->
  fmap Var . newRef . singleton . Repr . Unbound r.depth (const $ pure ()) =<< supply

newVar :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> VerseT m (Var m f)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Int m) => f (Var m f) -> m (Var m f)
newVar' x = fmap Var . newRef . singleton . Repr . Bound x =<< supply

readVar :: MonadRef m => Var m f -> VerseT m (f (Var m f))
readVar v = readVarState v >>= \ case
  Link v -> readVar v
  Repr (Bound x _) -> pure x
  x@(Repr (Unbound n k i)) -> VerseT $ \ yk r sk fk ek rk ->
    rotate (unYield yk) r sk fk ek rk $ \ k' ->
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
    when (n_x < r.depth) incrLength
    writeRepr v_x r_y
    k_x y
    resumeChildren $ subst v_y v_x
  (Found v_x r_x@(Bound x _), Found v_y (Unbound n_y k_y _)) -> do
    r <- ask'
    when (n_y < r.depth) incrLength
    writeRepr v_y r_x
    k_y x
    resumeChildren $ subst v_x v_y
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound n_y k_y i_y)) -> do
    r <- ask'
    case compare' n_x i_x n_y i_y of
      EQ -> pure ()
      LT -> do
        when (n_y < r.depth) incrLength
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeChildren $ subst v_x v_y
      GT -> do
        when (n_x < r.depth) incrLength
        writeLink v_x v_y
        writeRepr v_y $ Unbound n_y (\ x -> k_x x *> k_y x) i_y
        resumeChildren $ subst v_y v_x
  (Found _ r_x@(Bound x i_x), Found v_y (Bound y i_y)) ->
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y r_x
        for_ z $ uncurry unify
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y r_x
        v_zs <- freshVar
        v_xs' <- newVar $ f_x v_zs
        unify v_xs' v_ys
        v_ys' <- newVar $ f_y v_zs
        unify v_xs v_ys'

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
  (Found v_x (Unbound n_x k_x i_x), Found v_y (Unbound _ k_y i_y)) ->
    case i_x == i_y of
      True -> decrLength
      False -> do
        writeRepr v_x $ Unbound n_x (\ x -> k_x x *> k_y x) i_x
        writeLink v_y v_x
        resumeChildren $ subst v_x v_y
  (Found _ r_x@(Bound x i_x), Found v_y (Bound y i_y)) -> do
    decrLength
    when (i_x /= i_y) $ case rowMatch x y of
      Zip Nothing -> empty
      Zip (Just z) -> do
        writeRepr v_y r_x
        for_ z $ uncurry unify
      Uncons f_x v_xs f_y v_ys -> do
        writeRepr v_y r_x
        v_zs <- freshVar
        v_xs' <- newVar $ f_x v_zs
        unify v_xs' v_ys
        v_ys' <- newVar $ f_y v_zs
        unify v_xs v_ys'

compare' :: Int -> Int -> Int -> Int -> Ordering
compare' n_x i_x n_y i_y = compare (n_x, i_x) (n_y, i_y)

incrLength :: MonadRef m => VerseT m ()
incrLength = lift' (\ r -> incr r.length) (\ r -> decr r.length)

decrLength :: MonadRef m => VerseT m ()
decrLength = lift' (\ r -> decr r.length) (\ r -> incr r.length)

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
  Just (m', m'') -> VerseT $ \ _ r sk fk ek rk -> do
    m' <- (m' <|>) . (*> m) . fork <$> readRef right
    p <- (0 ==) <$> readRef length `andM` readHeapRef' left heap >>= \ case
      Just x -> Right . writeIVar result . Just . (heap, , m') <$> freshen' x heap
      Nothing -> writeRef right m' $> Left p
    sk p r fk (\ r -> m'' *> ek r) (m'' *> rk)

resume' :: (MonadRef m, MonadSupply Int m)
        => Process m -> VerseT m ()
        -> VerseT m (Either (Process m) (VerseT m ()))
resume' p@Process {..} m = do
  m' <- lift $ readRef right
  lift (msplit_ (fork m' *> m) Env { heap = Just heap, .. }) >>= \ case
    Nothing -> pure . Right $ writeIVar result Nothing
    Just (m', m'') -> VerseT $ \ _ r sk fk ek rk -> do
      p <- (0 ==) <$> readRef length `andM` readHeapRef' left heap >>= \ case
        Just x -> Right . writeIVar result . Just . (heap, , m') <$> freshen' x heap
        Nothing -> writeRef right m' $> Left p
      sk p r fk (\ r -> m'' *> ek r) (m'' *> rk)

fork :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
fork m = liftSuccess (\ r -> unVerseT m yk r sk fk fk $ pure ()) >>= reflect_
  where
    yk = Yield $ \ addSusp r sk fk _ rk -> do
      incr r.length
      removeSusp <- addSusp $ \ x -> do
        decrLength
        liftSuccess (\ r -> sk x r fk fk $ pure ()) >>= reflect_
      pure $ Just $ (, rk) $ liftFail $ \ r -> do
        removeSusp r
        decr r.length
    sk () _ fk _ rk = pure . Just . (, rk) $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing

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
split heap left m = VerseT $ \ _ r sk fk ek rk -> do
  children <- newRef mempty
  length <- newRef 0
  let depth = r.depth + 1
  msplit_ m Env { heap = Just heap, .. } >>= \ case
    Nothing -> do
      v <- newIVar' Nothing
      sk v r fk ek rk
    Just (m, m') -> (0 ==) <$> readRef length `andM` readHeapRef' left heap >>= \ case
      Just x -> do
        v <- newIVar' . Just . (heap, , m) =<< freshen' x heap
        sk v r fk (\ r -> m' *> ek r) (m' *> rk)
      Nothing -> do
        result <- freshIVar'
        right <- newRef m
        modifyRef' r.children (Process {..}:)
        sk result r fk (\ r -> m' *> ek r) (m' *> rk)

freshen' :: Freshenable a m => a -> Heap -> m a
freshen' x = runFreshenT (freshen x) . Just

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

msplit_ :: ( MonadRef m
           , MonadSupply Int m
           ) => VerseT m () -> Env m -> m (Maybe (VerseT m (), Rollback m))
msplit_ m r = unVerseT m yk r sk fk fk $ pure ()
  where
    yk = Yield $ \ k _ sk fk _ rk ->
      Just . (, rk) . liftFail <$>
      k (\ x -> liftSuccess (\ r -> sk x r fk fk $ pure ()) >>= reflect_)
    sk () _ fk _ rk = pure . Just . (, rk) $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing

newHeapRef :: MonadRef m => a -> VerseT m (HeapRef m a)
newHeapRef = lift . fmap HeapRef . newRef . singleton

readHeapRef' :: MonadRef m => HeapRef m a -> Heap -> m a
readHeapRef' r h = lookup (Just h) <$> readRef (unHeapRef r)

writeHeapRef :: MonadRef m => HeapRef m a -> a -> VerseT m ()
writeHeapRef ref x = do
  y <- liftSuccess $ \ r -> lookup r.heap <$> readRef (unHeapRef ref)
  lift' (\ r -> put' (unHeapRef ref) r.heap x) (\ r -> put' (unHeapRef ref) r.heap y)

newHeap :: MonadSupply Int m => VerseT m Heap
newHeap =
  (\ label r -> Heap { label, tail = r.heap, pred = Nothing }) <$>
  lift supply <*>
  ask'

ask' :: VerseT m (Env m)
ask' = VerseT $ \ _ r sk -> sk r r

liftSuccess :: Monad m => (Env m -> m a) -> VerseT m a
liftSuccess f = VerseT $ \ _ r sk fk ek rk -> f r >>= \ x -> sk x r fk ek rk

liftFail :: Applicative m => (Env m -> m ()) -> VerseT m ()
liftFail f = VerseT $ \ _ r _ fk _ _ -> f r *> fk r

lift' :: Monad m => (Env m -> m a) -> (Env m -> m ()) -> VerseT m a
lift' m n = VerseT $ \ _ r sk fk ek rk -> do
  x <- m r
  sk x r (\ r -> n r *> fk r) (\ r -> n r *> ek r) rk

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
  children <- newRef =<< local (const $ Just heap) . copyProcesses  =<< readRef children
  length <- newRef =<< readRef length
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

reflect_ :: Applicative m => Maybe (VerseT m (), Rollback m) -> VerseT m ()
reflect_ x = VerseT $ \ yk r sk fk ek rk -> case x of
  Nothing -> ek r
  Just (m, m') -> sk () r
    (\ r -> unVerseT m yk r sk fk fk $ pure ())
    (\ r -> unVerseT m yk r sk fk (\ r -> m' *> ek r) (m' *> rk))
    (m' *> rk)

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
