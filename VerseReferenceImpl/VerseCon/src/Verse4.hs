{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Verse4
  ( VerseT
  , runVerseT
  , Var
  , freshVar
  , newVar
  , readVar
  , writeVar
  , fork
  , one
  , if'
  , all
  , for
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe
import Data.Monoid
import Data.Traversable (traverse)

import Prelude (Int, Num (..), error, reverse, subtract)

import Ref
import Supply

newtype VerseT m a = VerseT
  { unVerseT :: forall r . Yield r m -> Logic r m a
  }

newtype Yield r m = Yield
  { unYield :: forall a . AddSusp m a -> Logic r m a
  }

type AddSusp m a = Susp m a -> m (Fail () m)

type Logic r m a = Env m -> Succeed r m a -> Fail r m -> m r

type Succeed r m a = a -> Env m -> Fail r m -> m r

type Fail r m = Env m -> m r

type Susp m a = a -> VerseT m ()

data Env m = Env
  { heap :: !(Maybe Heap)
  , children :: !(Ref m (Processes m))
  , suspCount :: !(Ref m Int)
  }

type Processes m = [Process m]

data Process m = forall a . Process
  { heap :: !Heap
  , children :: !(Ref m (Processes m))
  , suspCount :: !(Ref m Int)
  , left :: !(HeapRef m (Maybe a))
  , right :: !(Ref m (VerseT m ()))
  , result :: !(Var m (Maybe (Heap, a, VerseT m ())))
  }

data Heap = Heap
  { label :: {-# UNPACK #-} !Int
  , tail :: !(Maybe Heap)
  , pred :: !(Maybe Heap)
  }

newtype HeapRef m a = HeapRef
  { unHeapRef :: Ref m (HeapMap a)
  }

newtype Var m a = Var
  { unVar :: Ref m (HeapMap (VarState m a))
  }

data HeapMap a = HeapMap !a !(IntMap a)

type HeapKey = Maybe Heap

data VarState m a
  = Val !a
  | Susp !(Susp m a)

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ yk r sk -> unVerseT m yk r $ sk . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _ r sk -> sk x r
  f <*> x = VerseT $ \ yk r sk -> unVerseT f yk r $ \ f r -> unVerseT x yk r $ sk . f

instance (MonadRef m, MonadSupply Int m) => Alternative (VerseT m) where
  empty = VerseT $ \ _ r _ fk -> fk r
  x <|> y = VerseT $ \ yk r sk fk -> do
    xs <- readRef r.children
    writeRef r.children =<< runReaderT (copyProcesses xs) r.heap
    unVerseT x yk r sk $ \ r -> do
      writeRef r.children xs
      unVerseT y yk r sk fk

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ yk r sk -> unVerseT x yk r $ \ x r ->
    unVerseT (f x) yk r sk

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ r sk fk -> m >>= \ x -> sk x r fk

runVerseT :: MonadRef m => VerseT m a -> m (Maybe [a])
runVerseT m = do
  children <- newRef mempty
  suspCount <- newRef 0
  unVerseT m yk Env {..} sk fk
  where
    yk = Yield $ \ _ _ _ _ -> pure Nothing
    heap = Nothing
    sk x r fk = readRef r.suspCount >>= \ case
      0 -> fmap (x:) <$> fk r
      _ -> pure Nothing
    fk _ = pure $ Just []

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = lift freshVar'

freshVar' :: MonadRef m => m (Var m a)
freshVar' = fmap Var . newRef . singleton . Susp . const $ pure ()

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . newVar'

newVar' :: MonadRef m => a -> m (Var m a)
newVar' = fmap Var . newRef . singleton . Val

readVar :: MonadRef m => Var m a -> VerseT m a
readVar v = VerseT $ \ yk r sk fk -> readRef (unVar v) <&> lookupVarState r.heap >>= \ case
  Val x -> sk x r fk
  x@(Susp k) -> rotate (unYield yk) r sk fk $ \ k' ->
    put (unVar v) r.heap (Susp $ \ x -> k x *> k' x) $> \ r ->
    put (unVar v) r.heap x
  where
    rotate f a b c d = f d a b c

writeVar :: (MonadRef m, MonadSupply Int m) => Var m a -> a -> VerseT m ()
writeVar v x = readVarState v >>= \ case
  Val _ -> error "writeVar"
  y@(Susp k) -> do
    lift' (\ r -> put (unVar v) r.heap (Val x)) (\ r -> put (unVar v) r.heap y)
    resumeChildren $ writeLocalVar v x
    k x

writeLocalVar :: (MonadRef m, MonadSupply Int m) => Var m a -> a -> VerseT m ()
writeLocalVar v x = readLocalVarState v >>= \ case
  Val _ -> error "writeVar"
  y@(Susp k) -> do
    lift' (\ r -> put (unVar v) r.heap (Val x)) (\ r -> put (unVar v) r.heap y)
    resumeChildren $ writeLocalVar v x
    k x

readVarState :: MonadRef m => Var m a -> VerseT m (VarState m a)
readVarState v = liftSuccess $ \ r -> readRef (unVar v) <&> lookupVarState r.heap

readLocalVarState :: MonadRef m => Var m a -> VerseT m (VarState m a)
readLocalVarState v = liftSuccess $ \ r -> readRef (unVar v) <&> lookupLocalVarState r.heap

resumeChildren :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
resumeChildren m = do
  r <- ask'
  (xs, n) <- lift $ flip resumeAll m =<< readRef r.children
  lift $ writeRef r.children xs
  n

resumeAll :: ( MonadRef m
             , MonadSupply Int m
             ) => [Process m] -> VerseT m () -> m ([Process m], VerseT m ())
resumeAll xs m = fmap sequence_ . partitionEithers <$> traverse (flip resume m) xs

resume :: ( MonadRef m
          , MonadSupply Int m
          ) => Process m -> VerseT m () -> m (Either (Process m) (VerseT m ()))
resume p@Process {..} m = msplit_ m Env { heap = Just heap, .. } >>= \ case
  Nothing -> resume' p m
  Just m' -> do
    m' <- (m' <|>) . (*> m) . fork <$> readRef right
    (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
      Just x -> pure . Right . writeVar result $ Just (heap, x, m')
      Nothing -> writeRef right m' $> Left p

resume' :: ( MonadRef m
           , MonadSupply Int m
           ) => Process m -> VerseT m () -> m (Either (Process m) (VerseT m ()))
resume' p@Process {..} m = do
  m' <- readRef right
  msplit_ (fork m' *> m) Env { heap = Just heap, .. } >>= \ case
    Nothing -> pure . Right $ writeVar result Nothing
    Just m' -> (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
      Just x -> pure . Right . writeVar result $ Just (heap, x, m')
      Nothing -> writeRef right m' $> Left p

fork :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
fork m = liftSuccess (\ r -> unVerseT m yk r sk fk) >>= reflect_
  where
    yk = Yield $ \ addSusp r sk fk -> do
      incr r.suspCount
      removeSusp <- addSusp $ \ x -> do
        liftSuccess (\ r -> sk x r fk) >>= reflect_
        lift' (\ r -> decr r.suspCount) (\ r -> incr r.suspCount)
      pure $ Just $ liftFail $ \ r -> do
        removeSusp r
        decr r.suspCount
    sk () _ fk = pure . Just $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing

one :: (MonadRef m, MonadSupply Int m) => VerseT m a -> VerseT m (Var m a)
one m = do
  v <- freshVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    split h r (m >>= writeHeapRef r . Just) >>= readVar >>= \ case
      Nothing -> empty
      Just (_, x, _) -> writeVar v x
  pure v

if' :: ( MonadRef m
       , MonadSupply Int m
       ) => VerseT m a -> (a -> VerseT m b) -> VerseT m b -> VerseT m (Var m b)
if' p t e = do
  v <- freshVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    split h r (p >>= writeHeapRef r . Just) >>= readVar >>= \ case
      Nothing -> e >>= writeVar v
      Just (_, x, _) -> t x >>= writeVar v
  pure v

all :: (MonadRef m, MonadSupply Int m) => VerseT m a -> VerseT m (Var m [a])
all m = do
  v <- freshVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    loop h r (m >>= writeHeapRef r . Just) [] >>= writeVar v
  pure v
  where
    loop h r m xs = split h r m >>= readVar >>= \ case
      Nothing -> pure $ reverse xs
      Just (h, x, m) -> loop h r m $ x:xs

for :: (MonadRef m, MonadSupply Int m) => VerseT m a -> (a -> VerseT m b) -> VerseT m (Var m [b])
for m f = do
  v <- freshVar
  fork $ do
    h <- newHeap
    r <- newHeapRef Nothing
    loop h r (m >>= writeHeapRef r . Just) f [] >>= writeVar v
  pure v
  where
    loop h r m f xs = split h r m >>= readVar >>= \ case
      Nothing -> pure $ reverse xs
      Just (h, x, m) -> loop h r m f . (:xs) =<< f x

split :: (MonadRef m, MonadSupply Int m) =>
         Heap -> HeapRef m (Maybe a) -> VerseT m () ->
         VerseT m (Var m (Maybe (Heap, a, VerseT m ())))
split heap left m = liftSuccess $ \ r -> do
  children <- newRef mempty
  suspCount <- newRef 0
  msplit_ m Env { heap = Just heap, .. } >>= \ case
    Nothing -> newVar' Nothing
    Just m -> (0 ==) <$> readRef suspCount `andM` readHeapRef' left heap >>= \ case
      Just x -> newVar' $ Just (heap, x, m)
      Nothing -> do
        result <- freshVar'
        right <- newRef m
        modifyRef' r.children (Process {..}:)
        pure result

msplit_ :: (MonadRef m, MonadSupply Int m) => VerseT m () -> Env m -> m (Maybe (VerseT m ()))
msplit_ m r = unVerseT m yk r sk fk
  where
    yk = Yield $ \ k _ sk fk ->
      Just . liftFail <$> k (\ x -> liftSuccess (\ r -> sk x r fk) >>= reflect_)
    sk () _ fk = pure . Just $ liftSuccess fk >>= reflect_
    fk _ = pure Nothing

newHeapRef :: MonadRef m => a -> VerseT m (HeapRef m a)
newHeapRef = lift . fmap HeapRef . newRef . singleton

readHeapRef' :: MonadRef m => HeapRef m a -> Heap -> m a
readHeapRef' r h = lookup (Just h) <$> readRef (unHeapRef r)

writeHeapRef :: MonadRef m => HeapRef m a -> a -> VerseT m ()
writeHeapRef ref x = do
  y <- liftSuccess $ \ r -> lookup r.heap <$> readRef (unHeapRef ref)
  lift' (\ r -> put (unHeapRef ref) r.heap x) (\ r -> put (unHeapRef ref) r.heap y)

newHeap :: MonadSupply Int m => VerseT m Heap
newHeap =
  (\ label r -> Heap { label, tail = r.heap, pred = Nothing }) <$>
  lift supply <*>
  ask'

ask' :: VerseT m (Env m)
ask' = VerseT $ \ _ r sk -> sk r r

liftSuccess :: Monad m => (Env m -> m a) -> VerseT m a
liftSuccess f = VerseT $ \ _ r sk fk -> f r >>= \ x -> sk x r fk

liftFail :: Applicative m => (Env m -> m ()) -> VerseT m ()
liftFail f = VerseT $ \ _ r _ fk -> f r *> fk r

lift' :: Monad m => (Env m -> m a) -> (Env m -> m ()) -> VerseT m a
lift' m n = VerseT $ \ _ r sk fk -> m r >>= \ x -> sk x r $ \ r -> n r *> fk r

type CopyT = ReaderT (Maybe Heap)

copyProcesses :: (MonadRef m, MonadSupply Int m) => Processes m -> CopyT m (Processes m)
copyProcesses = traverse copyProcess

copyProcess :: (MonadRef m, MonadSupply Int m) => Process m -> CopyT m (Process m)
copyProcess Process {..} = do
  heap <- copyHeap heap
  children <- newRef =<< local (const $ Just heap) . copyProcesses  =<< readRef children
  suspCount <- newRef =<< readRef suspCount
  right <- newRef =<< readRef right
  pure Process {..}

copyHeap :: MonadSupply Int m => Heap -> CopyT m Heap
copyHeap pred = (\ label tail -> Heap { pred = Just pred, .. }) <$> supply <*> ask

lookupVarState :: HeapKey -> HeapMap (VarState m a) -> VarState m a
lookupVarState k xs@(HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    case lookup k.tail xs of
      Susp _ -> Susp (const $ pure ())
      x -> x

lookupLocalVarState :: HeapKey -> HeapMap (VarState m a) -> VarState m a
lookupLocalVarState k (HeapMap y ys) = case k of
  Nothing -> y
  Just k ->
    IntMap.lookup k.label ys `or`
    lookupPred k.pred ys `or`
    Susp (const $ pure ())

put :: MonadRef m => Ref m (HeapMap a) -> HeapKey -> a -> m ()
put r k = modifyRef' r . insert k

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

reflect_ :: Alternative m => Maybe (m ()) -> m ()
reflect_ = \ case
  Nothing -> empty
  Just m -> pure () <|> m

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
