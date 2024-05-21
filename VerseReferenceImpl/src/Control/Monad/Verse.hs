{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Verse
  ( VerseT
  , Level
  , getLevel
  , runVerseT
  , runVerse
  , abort
  , (<?>)
  , Freshenable (..)
  , FreshenT
  , one
  , all'
  , Stream (..)
  , split
  , verify
  , assume
  , fork
  , join'
  , yield
  , Freezable (..)
  , FreezeT
  , freeze'
  , Defaultable (..)
  , defaultVar
  , defaultGVar
  , Var
  , freshVar
  , freshDVar
  , newVar
  , readVar
  , readVarLevel
  , unifyEq
  , Match (..)
  , unify
  , GVar
  , freshGVar
  , freshDGVar
  , newGVar
  , readGVar
  , unifyG
  , unifyEqG
  , VerseRef
  , newVerseRef
  , readVerseRef
  , writeVerseRef
  ) where

import Control.Applicative
import Control.Monad.Extras
import Control.Monad.Fix
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.RWS.Strict
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Wrong

import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Lazy.Extras (lookupInsert)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Strict.Extras qualified as IntMap
import Data.Monoid

import GHC.Exts qualified

import Unsafe.Coerce (unsafeCoerce)

newtype VerseT m a = VerseT (
  forall r
  .  Yield r m
  -> R m
  -> S m
  -> Succeed r m a
  -> Fail r m
  -> Fail r m
  -> Abort r m
  -> Abort r m
  -> m r)

unVerseT
  :: VerseT m a
  -> Yield r m
  -> R m
  -> S m
  -> Succeed r m a
  -> Fail r m
  -> Fail r m
  -> Abort r m
  -> Abort r m
  -> m r
unVerseT (VerseT x) = x

newtype Yield r m = Yield (
  forall a
  .  ((a -> VerseT m ()) -> VerseT m ())
  -> S m
  -> Succeed r m a
  -> Fail r m
  -> Fail r m
  -> Abort r m
  -> Abort r m
  -> m r)

unYield
  :: Yield r m
  -> ((a -> VerseT m ()) -> VerseT m ())
  -> S m
  -> Succeed r m a
  -> Fail r m
  -> Fail r m
  -> Abort r m
  -> Abort r m
  -> m r
unYield (Yield x) = x

type Succeed r m a
  =  Heaps
  -> a
  -> S m
  -> Fail r m
  -> Fail r m
  -> Abort r m
  -> Abort r m
  -> m r

type Fail r m = Heaps -> Abort r m -> m r

type Abort r m = Heaps -> m r

data R m = R
  { level :: {-# UNPACK #-} !Level
  , heaps :: !Heaps
  , latch :: !(Latch m)
  }

type Level = Int

minLevel :: Level
minLevel = 0

data Heaps = Heaps
  { heap :: !(Maybe Heap)
  , verifyHeap :: !(Maybe Heap)
  }

newtype Latch m = Latch (HRef m (LatchState m))

data LatchState m = LatchState
  { suspCount :: {-# UNPACK #-} !Int
  , susp :: !(VerseT m ())
  }

emptyLatchState :: Applicative m => LatchState m
emptyLatchState = LatchState {..}
  where
    suspCount = 0
    susp = pure ()

newLatch :: MonadRef m => VerseT m (Latch m)
newLatch = Latch <$> newHRef emptyLatchState

newLatch'' :: MonadRef m => Heap -> m (Latch m)
newLatch'' = fmap Latch . newHRef'' emptyLatchState

readSuspCount' :: MonadRef m => Latch m -> Heap -> VerseT m Int
readSuspCount' latch = lift . readSuspCount'' latch

readSuspCount'' :: MonadRef m => Latch m -> Heap -> m Int
readSuspCount'' (Latch ref) = fmap (.suspCount) . readHRef'' ref

incrSuspCount :: MonadRef m => Latch m -> VerseT m ()
incrSuspCount (Latch ref) = modifyHRef' ref $ \ LatchState {..} ->
  LatchState { suspCount = suspCount + 1, .. }

decrSuspCount :: MonadRef m => Latch m -> VerseT m ()
decrSuspCount (Latch ref) = do
  LatchState {..} <- readHRef ref
  writeHRef ref $! LatchState { suspCount = suspCount - 1, .. }
  when (suspCount == 1) susp

asksV :: (R m -> a) -> VerseT m a
asksV f = VerseT $ \ _ r@R {..} s sk -> sk heaps (f r) s

asksMV :: Monad m => (R m -> m a) -> VerseT m a
asksMV f = VerseT $ \ _ r@R {..} s sk fk fk' ak ak' -> do
  x <- f r
  sk heaps x s fk fk' ak ak'

localV :: (R m -> R m) -> VerseT m a -> VerseT m a
localV f m = VerseT $ \ yk -> unVerseT m yk . f

getLevel :: VerseT m Level
getLevel = asksV (.level)

getHeap :: VerseT m (Maybe Heap)
getHeap = asksV (.heaps.heap)

getVerifyHeap :: VerseT m (Maybe Heap)
getVerifyHeap = asksV (.heaps.verifyHeap)

getLatch :: VerseT m (Latch m)
getLatch = asksV (.latch)

localLatch :: (Latch m -> Latch m) -> VerseT m a -> VerseT m a
localLatch f = localV $ \ R {..} -> R { latch = f latch, .. }

data S m = S
  { suspend :: !(Suspend m)
  , suspCounts :: !SuspCounts
  , default' :: !(Default m)
  , commit :: !(Commit m)
  , duplicate :: !(Duplicate m)
  , store :: !(Store m)
  }

emptyS :: Applicative m => S m
emptyS = S
  { suspend = const $ pure ()
  , suspCounts = mempty
  , default' = Nothing
  , commit = const $ pure ()
  , duplicate = const $ pure ()
  , store = mempty
  }

type Suspend m = Resume m -> VerseT m ()

type Resume m = VerseT m () -> VerseT m ()

type SuspCounts = IntMap Int

incrSuspCounts :: SuspCounts -> VerseT m ()
incrSuspCounts = modifySuspCounts . plusSuspCounts

incrLevelSuspCount :: Int -> VerseT m ()
incrLevelSuspCount k = modifySuspCounts $ IntMap.alter f k
  where
    f = \ case
      Nothing -> Just 1
      Just (-1) -> Nothing
      Just x -> Just $! x + 1

decrSuspCounts :: SuspCounts -> VerseT m ()
decrSuspCounts = modifySuspCounts . minusSuspCounts

decrLevelSuspCount :: Int -> VerseT m ()
decrLevelSuspCount k = modifySuspCounts $ IntMap.alter f k
  where
    f = \ case
      Nothing -> Just (-1)
      Just 1 -> Nothing
      Just x -> Just $! x - 1

plusSuspCounts :: SuspCounts -> SuspCounts -> SuspCounts
plusSuspCounts = IntMap.mergeWithKey f id id
  where
    f _ x y = case x + y of
      0 -> Nothing
      z -> Just z

minusSuspCounts :: SuspCounts -> SuspCounts -> SuspCounts
minusSuspCounts = IntMap.mergeWithKey f id id
  where
    f _ x y = case x - y of
      0 -> Nothing
      z -> Just z

modifySuspCounts :: (SuspCounts -> SuspCounts) -> VerseT m ()
modifySuspCounts f = modifyV' $ \ S {..} -> S { suspCounts = f suspCounts, .. }

type Default m = Maybe (VerseT m ())

type Commit m = Heap -> VerseT m ()

type Duplicate m = Maybe Heap -> VerseT m ()

type Store m = IntMap (StoreElem m)

data StoreElem m = forall a . Freshenable a m => StoreElem (HRef m a)

stateV :: (S m -> (a, S m)) -> VerseT m a
stateV f = VerseT $ \ _ R {..} s sk -> case f s of
  (a, s) -> sk heaps a s

putV :: S m -> VerseT m ()
putV s = VerseT $ \ _ R {..} _ sk -> sk heaps () s

modifyV' :: (S m -> S m) -> VerseT m ()
modifyV' f = VerseT $ \ _ R {..} s sk -> sk heaps () $! f s

whenSuspended :: Suspend m -> VerseT m ()
whenSuspended f = do
  latch <- getLatch
  modifyV' $ \ S {..} -> S
    { suspend = \ resume -> do
        suspend resume
        f $ resume . localLatch (const latch)
    , ..
    }

whenDefaulted :: VerseT m () -> VerseT m ()
whenDefaulted m = modifyV' $ \ S {..} -> S
  { default' = case default' of
      Nothing -> Just m
      Just m' -> Just $! m' *> m
  , ..
  }

whenCommitted :: Commit m -> VerseT m ()
whenCommitted f = modifyV' $ \ S {..} -> S
  { commit = \ x -> commit x *> f x
  , ..
  }

whenDuplicated :: Duplicate m -> VerseT m ()
whenDuplicated f = modifyV' $ \ S {..} -> S
  { duplicate = \ x -> duplicate x *> f x
  , ..
  }

modifyStore :: (Store m -> Store m) -> VerseT m ()
modifyStore f = modifyV' $ \ S {..} -> S { store = f store, .. }

data Heap = Heap
  { label :: {-# UNPACK #-} !Int
  , pred :: !(Maybe Heap)
  , tail :: !(Maybe Heap)
  }

newHeap :: MonadSupply Int m => VerseT m Heap
newHeap = lift . newHeap' =<< getHeap

newHeap' :: MonadSupply Int m => Maybe Heap -> m Heap
newHeap' tail = do
  label <- supply
  pure $ Heap { label, pred = Nothing, tail }

copyHeap :: MonadSupply Int m => Heap -> VerseT m Heap
copyHeap pred = do
  label <- supply
  tail <- getHeap
  pure $ Heap { label, pred = Just pred, tail }

instance Functor (VerseT m) where
   fmap f m = VerseT $ \ yk r s sk ->
     unVerseT m yk r s $ \ heap -> sk heap . f

instance Applicative (VerseT m) where
   pure x = VerseT $ \ _ R {..} s sk -> sk heaps x s
   f <*> x = VerseT $ \ yk r@R {..} s sk ->
     unVerseT f yk r s $ \ heaps f s ->
     unVerseT x yk R {..} s $ \ heaps -> sk heaps . f

instance Alternative (VerseT m) where
  empty = VerseT $ \ _ R {..} _ _ _ fk' _ -> fk' heaps
  x <|> y = VerseT $ \ yk r@R {..} s sk fk fk' ->
    unVerseT x yk r s sk
    (\ heaps -> dup $ unVerseT y yk R {..} s sk fk fk)
    (\ heaps -> dup $ unVerseT y yk R {..} s sk fk fk')

abort :: VerseT m a
abort = VerseT $ \ _ R {..} _ _ _ _ ak _ -> ak heaps

infixl 3 <?>
(<?>) :: VerseT m a -> VerseT m a -> VerseT m a
x <?> y = VerseT $ \ yk r@R {..} s sk fk fk' ak ak' ->
  dup (unVerseT x yk r s sk fk fk') $ \ heaps ->
  unVerseT y yk R {..} s sk fk fk' ak ak'

instance Monad (VerseT m) where
  m >>= k = VerseT $ \ yk r@R {..} s sk ->
    unVerseT m yk r s $ \ heaps x s ->
    unVerseT (k x) yk R {..} s sk

instance MonadSupply s m => MonadSupply s (VerseT m) where
  supply = lift supply

instance MonadWrong e m => MonadWrong e (VerseT m) where
  wrong = lift . wrong

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
    x <- m
    sk heaps x s fk fk' ak ak'

lift' :: Monad m => (R m -> S m -> m a) -> VerseT m a
lift' f = VerseT $ \ _ r@R {..} s sk fk fk' ak ak' -> do
  x <- f r s
  sk heaps x s fk fk' ak ak'

runVerseT :: (MonadRef m, MonadSupply Int m) => VerseT m a -> m (Maybe [[a]])
runVerseT m = do
  heap <- newHeap' verifyHeap
  latch <- newLatch'' heap
  let
    heaps = Heaps { heap = Just heap, verifyHeap }
    sk heaps x s fk _ ak _ = do
      suspCount <- readSuspCount'' latch heap
      runMaybeT $ do
        guard $ suspCount == 0
        lift $ commitStore' level heap s.store
        prepend x <$> MaybeT (fk heaps runAbort) <*> MaybeT (ak heaps)
  unVerseT (m <* runDefault) yk R {..} emptyS sk runFail runFail runAbort runAbort
  where
    yk = Yield $ \ _ _ _ _ _ _ _ -> pure Nothing
    level = minLevel
    verifyHeap = Nothing
    prepend x xss yss = ((x:) <$> xss) ++ yss

runDefault :: VerseT m ()
runDefault = stateV f >>= \ case
  Nothing -> pure ()
  Just m -> m *> runDefault
  where
    f S {..} = (default', S { default' = Nothing, .. })

commitStore' :: MonadRef m => Int -> Heap -> Store m -> m ()
commitStore' level heap store = for_ store $ commitStoreElem' level heap

commitStoreElem' :: MonadRef m => Int -> Heap -> StoreElem m -> m ()
commitStoreElem' level heap (StoreElem ref) =
  writeVerseRef'' ref heap =<< runFreshen' level heap =<< readVerseRef'' ref heap

runFreshen' :: Freshenable a m => Int -> Heap -> a -> m a
runFreshen' level heap x = runFreshenT (freshen x) FreshenEnv {..}

runFail :: Functor m => Fail (Maybe [[a]]) m
runFail heap ak = fmap ([]:) <$> ak heap

runAbort :: Applicative m => Abort (Maybe [[a]]) m
runAbort = const . pure $ Just []

runVerse :: (forall s . VerseT (IntSupplyT (ST s)) a) -> Maybe [[a]]
runVerse m = runST $ runIntSupplyT $ runVerseT m

class Monad m => Freshenable a m where
  freshen :: a -> FreshenT m a

freshen' :: Freshenable a m => a -> Heap -> VerseT m a
freshen' x heap = do
  level <- getLevel <&> (+ 1)
  lift $ runFreshenT (freshen x) FreshenEnv {..}

newtype FreshenT m a = FreshenT
  ( RWST FreshenEnv Any (IntMap (GHC.Exts.Any, Any)) m a
  ) deriving (Functor, Applicative)

unFreshenT
  :: FreshenT m a
  -> RWST FreshenEnv Any (IntMap (GHC.Exts.Any, Any)) m a
unFreshenT (FreshenT x) = x

data FreshenEnv = FreshenEnv
  { level :: {-# UNPACK #-} !Int
  , heap :: !Heap
  }

runFreshenT :: Monad m => FreshenT m a -> FreshenEnv -> m a
runFreshenT m r = fst <$> evalRWST (unFreshenT m) r mempty

instance Monad m => Freshenable () m where
  freshen = pure

instance (Freshenable a m, Freshenable b m) => Freshenable (a, b) m where
  freshen (a, b) = (,) <$> freshen a <*> freshen b

instance ( Freshenable a m
         , Freshenable b m
         , Freshenable c m
         ) => Freshenable (a, b, c) m where
  freshen (a, b, c) = (,,) <$> freshen a <*> freshen b <*> freshen c

instance Monad m => Freshenable Bool m where
  freshen = pure

instance Monad m => Freshenable Integer m where
  freshen = pure

instance Freshenable a m => Freshenable (Maybe a) m where
  freshen = traverse freshen

instance Freshenable a m => Freshenable [a] m where
  freshen = traverse freshen

instance Freshenable v m => Freshenable (HashMap k v) m where
  freshen = traverse freshen

instance Freshenable (f (g a)) m => Freshenable (Compose f g a) m where
  freshen = fmap Compose . freshen . getCompose

instance (Monad m, Freshenable (f (Fix f)) m) => Freshenable (Fix f) m where
  freshen = fmap Fix . freshen . getFix

one
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m a
one m = split m >>= \ case
  Done -> empty
  Step x _ -> pure x

all'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m [a]
all' = loop . split
  where
    loop m = m >>= \ case
      Done -> pure []
      Step x m -> (x:) <$> loop m

data Stream m a
  = Done
  | Step a (VerseT m (Stream m a))

split
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (Stream m a)
split m = do
  heap <- newHeap
  latch <- lift $ newLatch'' heap
  last <- lift $ newHRef'' Nothing heap
  split' (writeHRef last . Just =<< m) heap latch last

data SplitEnv m = forall a . Freshenable a m => SplitEnv
  { heap :: !Heap
  , latch :: !(Latch m)
  , susp :: !(Stream m a -> VerseT m ())
  , init :: !(Choices m ())
  , last :: !(HRef m (Maybe a))
  , suspend :: !(Suspend m)
  , suspCounts :: !SuspCounts
  , default' :: !(Default m)
  , commit :: !(Commit m)
  , duplicate :: !(Duplicate m)
  , store :: !(Store m)
  }

data Choices m a = Choices
  { fail :: !(VerseT m a)
  , fail' :: !(VerseT m a)
  , abort :: !(VerseT m a)
  , abort' :: !(VerseT m a)
  }

split'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> HRef m (Maybe a)
  -> VerseT m (Stream m a)
split' m heap latch last =
  splitS m heap latch emptyS >>= \ case
    AbortS -> abort
    FailS m_a' -> splitFail heap latch last m_a'
    SucceedS () s init -> splitSucceed heap latch init last s
    YieldS k s succeed init -> splitYield heap latch init last s k succeed

splitFail
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => Heap
  -> Latch m
  -> HRef m (Maybe a)
  -> VerseT m ()
  -> VerseT m (Stream m a)
splitFail heap latch last m_a' =
  pure Done
  <?>
  (do heap <- copyHeap heap
      split' m_a' heap latch last)

splitSucceed
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => Heap
  -> Latch m
  -> Choices m ()
  -> HRef m (Maybe a)
  -> S m
  -> VerseT m (Stream m a)
splitSucceed heap latch init last S {..} =
  case guard (IntMap.null suspCounts) *> default' of
    Just m_d ->
      let default' = Nothing
      in split'' m_d heap latch init last S {..}
    Nothing -> do
      suspCount <- readSuspCount' latch heap
      (guard (suspCount == 0) *>) <$> readHRef' last heap >>= \ case
        Nothing -> yield $ \ susp -> do
          incrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
          ref_env <- newHRef $ Just SplitEnv {..}
          suspend $ resumeSplit ref_env
        Just x ->
          (do commit heap
              commitStore store heap
              x <- freshen' x heap
              pure . Step x $ do
                heap <- copyHeap heap
                split' (do duplicate heap.tail
                           duplicateStore store heap.tail
                           init.fail) heap latch last)
          <?>
          (do heap <- copyHeap heap
              split' init.abort heap latch last)

splitYield
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => Heap
  -> Latch m
  -> Choices m ()
  -> HRef m (Maybe a)
  -> S m
  -> ((b -> VerseT m ()) -> VerseT m ())
  -> (b -> VerseT m ())
  -> VerseT m (Stream m a)
splitYield heap latch init last s@S {..} k succeed =
  case guard (IntMap.null suspCounts) *> default' of
    Nothing -> split'' (k succeed) heap latch init last s
    Just m_d ->
      let default' = Nothing
      in split'' (k succeed *> m_d) heap latch init last S {..}

split''
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> Choices m ()
  -> HRef m (Maybe a)
  -> S m
  -> VerseT m (Stream m a)
split'' m heap latch choices last s =
  splitS m heap latch s >>= \ case
    AbortS ->
      split' choices.abort' heap latch last
    SucceedS () s choices' ->
      let init = appendChoices choices' choices
      in splitSucceed heap latch init last s
    YieldS k s succeed choices' ->
      let init = appendChoices choices' choices
      in splitYield heap latch init last s k succeed
    FailS m_a' ->
      split' choices.fail' heap latch last
      <?>
      (do heap <- copyHeap heap
          split' (appendAbort m_a' choices) heap latch last)

splitS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m a
  -> Heap
  -> Latch m
  -> S m
  -> VerseT m (Split m a)
splitS m heap latch s = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getVerifyHeap
  let r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
  lift $ unVerseT m yieldS r s succeedS failS failS abortS abortS

resumeSplit
  :: (MonadRef m, MonadSupply Int m)
  => HRef m (Maybe (SplitEnv m))
  -> VerseT m ()
  -> VerseT m ()
resumeSplit ref_env m = readHRef ref_env >>= \ case
  Nothing -> pure ()
  Just SplitEnv {..} -> do
    heap <- copyHeap heap
    resumeSplit' ref_env SplitEnv {..} m

resumeSplit'
  :: (MonadRef m, MonadSupply Int m)
  => HRef m (Maybe (SplitEnv m))
  -> SplitEnv m
  -> VerseT m ()
  -> VerseT m ()
resumeSplit' ref_env env@SplitEnv {..} m =
  resumeSplitS m env >>= \ case
    AbortS -> do
      decrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
      writeHRef ref_env Nothing
      susp =<< split' init.abort heap latch last
    FailS m_a -> do
      decrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
      writeHRef ref_env Nothing
      susp =<< split' (init.fail' <?> appendAbortS m_a env) heap latch last
    YieldS k s@S {..} succeed choices ->
      let
        init = appendChoicesS choices env
        suspend resume = env.suspend resume *> s.suspend resume
        suspCounts = plusSuspCounts env.suspCounts s.suspCounts
      in do
        incrSuspCounts . flip IntMap.delete s.suspCounts =<< getLevel
        case guard (IntMap.null suspCounts) *> default' of
          Just m_d -> do
            let default' = Nothing
            writeHRef ref_env $ Just SplitEnv {..}
            s.suspend $ resumeSplit ref_env
            resumeSplit ref_env $ k succeed *> m_d
          Nothing -> do
            writeHRef ref_env $ Just SplitEnv {..}
            s.suspend $ resumeSplit ref_env
            resumeSplit ref_env $ k succeed
    SucceedS () s@S {..} choices ->
      let
        init = appendChoicesS choices env
        suspend resume = env.suspend resume *> s.suspend resume
        suspCounts = plusSuspCounts env.suspCounts s.suspCounts
      in do
        incrSuspCounts . flip IntMap.delete s.suspCounts =<< getLevel
        case guard (IntMap.null suspCounts) *> default' of
          Just m_d -> do
            let default' = Nothing
            writeHRef ref_env $ Just SplitEnv {..}
            s.suspend $ resumeSplit ref_env
            resumeSplit ref_env m_d
          Nothing -> do
            suspCount <- readSuspCount' latch heap
            (guard (suspCount == 0) *>) <$> readHRef' last heap >>= \ case
              Nothing -> do
                writeHRef ref_env $ Just SplitEnv {..}
                s.suspend $ resumeSplit ref_env
              Just x ->
                (do commit heap
                    commitStore store heap
                    x <- freshen' x heap
                    susp . Step x $ do
                      heap <- copyHeap heap
                      split' (do duplicate heap.tail
                                 duplicateStore store heap.tail
                                 init.fail) heap latch last)
                <?>
                (do heap <- copyHeap heap
                    susp =<< split' init.abort heap latch last)

appendChoicesS :: Choices m () -> SplitEnv m -> Choices m ()
appendChoicesS init env = Choices {..}
  where
    fail = appendFailS init.fail env
    fail' = appendFailS' init.fail' env
    abort = appendAbortS init.abort env
    abort' = appendAbortS' init.abort' env

appendFailS :: VerseT m () -> SplitEnv m -> VerseT m ()
appendFailS m_f SplitEnv {..} =
  appendFail (do m_f
                 whenSuspended suspend
                 incrSuspCounts suspCounts) init

appendFailS' :: VerseT m () -> SplitEnv m -> VerseT m ()
appendFailS' m_f' SplitEnv {..} =
  appendFail' (do m_f'
                  whenSuspended suspend
                  incrSuspCounts suspCounts) init

appendAbortS :: VerseT m () -> SplitEnv m -> VerseT m ()
appendAbortS m_a SplitEnv {..} =
  appendAbort (do m_a
                  whenSuspended suspend
                  incrSuspCounts suspCounts) init

appendAbortS' :: VerseT m () -> SplitEnv m -> VerseT m ()
appendAbortS' m_a' SplitEnv {..} =
  appendAbort' (do m_a'
                   whenSuspended suspend
                   incrSuspCounts suspCounts) init

resumeSplitS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m a
  -> SplitEnv m
  -> VerseT m (Split m a)
resumeSplitS m SplitEnv {..} = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getVerifyHeap
  let
    r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
    s = S { suspend = const $ pure (), suspCounts = mempty, .. }
  lift $ unVerseT m yieldS r s succeedS failS failS abortS abortS

verify :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
verify m = do
  heap <- newHeap
  latch <- lift $ newLatch'' heap
  last <- lift $ newHRef'' False heap
  verify' (m *> writeHRef last True) heap latch last

data VerifyEnv m = VerifyEnv
  { heap :: {-# UNPACK #-} !Heap
  , latch :: !(Latch m)
  , susp :: !(VerseT m ())
  , init :: !(Choices m ())
  , last :: !(HRef m Bool)
  , suspend :: !(Suspend m)
  , suspCounts :: !SuspCounts
  , default' :: !(Default m)
  , commit :: !(Commit m)
  , duplicate :: !(Duplicate m)
  }

verify'
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> HRef m Bool
  -> VerseT m ()
verify' m heap latch last =
  verifyS m heap latch emptyS >>= \ case
    AbortS -> pure ()
    FailS m_a' -> verify' m_a' heap latch last
    SucceedS () s init -> verifySucceed heap latch init last s
    YieldS k s succeed init -> verifyYield heap latch init last s k succeed

verify''
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> Choices m ()
  -> HRef m Bool
  -> S m
  -> VerseT m ()
verify'' m heap latch choices last s =
  verifyS m heap latch s >>= \ case
    AbortS ->
      verify' choices.abort heap latch last
    FailS m_a' ->
      let m = choices.fail' <?> appendAbort m_a' choices
      in verify' m heap latch last
    SucceedS () s choices' ->
      let init = appendChoices choices' choices
      in verifySucceed heap latch init last s
    YieldS k s succeed choices' ->
      let init = appendChoices choices' choices
      in verifyYield heap latch init last s k succeed

verifyYield
  :: (MonadRef m, MonadSupply Int m)
  => Heap
  -> Latch m
  -> Choices m ()
  -> HRef m Bool
  -> S m
  -> ((a -> VerseT m ()) -> VerseT m ())
  -> (a -> VerseT m ())
  -> VerseT m ()
verifyYield heap latch init last s@S {..} k succeed =
  case guard (IntMap.null suspCounts) *> default' of
    Nothing -> verify'' (k succeed) heap latch init last s
    Just m_d ->
      let default' = Nothing
      in verify'' (k succeed *> m_d) heap latch init last S {..}

verifySucceed
  :: (MonadRef m, MonadSupply Int m)
  => Heap
  -> Latch m
  -> Choices m ()
  -> HRef m Bool
  -> S m
  -> VerseT m ()
verifySucceed heap latch init last S {..} =
  case guard (IntMap.null suspCounts) *> default' of
    Just m_d ->
      let default' = Nothing
      in verify'' m_d heap latch init last S {..}
    Nothing -> do
      suspCount <- readSuspCount' latch heap
      (suspCount == 0 &&) <$> readHRef' last heap >>= \ case
        True -> do
          commit heap
          verify' (init.fail <?> init.abort) heap latch last
        False -> yield $ \ f -> do
          incrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
          ref_env <- newHRef $ Just VerifyEnv { susp = f (), .. }
          suspend $ resumeVerify ref_env

verifyS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m a
  -> Heap
  -> Latch m
  -> S m
  -> VerseT m (Split m a)
verifyS m heap latch s = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getHeap
  let r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
  lift $ unVerseT m yieldS r s succeedS failS failS abortS abortS

resumeVerify
  :: (MonadRef m, MonadSupply Int m)
  => HRef m (Maybe (VerifyEnv m))
  -> VerseT m ()
  -> VerseT m ()
resumeVerify ref_env m = readHRef ref_env >>= \ case
  Nothing -> pure ()
  Just VerifyEnv {..} -> do
    heap <- copyHeap heap
    resumeVerify' ref_env VerifyEnv {..} m

resumeVerify'
  :: (MonadRef m, MonadSupply Int m)
  => HRef m (Maybe (VerifyEnv m))
  -> VerifyEnv m
  -> VerseT m ()
  -> VerseT m ()
resumeVerify' ref_env env@VerifyEnv {..} m =
  resumeVerifyS m env >>= \ case
    AbortS -> do
      decrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
      writeHRef ref_env Nothing
      verify' init.abort heap latch last
      susp
    FailS m_a -> do
      decrSuspCounts . flip IntMap.delete suspCounts =<< getLevel
      writeHRef ref_env Nothing
      verify' (init.fail' <?> appendAbortV m_a env) heap latch last
      susp
    YieldS k s@S {..} succeed choices ->
      let
        init = appendChoicesV choices env
        suspend resume = env.suspend resume *> s.suspend resume
        suspCounts = plusSuspCounts env.suspCounts s.suspCounts
      in do
        incrSuspCounts . flip IntMap.delete s.suspCounts =<< getLevel
        case guard (IntMap.null suspCounts) *> default' of
          Just m_d -> do
            let default' = Nothing
            writeHRef ref_env $ Just VerifyEnv {..}
            s.suspend $ resumeVerify ref_env
            resumeVerify ref_env $ k succeed *> m_d
          Nothing -> do
            writeHRef ref_env $ Just VerifyEnv {..}
            s.suspend $ resumeVerify ref_env
            resumeVerify ref_env $ k succeed
    SucceedS () s@S {..} choices ->
      let
        init = appendChoicesV choices env
        suspend resume = env.suspend resume *> s.suspend resume
        suspCounts = plusSuspCounts env.suspCounts s.suspCounts
      in do
        incrSuspCounts . flip IntMap.delete s.suspCounts =<< getLevel
        case guard (IntMap.null suspCounts) *> default' of
          Just m_d -> do
            let default' = Nothing
            writeHRef ref_env $ Just VerifyEnv {..}
            s.suspend $ resumeVerify ref_env
            resumeVerify ref_env m_d
          Nothing -> do
            suspCount <- readSuspCount' latch heap
            (suspCount == 0 &&) <$> readHRef' last heap >>= \ case
              False -> do
                writeHRef ref_env $ Just VerifyEnv {..}
                s.suspend $ resumeVerify ref_env
              True -> do
                commit heap
                verify' (init.fail <?> init.abort) heap latch last
                susp

appendChoicesV :: Choices m () -> VerifyEnv m -> Choices m ()
appendChoicesV init env = Choices {..}
  where
    fail = appendFailV init.fail env
    fail' = appendFailV' init.fail' env
    abort = appendAbortV init.abort env
    abort' = appendAbortV' init.abort' env

appendFailV :: VerseT m () -> VerifyEnv m -> VerseT m ()
appendFailV m_f VerifyEnv {..} =
  appendFail (do m_f
                 whenSuspended suspend
                 incrSuspCounts suspCounts) init

appendFailV' :: VerseT m () -> VerifyEnv m -> VerseT m ()
appendFailV' m_f' VerifyEnv {..} =
  appendFail' (do m_f'
                  whenSuspended suspend
                  incrSuspCounts suspCounts) init

appendAbortV :: VerseT m () -> VerifyEnv m -> VerseT m ()
appendAbortV m_a VerifyEnv {..} =
  appendAbort (do m_a
                  whenSuspended suspend
                  incrSuspCounts suspCounts) init

appendAbortV' :: VerseT m () -> VerifyEnv m -> VerseT m ()
appendAbortV' m_a' VerifyEnv {..} =
  appendAbort' (do m_a'
                   whenSuspended suspend
                   incrSuspCounts suspCounts) init

resumeVerifyS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m a
  -> VerifyEnv m
  -> VerseT m (Split m a)
resumeVerifyS m VerifyEnv {..} = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getHeap
  let
    r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
    s = S { suspend = const $ pure (), suspCounts = mempty, store = mempty, .. }
  lift $ unVerseT m yieldS r s succeedS failS failS abortS abortS

appendChoices :: Choices m () -> Choices m () -> Choices m ()
appendChoices init init' = Choices {..}
  where
    fail = appendFail init.fail init'
    fail' = appendFail' init.fail' init'
    abort = appendAbort init.abort init'
    abort' = appendAbort' init.abort' init'

appendFail :: VerseT m () -> Choices m () -> VerseT m ()
appendFail m_f Choices {..} = m_f <|> fail

appendFail' :: VerseT m () -> Choices m () -> VerseT m ()
appendFail' m_f' Choices {..} = alt m_f' fail fail'

appendAbort :: VerseT m () -> Choices m () -> VerseT m ()
appendAbort m_a Choices {..} = alt m_a fail fail' <?> abort

appendAbort' :: VerseT m () -> Choices m () -> VerseT m ()
appendAbort' m_a' Choices {..} = alt m_a' fail fail' <?> abort'

assume
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m a
assume m = split m >>= \ case
  Done -> abort
  Step x _ -> pure x

fork :: MonadRef m => VerseT m () -> VerseT m ()
fork m = forkS m >>= \ case
  AbortS ->
    abort
  FailS m_a' ->
    empty' $ fork m_a'
  SucceedS x s choices ->
    alt' (x <$ putV s) (forkChoices choices)
  YieldS k s succeed choices ->
    alt'
    (do putV s
        latch <- getLatch
        incrSuspCount latch
        k $ \ x -> do
          fork $ succeed x
          decrSuspCount latch)
    (forkChoices choices)

forkChoices :: MonadRef m => Choices m () -> Choices m ()
forkChoices Choices {..} = Choices
  { fail = fork fail
  , fail' = fork fail'
  , abort = fork abort
  , abort' = fork abort'
  }

forkS :: MonadRef m => VerseT m a -> VerseT m (Split m a)
forkS m =
  lift' $ \ r s -> unVerseT m yieldS r s succeedS failS failS abortS abortS

join' :: MonadRef m => VerseT m a -> VerseT m a
join' m = do
  latch@(Latch ref) <- newLatch
  x <- localLatch (const latch) m
  LatchState {..} <- readHRef ref
  if suspCount == 0
    then pure x
    else yield $ \ f -> writeHRef ref $! LatchState { susp = f x, .. }

data Split m a
  = AbortS
  | FailS
    !(VerseT m a)
  | SucceedS
    !a
    !(S m)
    !(Choices m a)
  | forall b . YieldS
    !((b -> VerseT m ()) -> VerseT m ())
    !(S m)
    !(b -> VerseT m a)
    !(Choices m a)

yieldS :: MonadRef m => Yield (Split m a) m
yieldS = Yield $ \ k s sk fk fk' ak ak' -> pure $
  let
    fail = reflectFailS fk
    fail' = reflectFailS fk'
    abort = reflectAbortS ak
    abort' = reflectAbortS ak'
  in YieldS k s (reflectSucceedS sk) Choices {..}

succeedS :: Monad m => Succeed (Split m a) m a
succeedS _ x s fk fk' ak ak' = pure $ SucceedS x s Choices {..}
  where
    fail = reflectFailS fk
    fail' = reflectFailS fk'
    abort = reflectAbortS ak
    abort' = reflectAbortS ak'

failS :: Monad m => Fail (Split m a) m
failS _ ak = pure . FailS $ asksMV (\ R {..} -> ak heaps) >>= reflectS

abortS :: Applicative m => Abort (Split m a) m
abortS = const $ pure AbortS

reflectS :: Monad m => Split m a -> VerseT m a
reflectS = \ case
  AbortS ->
    abort
  FailS m_a' ->
    empty' m_a'
  SucceedS x s choices ->
    alt' (x <$ putV s) choices
  YieldS k s succeed choices ->
    alt'
    (putV s *> yield (\ f -> k $ succeed >=> f))
    choices

reflectSucceedS :: Monad m => Succeed (Split m b) m a -> a -> VerseT m b
reflectSucceedS sk x =
  lift' (\ R {..} s -> sk heaps x s failS failS abortS abortS) >>= reflectS

reflectFailS :: Monad m => Fail (Split m a) m -> VerseT m a
reflectFailS fk =
  asksMV (\ R {..} -> fk heaps abortS) >>= reflectS

reflectAbortS :: Monad m => Abort (Split m a) m -> VerseT m a
reflectAbortS ak =
  asksMV (\ R {..} -> ak heaps) >>= reflectS

alt :: VerseT m a -> VerseT m a -> VerseT m a -> VerseT m a
alt m m_f m_f' = VerseT $ \ yk r@R {..} s sk fk fk' ->
  unVerseT m yk r s sk
  (\ heaps -> dup $ unVerseT m_f yk R {..} s sk fk fk)
  (\ heaps -> dup $ unVerseT m_f' yk R {..} s sk fk fk')

alt' :: VerseT m a -> Choices m a -> VerseT m a
alt' m Choices {..} = VerseT $ \ yk r@R {..} s sk fk fk' ak ak' ->
  unVerseT m yk r s sk
  (\ heaps -> dup (unVerseT fail yk R {..} s sk fk fk))
  (\ heaps -> dup (unVerseT fail' yk R {..} s sk fk fk'))
  (\ heaps -> unVerseT abort yk R {..} s sk fk fk' ak ak')
  (\ heaps -> unVerseT abort' yk R {..} s sk fk fk' ak ak')

empty' :: VerseT m a -> VerseT m a
empty' m_a' = VerseT $ \ yk R {..} s sk fk fk' ak ak' ->
  fk' heaps $ \ heaps -> unVerseT m_a' yk R {..} s sk fk fk' ak ak'

yield :: ((a -> VerseT m ()) -> VerseT m ()) -> VerseT m a
yield f = VerseT $ \ yk _ -> unYield yk f

class Monad m => Freezable a b m | a -> b where
  freeze :: a -> FreezeT m b

freeze' :: Freezable a b m => a -> VerseT m b
freeze' x = lift . runFreezeT (freeze x) =<< getHeap

newtype FreezeT m a = FreezeT
  ( ReaderT (Maybe Heap) (StateT (IntMap GHC.Exts.Any) m) a
  ) deriving (Functor, Applicative, Monad)

unFreezeT
  :: FreezeT m a
  -> ReaderT (Maybe Heap) (StateT (IntMap GHC.Exts.Any) m) a
unFreezeT (FreezeT x) = x

runFreezeT :: Monad m => FreezeT m a -> Maybe Heap -> m a
runFreezeT m heap = evalStateT (runReaderT (unFreezeT m) heap) mempty

instance Monad m => Freezable () () m where
  freeze = pure

instance (Freezable a c m, Freezable b d m) => Freezable (a, b) (c, d) m where
  freeze (x, y) = (,) <$> freeze x <*> freeze y

instance Monad m => Freezable Bool Bool m where
  freeze = pure

instance Monad m => Freezable Integer Integer m where
  freeze = pure

instance Freezable a b m => Freezable (Maybe a) (Maybe b) m where
  freeze = traverse freeze

instance Freezable a b m => Freezable [a] [b] m where
  freeze = traverse freeze

instance Freezable a b m => Freezable (HashMap k a) (HashMap k b) m where
  freeze = traverse freeze

instance ( Monad m
         , Freezable (f (Fix f)) (g (Fix g)) m
         ) => Freezable (Fix f) (Fix g) m where
  freeze = fmap Fix . freeze . getFix

instance Freezable (a (b c)) (d (e f)) m =>
         Freezable (Compose a b c) (Compose d e f) m where
  freeze = fmap Compose . freeze . getCompose

class Monad m => Defaultable a m where
  defaultVars :: a -> VerseT m ()

defaultVar
  :: (MonadRef m, MonadSupply Int m, Defaultable a m)
  => Var m a -> a -> VerseT m ()
defaultVar (Var ref) binding = readVarState ref >>= \ case
  Link var -> defaultVar var binding
  Bound bound -> defaultVars bound.binding
  Unbound unbound -> do
    label <- supply
    let bound = MkBound {..}
    var <- lift $ newVar'' bound
    writeVarState ref $ Link var
    unbound.substSusp var $ BoundS bound

defaultGVar
  :: (MonadRef m, MonadSupply Int m, Defaultable a m)
  => GVar m a -> a -> VerseT m ()
defaultGVar = defaultGVar' . unGVar

defaultGVar'
  :: (MonadRef m, MonadSupply Int m, Defaultable a m)
  => Var m a -> a -> VerseT m ()
defaultGVar' (Var ref) binding = readVarState ref >>= \ case
  Link var -> defaultGVar' var binding
  Bound bound -> defaultVars bound.binding
  Unbound unbound -> do
    label <- supply
    let bound = MkBound {..}
    var <- lift $ newVar'' bound
    writeVarStateG ref $ Link var
    unbound.substSusp var $ BoundS bound

instance Monad m => Defaultable () m where
  defaultVars = const $ pure ()

instance ( Defaultable a m
         , Defaultable b m
         ) => Defaultable (a, b) m where
  defaultVars (a, b) = do
    defaultVars a
    defaultVars b

instance Monad m => Defaultable Integer m where
  defaultVars = const $ pure ()

newtype Var m a = Var (HRef m (VarState m a))

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Freshenable a m
         ) => Freshenable (Var m a) m where
  freshen var@(Var ref) = FreshenT $ do
    FreshenEnv {..} <- ask
    lift (readVarState'' ref heap) >>= \ case
      Link var -> tell (Any True) *> unFreshenT (freshen var)
      Bound bound -> unFreshenT $ freshenBound var bound
      Unbound unbound -> unFreshenT (freshenUnbound ref unbound) $> var

freshenBound
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => Var m a
  -> Bound a
  -> FreshenT m (Var m a)
freshenBound var bound = FreshenT . fmap fst . mfix $ \ ~(var', _) ->
  state' (lookupInsert bound.label (unsafeCoerce var', Any True)) >>= \ case
    Just (unsafeCoerce -> var', changed) -> do
      tell changed
      pure (var', changed)
    Nothing -> do
      (binding, Any changed) <- listen . unFreshenT $ freshen bound.binding
      if changed
        then lift $ newVar' binding <&> (, Any True)
        else do
          modify' $ IntMap.insert bound.label (unsafeCoerce var, Any False)
          pure (var, Any False)

freshenUnbound
  :: MonadRef m
  => HRef m (VarState m a)
  -> Unbound m a
  -> FreshenT m ()
freshenUnbound (HRef ref) unbound = FreshenT $ do
  FreshenEnv {..} <- ask
  lift . when (unbound.level == level) $
    let
      unbound' = Unbound MkUnbound
        { label = unbound.label
        , level = level - 1
        , substSusp = emptySubstSusp
        }
    in modifyRef' ref $ insertLocalHeap heap.tail unbound' . deleteLocalHeap' heap

instance ( MonadFix m
         , MonadRef m
         , Freezable a b m
         ) => Freezable (Var m a) (Maybe b) m where
  freeze (Var ref) = FreezeT $ do
    heap <- ask
    lift (lift $ readVarState' ref heap) >>= \ case
      Link var -> unFreezeT $ freeze var
      Unbound _ -> pure Nothing
      Bound bound -> mfix $ \ binding ->
        state' (lookupInsert bound.label $ unsafeCoerce binding) >>= \ case
          Just (unsafeCoerce -> binding) -> pure binding
          Nothing -> Just <$> unFreezeT (freeze bound.binding)

data VarState m a
  = Link !(Var m a)
  | Unbound !(Unbound m a)
  | Bound !(Bound a)

data Unbound m a = MkUnbound
  { label :: {-# UNPACK #-} !Int
  , level :: {-# UNPACK #-} !Int
  , substSusp :: !(Var m a -> Subst m a -> VerseT m ())
  }

emptySubstSusp :: Var m a -> Subst m a -> VerseT m ()
emptySubstSusp _ _ = pure ()

compareUnbound :: Unbound m a -> Unbound m a -> Ordering
compareUnbound x y = compare (x.level, x.label) (y.level, y.label)

data Bound a = MkBound
  { label :: {-# UNPACK #-} !Int
  , binding :: !a
  }

freshVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m a)
freshVar = do
  level <- getLevel
  heap <- getHeap
  lift $ freshVar' level heap

freshVar' :: (MonadRef m, MonadSupply Int m) => Int -> Maybe Heap -> m (Var m a)
freshVar' level heap = do
  label <- supply
  let substSusp = emptySubstSusp
  Var <$> newHRef' (Unbound MkUnbound {..}) heap

freshDVar
  :: (MonadRef m, MonadSupply Int m, Defaultable a m)
  => a -> VerseT m (Var m a)
freshDVar binding = do
  label <- supply
  level <- getLevel
  let substSusp = emptySubstSusp
  var <- Var <$> newHRef (Unbound MkUnbound {..})
  whenDefaulted $ defaultVar var binding
  pure var

newVar :: (MonadRef m, MonadSupply Int m) => a -> VerseT m (Var m a)
newVar = lift . newVar'

newVar' :: (MonadRef m, MonadSupply Int m) => a -> m (Var m a)
newVar' binding = do
  label <- supply
  newVar'' MkBound {..}

newVar'' :: MonadRef m => Bound a -> m (Var m a)
newVar'' bound = Var <$> newHRef' (Bound bound) Nothing

readVar :: MonadRef m => Var m a -> VerseT m a
readVar = fmap ((.binding) . snd) . readBound

readVarLevel :: MonadRef m => Var m a -> VerseT m Level
readVarLevel var = readRoot var <&> \ case
  (_, UnboundR unbound) -> unbound.level
  _ -> minLevel

unifyEq
  :: (MonadRef m, Eq a)
  => Var m a -> Var m a -> VerseT m ()
unifyEq = unify $ \ x y -> guard (x == y) $> (SEQ, pure ())

data Match = SEQ | LE | GE

unify
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Var m a -> VerseT m ()
unify f var1 var2 = (,) <$> readRoot var1 <*> readRoot var2 >>= \ case
  ((var1, UnboundR unbound1), (var2, UnboundR unbound2)) ->
    case compareUnbound unbound1 unbound2 of
      EQ -> pure ()
      LT -> unifyUnboundUnbound f var1 var2 unbound2
      GT -> unifyUnboundUnbound f var2 var1 unbound1
  ((var1, UnboundR unbound1), (var2, BoundR bound2)) ->
    unifyBoundUnbound f var2 bound2 var1 unbound1
  ((var1, BoundR bound1), (var2, UnboundR unbound2)) ->
    unifyBoundUnbound f var1 bound1 var2 unbound2
  ((var1, BoundR bound1), (var2, BoundR bound2)) ->
    unifyBoundBound f var1 bound1 var2 bound2

unifyUnboundUnbound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Var m a -> Unbound m a -> VerseT m ()
unifyUnboundUnbound f var1 var2@(Var ref2) unbound2 = do
  writeVarState ref2 $ Link var1
  unbound2.substSusp var1 LinkS
  whenM ((unbound2.level /=) <$> getLevel) $ do
    incrSuspCount =<< getLatch
    whenSuspended $ \ resume ->
      fork $ readSubst var2 >>= \ (var2, subst2) -> resume $ do
        unifySubst f var2 subst2 var1
        decrSuspCount =<< getLatch

unifyBoundUnbound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Unbound m a -> VerseT m ()
unifyBoundUnbound f var1 bound1 var2@(Var ref2) unbound2 = do
  writeVarState ref2 $ Link var1
  unbound2.substSusp var1 $ BoundS bound1
  whenM ((unbound2.level /=) <$> getLevel) $ do
    incrSuspCount =<< getLatch
    whenSuspended $ \ resume ->
      fork $ readBound var2 >>= \ (var2, bound2) -> resume $ do
        unifyBoundBound f var1 bound1 var2 bound2
        decrSuspCount =<< getLatch

unifyBoundBound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Bound a -> VerseT m ()
unifyBoundBound f var1@(Var ref1) bound1 var2@(Var ref2) bound2 =
  when (bound1.label /= bound2.label) $
    f bound1.binding bound2.binding >>= \ case
      (SEQ, m) -> writeVarState ref2 (Link var1) *> m
      (LE, m) -> (writeVerifyVarState ref2 (Link var1) *> m) <?> empty
      (GE, m) -> (writeVerifyVarState ref1 (Link var2) *> m) <?> empty

unifySubst
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Subst m a -> Var m a -> VerseT m ()
unifySubst f var1 subst1 var2 = case subst1 of
  LinkS -> unify f var1 var2
  BoundS bound1 -> unifyBound f var1 bound1 var2

unifyBound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> VerseT m ()
unifyBound f var1 bound1 var2 = readRoot var2 >>= \ case
  (var2, UnboundR unbound2) -> unifyBoundUnbound f var1 bound1 var2 unbound2
  (var2, BoundR bound2) -> unifyBoundBound f var1 bound1 var2 bound2

data Root m a
  = UnboundR !(Unbound m a)
  | BoundR !(Bound a)

readRoot :: MonadRef m => Var m a -> VerseT m (Var m a, Root m a)
readRoot var = lift . readRoot' var =<< getHeap

readRoot' :: MonadRef m => Var m a -> Maybe Heap -> m (Var m a, Root m a)
readRoot' var@(Var ref) heap = readVarState' ref heap >>= \ case
  Link var -> readRoot' var heap
  Bound bound -> pure (var, BoundR bound)
  Unbound unbound -> pure (var, UnboundR unbound)

data Subst m a
  = LinkS
  | BoundS !(Bound a)

readSubst :: MonadRef m => Var m a -> VerseT m (Var m a, Subst m a)
readSubst var@(Var ref) = readVarState ref >>= \ case
  Link var -> pure (var, LinkS)
  Bound bound -> pure (var, BoundS bound)
  Unbound unbound -> yield $ \ k -> do
    level <- getLevel
    if unbound.level == level then
      writeVarState ref $ Unbound unbound
        { substSusp = \ var subst -> do
            unbound.substSusp var subst
            k (var, subst)
        }
    else do
      incrLevelSuspCount unbound.level
      k <- once $ \ x -> do
        decrLevelSuspCount unbound.level
        k x
      writeVarState ref $ Unbound unbound
        { substSusp = \ var subst -> do
            unbound.substSusp var subst
            k (var, subst)
        }
      whenSuspended $ \ resume -> fork $ do
        (var, subst) <- readSubst var
        resume $ do
          writeVarState ref $ Link var
          k (var, subst)

readBound :: MonadRef m => Var m a -> VerseT m (Var m a, Bound a)
readBound var = readSubst var >>= \ case
  (var, LinkS) -> readBound var
  (var, BoundS bound) -> pure (var, bound)

once :: MonadRef m => (a -> VerseT m ()) -> VerseT m (a -> VerseT m ())
once f = do
  ref <- newHRef True
  pure $ \ x -> whenM (readHRef ref) $ do
    writeHRef ref False
    f x

readVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> VerseT m (VarState m a)
readVarState ref = lift . readVarState' ref =<< getHeap

readVarState'
  :: MonadRef m
  => HRef m (VarState m a)
  -> Maybe Heap
  -> m (VarState m a)
readVarState' (HRef ref) heap = findVarState heap <$> readRef ref

readVarState''
  :: MonadRef m
  => HRef m (VarState m a)
  -> Heap
  -> m (VarState m a)
readVarState'' (HRef ref) heap = findVarState' heap <$> readRef ref

writeVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> VarState m a
  -> VerseT m ()
writeVarState (HRef ref) x = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
  y <- findVarState heaps.heap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.heap x)
  sk heaps () s
    (\ heaps@Heaps {..} ak -> do
        x <- findLocalHeap heap <$> readRef ref
        modifyRef' ref (insertLocalHeap heap y)
        fk heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak heaps)
    (\ heaps@Heaps {..} ak' -> do
        x <- findLocalHeap heap <$> readRef ref
        modifyRef' ref $ insertLocalHeap heap y
        fk' heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak' heaps)
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap y
        ak heaps)
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap y
        ak' heaps)

writeVerifyVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> VarState m a
  -> VerseT m ()
writeVerifyVarState (HRef ref) x = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
  y <- findVarState heaps.verifyHeap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.verifyHeap x)
  sk heaps () s fk fk'
    (\ heaps@Heaps {..} -> do
        modifyRef' ref (insertLocalHeap verifyHeap y)
        ak heaps)
    (\ heaps@Heaps {..} -> do
        modifyRef' ref (insertLocalHeap verifyHeap y)
        ak' heaps)

newtype GVar m a = GVar (Var m a)

unGVar :: GVar m a -> Var m a
unGVar (GVar x) = x

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Freshenable a m
         ) => Freshenable (GVar m a) m where
  freshen = fmap GVar . freshen . unGVar

instance ( MonadFix m
         , MonadRef m
         , Freezable a b m
         ) => Freezable (GVar m a) (Maybe b) m where
  freeze = freeze . unGVar

freshGVar :: (MonadRef m, MonadSupply Int m) => VerseT m (GVar m a)
freshGVar = GVar <$> freshVar

freshDGVar
  :: (MonadRef m, MonadSupply Int m, Defaultable a m)
  => a -> VerseT m (GVar m a)
freshDGVar binding = do
  label <- supply
  level <- getLevel
  let substSusp = emptySubstSusp
  var <- Var <$> newHRef (Unbound MkUnbound {..})
  whenDefaulted $ defaultGVar' var binding
  pure $ GVar var

newGVar :: (MonadRef m, MonadSupply Int m) => a -> VerseT m (GVar m a)
newGVar = fmap GVar . newVar

readGVar :: MonadRef m => GVar m a -> VerseT m a
readGVar = readVar . unGVar

unifyEqG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m, Eq a)
  => GVar m a -> GVar m a -> VerseT m ()
unifyEqG = unifyG $ \ x y -> guard (x == y) $> pure ()

unifyG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> GVar m a -> GVar m a -> VerseT m ()
unifyG f (GVar var1) (GVar var2) = unifyG' f var1 var2

unifyG'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Var m a -> VerseT m ()
unifyG' f var1 var2 = (,) <$> readRoot var1 <*> readRoot var2 >>= \ case
  ((var1, UnboundR unbound1), (var2, UnboundR unbound2)) ->
    case compareUnbound unbound1 unbound2 of
      EQ -> pure ()
      LT -> unifyUnboundUnboundG f var1 var2 unbound2
      GT -> unifyUnboundUnboundG f var2 var1 unbound1
  ((var1, UnboundR unbound1), (var2, BoundR bound2)) ->
    unifyBoundUnboundG f var2 bound2 var1 unbound1
  ((var1, BoundR bound1), (var2, UnboundR unbound2)) ->
    unifyBoundUnboundG f var1 bound1 var2 unbound2
  ((_, BoundR bound1), (var2, BoundR bound2)) ->
    unifyBoundBoundG f var1 bound1 var2 bound2

unifyUnboundUnboundG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Var m a -> Unbound m a -> VerseT m ()
unifyUnboundUnboundG f var1 var2@(Var ref2) unbound2 = do
  writeVarStateG ref2 $ Link var1
  unbound2.substSusp var1 LinkS
  whenM ((unbound2.level /=) <$> getLevel) $ do
    whenSuspended $ \ resume ->
      fork $ readSubst var2 >>= \ (var2, subst2) -> resume $
        unifySubstG f var2 subst2 var1
    whenCommitted . const $
      unifyG' f var1 var2
    whenDuplicated $
      writeVarStateG ref2 <=< lift . readVarState' ref2

unifyBoundUnboundG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Unbound m a -> VerseT m ()
unifyBoundUnboundG f var1 bound1 var2@(Var ref2) unbound2 = do
  writeVarStateG ref2 $ Link var1
  unbound2.substSusp var1 $ BoundS bound1
  whenM ((unbound2.level /=) <$> getLevel) $ do
    whenSuspended $ \ resume ->
      fork $ readBound var2 >>= \ (var2, bound2) -> resume $
        unifyBoundBoundG f var1 bound1 var2 bound2
    whenCommitted $
      unifyG' f var2 <=< newVar <=< freshen' bound1.binding
    whenDuplicated $
      writeVarStateG ref2 <=< lift . readVarState' ref2

unifyBoundBoundG
  :: MonadRef m
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Bound a -> VerseT m ()
unifyBoundBoundG f var1 bound1 (Var ref2) bound2 =
  when (bound1.label /= bound2.label) $ do
    m <- f bound1.binding bound2.binding
    writeVarStateG ref2 $ Link var1
    m

unifySubstG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Subst m a -> Var m a -> VerseT m ()
unifySubstG f var1 subst1 var2 = case subst1 of
  LinkS -> unifyG' f var1 var2
  BoundS bound1 -> unifyBoundG f var1 bound1 var2

unifyBoundG
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => (a -> a -> VerseT m (VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> VerseT m ()
unifyBoundG f var1 bound1 var2 = readRoot var2 >>= \ case
  (var2, UnboundR unbound2) -> unifyBoundUnboundG f var1 bound1 var2 unbound2
  (var2, BoundR bound2) -> unifyBoundBoundG f var1 bound1 var2 bound2

writeVarStateG
  :: MonadRef m
  => HRef m (VarState m a)
  -> VarState m a
  -> VerseT m ()
writeVarStateG (HRef ref) x = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
  y <- findVarState heaps.heap <$> readRef ref
  modifyRef' ref $ insertLocalHeap heaps.heap x
  sk heaps () s
    fk
    (\ heaps@Heaps {..} ak' -> do
        x <- findLocalHeap heap <$> readRef ref
        modifyRef' ref $ insertLocalHeap heap y
        fk' heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak' heaps)
    ak
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap y
        ak' heaps)

data VerseRef m a = VerseRef
  { label :: {-# UNPACK #-} !Int
  , ref :: !(HRef m a)
  }

instance Eq (VerseRef m a) where
  x == y = x.label == y.label

instance ( MonadRef m
         , Freezable a b m
         ) => Freezable (VerseRef m a) (Identity b) m where
  freeze VerseRef {..} =
    fmap Identity . freeze =<< FreezeT (lift . lift . readVerseRef' ref =<< ask)

newVerseRef
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => a -> VerseT m (VerseRef m a)
newVerseRef x = getHeap >>= \ case
  Just heap -> do
    label <- supply
    ref <- lift $ HRef <$> newRef HeapMap
      { nothing = Nothing
      , just = IntMap.singleton heap.label x
      }
    modifyStore . IntMap.insert label $ StoreElem ref
    pure $ VerseRef {..}
  Nothing -> do
    label <- supply
    ref <- lift $ HRef <$> newRef HeapMap
      { nothing = Just x
      , just = mempty
      }
    pure $ VerseRef {..}

readVerseRef :: MonadRef m => VerseRef m a -> VerseT m a
readVerseRef VerseRef {..} = lift . readVerseRef' ref =<< getHeap

readVerseRef' :: MonadRef m => HRef m a -> Maybe Heap -> m a
readVerseRef' (HRef ref) heap = findHeap heap <$> readRef ref

readVerseRef'' :: MonadRef m => HRef m a -> Heap -> m a
readVerseRef'' (HRef ref) heap = findHeap' heap <$> readRef ref

writeVerseRef :: (MonadRef m, Freshenable a m) => VerseRef m a -> a -> VerseT m ()
writeVerseRef VerseRef {..} x = do
  writeVerseRef' ref x
  modifyStore . IntMap.insert label $ StoreElem ref

writeVerseRef' :: MonadRef m => HRef m a -> a -> VerseT m ()
writeVerseRef' (HRef ref) x = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
  y <- lookupHeap heaps.heap <$> readRef ref
  modifyRef' ref $ insertLocalHeap heaps.heap x
  let
  sk heaps () s
    fk
    (\ heaps@Heaps {..} ak' -> do
        x <- findLocalHeap heaps.heap <$> readRef ref
        modifyRef' ref $ alterLocalHeap heap y
        fk' heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak' heaps)
    ak
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ alterLocalHeap heap y
        ak' heaps)

writeVerseRef'' :: MonadRef m => HRef m a -> Heap -> a -> m ()
writeVerseRef'' (HRef ref) heap = modifyRef' ref . insertLocalHeap' heap

commitStore :: MonadRef m => Store m -> Heap -> VerseT m ()
commitStore store heap = IntMap.forWithKey_ store $ commitStoreElem heap

commitStoreElem :: MonadRef m => Heap -> Int -> StoreElem m -> VerseT m ()
commitStoreElem heap label elem@(StoreElem ref) = do
  writeVerseRef' ref <=< flip freshen' heap <=< lift $ readVerseRef'' ref heap
  modifyStore $ IntMap.insert label elem

duplicateStore :: MonadRef m => Store m -> Maybe Heap -> VerseT m ()
duplicateStore store heap = for_ store $ duplicateStoreElem heap

duplicateStoreElem :: MonadRef m => Maybe Heap -> StoreElem m -> VerseT m ()
duplicateStoreElem heap (StoreElem ref) =
  writeVerseRef' ref <=< lift $ readVerseRef' ref heap

newtype HRef m a = HRef (Ref m (HeapMap a))

newHRef :: MonadRef m => a -> VerseT m (HRef m a)
newHRef x = getHeap >>= lift . newHRef' x

newHRef' :: MonadRef m => a -> Maybe Heap -> m (HRef m a)
newHRef' x = \ case
  Just heap -> newHRef'' x heap
  Nothing -> HRef <$> newRef HeapMap
    { nothing = Just x
    , just = mempty
    }

newHRef'' :: MonadRef m => a -> Heap -> m (HRef m a)
newHRef'' x Heap {..} = HRef <$> newRef HeapMap
  { nothing = Nothing
  , just = IntMap.singleton label x
  }

readHRef :: MonadRef m => HRef m a -> VerseT m a
readHRef (HRef ref) = findLocalHeap <$> getHeap <*> lift (readRef ref)

readHRef' :: MonadRef m => HRef m a -> Heap -> VerseT m a
readHRef' ref = lift . readHRef'' ref

readHRef'' :: MonadRef m => HRef m a -> Heap -> m a
readHRef'' (HRef ref) h = findLocalHeap' h . (.just) <$> readRef ref

writeHRef :: MonadRef m => HRef m a -> a -> VerseT m ()
writeHRef (HRef ref) x = VerseT $ \ _ R {..} s sk fk fk' ak ak' -> do
  y <- findLocalHeap heaps.heap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.heap x)
  sk heaps () s
    (\ heaps@Heaps {..} ak -> do
        x <- findLocalHeap heap <$> readRef ref
        modifyRef' ref $ insertLocalHeap heap y
        fk heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak heaps)
    (\ heaps@Heaps {..} ak' -> do
        x <- findLocalHeap heap <$> readRef ref
        modifyRef' ref $ insertLocalHeap heap y
        fk' heaps $ \ heaps@Heaps {..} -> do
          modifyRef' ref $ insertLocalHeap heap x
          ak' heaps)
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap y
        ak heaps)
    (\ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap y
        ak' heaps)

modifyHRef' :: MonadRef m => HRef m a -> (a -> a) -> VerseT m ()
modifyHRef' ref f = do
  x <- readHRef ref
  writeHRef ref $! f x

data HeapMap a = HeapMap
  { nothing :: Maybe a
  , just :: IntMap a
  }

findVarState :: Maybe Heap -> HeapMap (VarState m a) -> VarState m a
findVarState k xs@HeapMap {..} = case k of
  Just k -> findVarState' k xs
  Nothing -> nothing `orError` "findVarState"

findVarState' :: Heap -> HeapMap (VarState m a) -> VarState m a
findVarState' k@Heap {..} xs@HeapMap {..} = case lookupLocalHeap' k just of
  Just x -> x
  Nothing -> case findHeap tail xs of
    Unbound unbound -> Unbound unbound
      { substSusp = emptySubstSusp
      }
    x -> x

findHeap :: Maybe Heap -> HeapMap a -> a
findHeap k xs = lookupHeap k xs `orError` "findHeap"

findHeap' :: Heap -> HeapMap a -> a
findHeap' k xs = lookupHeap' k xs `orError` "findHeap'"

lookupHeap :: Maybe Heap -> HeapMap a -> Maybe a
lookupHeap k xs@HeapMap {..} = case k of
  Just k -> lookupHeap' k xs
  Nothing -> nothing

lookupHeap' :: Heap -> HeapMap a -> Maybe a
lookupHeap' k@Heap {..} xs@HeapMap {..} = case lookupLocalHeap' k just of
  x@Just {} -> x
  Nothing -> lookupHeap tail xs

findLocalHeap :: Maybe Heap -> HeapMap a -> a
findLocalHeap k HeapMap {..} = case k of
  Just k -> findLocalHeap' k just
  Nothing -> nothing `orError` "findLocalHeap"

findLocalHeap' :: Heap -> IntMap a -> a
findLocalHeap' k xs = lookupLocalHeap' k xs `orError` "findLocalHeap'"

lookupLocalHeap' :: Heap -> IntMap a -> Maybe a
lookupLocalHeap' Heap {..} xs = case IntMap.lookup label xs of
  x@Just {} -> x
  Nothing -> case pred of
    Nothing -> Nothing
    Just k -> lookupLocalHeap' k xs

alterLocalHeap :: Maybe Heap -> Maybe a -> HeapMap a -> HeapMap a
alterLocalHeap k x xs = case x of
  Nothing -> deleteLocalHeap k xs
  Just x -> insertLocalHeap k x xs

insertLocalHeap :: Maybe Heap -> a -> HeapMap a -> HeapMap a
insertLocalHeap k x xs = case k of
  Nothing -> xs { nothing = Just x }
  Just k -> insertLocalHeap' k x xs

insertLocalHeap' :: Heap -> a -> HeapMap a -> HeapMap a
insertLocalHeap' Heap {..} x xs = xs { just = IntMap.insert label x xs.just }

deleteLocalHeap :: Maybe Heap -> HeapMap a -> HeapMap a
deleteLocalHeap k xs = case k of
  Nothing -> xs { nothing = Nothing }
  Just k -> deleteLocalHeap' k xs

deleteLocalHeap' :: Heap -> HeapMap a -> HeapMap a
deleteLocalHeap' Heap {..} xs = xs { just = IntMap.delete label xs.just }

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

orError :: Maybe a -> String -> a
orError x y = case x of
  Just x -> x
  Nothing -> error y

dup :: (a -> a -> b) -> a -> b
dup f x = f x x
