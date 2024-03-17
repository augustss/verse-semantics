{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
module Control.Monad.Verse
  ( VerseT
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
  , Freezable (..)
  , FreezeT
  , freeze'
  , Var
  , freshVar
  , newVar
  , newVerifyVar
  , readVar
  , unifyEq
  , Match (..)
  , unify
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
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Wrong

import Data.Fix
import Data.Foldable
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Lazy.Extras qualified as IntMap.Lazy
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntMap.Strict.Extras qualified as IntMap

import GHC.Exts qualified

import Unsafe.Coerce (unsafeCoerce)

newtype VerseT m a = VerseT
  { unVerseT
    :: forall r . Yield r m
    -> R m
    -> S m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> Abort r m
    -> m r
  }

newtype Yield r m = Yield
  { unYield
    :: forall a . (Susp m a -> VerseT m ())
    -> S m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> Abort r m
    -> m r
  }

type Succeed r m a = Heaps -> S m -> a -> Fail r m -> Empty r m -> Abort r m -> m r

type Fail r m = Heaps -> Abort r m -> m r

type Empty r m = Heaps -> Abort r m -> m r

type Abort r m = Heaps -> m r

type Susp m a = a -> VerseT m ()

data R m = R
  { level :: {-# UNPACK #-} !Level
  , heaps :: !Heaps
  , latch :: !(Latch m)
  }

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
  when (suspCount == 1) susp
  writeHRef ref $! LatchState { suspCount = suspCount - 1, .. }

asksV :: (R m -> a) -> VerseT m a
asksV f = VerseT $ \ _ r@R {..} s sk -> sk heaps s $ f r

asksMV :: Monad m => (R m -> m a) -> VerseT m a
asksMV f = VerseT $ \ _ r@R {..} s sk fk ek ak -> do
  x <- f r
  sk heaps s x fk ek ak

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

data S m = S
  { suspend :: !(Suspend m)
  , store :: !(Store m)
  }

emptyS :: Applicative m => S m
emptyS = S
  { suspend = const $ pure ()
  , store = mempty
  }

type Suspend m = Resume m -> VerseT m ()

type Resume m = VerseT m () -> VerseT m ()

type Store m = IntMap (StoreElem m)

data StoreElem m = forall a . Freshenable a m => StoreElem (HRef m a)

putV :: S m -> VerseT m ()
putV s = VerseT $ \ _ R {..} _ sk -> sk heaps s ()

modifyV' :: (S m -> S m) -> VerseT m ()
modifyV' f = VerseT $ \ _ R {..} s sk ->
  let s' = f s in s' `seq` sk heaps s' ()

whenSuspended :: Suspend m -> VerseT m ()
whenSuspended f = modifyV' $ \ S {..} ->
  S { suspend = \ resume -> suspend resume *> f resume, .. }

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

type Level = Int

instance Functor (VerseT m) where
   fmap f m = VerseT $ \ yk r s sk ->
     unVerseT m yk r s $ \ heap s -> sk heap s . f

instance Applicative (VerseT m) where
   pure x = VerseT $ \ _ R {..} s sk -> sk heaps s x
   f <*> x = VerseT $ \ yk r@R {..} s sk ->
     unVerseT f yk r s $ \ heaps s f ->
     unVerseT x yk R {..} s $ \ heaps s -> sk heaps s . f

instance Alternative (VerseT m) where
  empty = VerseT $ \ _ R {..} _ _ _ ek -> ek heaps
  x <|> y = VerseT $ \ yk r@R {..} s sk fk ek ->
    unVerseT x yk r s sk
    (\ heaps -> unVerseT y yk R {..} s sk fk fk)
    (\ heaps -> unVerseT y yk R {..} s sk fk ek)

abort :: VerseT m a
abort = VerseT $ \ _ R {..} _ _ _ _ ak -> ak heaps

infixl 3 <?>
(<?>) :: VerseT m a -> VerseT m a -> VerseT m a
x <?> y = VerseT $ \ yk r@R {..} s sk fk ek ak ->
  unVerseT x yk r s sk fk ek $ \ heaps ->
  unVerseT y yk R {..} s sk fk ek ak

instance Monad (VerseT m) where
  m >>= k = VerseT $ \ yk r@R {..} s sk ->
    unVerseT m yk r s $ \ heaps s x ->
    unVerseT (k x) yk R {..} s sk

instance MonadSupply s m => MonadSupply s (VerseT m) where
  supply = lift supply

instance MonadWrong e m => MonadWrong e (VerseT m) where
  wrong = lift . wrong

instance MonadTrans VerseT where
  lift m = VerseT $ \ _ R {..} s sk fk ek ak -> do
    x <- m
    sk heaps s x fk ek ak

lift' :: Monad m => (R m -> S m -> m a) -> VerseT m a
lift' f = VerseT $ \ _ r@R {..} s sk fk ek ak -> do
  x <- f r s
  sk heaps s x fk ek ak

runVerseT :: (MonadRef m, MonadSupply Int m) => VerseT m a -> m (Maybe [[a]])
runVerseT m = do
  heap <- newHeap' verifyHeap
  latch <- newLatch'' heap
  let
    heaps = Heaps { heap = Just heap, verifyHeap }
    sk heaps s x fk _ ak = do
      suspCount <- readSuspCount'' latch heap
      runMaybeT $ do
        guard $ suspCount == 0
        lift $ commitRun level heap s.store
        prepend x <$> MaybeT (fk heaps abortRun) <*> MaybeT (ak heaps)
  unVerseT m yk R {..} emptyS sk failRun failRun abortRun
  where
    yk = Yield $ \ _ _ _ _ _ _ -> pure Nothing
    level = 0
    verifyHeap = Nothing
    prepend x xss yss = ((x:) <$> xss) ++ yss

commitRun :: MonadRef m => Int -> Heap -> Store m -> m ()
commitRun level heap store = for_ store $ \ (StoreElem ref) ->
  commitRun' level heap ref

commitRun' :: (MonadRef m, Freshenable a m) => Int -> Heap -> HRef m a -> m ()
commitRun' level heap (HRef ref) = do
  x <- freshenRun' level heap . findHeap' heap =<< readRef ref
  modifyRef' ref $ insertLocalHeap' heap x

freshenRun' :: Freshenable a m => Int -> Heap -> a -> m a
freshenRun' level heap x = runFreshenT (freshen x) FreshenEnv {..}

failRun :: Functor m => Fail (Maybe [[a]]) m
failRun heap ak = fmap ([]:) <$> ak heap

abortRun :: Applicative m => Abort (Maybe [[a]]) m
abortRun = const . pure $ Just []

runVerse :: (forall s . VerseT (IntSupplyT (ST s)) a) -> Maybe [[a]]
runVerse m = runST $ runIntSupplyT $ runVerseT m

class Monad m => Freshenable a m where
  freshen :: a -> FreshenT m a

freshen' :: Freshenable a m => a -> Heap -> VerseT m a
freshen' x heap = do
  level <- getLevel <&> (+ 1)
  lift $ runFreshenT (freshen x) FreshenEnv {..}

newtype FreshenT m a = FreshenT
  { unFreshenT :: ReaderT FreshenEnv (StateT (IntMap GHC.Exts.Any) m) a
  } deriving (Functor, Applicative, Monad)

data FreshenEnv = FreshenEnv
  { level :: {-# UNPACK #-} !Int
  , heap :: !Heap
  }

runFreshenT :: Monad m => FreshenT m a -> FreshenEnv -> m a
runFreshenT m r = evalStateT (runReaderT (unFreshenT m) r) mempty

instance Monad m => Freshenable () m where
  freshen = pure

instance (Freshenable a m, Freshenable b m) => Freshenable (a, b) m where
  freshen (a, b) = (,) <$> freshen a <*> freshen b

instance ( Freshenable a m
         , Freshenable b m
         , Freshenable c m
         ) => Freshenable (a, b, c) m where
  freshen (a, b, c) = (,,) <$> freshen a <*> freshen b <*> freshen c

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
  , susp :: !(Susp m (Stream m a))
  , init :: !(VerseT m (), VerseT m (), VerseT m ())
  , last :: !(HRef m (Maybe a))
  , suspend :: !(Suspend m)
  , store :: !(Store m)
  }

split'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> HRef m (Maybe a)
  -> VerseT m (Stream m a)
split' m heap latch last =
  splitS m heap latch >>= \ case
    AbortS -> abort
    FailS m_a ->
      pure Done <?>
      (do heap <- copyHeap heap
          split' m_a heap latch last)
    YieldS s@S {..} m_y m_f m_e m_a ->
      let
        f susp = do
          let init = (m_f, m_e, m_a)
          ref_env <- newHRef $ Just SplitEnv {..}
          s.suspend $ resumeSplit ref_env
          resumeSplit ref_env m_y
      in
        yield f
    SucceedS s@S {..} m_f m_e m_a -> do
      suspCount <- readSuspCount' latch heap
      (guard (suspCount == 0) *>) <$> readHRef' last heap >>= \ case
        Just x ->
          (do commit store heap
              x <- freshen' x heap
              pure . Step x $ do
                heap <- copyHeap heap
                split' (duplicate store heap.tail *> m_f) heap latch last) <?>
          (do heap <- copyHeap heap
              split' m_a heap latch last)
        Nothing ->
          let
            f susp = do
              let init = (m_f, m_e, m_a)
              ref_env <- newHRef $ Just SplitEnv {..}
              s.suspend $ resumeSplit ref_env
          in
            yield f

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
resumeSplit' ref_env env@SplitEnv { init = (m_f', m_e', m_a'), .. } m =
  splitS m heap latch >>= \ case
    AbortS -> do
      writeHRef ref_env Nothing
      susp =<< split' m_a' heap latch last
    FailS m_a -> do
      writeHRef ref_env Nothing
      susp =<< split' (m_e' <?> m_a <?> m_a') heap latch last
    YieldS s m_y m_f m_e m_a ->
      let
        init =
          ( (whenSuspended env.suspend *> m_f) <|> m_f'
          , alt (whenSuspended env.suspend *> m_e) m_f' m_e'
          , (whenSuspended env.suspend *> m_a) <?> m_a'
          )
        suspend resume = env.suspend resume *> s.suspend resume
        store = env.store <> s.store
      in do
        writeHRef ref_env $ Just SplitEnv {..}
        s.suspend $ resumeSplit ref_env
        resumeSplit ref_env m_y
    SucceedS s m_f m_e m_a -> do
      let
        m_f'' = (whenSuspended env.suspend *> m_f) <|> m_f'
        m_a'' = (whenSuspended env.suspend *> m_a) <?> m_a'
        store = env.store <> s.store
      suspCount <- readSuspCount' latch heap
      (guard (suspCount == 0) *>) <$> readHRef' last heap >>= \ case
        Just x ->
          (do commit store heap
              x <- freshen' x heap
              susp . Step x $ do
                heap <- copyHeap heap
                split' (duplicate store heap.tail *> m_f'') heap latch last) <?>
          (do heap <- copyHeap heap
              susp =<< split' m_a'' heap latch last)
        Nothing -> do
          let
            init =
              ( m_f''
              , alt (whenSuspended env.suspend *> m_e) m_f' m_e'
              , m_a''
              )
            suspend resume = env.suspend resume *> s.suspend resume
          writeHRef ref_env $ Just SplitEnv {..}
          s.suspend $ resumeSplit ref_env

splitS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> VerseT m (Split m)
splitS m heap latch = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getVerifyHeap
  let r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
  lift $ unVerseT m yieldS r emptyS succeedS failS failS abortS

verify :: (MonadRef m, MonadSupply Int m) => VerseT m () -> VerseT m ()
verify m = do
  heap <- newHeap
  latch <- lift $ newLatch'' heap
  last <- lift $ newHRef'' False heap
  verify' (m *> writeHRef last True) heap latch last

data VerifyEnv m = VerifyEnv
  { heap :: {-# UNPACK #-} !Heap
  , latch :: !(Latch m)
  , susp :: !(Susp m ())
  , init :: !(VerseT m (), VerseT m (), VerseT m ())
  , last :: !(HRef m Bool)
  , suspend :: !(Suspend m)
  }

verify'
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> HRef m Bool
  -> VerseT m ()
verify' m heap latch last =
  verifyS m heap latch >>= \ case
    AbortS -> pure ()
    FailS m_a -> verify' m_a heap latch last
    YieldS s@S {..} m_y m_f m_e m_a ->
      let
        f susp = do
          let init = (m_f, m_e, m_a)
          ref_env <- newHRef $ Just VerifyEnv {..}
          s.suspend $ resumeVerify ref_env
          resumeVerify ref_env m_y
      in
        yield f
    SucceedS s@S {..} m_f m_e m_a -> do
      suspCount <- readSuspCount' latch heap
      (suspCount == 0 &&) <$> readHRef' last heap >>= \ case
        True -> verify' (m_f <?> m_a) heap latch last
        False ->
          let
            f susp = do
              let init = (m_f, m_e, m_a)
              ref_env <- newHRef $ Just VerifyEnv {..}
              s.suspend $ resumeVerify ref_env
          in
            yield f

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
resumeVerify' ref_env env@VerifyEnv { init = (m_f', m_e', m_a'), .. } m =
  verifyS m heap latch >>= \ case
    AbortS -> do
      writeHRef ref_env Nothing
      susp =<< verify' m_a' heap latch last
    FailS m_a -> do
      writeHRef ref_env Nothing
      susp =<< verify' (m_e' <?> m_a <?> m_a') heap latch last
    YieldS s m_y m_f m_e m_a ->
      let
        init =
          ( (whenSuspended env.suspend *> m_f) <|> m_f'
          , alt (whenSuspended env.suspend *> m_e) m_f' m_e'
          , (whenSuspended env.suspend *> m_a) <?> m_a'
          )
        suspend resume = env.suspend resume *> s.suspend resume
      in do
        writeHRef ref_env $ Just VerifyEnv {..}
        s.suspend $ resumeVerify ref_env
        resumeVerify ref_env m_y
    SucceedS s m_f m_e m_a -> do
      let
        m_f'' = alt (whenSuspended env.suspend *> m_f) m_f' m_e'
        m_a'' = (whenSuspended env.suspend *> m_a) <?> m_a'
      suspCount <- readSuspCount' latch heap
      (suspCount == 0 &&) <$> readHRef' last heap >>= \ case
        True -> susp =<< verify' (m_f'' <?> m_a'') heap latch last
        False -> do
          let
            init =
              ( m_f''
              , alt (whenSuspended env.suspend *> m_e) m_f' m_e'
              , m_a''
              )
            suspend resume = env.suspend resume *> s.suspend resume
          writeHRef ref_env $ Just VerifyEnv {..}
          s.suspend $ resumeVerify ref_env

verifyS
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m ()
  -> Heap
  -> Latch m
  -> VerseT m (Split m)
verifyS m heap latch = do
  level <- getLevel <&> (+ 1)
  verifyHeap <- getHeap
  let r = R { level, heaps = Heaps { heap = Just heap, verifyHeap }, latch }
  lift $ unVerseT m yieldS r emptyS succeedS failS failS abortS

assume
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m a
assume m = split m >>= \ case
  Done -> abort
  Step x _ -> pure x

fork :: MonadRef m => VerseT m () -> VerseT m ()
fork m =
  lift' (\ r s -> unVerseT m yieldS r s succeedS failS failS abortS) >>=
  reflectS

join' :: MonadRef m => VerseT m a -> VerseT m a
join' m = do
  latch@(Latch ref) <- newLatch
  x <- localV (\ R { latch = _, .. } -> R {..}) m
  LatchState {..} <- readHRef ref
  if suspCount == 0 then pure x else yield $ \ f ->
    let susp = f x in writeHRef ref $! LatchState {..}

data Split m
  = YieldS !(S m) (VerseT m ()) (VerseT m ()) (VerseT m ()) (VerseT m ())
  | SucceedS !(S m) (VerseT m ()) (VerseT m ()) (VerseT m ())
  | FailS (VerseT m ())
  | AbortS

yieldS :: MonadRef m => Yield (Split m) m
yieldS = Yield $ \ f s sk fk ek ak -> pure $ YieldS
  s
  (do latch <- getLatch
      incrSuspCount latch
      f $ \ x -> do
        reflectSucceedS sk x
        decrSuspCount latch)
  (reflectFailS fk)
  (reflectFailS ek)
  (reflectAbortS ak)

succeedS :: Monad m => Succeed (Split m) m ()
succeedS _ s () fk ek ak =
  pure $ SucceedS s (reflectFailS fk) (reflectFailS ek) (reflectAbortS ak)

failS :: Monad m => Fail (Split m) m
failS _ ak = pure . FailS $ asksMV (\ R {..} -> ak heaps) >>= reflectS

abortS :: Applicative m => Abort (Split m) m
abortS = const $ pure AbortS

reflectS :: Split m -> VerseT m ()
reflectS = \ case
  YieldS s m_y m_f m_e m_a -> alt (putV s *> m_y) m_f m_e <?> m_a
  SucceedS s m_f m_e m_a -> alt (putV s) m_f m_e <?> m_a
  FailS m_a -> empty <?> m_a
  AbortS -> abort

reflectSucceedS :: Monad m => Succeed (Split m) m a -> a -> VerseT m ()
reflectSucceedS sk x =
  lift' (\ R {..} s -> sk heaps s x failS failS abortS) >>= reflectS

reflectFailS :: Monad m => Fail (Split m) m -> VerseT m ()
reflectFailS fk =
  asksMV (\ R {..} -> fk heaps abortS) >>= reflectS

reflectAbortS :: Monad m => Abort (Split m) m -> VerseT m ()
reflectAbortS ak =
  asksMV (\ R {..} -> ak heaps) >>= reflectS

alt :: VerseT m a -> VerseT m a -> VerseT m a -> VerseT m a
alt x y z = VerseT $ \ yk r@R {..} s sk fk ek ->
  unVerseT x yk r s sk
  (\ heaps -> unVerseT y yk R {..} s sk fk fk)
  (\ heaps -> unVerseT z yk R {..} s sk fk ek)

yield :: (Susp m a -> VerseT m ()) -> VerseT m a
yield f = VerseT $ \ yk _ -> unYield yk f

class Monad m => Freezable a b m | a -> b where
  freeze :: a -> FreezeT m b

freeze' :: Freezable a b m => a -> VerseT m b
freeze' x = lift . runFreezeT (freeze x) =<< getHeap

newtype FreezeT m a = FreezeT
  { unFreezeT :: ReaderT (Maybe Heap) (StateT (IntMap GHC.Exts.Any) m) a
  } deriving (Functor, Applicative, Monad)

runFreezeT :: Monad m => FreezeT m a -> Maybe Heap -> m a
runFreezeT m heap = evalStateT (runReaderT (unFreezeT m) heap) mempty

instance Monad m => Freezable () () m where
  freeze = pure

instance (Freezable a c m, Freezable b d m) => Freezable (a, b) (c, d) m where
  freeze (x, y) = (,) <$> freeze x <*> freeze y

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

newtype Var m a = Var (HRef m (VarState m a))

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         , Freshenable a m
         ) => Freshenable (Var m a) m where
  freshen var@(Var ref) = FreshenT $ do
    FreshenEnv {..} <- ask
    lift (lift $ readLocalVarState ref heap) >>= \ case
      Nothing -> pure var
      Just (Link var) -> unFreshenT $ freshen var
      Just (Unbound unbound) ->
        if unbound.level < level then pure var else mfix $ \ var' ->
          state' (IntMap.Lazy.lookupInsert unbound.label $ unsafeCoerce var') >>= \ case
            Just (unsafeCoerce -> var') -> pure var'
            Nothing -> lift . lift $ freshVar' (level - 1) heap.tail
      Just (Bound bound) -> mfix $ \ var' ->
        state' (IntMap.Lazy.lookupInsert bound.label $ unsafeCoerce var') >>= \ case
          Just (unsafeCoerce -> var') -> pure var'
          Nothing -> do
            binding <- unFreshenT (freshen bound.binding)
            lift . lift $ newVar' binding heap.tail

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
        state' (IntMap.Lazy.lookupInsert bound.label $ unsafeCoerce binding) >>= \ case
          Just (unsafeCoerce -> binding) -> pure binding
          Nothing -> Just <$> unFreezeT (freeze bound.binding)

data VarState m a
  = Link !(Var m a)
  | Unbound !(Unbound m a)
  | Bound !(Bound a)

data Unbound m a = MkUnbound
  { label :: {-# UNPACK #-} !Int
  , level :: {-# UNPACK #-} !Int
  , substSusp :: !(Susp m (Subst m a))
  }

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
  let substSusp = const $ pure ()
  Var <$> newHRef' (Unbound MkUnbound {..}) heap

newVar :: (MonadRef m, MonadSupply Int m) => a -> VerseT m (Var m a)
newVar binding = do
  label <- supply
  Var <$> newHRef (Bound MkBound {..})

newVerifyVar :: (MonadRef m, MonadSupply Int m) => a -> VerseT m (Var m a)
newVerifyVar binding = do
  label <- supply
  Var <$> newVerifyHRef (Bound MkBound {..})

newVar' :: (MonadRef m, MonadSupply Int m) => a -> Maybe Heap -> m (Var m a)
newVar' binding heap = do
  label <- supply
  Var <$> newHRef' (Bound MkBound {..}) heap

readVar :: MonadRef m => Var m a -> VerseT m a
readVar = fmap ((.binding) . snd) . readBound

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
  (UnboundR var1 unbound1, UnboundR var2 unbound2) ->
    case compareUnbound unbound1 unbound2 of
      EQ -> pure ()
      LT -> unifyUnboundUnbound f var1 unbound1 var2 unbound2
      GT -> unifyUnboundUnbound f var2 unbound2 var1 unbound1
  (UnboundR var1 unbound1, BoundR var2 bound2) ->
    unifyBoundUnbound f var2 bound2 var1 unbound1
  (BoundR var1 bound1, UnboundR var2 unbound2) ->
    unifyBoundUnbound f var1 bound1 var2 unbound2
  (BoundR var1 bound1, BoundR var2 bound2) ->
    unifyBoundBound f var1 bound1 var2 bound2

unifySubst
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Subst m a -> Var m a -> VerseT m ()
unifySubst f subst1 var2 = case subst1 of
  LinkS var1 -> unify f var1 var2
  BoundS var1 bound1 -> unifyBound f var1 bound1 var2

unifyBound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> VerseT m ()
unifyBound f var1 bound1 var2 = readRoot var2 >>= \ case
  UnboundR var2 unbound2 -> unifyBoundUnbound f var1 bound1 var2 unbound2
  BoundR var2 bound2 -> unifyBoundBound f var1 bound1 var2 bound2

unifyUnboundUnbound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Unbound m a -> Var m a -> Unbound m a -> VerseT m ()
unifyUnboundUnbound f var1 _ var2@(Var ref2) unbound2 = do
  writeVarState ref2 $ Link var1
  unbound2.substSusp $ LinkS var1
  whenM ((unbound2.level /=) <$> getLevel) $ do
    latch <- getLatch
    incrSuspCount latch
    whenSuspended $ \ resume ->
      fork $ readSubst var2 >>= \ subst -> resume $ do
        unifySubst f subst var2
        decrSuspCount latch

unifyBoundUnbound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Unbound m a -> VerseT m ()
unifyBoundUnbound f var1 bound1 var2@(Var ref2) unbound2 = do
  writeVarState ref2 $ Link var1
  unbound2.substSusp $ BoundS var1 bound1
  whenM ((unbound2.level /=) <$> getLevel) $ do
    latch <- getLatch
    incrSuspCount latch
    whenSuspended $ \ resume ->
      fork $ readBound var2 >>= \ (var2, bound2) -> resume $ do
        unifyBoundBound f var1 bound1 var2 bound2
        decrSuspCount latch

unifyBoundBound
  :: MonadRef m
  => (a -> a -> VerseT m (Match, VerseT m ()))
  -> Var m a -> Bound a -> Var m a -> Bound a -> VerseT m ()
unifyBoundBound f (Var ref1) bound1 (Var ref2) bound2 =
  when (bound1.label /= bound2.label) $
    f bound1.binding bound2.binding >>= \ case
      (SEQ, m) -> writeVarState ref2 (Bound bound1) *> m
      (LE, m) -> (writeVerifyVarState ref2 (Bound bound1) *> m) <?> empty
      (GE, m) -> (writeVerifyVarState ref1 (Bound bound2) *> m) <?> empty

data Root m a
  = UnboundR !(Var m a) !(Unbound m a)
  | BoundR !(Var m a) !(Bound a)

readRoot :: MonadRef m => Var m a -> VerseT m (Root m a)
readRoot var@(Var ref) = readVarState ref >>= \ case
  Link var -> readRoot var
  Bound bound -> pure $ BoundR var bound
  Unbound unbound -> pure $ UnboundR var unbound

data Subst m a
  = LinkS !(Var m a)
  | BoundS !(Var m a) !(Bound a)

readSubst :: MonadRef m => Var m a -> VerseT m (Subst m a)
readSubst var@(Var ref) = readVarState ref >>= \ case
  Link var -> pure $ LinkS var
  Bound bound -> pure $ BoundS var bound
  Unbound unbound -> yield $ \ k -> do
    k <- once k
    writeVarState ref $ Unbound unbound
      { substSusp = \ x -> unbound.substSusp x *> k x
      }
    whenM ((unbound.level /=) <$> getLevel) $
      whenSuspended $ \ resume -> fork $ readSubst var >>= resume . k

readBound :: MonadRef m => Var m a -> VerseT m (Var m a, Bound a)
readBound var = readSubst var >>= \ case
  LinkS var -> readBound var
  BoundS var bound -> pure (var, bound)

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

readLocalVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> Heap
  -> m (Maybe (VarState m a))
readLocalVarState (HRef ref) heap = lookupLocalHeap' heap . (.just) <$> readRef ref

writeVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> VarState m a
  -> VerseT m ()
writeVarState (HRef ref) x = VerseT $ \ _ R {..} s sk fk ek ak -> do
  y <- findVarState heaps.heap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.heap x)
  let
    fk' heaps@Heaps {..} ak = do
      x <- findVarState heap <$> readRef ref
      modifyRef' ref (insertLocalHeap heap y)
      fk heaps $ \ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap x
        ak heaps
    ek' heaps@Heaps {..} ak = do
      x <- findVarState heap <$> readRef ref
      modifyRef' ref $ insertLocalHeap heap y
      ek heaps $ \ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap x
        ak heaps
    ak' heaps@Heaps {..} = do
      modifyRef' ref $ insertLocalHeap heap y
      ak heaps
  sk heaps s () fk' ek' ak'

writeVerifyVarState
  :: MonadRef m
  => HRef m (VarState m a)
  -> VarState m a
  -> VerseT m ()
writeVerifyVarState (HRef ref) x = VerseT $ \ _ R {..} s sk fk ek ak -> do
  y <- findVarState heaps.verifyHeap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.verifyHeap x)
  let
    ak' heaps@Heaps {..} =
      modifyRef' ref (insertLocalHeap verifyHeap y) *> ak heaps
  sk heaps s () fk ek ak'

data VerseRef m a = VerseRef
  { label :: {-# UNPACK #-} !Int
  , ref :: !(HRef m a)
  }

instance Eq (VerseRef m a) where
  x == y = x.label == y.label

instance (MonadRef m, Freezable a b m) => Freezable (VerseRef m a) (Identity b) m where
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
writeVerseRef' (HRef ref) x = VerseT $ \ _ R {..} s sk fk ek ak -> do
  y <- lookupHeap heaps.heap <$> readRef ref
  modifyRef' ref $ insertLocalHeap heaps.heap x
  let
    ek' heaps@Heaps {..} ak = do
      x <- findHeap heap <$> readRef ref
      modifyRef' ref $ alterLocalHeap heap y
      ek heaps $ \ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap x
        ak heaps
    ak' heaps@Heaps {..} = do
      modifyRef' ref $ alterLocalHeap heap y
      ak heaps
  sk heaps s () fk ek' ak'

commit :: MonadRef m => Store m -> Heap -> VerseT m ()
commit store heap = IntMap.forWithKey_ store $ \ label elem ->
  commit' label elem heap

commit' :: MonadRef m => Int -> StoreElem m -> Heap -> VerseT m ()
commit' label elem@(StoreElem ref) heap = do
  x <- flip freshen' heap =<< lift (readVerseRef'' ref heap)
  writeVerseRef' ref x
  modifyStore $ IntMap.insert label elem

duplicate :: MonadRef m => Store m -> Maybe Heap -> VerseT m ()
duplicate store heap = for_ store $ \ (StoreElem ref) ->
  duplicate' ref heap

duplicate' :: MonadRef m => HRef m a -> Maybe Heap -> VerseT m ()
duplicate' ref = writeVerseRef' ref <=< lift . readVerseRef' ref

newtype HRef m a = HRef (Ref m (HeapMap a))

newHRef :: MonadRef m => a -> VerseT m (HRef m a)
newHRef x = getHeap >>= lift . newHRef' x

newVerifyHRef :: MonadRef m => a -> VerseT m (HRef m a)
newVerifyHRef x = getVerifyHeap >>= lift . newHRef' x

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
writeHRef (HRef ref) x = VerseT $ \ _ R {..} s sk fk ek ak -> do
  y <- findLocalHeap heaps.heap <$> readRef ref
  modifyRef' ref (insertLocalHeap heaps.heap x)
  let
    fk' heaps@Heaps {..} ak = do
      x <- findLocalHeap heap <$> readRef ref
      modifyRef' ref $ insertLocalHeap heap y
      fk heaps $ \ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap x
        ak heaps
    ek' heaps@Heaps {..} ak = do
      x <- findLocalHeap heap <$> readRef ref
      modifyRef' ref $ insertLocalHeap heap y
      ek heaps $ \ heaps@Heaps {..} -> do
        modifyRef' ref $ insertLocalHeap heap x
        ak heaps
    ak' heaps@Heaps {..} = do
      modifyRef' ref $ insertLocalHeap heap y
      ak heaps
  sk heaps s () fk' ek' ak'

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
      { substSusp = const $ pure ()
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
  Just Heap {..} -> xs { just = IntMap.delete label xs.just }

state' :: MonadState s m => (s -> Either a s) -> m (Maybe a)
state' f = state $ \ s -> case f s of
  Left x -> (Just x, s)
  Right s -> (Nothing, s)

orError :: Maybe a -> String -> a
orError x y = case x of
  Just x -> x
  Nothing -> error y
