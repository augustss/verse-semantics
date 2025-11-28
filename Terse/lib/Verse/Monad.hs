{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Verse.Monad
  ( VerseT
  , runVerseT
  , liftPut
  , all'
  , one
  , if'
  , Stream (..)
  , split
  , fork
  , fork1
  , fork2
  , fork3
  , stuck
  , Var
  , Vars (..)
  , ZipVars_ (..)
  , freshVar
  , newVar
  , readVar
  , readVar'
  , unifyVar
  , VarsRef
  , newVarsRef
  , readVarsRef
  , writeVarsRef
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Zip

import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.HashMap.Strict qualified as Strict (HashMap)
import Data.Kind
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Tuple
import Data.Vector (Vector)

import Fix
import Ref

newtype VerseT (m :: Type -> Type) a = VerseT
  { unVerseT
    :: forall r . Level
    -> Count
    -> Heap m
    -> Mem m
    -> Yield r m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> m r
  }

newtype Level = Level Int deriving (Eq, Ord, Num)

newtype Count = Count Int deriving (Eq, Num)

type Heap m = Var m ()

data Mem m = Mem
  { label :: {-# UNPACK #-} !Label
  , varMin :: {-# UNPACK #-} !Label
  , varMinBound :: {-# UNPACK #-} !Label
  , refMin :: {-# UNPACK #-} !Label
  , refMinBound :: {-# UNPACK #-} !Label
  , forward :: !(m ())
  , backward :: !(m ())
  , backward' :: !(m ())
  }

appendMemVar :: Applicative m => Mem m -> Label -> m () -> m () -> Mem m
{-# INLINE appendMemVar #-}
appendMemVar mem label forward backward = mem
  { varMin = min mem.varMin label
  , forward = mem.forward *> forward
  , backward = backward *> mem.backward
  }

appendMemRef :: Applicative m => Mem m -> Label -> m () -> Mem m
{-# INLINE appendMemRef #-}
appendMemRef mem label backward' = mem
  { refMin = min mem.refMin label
  , backward' = backward' *> mem.backward'
  }

newWeakTrail :: MonadWeakRef m => Ref m a -> m () -> m (m ())
{-# INLINABLE newWeakTrail #-}
newWeakTrail ref = fmap (fromMaybe (pure ()) <=< readWeakRef) . newWeakRef ref

type Label = Int

newtype Yield r m = Yield
  { unYield
    :: forall a . Level
    -> Handler m a
    -> Count
    -> Mem m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> m r
  }

type Handler m a = (VerseT m a -> VerseT m ()) -> VerseT m ()

type Succeed r m a = Count -> Mem m -> a -> Fail r m -> Empty r m -> m r

type Fail r m = Heap m -> Mem m -> m r

type Empty r m = Mem m -> m r

instance Functor (VerseT m) where
  {-# INLINE fmap #-}
  fmap f m = VerseT $ \ level count heap mem yk sk ->
    unVerseT m level count heap mem yk $ \ count mem -> sk count mem . f

instance Applicative (VerseT m) where
  {-# INLINE pure #-}
  pure x = VerseT $ \ _level count _heap mem _yk sk ->
    sk count mem x
  {-# INLINE (<*>) #-}
  f <*> x = VerseT $ \ level count heap mem yk sk ->
    unVerseT f level count heap mem yk $ \ count mem f ->
    unVerseT x level count heap mem yk $ \ count mem x ->
    sk count mem $ f x

instance Alternative (VerseT m) where
  {-# INLINE empty #-}
  empty = VerseT $ \ _level _count _heap mem _yk _sk _fk ek ->
    ek mem
  {-# INLINABLE (<|>) #-}
  x <|> y = VerseT $ \ level count heap mem@Mem { label } yk sk fk ek ->
    let
      varMinBound = label
      refMinBound = label
    in
      unVerseT x level count heap mem { varMinBound, refMinBound } yk sk
      (\ heap mem -> unVerseT y level count heap mem yk sk fk $ fk heap)
      (\ mem -> unVerseT y level count heap mem yk sk fk ek)

instance Monad (VerseT m) where
  {-# INLINE (>>=) #-}
  x >>= f = VerseT $ \ level count heap mem yk sk ->
    unVerseT x level count heap mem yk $ \ count mem x ->
    unVerseT (f x) level count heap mem yk sk

instance MonadPlus (VerseT m)

instance MonadTrans VerseT where
  {-# INLINE lift #-}
  lift m = VerseT $ \ _level count _heap mem _yk sk fk ek ->
    m >>= \ x -> sk count mem x fk ek

instance MonadIO m => MonadIO (VerseT m) where
  {-# INLINE liftIO #-}
  liftIO = lift . liftIO

instance PrimMonad m => PrimMonad (VerseT m) where
  type PrimState (VerseT m) = PrimState m
  {-# INLINE primitive #-}
  primitive = lift . primitive

instance MonadReader (Var m ()) (VerseT m) where
  {-# INLINE ask #-}
  ask = VerseT $ \ _level count heap mem _yk sk ->
    sk count mem heap
  {-# INLINE local #-}
  local f m = VerseT $ \ level count heap mem yk sk fk ek ->
    unVerseT m level count (f heap) mem yk sk fk ek
  {-# INLINE reader #-}
  reader f = VerseT $ \ _level count heap mem _yk sk ->
    sk count mem $ f heap

instance MonadState s m => MonadState s (VerseT m) where
  {-# INLINE get #-}
  get = lift get
  {-# INLINE put #-}
  put = lift . put
  {-# INLINE state #-}
  state = lift . state

supply :: VerseT m Label
{-# INLINE supply #-}
supply = VerseT $ \ _level count _heap Mem {..} _yk sk ->
  sk count Mem { label = label + 1, .. } label

liftPut :: Monad m => m () -> m () -> VerseT m ()
{-# INLINE liftPut #-}
liftPut forward backward = do
  lift forward
  tellVar minBound forward backward

tellVar :: Applicative m => Label -> m () -> m () -> VerseT m ()
{-# INLINABLE tellVar #-}
tellVar label forward backward = VerseT $ \ _level count _heap mem _yk sk fk ek ->
  sk count (appendMemVar mem label forward backward) ()
  (\ heap mem -> backward *> tellVar' label backward forward (fk heap) mem)
  (\ mem -> backward *> tellVar' label backward forward ek mem)

tellVar'
  :: Applicative m
  => Label -> m () -> m () -> (Mem m -> m r) -> Mem m -> m r
{-# INLINE tellVar' #-}
tellVar' label forward backward f mem
  | label < mem.varMinBound = f $ appendMemVar mem label forward backward
  | otherwise = f mem

tellRef :: Applicative m => Label -> m () -> VerseT m ()
{-# INLINABLE tellRef #-}
tellRef label backward' = VerseT $ \ _level count _heap mem _yk sk fk ek ->
  sk count (appendMemRef mem label backward') ()
  fk (\ mem -> backward' *> ek mem)

yield :: Level -> Handler m a -> VerseT m a
{-# INLINE yield #-}
yield i f = VerseT $ \ _level count _heap mem yk ->
  unYield yk i f count mem

getLevel :: VerseT m Level
{-# INLINE getLevel #-}
getLevel = VerseT $ \ level count _heap mem _yk sk ->
  sk count mem level

putCount :: Count -> VerseT m ()
{-# INLINE putCount #-}
putCount !count = VerseT $ \ _level _count _heap mem _yk sk ->
  sk count mem ()

modifyCount :: (Count -> Count) -> VerseT m ()
{-# INLINE modifyCount #-}
modifyCount f = VerseT $ \ _level count _heap mem _yk sk ->
  let
    !count' = f count
  in
    sk count' mem ()

incrCount :: VerseT m ()
{-# INLINE incrCount #-}
incrCount = modifyCount (+ 1)

decrCount :: VerseT m ()
{-# INLINE decrCount #-}
decrCount = modifyCount (subtract 1)

getVarMinBound :: VerseT m Label
{-# INLINE getVarMinBound #-}
getVarMinBound = VerseT $ \ _level count _heap mem _yk sk ->
  sk count mem mem.varMinBound

getRefMinBound :: VerseT m Label
{-# INLINE getRefMinBound #-}
getRefMinBound = VerseT $ \ _level count _heap mem _yk sk ->
  sk count mem mem.refMinBound

putMem :: Mem m -> VerseT m ()
{-# INLINE putMem #-}
putMem !mem = VerseT $ \ _level count _heap _mem _yk sk ->
  sk count mem ()

putLabel :: Label -> VerseT m ()
{-# INLINE putLabel #-}
putLabel !label = VerseT $ \ _level count _heap Mem { label = _, .. } _yk sk ->
  sk count Mem {..} ()

runVerseT :: (MonadRef m, Vars a m) => VerseT m a -> m (Maybe [a])
{-# INLINABLE runVerseT #-}
runVerseT m = do
  let
    sk count mem x fk _ek
      | count == 0 = runFindT (findVars' x) 0 mem.label >>= \ case
        Nothing -> pure Nothing
        Just (x, label) ->
          fmap (x:) <$>
          fk heap (splitMem label mem.varMinBound)
      | otherwise = pure Nothing
  unVerseT m level count heap (splitMem label varMinBound) yk sk fk ek
  where
    heap = newVar' ()
    label = minBound + 1
    varMinBound = label
    level = 1
    count = 0
    yk = Yield $ \ _i _f _count _mem _sk _fk _ek -> pure Nothing
    fk _heap _mem = pure $ Just []
    ek _mem = pure $ Just []

all' :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m [a]
{-# INLINABLE all' #-}
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> fmap (x:) . loop =<< m

one :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m a
{-# INLINE one #-}
one = split >=> \ case
  Done -> empty
  Step x _m -> pure x

if'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> (a -> VerseT m b) -> VerseT m b -> VerseT m b
{-# INLINE if' #-}
if' m f n = split m >>= \ case
  Done -> n
  Step x _m -> f x

split :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m (Stream m a)
{-# INLINE split #-}
split m = split' m 0

split'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> Count -> VerseT m (Stream m a)
{-# INLINE split' #-}
split' m count = split'' m count succeedS failS emptyS

split''
  :: Monad m
  => VerseT m a
  -> Count
  -> Succeed (VerseT m b) m a
  -> Fail (VerseT m b) m
  -> Empty (VerseT m b) m
  -> VerseT m b
{-# INLINABLE split'' #-}
split'' m count' sk' fk' ek' = VerseT $ \ level count heap mem yk sk fk ek ->
  let
    !level' = level + 1
    !mem' = splitMem mem.label mem.varMinBound
  in
    unVerseT m level' count' heap mem' yieldS sk' fk' ek' >>= \ m ->
    unVerseT m level count heap mem yk sk fk ek

yieldS :: Monad m => Yield (VerseT m a) m
{-# INLINABLE yieldS #-}
yieldS = Yield $ \ i f count mem sk fk ek -> pure $ do
  whenM ((mem.varMin <) <$> getVarMinBound) $
    tellVar mem.varMin mem.forward mem.backward
  whenM ((mem.refMin <) <$> getRefMinBound) $
    tellRef mem.refMin mem.backward'
  putLabel mem.label
  level <- getLevel
  if i > level then
    stuck
  else
    yield i $ \ k -> f $ \ m -> k $ split'' m count sk fk ek

succeedS :: (MonadRef m, Vars a m) => Succeed (VerseT m (Stream m a)) m a
{-# INLINABLE succeedS #-}
succeedS count mem x fk _ek = pure $ do
  whenM ((mem.varMin <) <$> getVarMinBound) $
    tellVar mem.varMin mem.forward mem.backward
  whenM ((mem.refMin <) <$> getRefMinBound) $
    tellRef mem.refMin mem.backward'
  if count == 0 then do
    level <- getLevel
    lift (runFindT (findVars' x) level mem.label) >>= \ case
      Nothing -> do
        putLabel mem.label
        stuck
      Just (x, label) -> do
        putLabel label
        pure . Step x $ liftFailS fk
  else do
    putLabel mem.label
    stuck

failS :: Monad m => Fail (VerseT m (Stream m a)) m
{-# INLINE failS #-}
failS _heap = emptyS

emptyS :: Monad m => Empty (VerseT m (Stream m a)) m
{-# INLINABLE emptyS #-}
emptyS mem = pure $ do
  whenM ((mem.varMin <) <$> getVarMinBound) $
    tellVar mem.varMin mem.forward mem.backward
  whenM ((mem.refMin <) <$> getRefMinBound) $
    tellRef mem.refMin mem.backward'
  putLabel mem.label
  pure Done

liftFailS :: Monad m => Fail (VerseT m a) m -> VerseT m a
{-# INLINABLE liftFailS #-}
liftFailS fk' = VerseT $ \ level count heap mem yk sk fk ek -> do
  m <- fk' heap $ splitMem mem.label mem.varMinBound
  unVerseT m level count heap mem yk sk fk ek

splitMem :: Applicative m => Label -> Label -> Mem m
{-# INLINE splitMem #-}
splitMem label varMinBound = Mem {..}
  where
    varMin = maxBound
    refMin = maxBound
    refMinBound = label
    forward = pure ()
    backward = pure ()
    backward' = pure ()

data Stream m a = Done | Step !a (VerseT m (Stream m a))

fork :: Monad m => VerseT m () -> VerseT m ()
{-# INLINE fork #-}
fork m = fork' m succeedF

fork' :: Monad m => VerseT m a -> Succeed (VerseT m ()) m a -> VerseT m ()
{-# INLINABLE fork' #-}
fork' m sk' = VerseT $ \ level count heap mem yk sk fk ek ->
  unVerseT m level count heap mem yieldF sk' failF emptyF >>= \ m ->
  unVerseT m level count heap mem yk sk fk ek

yieldF :: Monad m => Yield (VerseT m ()) m
{-# INLINABLE yieldF #-}
yieldF = Yield $ \ i f count mem sk fk ek -> pure $ do
  putCount count
  putMem mem
  level <- getLevel
  if i < level then
    altF (yield i (\ k -> f $ \ m -> k $ fork' m sk)) fk ek
  else do
    incrCount
    altF (f $ \ m -> decrCount *> fork' m sk) fk ek

fork1
  :: (MonadWeakRef m, ZipVars_ a m)
  => VerseT m (Var m a) -> VerseT m (Var m a)
{-# INLINE fork1 #-}
fork1 m = fork1' m succeedF

fork1'
  :: (MonadWeakRef m, ZipVars_ b m)
  => VerseT m a
  -> Succeed (VerseT m (Var m b)) m a
  -> VerseT m (Var m b)
{-# INLINABLE fork1' #-}
fork1' m sk' = VerseT $ \ level count heap mem yk sk fk ek ->
  unVerseT m level count heap mem yieldF1 sk' failF emptyF >>= \ m ->
  unVerseT m level count heap mem yk sk fk ek

yieldF1
  :: (MonadWeakRef m, ZipVars_ a m)
  => Yield (VerseT m (Var m a)) m
{-# INLINABLE yieldF1 #-}
yieldF1 = Yield $ \ i f count mem sk fk ek -> pure $ do
  putCount count
  putMem mem
  level <- getLevel
  if i < level then
    altF (yield i (\ k -> f $ \ m -> k $ fork1' m sk)) fk ek
  else do
    incrCount
    var <- freshVar
    altF
      (f (\ m -> decrCount *> fork (m >>= liftSucceedF1 sk var)) $> var)
      fk
      ek

liftSucceedF1
  :: (MonadWeakRef m, ZipVars_ b m)
  => Succeed (VerseT m (Var m b)) m a -> Var m b -> a -> VerseT m ()
{-# INLINABLE liftSucceedF1 #-}
liftSucceedF1 sk' var x = VerseT $ \ level count heap mem yk sk fk ek ->
  sk' count mem x failF emptyF >>= \ m ->
  unVerseT (m >>= unifyVar var) level count heap mem yk sk fk ek

fork2
  :: (MonadWeakRef m, ZipVars_ a m, ZipVars_ b m)
  => VerseT m (Var m a, Var m b) -> VerseT m (Var m a, Var m b)
{-# INLINE fork2 #-}
fork2 m = fork2' m succeedF

fork2'
  :: (MonadWeakRef m, ZipVars_ b m, ZipVars_ c m)
  => VerseT m a
  -> Succeed (VerseT m (Var m b, Var m c)) m a
  -> VerseT m (Var m b, Var m c)
{-# INLINABLE fork2' #-}
fork2' m sk' = VerseT $ \ level count heap mem yk sk fk ek ->
  unVerseT m level count heap mem yieldF2 sk' failF emptyF >>= \ m ->
  unVerseT m level count heap mem yk sk fk ek

yieldF2
  :: (MonadWeakRef m, ZipVars_ a m, ZipVars_ b m)
  => Yield (VerseT m (Var m a, Var m b)) m
{-# INLINABLE yieldF2 #-}
yieldF2 = Yield $ \ i f count mem sk fk ek -> pure $ do
  putCount count
  putMem mem
  level <- getLevel
  if i < level then
    altF (yield i (\ k -> f $ \ m -> k $ fork2' m sk)) fk ek
  else do
    incrCount
    vars <- (,) <$> freshVar <*> freshVar
    altF
      (f (\ m -> decrCount *> fork (m >>= liftSucceedF2 sk vars)) $> vars)
      fk
      ek

liftSucceedF2
  :: (MonadWeakRef m, ZipVars_ b m, ZipVars_ c m)
  => Succeed (VerseT m (Var m b, Var m c)) m a
  -> (Var m b, Var m c)
  -> a
  -> VerseT m ()
{-# INLINABLE liftSucceedF2 #-}
liftSucceedF2 sk' vars x = VerseT $ \ level count heap mem yk sk fk ek ->
  sk' count mem x failF emptyF >>= \ m ->
  unVerseT (m >>= unifyVar2 vars) level count heap mem yk sk fk ek
  where
    unifyVar2 (a, b) = \ (c, d) -> unifyVar a c *> unifyVar b d

fork3
  :: (MonadWeakRef m, ZipVars_ a m, ZipVars_ b m, ZipVars_ c m)
  => VerseT m (Var m a, Var m b, Var m c)
  -> VerseT m (Var m a, Var m b, Var m c)
{-# INLINE fork3 #-}
fork3 m = fork3' m succeedF

fork3'
  :: (MonadWeakRef m, ZipVars_ b m, ZipVars_ c m, ZipVars_ d m)
  => VerseT m a
  -> Succeed (VerseT m (Var m b, Var m c, Var m d)) m a
  -> VerseT m (Var m b, Var m c, Var m d)
{-# INLINABLE fork3' #-}
fork3' m sk' = VerseT $ \ level count heap mem yk sk fk ek ->
  unVerseT m level count heap mem yieldF3 sk' failF emptyF >>= \ m ->
  unVerseT m level count heap mem yk sk fk ek

yieldF3
  :: (MonadWeakRef m, ZipVars_ a m, ZipVars_ b m, ZipVars_ c m)
  => Yield (VerseT m (Var m a, Var m b, Var m c)) m
{-# INLINABLE yieldF3 #-}
yieldF3 = Yield $ \ i f count mem sk fk ek -> pure $ do
  putCount count
  putMem mem
  level <- getLevel
  if i < level then
    altF (yield i (\ k -> f $ \ m -> k $ fork3' m sk)) fk ek
  else do
    incrCount
    vars <- (,,) <$> freshVar <*> freshVar <*> freshVar
    altF
      (f (\ m -> decrCount *> fork (m >>= liftSucceedF3 sk vars)) $> vars)
      fk
      ek

liftSucceedF3
  :: (MonadWeakRef m, ZipVars_ b m, ZipVars_ c m, ZipVars_ d m)
  => Succeed (VerseT m (Var m b, Var m c, Var m d)) m a
  -> (Var m b, Var m c, Var m d)
  -> a
  -> VerseT m ()
{-# INLINABLE liftSucceedF3 #-}
liftSucceedF3 sk' vars x = VerseT $ \ level count heap mem yk sk fk ek ->
  sk' count mem x failF emptyF >>= \ m ->
  unVerseT (m >>= unifyVar3 vars) level count heap mem yk sk fk ek
  where
    unifyVar3 (a, b, c) = \ (d, e, f) ->
      unifyVar a d *> unifyVar b e *> unifyVar c f

succeedF :: Monad m => Succeed (VerseT m a) m a
{-# INLINABLE succeedF #-}
succeedF count mem x fk ek = pure $ do
  putCount count
  putMem mem
  altF (pure x) fk ek

failF :: Applicative m => Fail (VerseT m a) m
{-# INLINE failF #-}
failF _heap = emptyF

emptyF :: Applicative m => Empty (VerseT m a) m
{-# INLINE emptyF #-}
emptyF mem = pure $ do
  putMem mem
  empty

altF
  :: Monad m
  => VerseT m a
  -> Fail (VerseT m a) m
  -> Empty (VerseT m a) m
  -> VerseT m a
{-# INLINABLE altF #-}
altF m fk' ek' = VerseT $ \ level count heap mem yk sk fk ek ->
  unVerseT m level count heap mem yk sk
  (\ heap mem -> fk' heap mem >>= \ m ->
     unVerseT m level count heap mem yk sk fk $ fk heap)
  (\ mem -> ek' mem >>= \ m ->
     unVerseT m level count heap mem yk sk fk ek)

stuck :: VerseT m a
{-# INLINE stuck #-}
stuck = VerseT $ \ level count _heap mem yk ->
  unYield yk level (const $ pure ()) count mem

data Var m a
  = Ref {-# UNPACK #-} !(Ref m (RefState m a))
  | Bound !a !Bool

class Vars a m where
  vars
    :: Applicative f
    => (forall b . Vars b m => Var m b -> f (Var m b))
    -> a -> f a

instance Vars a m => Vars (Var m a) m where
  vars f = f

instance Vars Bool m where
  vars _ = pure

instance Vars Char m where
  vars _ = pure

instance Vars Integer m where
  vars _ = pure

instance Vars () m where
  vars _ = pure

instance Vars a m => Vars (Solo a) m where
  vars f = traverse (vars f)

instance (Vars a m, Vars b m) => Vars (a, b) m where
  vars f (a, b) =
    (,) <$> vars f a <*> vars f b

instance (Vars a m, Vars b m, Vars c m) => Vars (a, b, c) m where
  vars f (a, b, c) =
    (,,) <$> vars f a <*> vars f b <*> vars f c

instance (Vars a m, Vars b m, Vars c m, Vars d m) => Vars (a, b, c, d) m where
  vars f (a, b, c, d) =
    (,,,) <$> vars f a <*> vars f b <*> vars f c <*> vars f d

instance ( Vars a m
         , Vars b m
         , Vars c m
         , Vars d m
         , Vars e m
         ) => Vars (a, b, c, d, e) m where
  vars f (a, b, c, d, e) =
    (,,,,) <$> vars f a <*> vars f b <*> vars f c <*> vars f d <*> vars f e

instance ( Vars a m
         , Vars b m
         , Vars c m
         , Vars d m
         , Vars e m
         , Vars g m
         ) => Vars (a, b, c, d, e, g) m where
  vars f (a, b, c, d, e, g) =
    (,,,,,)
    <$> vars f a
    <*> vars f b
    <*> vars f c
    <*> vars f d
    <*> vars f e
    <*> vars f g


instance ( Vars a m
         , Vars b m
         , Vars c m
         , Vars d m
         , Vars e m
         , Vars g m
         , Vars h m
         ) => Vars (a, b, c, d, e, g, h) m where
  vars f (a, b, c, d, e, g, h) =
    (,,,,,,)
    <$> vars f a
    <*> vars f b
    <*> vars f c
    <*> vars f d
    <*> vars f e
    <*> vars f g
    <*> vars f h

instance Vars a m => Vars (Maybe a) m where
  vars f = \ case
    Nothing -> pure Nothing
    Just x -> Just <$> vars f x

instance Vars a m => Vars [a] m where
  vars f = traverse (vars f)

instance Vars a m => Vars (Vector a) m where
  vars f = traverse (vars f)

instance Vars (f (g a)) m => Vars (Compose f g a) m where
  vars f = fmap Compose . vars f . getCompose

instance Vars (f (Fix f)) m => Vars (Fix f) m where
  vars f = fmap Fix . vars f . getFix

instance Vars v m => Vars (Strict.HashMap k v) m where
  vars f = traverse (vars f)

class ZipVars_ a m where
  zipVars_
    :: Alternative f
    => (forall b . ZipVars_ b m => Var m b -> Var m b -> f ())
    -> a -> a -> f ()

instance ZipVars_ a m => ZipVars_ (Var m a) m where
  zipVars_ f = f

instance ZipVars_ () m where
  zipVars_ _ () () = pure ()

instance (ZipVars_ a m, ZipVars_ b m) => ZipVars_ (a, b) m where
  zipVars_ f (x1, y1) (x2, y2) =
    zipVars_ f x1 x2 *>
    zipVars_ f y1 y2

instance ZipVars_ Bool m where
  zipVars_ _ = curry $ \ case
    (False, False) -> pure ()
    (True, True) -> pure ()
    _ -> empty

instance ZipVars_ a m => ZipVars_ (Maybe a) m where
  zipVars_ f = curry $ \ case
    (Nothing, Nothing) -> pure ()
    (Just x, Just y) -> zipVars_ f x y
    _ -> empty

instance ZipVars_ a m => ZipVars_ [a] m where
  zipVars_ f = curry $ \ case
    ([], []) -> pure ()
    (x:xs, y:ys) -> zipVars_ f x y *> zipVars_ f xs ys
    _ -> empty

instance ZipVars_ a m => ZipVars_ (Vector a) m where
  zipVars_ f x y
    | length x /= length y = empty
    | otherwise = traverse_ (uncurry $ zipVars_ f) $ mzip x y

instance ZipVars_ (f (g a)) m => ZipVars_ (Compose f g a) m where
  zipVars_ f = zipVars_ f `on` getCompose

instance ZipVars_ (f (Fix f)) m => ZipVars_ (Fix f) m where
  zipVars_ f = zipVars_ f `on` getFix

data RefState m a
  = Unbound !(Unbound m a)
  | Link !(Var m a)

data Root m a
  = UnboundR !(Ref m (RefState m a)) !(Unbound m a)
  | BoundR !a

data Unbound m a = MkUnbound
  { label :: {-# UNPACK #-} !Label
  , level :: {-# UNPACK #-} !Level
  , susp :: !(Var m a -> Ap (VerseT m) ())
  }

freshVar :: MonadRef m => VerseT m (Var m a)
{-# INLINABLE freshVar #-}
freshVar = do
  label <- supply
  level <- getLevel
  lift . fmap Ref . newRef $! Unbound MkUnbound {..}
  where
    susp = const $ pure ()

newVar :: Vars a m => a -> VerseT m (Var m a)
{-# INLINE newVar #-}
newVar = pure . newVar'

newVar' :: forall a m . Vars a m => a -> Var m a
{-# INLINE newVar' #-}
newVar' x = Bound x $ anyRefs (Proxy @m) x

anyRefs :: forall a m . Vars a m => Proxy m -> a -> Bool
{-# INLINE anyRefs #-}
anyRefs _ = getAny . getConst . vars @a @m f
  where
    f = \ case
      Ref _ -> Const $ Any True
      Bound _ anyRefs -> Const $ Any anyRefs

readVar :: MonadWeakRef m => Var m a -> VerseT m a
{-# INLINE readVar #-}
readVar = \ case
  Bound binding _ -> pure binding
  Ref ref -> readRefBinding ref

readVar' :: MonadRef m => Var m a -> VerseT m (Maybe a)
readVar' = lift . readVar''

readVar'' :: MonadRef m => Var m a -> m (Maybe a)
readVar'' = \ case
  Bound binding _ -> pure $ Just binding
  Ref ref -> readRef ref >>= \ case
    Link var -> readVar'' var
    Unbound _ -> pure Nothing

readRefBinding :: MonadWeakRef m => Ref m (RefState m a) -> VerseT m a
{-# INLINABLE readRefBinding #-}
readRefBinding ref = lift (readRef ref) >>= \ case
  Link var -> readVar var
  Unbound x -> readVar =<< readRefLink ref x

readRefLink
  :: MonadWeakRef m
  => Ref m (RefState m a)
  -> Unbound m a
  -> VerseT m (Var m a)
{-# INLINE readRefLink #-}
readRefLink ref x = yield x.level $ \ f ->
  let
    !susp = x.susp <> Ap . f . pure
  in
    writeRefState ref x.label $ Unbound x { susp }

unifyVar :: (MonadWeakRef m, ZipVars_ a m) => Var m a -> Var m a -> VerseT m ()
{-# INLINABLE unifyVar #-}
unifyVar var1 var2 = (,) <$> readRoot var1 <*> readRoot var2 >>= \ case
  ((var1, UnboundR ref1 x1), (var2, UnboundR ref2 x2)) ->
    when (x1.label /= x2.label) $ do
      level <- getLevel
      if x1.level < level then
        if x2.level < level then
          if x1.label < x2.label then do
            var2 <- readRefLink ref2 x2
            unifyVar var1 var2
          else do
            var1 <- readRefLink ref1 x1
            unifyVar var1 var2
        else do
          writeRefState ref2 x2.label $ Link var1
          getAp $ x2.susp var1
      else if x2.level < level then do
        writeRefState ref1 x1.label $ Link var2
        getAp $ x1.susp var2
      else if x1.label < x2.label then do
        writeRefState ref2 x2.label $ Link var1
        getAp $ x2.susp var1
      else do
        writeRefState ref1 x1.label $ Link var2
        getAp $ x1.susp var2
  ((_var1, UnboundR ref1 x1), (var2, BoundR binding2)) -> do
    level <- getLevel
    if x1.level < level then do
      binding1 <- readRefBinding ref1
      zipVars_ unifyVar binding1 binding2
    else do
      writeRefState ref1 x1.label $ Link var2
      getAp $ x1.susp var2
  ((var1, BoundR binding1), (_var2, UnboundR ref2 x2)) -> do
    level <- getLevel
    if x2.level < level then do
      binding2 <- readRefBinding ref2
      zipVars_ unifyVar binding1 binding2
    else do
      writeRefState ref2 x2.label $ Link var1
      getAp $ x2.susp var1
  ((_var1, BoundR binding1), (_var2, BoundR binding2)) ->
    zipVars_ unifyVar binding1 binding2

writeRefState
  :: MonadWeakRef m
  => Ref m (RefState m a)
  -> Label
  -> RefState m a
  -> VerseT m ()
{-# INLINABLE writeRefState #-}
writeRefState ref label !x = (label <) <$> getVarMinBound >>= \ case
  False -> lift $ writeRef ref x
  True -> uncurry (tellVar label) <=< lift $ do
    y <- readRef ref
    let forward = writeRef ref x
    forward
    forward <- newWeakTrail ref forward
    backward <- newWeakTrail ref $ writeRef ref y
    pure (forward, backward)

readRoot :: MonadRef m => Var m a -> VerseT m (Var m a, Root m a)
{-# INLINABLE readRoot #-}
readRoot var = case var of
  Bound binding _ -> pure (var, BoundR binding)
  Ref ref -> lift (readRef ref) >>= \ case
    Link var -> readRoot var
    Unbound x -> pure (var, UnboundR ref x)

data VarsRef m a = VarsRef {-# UNPACK #-} !Label {-# UNPACK #-} !(Ref m a)

instance Eq (VarsRef m a) where
  {-# INLINE (==) #-}
  VarsRef _ x == VarsRef _ y = x == y
  {-# INLINE (/=) #-}
  VarsRef _ x /= VarsRef _ y = x /= y

newVarsRef :: (MonadWeakRef m, Vars a m) => a -> VerseT m (VarsRef m a)
{-# INLINE newVarsRef #-}
newVarsRef x = do
  label <- supply
  lift . fmap (VarsRef label) . newRef =<< findVars x

readVarsRef :: MonadRef m => VarsRef m a -> VerseT m a
{-# INLINE readVarsRef #-}
readVarsRef (VarsRef _ ref) = lift $ readRef ref

writeVarsRef :: (MonadWeakRef m, Vars a m) => VarsRef m a -> a -> VerseT m ()
{-# INLINABLE writeVarsRef #-}
writeVarsRef (VarsRef label ref) x = (label <) <$> getRefMinBound >>= \ case
  False -> lift . writeRef ref =<< findVars x
  True -> do
    x <- findVars x
    tellRef label <=< lift $ do
      y <- readRef ref
      writeRef ref x
      newWeakTrail ref $ writeRef ref y

findVars :: (MonadWeakRef m, Vars a m) => a -> VerseT m a
{-# INLINE findVars #-}
findVars = vars findVar

findVar :: (MonadWeakRef m, Vars a m) => Var m a -> VerseT m (Var m a)
{-# INLINABLE findVar #-}
findVar = \ case
  var@(Bound binding anyRefs)
    | anyRefs -> newVar' <$> findVars binding
    | otherwise -> pure var
  var@(Ref ref) -> lift (readRef ref) >>= \ case
    Link var -> findVar var
    Unbound x -> (x.level <) <$> getLevel >>= \ case
      True -> pure var
      False -> findVar =<< readRefLink ref x

newtype FindT m a = FindT
  { unFindT :: In -> m (Out a)
  }

data In = In
  { level :: {-# UNPACK #-} !Level
  , label :: {-# UNPACK #-} !Label
  }

data Out a = Err | Out !a {-# UNPACK #-} !Label

instance Functor m => Functor (FindT m) where
  {-# INLINE fmap #-}
  fmap f x = FindT $ \ s -> unFindT x s <&> \ case
    Err -> Err
    Out x label -> Out (f x) label

instance Monad m => Applicative (FindT m) where
  {-# INLINE pure #-}
  pure x = FindT $ \ In {..} -> pure $! Out x label
  {-# INLINE (<*>) #-}
  f <*> x = FindT $ \ s@In {..} -> unFindT f s >>= \ case
    Err -> pure Err
    Out f label -> unFindT x In {..} <&> \ case
      Err -> Err
      Out x label -> Out (f x) label

instance Monad m => Monad (FindT m) where
  {-# INLINE (>>=) #-}
  x >>= f = FindT $ \ s@In {..} -> unFindT x s >>= \ case
    Err -> pure Err
    Out x label -> unFindT (f x) In {..}

instance MonadTrans FindT where
  {-# INLINE lift #-}
  lift m = FindT $ \ In {..} -> m <&> \ x -> Out x label

type instance World (FindT m) = World m

instance MonadRef m => MonadRef (FindT m) where
  {-# INLINE newRef #-}
  newRef = lift . newRef
  {-# INLINE readRef #-}
  readRef = lift . readRef
  {-# INLINE writeRef #-}
  writeRef = (lift .) . writeRef

runFindT :: Functor m => FindT m a -> Level -> Label -> m (Maybe (a, Label))
{-# INLINE runFindT #-}
runFindT m level label = unFindT m In {..} <&> \ case
  Err -> Nothing
  Out x label -> Just (x, label)

findVars' :: (MonadRef m, Vars a m) => a -> FindT m a
{-# INLINE findVars' #-}
findVars' = vars findVar'

findVar' :: (MonadRef m, Vars a m) => Var m a -> FindT m (Var m a)
{-# INLINABLE findVar' #-}
findVar' = \ case
  var@(Bound binding anyRefs)
    | anyRefs -> newVar' <$> findVars' binding
    | otherwise -> pure var
  var@(Ref ref) -> readRef ref >>= \ case
    Link var -> findVar' var
    Unbound x -> FindT $ \ In {..} ->
      pure $! if x.level <= level then Out var label else Err

bracket :: Monad m => m () -> m () -> FindT m a -> FindT m a
{-# INLINABLE bracket #-}
bracket x y z = FindT $ \ s -> x *> unFindT z s >>= \ case
  Err -> y $> Err
  Out z label -> y $> Out z label

whenM :: Monad m => m Bool -> m () -> m ()
{-# INLINE whenM #-}
whenM x y = x >>= flip when y
