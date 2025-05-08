{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
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
  , stuck
  , Var
  , Vars (..)
  , ZipVars_ (..)
  , freshVar
  , newVar
  , readVar
  , unifyVar
  , VarsRef
  , newVarsRef
  , readVarsRef
  , writeVarsRef
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class

import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.HashMap.Strict qualified as Strict (HashMap)
import Data.Kind
import Data.Monoid (Ap (..), Sum (..))

import GHC.Exts (Any)

import Unsafe.Coerce (unsafeCoerce)

import Fix
import IntMap (IntMap, (!))
import IntMap qualified
import Ref

newtype VerseT (m :: Type -> Type) a = VerseT
  { unVerseT
    :: forall r . R
    -> S
    -> Env m
    -> Mem m
    -> Yield r m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> m r
  }

newtype R = R { level :: Level }

type Level = Sum Int

newtype S = S { count :: Int }

succS :: S -> S
succS !s = s { count = s.count + 1 }

predS :: S -> S
predS !s = s { count = s.count - 1 }

type Env m = Var m ()

data Mem m = Mem
  { label :: {-# UNPACK #-} !Label
  , heap :: !(Heap m)
  , forward :: !(m ())
  , backward :: !(m ())
  }

appendMem :: Applicative m => Mem m -> m () -> m () -> Mem m
appendMem mem forward backward = mem
  { forward = mem.forward *> forward
  , backward = backward *> mem.backward
  }

type Label = Int

type Heap m = IntMap (SomeVars m)

data SomeVars m = forall a . Vars a m => SomeVars !a

newtype Yield r m = Yield
  { unYield
    :: forall a . Level
    -> Handler m a
    -> S
    -> Mem m
    -> Succeed r m a
    -> Fail r m
    -> Empty r m
    -> m r
  }

type Handler m a = (VerseT m a -> VerseT m ()) -> VerseT m ()

type Succeed r m a = S -> Mem m -> a -> Fail r m -> Empty r m -> m r

type Fail r m = Env m -> Mem m -> m r

type Empty r m = Mem m -> m r

instance Functor (VerseT m) where
  fmap f m = VerseT $ \ r s env mem yk sk ->
    unVerseT m r s env mem yk $ \ s mem -> sk s mem . f

instance Applicative (VerseT m) where
  pure x = VerseT $ \ _r s _env mem _yk sk ->
    sk s mem x
  f <*> x = VerseT $ \ r s env mem yk sk ->
    unVerseT f r s env mem yk $ \ s mem f ->
    unVerseT x r s env mem yk $ \ s mem x ->
    sk s mem $ f x

instance Alternative (VerseT m) where
  empty = VerseT $ \ _r _s _env mem _yk _sk _fk ek ->
    ek mem
  x <|> y = VerseT $ \ r s env mem yk sk fk ek ->
    unVerseT x r s env mem yk sk
    (\ env mem -> unVerseT y r s env mem yk sk fk $ fk env)
    (\ mem -> unVerseT y r s env mem yk sk fk ek)

alt :: VerseT m a -> VerseT m a -> VerseT m a -> VerseT m a
alt x y z = VerseT $ \ r s env mem yk sk fk ek ->
  unVerseT x r s env mem yk sk
  (\ env mem -> unVerseT y r s env mem yk sk fk $ fk env)
  (\ mem -> unVerseT z r s env mem yk sk fk ek)

instance Monad (VerseT m) where
  x >>= f = VerseT $ \ r s env mem yk sk ->
    unVerseT x r s env mem yk $ \ s mem x ->
    unVerseT (f x) r s env mem yk sk

instance MonadPlus (VerseT m)

instance MonadTrans VerseT where
  lift m = VerseT $ \ _r s _env mem _yk sk fk ek ->
    m >>= \ x -> sk s mem x fk ek

instance MonadIO m => MonadIO (VerseT m) where
  liftIO = lift . liftIO

instance MonadReader (Var m ()) (VerseT m) where
  ask = VerseT $ \ _r s env mem _yk sk ->
    sk s mem env
  local f m = VerseT $ \ r s env mem yk sk fk ek ->
    unVerseT m r s (f env) mem yk sk fk ek
  reader f = VerseT $ \ _r s env mem _yk sk ->
    sk s mem $ f env

instance MonadState s m => MonadState s (VerseT m) where
  get = lift get
  put = lift . put
  state = lift . state

liftPut :: Monad m => m () -> m () -> VerseT m ()
liftPut forward backward = do
  lift forward
  tell forward backward

tell :: Applicative m => m () -> m () -> VerseT m ()
tell forward backward = VerseT $ \ _r s _env mem _yk sk fk ek ->
  sk s (appendMem mem forward backward) ()
  (\ env mem -> backward *> fk env (appendMem mem backward forward))
  (\ mem -> backward *> ek (appendMem mem backward forward))

yield :: Level -> Handler m a -> VerseT m a
yield i f = VerseT $ \ _r s _env mem yk ->
  unYield yk i f s mem

getLevel :: VerseT m Level
getLevel = VerseT $ \ r s _env mem _yk sk ->
  sk s mem r.level

putS :: S -> VerseT m ()
putS !s = VerseT $ \ _r _s _env mem _yk sk ->
  sk s mem ()

modifyS :: (S -> S) -> VerseT m ()
modifyS f = VerseT $ \ _r s _env mem _yk sk ->
  let
    !s' = f s
  in
    sk s' mem ()

putMem :: Mem m -> VerseT m ()
putMem mem = VerseT $ \ _r s _env _mem _yk sk ->
  sk s mem ()

putLabel :: Label -> VerseT m ()
putLabel label = VerseT $ \ _r s _env Mem { heap, forward, backward } _yk sk ->
  sk s Mem {..} ()

getHeap :: VerseT m (Heap m)
getHeap = VerseT $ \ _r s _env mem _yk sk ->
  sk s mem mem.heap

putHeap :: Heap m -> VerseT m ()
putHeap heap = VerseT $ \ _r s _env mem _yk sk ->
  sk s mem { heap } ()

runVerseT :: (MonadRef m, Vars a m) => VerseT m a -> m (Maybe [a])
runVerseT m = do
  (env, label) <- newVar' () 0
  let
    sk s mem x fk _ek
      | s.count == 0 =
        runFindT (findVars (x, mem.heap)) 0 mem.label >>= \ case
          Nothing -> pure Nothing
          Just ((x, heap), label) -> fmap (x:) <$> fk env Mem {..}
      | otherwise = pure Nothing
  unVerseT m r s env Mem {..} yk sk fk ek
  where
    r = R { level = 1 }
    s = S { count = 0 }
    heap = mempty
    forward = pure ()
    backward = pure ()
    yk = Yield $ \ _i _f _s _mem _sk _fk _ek -> pure Nothing
    fk _env _mem = pure $ Just []
    ek _mem = pure $ Just []

all' :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m [a]
all' = split >=> loop
  where
    loop = \ case
      Done -> pure []
      Step x m -> (x:) <$> (m >>= loop)

one :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m a
one = split >=> \ case
  Done -> empty
  Step x _m -> pure x

if'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> (a -> VerseT m b) -> VerseT m b -> VerseT m b
if' m f n = split m >>= \ case
  Done -> n
  Step x _m -> f x

split :: (MonadRef m, Vars a m) => VerseT m a -> VerseT m (Stream m a)
split m = split' m 0 =<< getHeap

split'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> Int -> Heap m -> VerseT m (Stream m a)
split' m count heap = splitS m count heap >>= \ case
  FailS mem -> do
    tell mem.forward mem.backward
    putLabel mem.label
    pure Done
  YieldS i f s mem f_s m_f m_e -> do
    tell mem.forward mem.backward
    putLabel mem.label
    level <- getLevel
    if i > level then
      stuck
    else
      yield i $ \ k ->
      f $ \ m -> k $ split' (alt (m >>= f_s) m_f m_e) s.count mem.heap
  SucceedS s mem x m_f _m_e -> do
    tell mem.forward mem.backward
    if s.count == 0 then do
      level <- getLevel
      lift (runFindT (findVars (mem.heap, x)) level mem.label) >>= \ case
        Nothing -> do
          putLabel mem.label
          stuck
        Just ((heap, x), label) -> do
          putLabel label
          putHeap heap
          pure . Step x $ split m_f
    else do
      putLabel mem.label
      stuck

data Stream m a = Done | Step a (VerseT m (Stream m a))

splitS :: Monad m => VerseT m a -> Int -> Heap m -> VerseT m (Split m a)
splitS m !count !heap = VerseT $ \ r s env mem _yk sk fk ek ->
  let
    !r' = R { level = r.level <> 1 }
    !s' = S { count }
    !mem' = mem { heap, forward = pure (), backward = pure () }
  in
    unVerseT m r' s' env mem' yieldS succeedS failS emptyS >>= \ x ->
    sk s mem x fk ek

yieldS :: Monad m => Yield (Split m a) m
yieldS = Yield $ \ i f s mem sk fk ek ->
  pure $
  YieldS i f s mem
  (liftS sk >=> reflectS)
  (liftF fk >>= reflectS)
  (liftE ek >>= reflectS)

succeedS :: Monad m => Succeed (Split m a) m a
succeedS s mem x fk ek =
  pure $
  SucceedS s mem x
  (liftF fk >>= reflectS)
  (liftE ek >>= reflectS)

failS :: Applicative m => Fail (Split m a) m
failS _env = pure . FailS

emptyS :: Applicative m => Empty (Split m a) m
emptyS = pure . FailS

reflectS :: Split m a -> VerseT m a
reflectS = \ case
  FailS mem ->
    reflectFailS mem
  SucceedS s mem x m_f m_e ->
    reflectSucceedS s mem x m_f m_e
  YieldS i f s mem f_s m_f m_e -> do
    putS s
    putMem mem
    alt (yield i $ \ k -> f $ \ m -> k $ m >>= f_s) m_f m_e

reflectSucceedS :: S -> Mem m -> a -> VerseT m a -> VerseT m a -> VerseT m a
reflectSucceedS s mem x m_f m_e = do
  putS s
  putMem mem
  alt (pure x) m_f m_e

reflectFailS :: Mem m -> VerseT m a
reflectFailS mem = do
  putMem mem
  empty

fork :: Monad m => VerseT m () -> VerseT m ()
fork m = forkS m >>= reflectF

forkS :: Monad m => VerseT m () -> VerseT m (Split m ())
forkS m = VerseT $ \ r s env mem _yk sk fk ek ->
  unVerseT m r s env mem yieldF succeedF failS emptyS >>= \ x ->
  sk s mem x fk ek

reflectF :: Monad m => Split m () -> VerseT m ()
reflectF = \ case
  FailS mem ->
    reflectFailS mem
  SucceedS s mem () m_f m_e ->
    reflectSucceedS s mem () m_f m_e
  YieldS i f s mem f_s m_f m_e -> do
    putMem mem
    level <- getLevel
    if i < level then do
      putS s
      alt (yield i (\ k -> f $ \ m -> k . fork $ m >>= f_s)) m_f m_e
    else do
      putS $ succS s
      alt (f $ \ m -> modifyS predS >> fork (m >>= f_s)) m_f m_e

yieldF :: Monad m => Yield (Split m ()) m
yieldF = Yield $ \ i f s mem sk fk ek ->
  pure $
  YieldS i f s mem
  (liftS sk >=> reflectS)
  (liftF fk >>= reflectF)
  (liftE ek >>= reflectF)

succeedF :: Monad m => Succeed (Split m ()) m ()
succeedF s mem () fk ek =
  pure $
  SucceedS s mem ()
  (liftF fk >>= reflectF)
  (liftE ek >>= reflectF)

liftS :: Monad m => Succeed (Split m a) m b -> b -> VerseT m (Split m a)
liftS f x = VerseT $ \ _r s _env mem _yk sk fk ek ->
  f s mem x failS emptyS >>= \ x -> sk s mem x fk ek

liftF :: Monad m => Fail (Split m a) m -> VerseT m (Split m a)
liftF f = VerseT $ \ _r s env mem _yk sk fk ek ->
  f env mem >>= \ x -> sk s mem x fk ek

liftE :: Monad m => Empty (Split m a) m -> VerseT m (Split m a)
liftE f = VerseT $ \ _r s _env mem _yk sk fk ek ->
  f mem >>= \ x -> sk s mem x fk ek

data Split m a
  = forall b .
    YieldS
    {-# UNPACK #-} !Level
    !(Handler m b)
    {-# UNPACK #-} !S
    !(Mem m)
    !(b -> VerseT m a)
    !(VerseT m a)
    !(VerseT m a)
  | SucceedS
    {-# UNPACK #-} !S
    !(Mem m)
    !a
    !(VerseT m a)
    !(VerseT m a)
  | FailS
    !(Mem m)

stuck :: VerseT m a
stuck = VerseT $ \ r s _env mem yk ->
  unYield yk r.level (const $ pure ()) s mem

newtype Var m a = Var (Ref m (VarState m a))

class Vars a m where
  vars
    :: Applicative f
    => (forall b . Vars b m => Var m b -> f (Var m b))
    -> a -> f a

instance Vars a m => Vars (Var m a) m where
  vars f = f

instance Vars (Heap m) m where
  vars f = traverse $ \ (SomeVars x) -> SomeVars <$> vars f x

instance Vars Bool m where
  vars _ = pure

instance Vars Char m where
  vars _ = pure

instance Vars Integer m where
  vars _ = pure

instance Vars () m where
  vars _ = pure

instance (Vars a m, Vars b m) => Vars (a, b) m where
  vars f (x, y) = (,) <$> vars f x <*> vars f y

instance Vars a m => Vars (Maybe a) m where
  vars f = \ case
    Nothing -> pure Nothing
    Just x -> Just <$> vars f x

instance Vars a m => Vars [a] m where
  vars f = \ case
    [] -> pure []
    x:xs -> (:) <$> vars f x <*> vars f xs

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

instance ZipVars_ (f (g a)) m => ZipVars_ (Compose f g a) m where
  zipVars_ f = zipVars_ f `on` getCompose

instance ZipVars_ (f (Fix f)) m => ZipVars_ (Fix f) m where
  zipVars_ f = zipVars_ f `on` getFix

data VarState m a
  = Unbound !(Unbound m a)
  | Bound !(Bound a)
  | Link !(Var m a)

data Root m a
  = UnboundR !(Unbound m a)
  | BoundR !(Bound a)

data Unbound m a = MkUnbound
  { label :: {-# UNPACK #-} !Label
  , level :: {-# UNPACK #-} !Level
  , susp :: !(Var m a -> Ap (VerseT m) ())
  }

data Bound a = MkBound
  { label :: {-# UNPACK #-} !Label
  , binding :: !a
  }

instance ZipVars_ a m => ZipVars_ (Bound a) m where
  zipVars_ f x y
    | x.label == y.label = pure ()
    | otherwise = zipVars_ f x.binding y.binding

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = VerseT $ \ r s _env mem _yk sk fk ek ->
  let
    !label = mem.label
    !mem' = mem { label = label + 1, heap = mem.heap }
    !level = r.level
    !susp = const $ pure ()
    !x = Unbound MkUnbound {..}
  in
    newRef x >>= \ ref ->
    sk s mem' (Var ref) fk ek

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . fmap Var . newRef . Bound <=< newBound

newVar' :: MonadRef m => a -> Label -> m (Var m a, Label)
newVar' binding label =
  let
    !label' = label + 1
  in
    fmap ((, label') . Var) . newRef $ Bound MkBound {..}

newBound :: a -> VerseT m (Bound a)
newBound !binding = VerseT $ \ _r s _env mem _yk sk ->
  let
    !label = mem.label
    !mem' = mem { label = label + 1, heap = mem.heap }
    !x = MkBound {..}
  in
    sk s mem' x

readVar :: MonadRef m => Var m a -> VerseT m a
readVar = fmap (.binding) . readBound

readBound :: MonadRef m => Var m a -> VerseT m (Bound a)
readBound var@(Var ref) = lift (readRef ref) >>= \ case
  Link var -> readBound var
  Bound x -> pure x
  Unbound x -> readBound =<< readLink var x

readLink :: MonadRef m => Var m a -> Unbound m a -> VerseT m (Var m a)
readLink var x = yield x.level $ \ f ->
  let
    !label = x.label
    !level = x.level
    !susp = x.susp <> Ap . f . pure
  in
    writeVar var $ Unbound MkUnbound {..}

unifyVar :: (MonadRef m, ZipVars_ a m) => Var m a -> Var m a -> VerseT m ()
unifyVar var1 var2 = (,) <$> readRoot var1 <*> readRoot var2 >>= \ case
  ((var1, UnboundR x1), (var2, UnboundR x2)) ->
    when (x1.label /= x2.label) $ do
      level <- getLevel
      if x1.level < level then
        if x2.level < level then
          if x1.label < x2.label then do
            var2 <- readLink var2 x2
            unifyVar var1 var2
          else do
            var1 <- readLink var1 x1
            unifyVar var1 var2
        else do
          writeVar var2 $ Link var1
          getAp $ x2.susp var1
      else if x2.level < level then do
        writeVar var1 $ Link var2
        getAp $ x1.susp var2
      else if x1.label < x2.label then do
        writeVar var2 $ Link var1
        getAp $ x2.susp var1
      else do
        writeVar var1 $ Link var2
        getAp $ x1.susp var2
  ((var1, UnboundR x1), (var2, BoundR x2)) -> do
    level <- getLevel
    if x1.level < level then do
      binding1 <- readVar var1
      zipVars_ unifyVar binding1 x2.binding
    else do
      writeVar var1 $ Link var2
      getAp $ x1.susp var2
  ((var1, BoundR x1), (var2, UnboundR x2)) -> do
    level <- getLevel
    if x2.level < level then do
      binding2 <- readVar var2
      zipVars_ unifyVar x1.binding binding2
    else do
      writeVar var2 $ Link var1
      getAp $ x2.susp var1
  ((_var1, BoundR x1), (_var2, BoundR x2)) ->
    zipVars_ unifyVar x1 x2

writeVar :: MonadRef m => Var m a -> VarState m a -> VerseT m ()
writeVar (Var ref) x = do
  y <- lift $ readRef ref
  liftPut (writeRef ref x) (writeRef ref y)

readRoot :: MonadRef m => Var m a -> VerseT m (Var m a, Root m a)
readRoot var@(Var ref) = lift (readRef ref) >>= \ case
  Link var -> readRoot var
  Bound x -> pure (var, BoundR x)
  Unbound x -> pure (var, UnboundR x)

newtype FindT m a = FindT
  { unFindT :: In -> m (Out a)
  }

data In = In
  { level :: {-# UNPACK #-} !Level
  , label :: {-# UNPACK #-} !Label
  , visited :: !(IntMap Any)
  }

data Out a = Err | Out !a {-# UNPACK #-} !Label !(IntMap Any)

instance Functor m => Functor (FindT m) where
  fmap f x = FindT $ \ s -> unFindT x s <&> \ case
    Err -> Err
    Out x label visited -> Out (f x) label visited

instance Monad m => Applicative (FindT m) where
  pure x = FindT $ \ In {..} -> pure $! Out x label visited
  f <*> x = FindT $ \ s@In {..} -> unFindT f s >>= \ case
    Err -> pure Err
    Out f label visited -> unFindT x In {..} <&> \ case
      Err -> Err
      Out x label visited -> Out (f x) label visited

instance Monad m => Monad (FindT m) where
  x >>= f = FindT $ \ s@In {..} -> unFindT x s >>= \ case
    Err -> pure Err
    Out x label visited -> unFindT (f x) In {..}

instance MonadTrans FindT where
  lift m = FindT $ \ In {..} -> m <&> \ x -> Out x label visited

instance (Monad m, a ~ Any) => MonadState (IntMap a) (FindT m) where
  get = FindT $ \ In {..} -> pure $! Out visited label visited
  put visited = FindT $ \ In { label } -> pure $! Out () label visited
  state f = FindT $ \ In {..} -> case f visited of
    (x, visited) -> pure $! Out x label visited

instance MonadRef m => MonadRef (FindT m) where
  type Ref (FindT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

runFindT :: Functor m => FindT m a -> Level -> Label -> m (Maybe (a, Label))
runFindT m level label = unFindT m In {..} <&> \ case
  Err -> Nothing
  Out x label _ -> Just (x, label)
  where
    visited = mempty

findVars :: (MonadRef m, Vars a m) => a -> FindT m a
findVars = vars findVar

findVar :: (MonadRef m, Vars a m) => Var m a -> FindT m (Var m a)
findVar var@(Var ref) = readRef ref >>= \ case
  Link var ->
    findVar var
  Unbound x -> FindT $ \ In {..} ->
    pure $! if level >= x.level then Out var label visited else Err
  Bound x -> do
    (var@(Var ref), s) <- lookupInsertA x freshVar' =<< get
    case s of
      Nothing ->
        pure var
      Just !s -> do
        put s
        writeRef ref . Bound =<< newBound' =<< findVars x.binding
        pure var
  where
    lookupInsertA k x =
      fmap (first unsafeCoerce) .
      IntMap.lookupInsertA k.label (unsafeCoerce <$> x)

freshVar' :: MonadRef m => FindT m (Var m a)
freshVar' = FindT $ \ In {..} ->
  let
    !susp = const $ pure ()
    !x = Unbound MkUnbound {..}
  in
    newRef x <&> \ ref -> Out (Var ref) (label + 1) visited

newBound' :: Applicative m => a -> FindT m (Bound a)
newBound' !binding = FindT $ \ In {..} ->
  pure $! Out MkBound {..} (label + 1) visited

newtype VarsRef m a = VarsRef Label deriving Eq

newVarsRef :: Vars a m => a -> VerseT m (VarsRef m a)
newVarsRef x = VerseT $ \ _r s _env mem _yk sk ->
  let
    !label = mem.label
    !mem' = mem
      { label = label + 1
      , heap = IntMap.insert label (SomeVars x) mem.heap
      }
  in
    sk s mem' (VarsRef label)

readVarsRef :: VarsRef m a -> VerseT m a
readVarsRef (VarsRef i) = getHeap <&> \ heap -> case heap!i of
  SomeVars x -> unsafeCoerce x

writeVarsRef :: Vars a m => VarsRef m a -> a -> VerseT m ()
writeVarsRef (VarsRef i) x = VerseT $ \ _r s _env mem _yk sk fk ek ->
  let
    !y = mem.heap!i
    !mem' = mem { heap = IntMap.insert i (SomeVars x) mem.heap }
  in
    sk s mem' () fk $ \ mem ->
    ek mem { heap = IntMap.insert i y mem.heap }
