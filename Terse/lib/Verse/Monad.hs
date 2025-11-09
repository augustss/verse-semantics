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
import Data.Tuple

import Fix
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
  , forward :: !(m ())
  , backward :: !(m ())
  , backward' :: !(m ())
  }

appendMem :: Applicative m => Mem m -> m () -> m () -> Mem m
appendMem mem forward backward = mem
  { forward = mem.forward *> forward
  , backward = backward *> mem.backward
  }

appendMem' :: Applicative m => Mem m -> m () -> Mem m
appendMem' mem backward' = mem
  { backward' = backward' *> mem.backward'
  }

type Label = Int

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

liftPut' :: Monad m => m () -> m () -> VerseT m ()
liftPut' forward backward' = do
  lift forward
  tell' backward'

tell' :: Applicative m => m () -> VerseT m ()
tell' backward' = VerseT $ \ _r s _env mem _yk sk fk ek ->
  sk s (appendMem' mem backward') () fk (\ mem -> backward' *> ek mem)

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

getMem :: VerseT m (Mem m)
getMem = VerseT $ \ _r s _env mem _yk sk ->
  sk s mem mem

putMem :: Mem m -> VerseT m ()
putMem mem = VerseT $ \ _r s _env _mem _yk sk ->
  sk s mem ()

putLabel :: Label -> VerseT m ()
putLabel label = VerseT $ \ _r s _env Mem { label = _, .. } _yk sk ->
  sk s Mem {..} ()

runVerseT :: (MonadRef m, Vars a m) => VerseT m a -> m (Maybe [a])
runVerseT m = do
  (env, label) <- newVar' () 0
  let
    sk s mem x fk _ek
      | s.count == 0 =
        runFindT (findVars x) 0 mem.label >>= \ case
          Nothing -> pure Nothing
          Just (x, label) -> fmap (x:) <$> fk env (splitMem label)
      | otherwise = pure Nothing
  unVerseT m r s env (splitMem label) yk sk fk ek
  where
    r = R { level = 1 }
    s = S { count = 0 }
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
split m = split' m S { count = 0 }

split'
  :: (MonadRef m, Vars a m)
  => VerseT m a -> S -> VerseT m (Stream m a)
split' m s = split'' m s succeedS failS emptyS

split''
  :: (MonadRef m, Vars b m)
  => VerseT m a
  -> S
  -> Succeed (VerseT m (Stream m b)) m a
  -> Fail (VerseT m (Stream m b)) m
  -> Empty (VerseT m (Stream m b)) m
  -> VerseT m (Stream m b)
split'' m s' sk' fk' ek' = VerseT $ \ r s env mem@Mem { label } yk sk fk ek ->
  let
    !r' = R { level = r.level <> 1 }
    !mem' = splitMem label
  in
    unVerseT m r' s' env mem' yieldS sk' fk' ek' >>= \ m ->
    unVerseT m r s env mem yk sk fk ek

yieldS :: (MonadRef m, Vars a m) => Yield (VerseT m (Stream m a)) m
yieldS = Yield $ \ i f s mem sk fk ek -> pure $ do
  tell mem.forward mem.backward
  tell' mem.backward'
  putLabel mem.label
  level <- getLevel
  if i > level then
    stuck
  else
    yield i $ \ k -> f $ \ m -> k $ split'' m s sk fk ek

succeedS :: (MonadRef m, Vars a m) => Succeed (VerseT m (Stream m a)) m a
succeedS s mem x fk _ek = pure $ do
  tell mem.forward mem.backward
  tell' mem.backward'
  if s.count == 0 then do
    level <- getLevel
    lift (runFindT (findVars x) level mem.label) >>= \ case
      Nothing -> do
        putLabel mem.label
        stuck
      Just (x, label) -> do
        putLabel label
        pure . Step x $ liftFailS fk
  else do
    putLabel mem.label
    stuck

failS :: Applicative m => Fail (VerseT m (Stream m a)) m
failS _env = emptyS

emptyS :: Applicative m => Empty (VerseT m (Stream m a)) m
emptyS mem = pure $ do
  tell mem.forward mem.backward
  tell' mem.backward'
  putLabel mem.label
  pure Done

liftFailS :: Monad m => Fail (VerseT m a) m -> VerseT m a
liftFailS fk = do
  env <- ask
  !Mem { label } <- getMem
  join . lift . fk env $ splitMem label

splitMem :: Applicative m => Label -> Mem m
splitMem label = Mem {..}
  where
    !forward = pure ()
    !backward = pure ()
    !backward' = pure ()

data Stream m a = Done | Step a (VerseT m (Stream m a))

fork :: Monad m => VerseT m () -> VerseT m ()
fork m = fork' m succeedF

fork' :: Monad m => VerseT m a -> Succeed (VerseT m ()) m a -> VerseT m ()
fork' m sk' = VerseT $ \ r s env mem yk sk fk ek ->
  unVerseT m r s env mem yieldF sk' failF emptyF >>= \ m ->
  unVerseT m r s env mem yk sk fk ek

yieldF :: Monad m => Yield (VerseT m ()) m
yieldF = Yield $ \ i f s mem sk fk ek -> pure $ do
  putS s
  putMem mem
  level <- getLevel
  let
    m_f = liftFailF fk
    m_e = liftEmptyF ek
  if i < level then
    alt (yield i (\ k -> f $ \ m -> k $ fork' m sk)) m_f m_e
  else do
    modifyS succS
    alt (f $ \ m -> modifyS predS *> fork' m sk) m_f m_e

succeedF :: Monad m => Succeed (VerseT m ()) m ()
succeedF s mem () fk ek = pure $ do
  putS s
  putMem mem
  alt (pure ()) (liftFailF fk) (liftEmptyF ek)

failF :: Applicative m => Fail (VerseT m ()) m
failF _env mem = pure $ do
  putMem mem
  empty

emptyF :: Applicative m => Empty (VerseT m ()) m
emptyF mem = pure $ do
  putMem mem
  empty

liftFailF :: Monad m => Fail (VerseT m a) m -> VerseT m a
liftFailF fk = do
  env <- ask
  mem <- getMem
  join . lift $ fk env mem

liftEmptyF :: Monad m => Empty (VerseT m a) m -> VerseT m a
liftEmptyF ek = do
  mem <- getMem
  join . lift $ ek mem

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
  | Bound !(Bound m a)
  | Link !(Var m a)

data Root m a
  = UnboundR !(Unbound m a)
  | BoundR !(Bound m a)

data Unbound m a = MkUnbound
  { label :: {-# UNPACK #-} !Label
  , level :: {-# UNPACK #-} !Level
  , susp :: !(Var m a -> Ap (VerseT m) ())
  }

data Bound m a = MkBound
  { visited :: !(Ref m (Maybe (Var m a)))
  , label :: {-# UNPACK #-} !Label
  , binding :: !a
  }

instance ZipVars_ a m => ZipVars_ (Bound m a) m where
  zipVars_ f x y
    | x.label == y.label = pure ()
    | otherwise = zipVars_ f x.binding y.binding

freshVar :: MonadRef m => VerseT m (Var m a)
freshVar = VerseT $ \ r s _env Mem {..} _yk sk fk ek ->
  let
    !mem = Mem { label = label + 1, .. }
    !level = r.level
    !susp = const $ pure ()
    !x = Unbound MkUnbound {..}
  in
    newRef x >>= \ ref ->
    sk s mem (Var ref) fk ek

newVar :: MonadRef m => a -> VerseT m (Var m a)
newVar = lift . fmap Var . newRef . Bound <=< newBound

newVar' :: MonadRef m => a -> Label -> m (Var m a, Label)
newVar' binding label = do
  visited <- newRef Nothing
  let !label' = label + 1
  newRef (Bound MkBound {..}) <&> \ ref -> (Var ref, label')

newBound :: MonadRef m => a -> VerseT m (Bound m a)
newBound !binding = VerseT $ \ _r s _env Mem {..} _yk sk fk ek -> do
  !visited <- newRef Nothing
  let
    !mem = Mem { label = label + 1, .. }
    !x = MkBound {..}
  sk s mem x fk ek

readVar :: MonadRef m => Var m a -> VerseT m a
readVar = fmap (.binding) . readBound

readBound :: MonadRef m => Var m a -> VerseT m (Bound m a)
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
  }

data Out a = Err | Out !a {-# UNPACK #-} !Label

instance Functor m => Functor (FindT m) where
  fmap f x = FindT $ \ s -> unFindT x s <&> \ case
    Err -> Err
    Out x label -> Out (f x) label

instance Monad m => Applicative (FindT m) where
  pure x = FindT $ \ In {..} -> pure $! Out x label
  f <*> x = FindT $ \ s@In {..} -> unFindT f s >>= \ case
    Err -> pure Err
    Out f label -> unFindT x In {..} <&> \ case
      Err -> Err
      Out x label -> Out (f x) label

instance Monad m => Monad (FindT m) where
  x >>= f = FindT $ \ s@In {..} -> unFindT x s >>= \ case
    Err -> pure Err
    Out x label -> unFindT (f x) In {..}

instance MonadTrans FindT where
  lift m = FindT $ \ In {..} -> m <&> \ x -> Out x label

instance MonadRef m => MonadRef (FindT m) where
  type Ref (FindT m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef = (lift .) . writeRef

runFindT :: Functor m => FindT m a -> Level -> Label -> m (Maybe (a, Label))
runFindT m level label = unFindT m In {..} <&> \ case
  Err -> Nothing
  Out x label -> Just (x, label)

findVars :: (MonadRef m, Vars a m) => a -> FindT m a
findVars = vars findVar

findVar :: (MonadRef m, Vars a m) => Var m a -> FindT m (Var m a)
findVar var@(Var ref) = readRef ref >>= \ case
  Link var ->
    findVar var
  Unbound x -> FindT $ \ In {..} ->
    pure $! if level >= x.level then Out var label else Err
  Bound x -> readRef x.visited >>= \ case
    Just var -> pure var
    Nothing -> do
      var@(Var ref) <- freshVar'
      bracket (writeRef x.visited $ Just var) (writeRef x.visited Nothing) $
        writeRef ref . Bound =<< newBound' =<< findVars x.binding
      pure var

freshVar' :: MonadRef m => FindT m (Var m a)
freshVar' = FindT $ \ In {..} ->
  let
    !susp = const $ pure ()
    !x = Unbound MkUnbound {..}
  in
    newRef x <&> \ ref -> Out (Var ref) (label + 1)

newBound' :: MonadRef m => a -> FindT m (Bound m a)
newBound' !binding = FindT $ \ In {..} -> do
  visited <- newRef Nothing
  pure $! Out MkBound {..} (label + 1)

bracket :: Monad m => m () -> m () -> FindT m a -> FindT m a
bracket x y z = FindT $ \ s -> x *> unFindT z s >>= \ case
  Err -> y $> Err
  Out z label -> y $> Out z label

data VarsRef m a = VarsRef {-# UNPACK #-} !Label !(Ref m a)

instance Eq (VarsRef m a) where
  VarsRef x _ == VarsRef y _ = x == y
  VarsRef x _ /= VarsRef y _ = x /= y

newVarsRef :: MonadRef m => a -> VerseT m (VarsRef m a)
newVarsRef x = VerseT $ \ _r s _env Mem {..} _yk sk fk ek ->
  let
    !mem = Mem { label = label + 1, .. }
  in
    newRef x >>= \ ref ->
    sk s mem (VarsRef label ref) fk ek

readVarsRef :: MonadRef m => VarsRef m a -> VerseT m a
readVarsRef (VarsRef _ ref) = lift $ readRef ref

writeVarsRef :: MonadRef m => VarsRef m a -> a -> VerseT m ()
writeVarsRef (VarsRef _ ref) x = do
  y <- lift $ readRef ref
  liftPut' (writeRef ref x) (writeRef ref y)
