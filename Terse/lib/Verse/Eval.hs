{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Verse.Eval
  ( eval
  ) where

import Control.Category ((>>>))
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Env
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap

import Prettyprinter
import Prettyprinter.Render.Text

import Fix
import Loc
import Ref

import Verse.Exp
import Verse.Fun (Fun)
import Verse.Fun qualified as Fun
import Verse.Monad (VerseT, runVerseT)
import Verse.Monad qualified as Monad
import Verse.Name
import Verse.Val (Val)
import Verse.Val qualified as Val

eval :: LExp -> IO (Either [[Loc]] [Fix (Val Identity)])
eval e = fmap done . flip runStateT Mem {..} . runVerseT $ do
  s1 <- newS'
  s2 <- freshS'
  freeze' <=< runEvalT $ eval' s1 s2 e
  where
    label = 0
    stacks = mempty
    done = \ case
      (Nothing, Mem { stacks }) -> Left $ toList stacks
      (Just x, _) -> Right x

type EvalT m = ReaderT (R m) (VerseT m)

data R m = R
  { env :: !(Env m)
  , stack :: [Loc]
  }

data Mem = Mem
  { label :: {-# UNPACK #-} !Int
  , stacks :: !(IntMap [Loc])
  }

type Env m = HashMap Name (Var m)

type Heap m = Monad.Var m ()

data S m = S
  { choiceFree :: !(Monad.Var m ())
  , storeFree :: !(Monad.Var m ())
  }

runEvalT :: (MonadIO m, MonadRef m) => EvalT m a -> VerseT m a
runEvalT m = runReaderT m =<< newR'

newR' :: MonadRef m => VerseT m (R m)
newR' = do
  env <- newEnv'
  pure R {..}
  where
    stack = mempty

newEnv' :: MonadRef m => VerseT m (Env m)
newEnv' =
  fmap Env.fromList .
  traverse (traverse (newVar' . Val.Fun)) $
  fmap (\ x -> (renderStrict . layoutCompact $ pretty x, x))
  [minBound .. maxBound]

eval'
  :: (MonadIO m, MonadRef m, MonadState Mem m)
  => S m -> S m -> LExp -> EvalT m (Var m)
eval' s1 s2 = wrap $ \ case
  Var x -> unifyS s1 s2 >> asks (Env.lookup x . (.env)) >>= \ case
    Just var -> pure var
    Nothing -> fork' stuck *> freshVar
  Abs x e -> unifyS s1 s2 >> asks (.env) >>= \ env ->
    newVar $ Val.Lam env x e
  App e1 e2 -> do
    s3 <- freshS
    var1 <- eval' s1 s3 e1
    s4 <- freshS
    var2 <- eval' s3 s4 e2
    var <- freshVar
    fork' $ unifyVar var =<< evalApp s4 s2 var1 var2
    pure var
  Exi x e -> do
    var <- freshVar
    localEnv (Env.insert x var) $ eval' s1 s2 e
  Int x -> do
    unifyS s1 s2
    newVar $ Val.Int x
  e1 :& e2 -> do
    s3 <- freshS
    eval' s1 s3 e1 *> eval' s3 s2 e2
  Tup xs -> do
    (s1, reverse -> xs) <- foldlM (\ (s1, xs) x -> do
      s3 <- freshS
      x <- eval' s1 s3 x
      pure (s3, x:xs)) (s1, []) xs
    unifyS s1 s2
    newTup xs
  e1 := e2 -> do
    s3 <- freshS
    var1 <- eval' s1 s3 e1
    var2 <- eval' s3 s2 e2
    i <- addStack
    unifyVar var1 var2
    removeStack i
    pure var1
  e1 :| e2 -> do
    var <- freshVar
    fork' $ do
      readChoiceFree s1
      unifyVar var =<< eval' s1 s2 e1 <|> eval' s1 s2 e2
    pure var
  e1 :.. e2 -> do
    var <- freshVar
    fork' $ do
      readChoiceFree s1
      s3 <- freshS
      var1 <- eval' s1 s3 e1
      var2 <- eval' s3 s2 e2
      (,) <$> readVar var1 <*> readVar var2 >>= \ case
        (Val.Int x1, Val.Int x2) ->
          unifyVar var <=< asum $ newVar . Val.Int <$> [x1 .. x2]
        _ ->
          stuck
    pure var
  e1 :+ e2 -> do
    s3 <- freshS
    var1 <- eval' s1 s3 e1
    s4 <- freshS
    var2 <- eval' s3 s4 e2
    var <- freshVar
    fork' $ unifyVar var =<< evalPlus s4 s2 var1 var2
    pure var
  e1 :- e2 -> do
    s3 <- freshS
    var1 <- eval' s1 s3 e1
    s4 <- freshS
    var2 <- eval' s3 s4 e2
    var <- freshVar
    fork' $ unifyVar var =<< evalMinus s4 s2 var1 var2
    pure var
  e1 :< e2 -> do
    s3 <- freshS
    var1 <- eval' s1 s3 e1
    s4 <- freshS
    var2 <- eval' s3 s4 e2
    var <- freshVar
    fork' $ unifyVar var =<< evalLess s4 s2 var1 var2
    pure var
  Fail ->
    empty
  All e -> do
    unifyChoiceFree s1 s2
    var <- freshVar
    heap <- newHeap s1
    fork $ do
      i <- addStack
      unifyVar var <=< newTup <=< all' $ do
        s1 <- newS
        s2 <- freshS
        localHeap (const heap) $ eval' s1 s2 e
      removeStack i
      unifyStoreFree s1 s2
    pure var
  For e1 x e2 -> do
    let
      init s1 = do
        heap <- newHeap s1
        split $ do
          s1 <- newS
          s2 <- freshS
          localHeap (const heap) $ eval' s1 s2 e1
      loop s1 = \ case
        Monad.Done -> do
          unifyS s1 s2
          pure []
        Monad.Step var1 m -> do
          s3 <- freshS
          var2 <- localEnv (Env.insert x var1) $ eval' s1 s3 e2
          heap <- newHeap s3
          fmap (var2:) . loop s3 <=< lift $ local (const heap) m
    var <- freshVar
    fork' $ unifyVar var =<< newTup =<< loop s1 =<< init s1
    pure var
  One e -> do
    unifyChoiceFree s1 s2
    var <- freshVar
    heap <- newHeap s1
    fork $ do
      i <- addStack
      unifyVar var <=< one $ do
        s1 <- newS
        s2 <- freshS
        localHeap (const heap) $ eval' s1 s2 e
      removeStack i
      unifyStoreFree s1 s2
    pure var
  If e1 x e2 e3 -> do
    var <- freshVar
    heap <- newHeap s1
    fork' $ unifyVar var =<< if'
      (do s1 <- newS
          s2 <- freshS
          localHeap (const heap) $ eval' s1 s2 e1)
      (\ var1 -> localEnv (Env.insert x var1) $ eval' s1 s2 e2)
      (eval' s1 s2 e3)
    pure var

evalApp
  :: (MonadIO m, MonadRef m, MonadState Mem m)
  => S m -> S m -> Var m -> Var m -> EvalT m (Var m)
evalApp s1 s2 var1 var2 = readVar var1 >>= \ case
  Val.Int _ ->
    stuck
  Val.Lam env x e ->
    localEnv (const $ Env.insert x var2 env) $ eval' s1 s2 e
  Val.Tup xs -> do
    unifyStoreFree s1 s2
    readChoiceFree s1
    var <- asum $ zip [0 ..] xs <&> \ (i, var1) -> do
      unifyVar var2 <=< newVar $ Val.Int i
      pure var1
    unifyChoiceFree s1 s2
    pure var
  Val.Fun f ->
    evalAppFun s1 s2 f var2
  Val.Ptr _ ->
    stuck
  Val.Map xs -> readVar var2 >>= \ case
    Val.Int k
      | toInteger minInt <= k && k <= toInteger maxInt ->
          evalAppMap (fromInteger k) xs
      | otherwise ->
          empty
    _ -> stuck

evalAppFun
  :: (MonadIO m, MonadRef m, MonadState Mem m)
  => S m -> S m -> Fun -> Var m -> EvalT m (Var m)
evalAppFun s1 s2 f x = case f of
  Fun.Plus -> do
    (x1, x2) <- one $ readPair x <|> stuck
    evalPlus s1 s2 x1 x2
  Fun.Minus -> do
    (x1, x2) <- one $ readPair x <|> stuck
    evalMinus s1 s2 x1 x2
  Fun.Less -> do
    (x1, x2) <- one $ readPair x <|> stuck
    evalLess s1 s2 x1 x2
  Fun.Alloc -> do
    unifyChoiceFree s1 s2
    readStoreFree s1
    readHeap
    var <- newVar . Val.Ptr <=< lift $ Monad.newVarsRef x
    unifyStoreFree s1 s2
    pure var
  Fun.Read -> readVar x >>= \ case
    Val.Ptr x -> do
      unifyChoiceFree s1 s2
      readStoreFree s1
      readHeap
      var <- lift $ Monad.readVarsRef x
      unifyStoreFree s1 s2
      pure var
    _ -> stuck
  Fun.Write -> do
    (x1, x2) <- one $ readPair x <|> stuck
    readVar x1 >>= \ case
      Val.Ptr x1 -> do
        unifyChoiceFree s1 s2
        readStoreFree s1
        readHeap
        lift $ Monad.writeVarsRef x1 x2
        unifyStoreFree s1 s2
        newTup []
      _ -> stuck
  Fun.GetLine -> do
    one $ (unifyVar x =<< newTup []) <|> stuck
    unifyChoiceFree s1 s2
    readStoreFree s1
    readHeap
    var <- newString =<< liftIO getLine
    unifyStoreFree s1 s2
    pure var
  Fun.ReadInt -> do
    x <- one $ readString x <|> stuck
    unifyS s1 s2
    newTup <=< traverse (newPair <=< newInteger *** newString) $ reads x
  Fun.Print -> do
    unifyChoiceFree s1 s2
    readStoreFree s1
    readHeap
    liftIO . print . pretty =<< freeze x
    unifyStoreFree s1 s2
    newTup []
  Fun.Map -> do
    unifyChoiceFree s1 s2
    heap <- newHeap s1
    let
      loop !xs = \ case
        Monad.Done ->
          newVar $ Val.Map xs
        Monad.Step (k, v) m -> readVar k >>= \ case
          Val.Int k | toInteger minInt <= k && k <= toInteger maxInt ->
            loop (insert (fromInteger k) v xs) <=<
            lift $ local (const heap) m
          _ ->
            stuck
    var <- loop mempty <=< split $ do
      s1 <- newS
      s2 <- freshS
      y <- freshVar
      x <- localHeap (const heap) $ evalApp s1 s2 x y
      pure (y, x)
    unifyStoreFree s1 s2
    pure var

wrap :: (ExpF LExp -> EvalT m (Var m)) -> LExp -> EvalT m (Var m)
wrap f (L i x) = local (\ r -> r { stack = i:r.stack }) $ f x

evalPlus :: MonadRef m => S m -> S m -> Var m -> Var m -> EvalT m (Var m)
evalPlus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $ x1 + x2

evalMinus :: MonadRef m => S m -> S m -> Var m -> Var m -> EvalT m (Var m)
evalMinus s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  newInteger $ x1 - x2

evalLess :: MonadRef m => S m -> S m -> Var m -> Var m -> EvalT m (Var m)
evalLess s1 s2 var1 var2 = do
  (x1, x2) <- one $ (,) <$> readInteger var1 <*> readInteger var2 <|> stuck
  unifyS s1 s2
  guard $ x1 < x2
  pure var1

evalAppMap :: MonadRef m => Int -> IntMap [Var m] -> EvalT m (Var m)
evalAppMap k = IntMap.lookup k >>> \ case
  Nothing -> empty
  Just xs -> asum $ pure <$> xs

type Var m = Fix (Compose (Monad.Var m) (Val (Monad.VarsRef m)))

newString :: MonadRef m => String -> EvalT m (Var m)
newString = newTup <=< traverse newChar

newChar :: MonadRef m => Char -> EvalT m (Var m)
newChar = newInt . ord

newInt :: MonadRef m => Int -> EvalT m (Var m)
newInt = newInteger . toInteger

newInteger :: MonadRef m => Integer -> EvalT m (Var m)
newInteger = newVar . Val.Int

newPair :: MonadRef m => (Var m, Var m) -> EvalT m (Var m)
newPair (x, y) = newTup [x, y]

newTup :: MonadRef m => [Var m] -> EvalT m (Var m)
newTup = newVar . Val.Tup

readString :: MonadRef m => Var m -> EvalT m String
readString = readVar >=> \ case
  Val.Tup xs -> traverse readChar xs
  _ -> empty

readChar :: MonadRef m => Var m -> EvalT m Char
readChar = fmap chr . readInt

readInt :: MonadRef m => Var m -> EvalT m Int
readInt = readInteger >=> \ x -> do
  guard $ toInteger minInt <= x && x <= toInteger maxInt
  pure $ fromInteger x

readInteger :: MonadRef m => Var m -> EvalT m Integer
readInteger = readVar >=> \ case
  Val.Int x -> pure x
  _ -> empty

readPair :: MonadRef m => Var m -> EvalT m (Var m, Var m)
readPair = readVar >=> \ case
  Val.Tup [x1, x2] -> pure (x1, x2)
  _ -> empty

freeze :: MonadRef m => Var m -> EvalT m (Fix (Val Identity))
freeze = lift . freeze'

freeze' :: MonadRef m => Var m -> VerseT m (Fix (Val Identity))
freeze' = getFix >>> getCompose >>> Monad.readVar >=> fmap Fix . \ case
  Val.Int x -> pure $ Val.Int x
  Val.Lam r x e -> traverse freeze' r <&> \ r -> Val.Lam r x e
  Val.Tup x -> Val.Tup <$> traverse freeze' x
  Val.Fun x -> pure $ Val.Fun x
  Val.Ptr x -> fmap (Val.Ptr . Identity) . freeze' =<< Monad.readVarsRef x
  Val.Map x -> Val.Map <$> traverse (traverse freeze') x

localEnv :: (Env m -> Env m) -> EvalT m a -> EvalT m a
localEnv f = local (\ r -> r { env = f r.env })

newHeap :: MonadRef m => S m -> EvalT m (Heap m)
newHeap s1 = do
  heap <- freshHeap
  fork $ do
    readStoreFree s1
    unifyHeap heap =<< askHeap
  pure heap

readHeap :: MonadRef m => EvalT m ()
readHeap = lift $ Monad.readVar =<< ask

askHeap :: EvalT m (Heap m)
askHeap = lift ask

localHeap :: (Heap m -> Heap m) -> EvalT m a -> EvalT m a
localHeap f m = ReaderT $ local f . runReaderT m

fork' :: (MonadRef m, MonadState Mem m) => EvalT m () -> EvalT m ()
fork' m = fork $ do
  i <- addStack
  m
  removeStack i

fork :: MonadRef m => EvalT m () -> EvalT m ()
fork m = ReaderT $ Monad.fork . runReaderT m

all' :: (MonadRef m, Monad.Vars a m) => EvalT m a -> EvalT m [a]
all' m = ReaderT $ Monad.all' . runReaderT m

one :: (MonadRef m, Monad.Vars a m) => EvalT m a -> EvalT m a
one m = ReaderT $ Monad.one . runReaderT m

if'
  :: (MonadRef m, Monad.Vars a m)
  => EvalT m a -> (a -> EvalT m b) -> EvalT m b -> EvalT m b
if' m f n = ReaderT $ \ r ->
  Monad.if' (runReaderT m r) (flip runReaderT r . f) (runReaderT n r)

split
  :: (MonadRef m, Monad.Vars a m)
  => EvalT m a -> EvalT m (Monad.Stream m a)
split m = ReaderT $ Monad.split . runReaderT m

stuck :: EvalT m a
stuck = lift Monad.stuck

freshVar :: MonadRef m => EvalT m (Var m)
freshVar = lift $ Fix . Compose <$> Monad.freshVar

newVar :: MonadRef m => Val (Monad.VarsRef m) (Var m) -> EvalT m (Var m)
newVar = lift . newVar'

newVar' :: MonadRef m => Val (Monad.VarsRef m) (Var m) -> VerseT m (Var m)
newVar' = fmap (Fix . Compose) . Monad.newVar

readVar :: MonadRef m => Var m -> EvalT m (Val (Monad.VarsRef m) (Var m))
readVar = lift . Monad.readVar . getCompose . getFix

unifyVar :: MonadRef m => Var m -> Var m -> EvalT m ()
unifyVar = (lift .) . Monad.unifyVar `on` getCompose . getFix

freshS :: MonadRef m => EvalT m (S m)
freshS = lift $ do
  choiceFree <- Monad.freshVar
  storeFree <- Monad.freshVar
  pure S {..}

freshS' :: MonadRef m => VerseT m (S m)
freshS' = do
  choiceFree <- Monad.freshVar
  storeFree <- Monad.freshVar
  pure S {..}

newS :: MonadRef m => EvalT m (S m)
newS = lift newS'

newS' :: MonadRef m => VerseT m (S m)
newS' = do
  choiceFree <- Monad.newVar ()
  storeFree <- Monad.newVar ()
  pure S {..}

unifyS :: MonadRef m => S m -> S m -> EvalT m ()
unifyS s1 s2 = unifyChoiceFree s1 s2 *> unifyStoreFree s1 s2

readChoiceFree :: MonadRef m => S m -> EvalT m ()
readChoiceFree = lift . Monad.readVar . (.choiceFree)

readStoreFree :: MonadRef m => S m -> EvalT m ()
readStoreFree = lift . Monad.readVar . (.storeFree)

unifyChoiceFree :: MonadRef m => S m -> S m -> EvalT m ()
unifyChoiceFree s1 s2 = lift $ Monad.unifyVar s1.choiceFree s2.choiceFree

unifyStoreFree :: MonadRef m => S m -> S m -> EvalT m ()
unifyStoreFree s1 s2 = lift $ Monad.unifyVar s1.storeFree s2.storeFree

freshHeap :: MonadRef m => EvalT m (Heap m)
freshHeap = lift Monad.freshVar

unifyHeap :: MonadRef m => Heap m -> Heap m -> EvalT m ()
unifyHeap = (lift .) . Monad.unifyVar

addStack :: MonadState Mem m => EvalT m Int
addStack = asks (.stack) >>= \ stack -> lift $ do
  i <- supply
  Monad.liftPut
    (modify' $ \ s -> s { stacks = IntMap.insert i stack s.stacks })
    (modify' $ \ s -> s { stacks = IntMap.delete i s.stacks })
  pure i

removeStack :: MonadState Mem m => Int -> EvalT m ()
removeStack i = lift $ do
  stack <- gets $ (! i) . (.stacks)
  Monad.liftPut
    (modify' $ \ s -> s { stacks = IntMap.delete i s.stacks })
    (modify' $ \ s -> s { stacks = IntMap.insert i stack s.stacks })

supply :: MonadState Mem m =>  m Int
supply = do
  s <- get
  put s { label = s.label + 1 }
  pure s.label

insert :: MonadRef m => Int -> Var m -> IntMap [Var m] -> IntMap [Var m]
insert k = IntMap.insertWith (++) k . (:[])

infixr 3 ***
(***) :: Monad m => (a -> m c) -> (b -> m d) -> (a, b) -> m (c, d)
(f *** g) (x, y) = (,) <$> f x <*> g y

minInt :: Int
minInt = minBound

maxInt :: Int
maxInt = maxBound
