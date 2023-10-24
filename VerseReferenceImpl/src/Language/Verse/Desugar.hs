{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Abort
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Functor
import Data.Functor.Apply
import Data.HashMap.Strict (foldlWithKey')
import Data.HashMap.Strict qualified as HashMap

import Language.Verse.Desugar.Exp ( Exp (..)
                                  , Quantifier (..)
                                  , unify
                                  , verify
                                  , succeeds
                                  , assume
                                  , forall'
                                  , bracketInvoke
                                  , fun
                                  , name
                                  , then'
                                  )
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Mode
import Language.Verse.Rewrite.Exp ( pattern List
                                  , pattern Where
                                  , pattern MixfixVarColonEqual
                                  , pattern InfixColonEqual
                                  , pattern PrefixColon
                                  , pattern MixfixArrowColonEqual
                                  , pattern (:|>:)
                                  )
import Language.Verse.Rewrite.Exp qualified as Rewrite

type DesugarT m = StateT Env (ReaderT Mode m)

type Env = IdentMap (Loc, Quantifier L Ident)

runDesugarT
  :: Functor m
  => DesugarT m a
  -> ReaderT Mode m (a, IdentMap (Quantifier L Ident))
runDesugarT = fmap (fmap (fmap snd)) . runDesugarT'

runDesugarT' :: DesugarT m a -> ReaderT Mode m (a, Env)
runDesugarT' m = runStateT m mempty

desugar
  :: (MonadAbort Error m, MonadSupply Label m)
  => Mode
  -> L (Rewrite.Exp L Ident)
  -> m (L (Exp L Ident))
desugar mode e =
  runReaderT (runDesugarT' (desugarExp e) <&> \ (e, xs) -> exists' xs e) mode

desugarExp
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp e = do
  x <- name <$> freshIdent (loc e)
  desugarExp' e False x

desugarExp'
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp' e pi x = (e $>) <$> desugarExp'' e pi x

desugarExp''
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarExp'' e pi i = case extract e of
  Rewrite.Fun e1 e2 ->
    desugarFun e1 e2 pi i
  (Rewrite.:=:) e1 e2 -> do
    e1 <- desugarExp' e1 pi i
    e2 <- desugarExp' e2 pi i
    pure $ e1 :=: e2
  (Rewrite.:.:) e' x ->
    valM i (e $>) $ desugarExp e' <&> (:.: x)
  (Rewrite.:|:) e1 e2 -> do
    e1 <- desugarExp' e1 pi i
    e2 <- desugarExp' e2 pi i
    pure $ e1 :|: e2
  List [] ->
    pure $ i :=: (Tuple [] <$ e)
  List (e:es) ->
    desugarNonEmpty' pi i e es
  e1 `Where` e2 -> do
    x <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 pi i
    e2 <- desugarExp e2
    pure $ unify x e1 `then'` e2 :*>: x
  Rewrite.Fail ->
    pure Fail
  Rewrite.One e' ->
    valM i (e $>) $ One <$> exists (desugarExp e')
  Rewrite.All e' ->
    valM i (e $>) $ All <$> exists (desugarExp e')
  Rewrite.Not e' ->
    valM i (e $>) $ Not <$> exists (desugarExp e')
  Rewrite.Verify e' ->
    valM i (e $>) $ Verify <$> exists (desugarExp e')
  Rewrite.Succeeds e' ->
    valM i (e $>) $ Succeeds <$> exists (desugarExp e')
  Rewrite.Fails e' ->
    valM i (e $>) $ Fails <$> exists (desugarExp e')
  Rewrite.Decides e' ->
    valM i (e $>) $ Decides <$> exists (desugarExp e')
  Rewrite.Assume e' ->
    valM i (e $>) $ Assume <$> exists (desugarExp e')
  Rewrite.Module e' -> valM i (e $>) $ do
    i <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    pure $ Module i xs e'
  Rewrite.Enum xs -> valM i (e $>) $ do
    j <- supply
    pure $ Enum j xs
  Rewrite.Struct e' -> valM i (e $>) $ do
    j <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    pure $ Struct j xs e'
  Rewrite.Class e1 e2 -> valM i (e $>) $ do
    j <- supply
    e1 <- traverse desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Class j e1 xs e2
  Rewrite.Inst e1 e2 -> valM i (e $>) $ do
    e1 <- desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Inst e1 xs e2
  Rewrite.IfThenElse e1 e2 e3 -> do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    IfThenElse xs e1 <$>
      exists (desugarExp' e2 pi i) <*>
      exists (desugarExp' e3 pi i)
  Rewrite.ForDo e1 e2 -> valM i (e $>) $ do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    e2 <- exists $ desugarExp e2
    pure $ ForDo xs e1 e2
  Rewrite.Block e' ->
    (i :=:) <$> exists (desugarExp e')
  Rewrite.BracketInvoke e1 e2 -> valM i (e $>) $ do
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ BracketInvoke e1 e2
  Rewrite.Exists x -> do
    tellName x Exists
    pure $ i :=: name x
  Rewrite.Forall x -> do
    tellName x Forall
    pure $ i :=: name x
  Rewrite.Set x e' ->
    valM i (e $>) $ Set x <$> desugarExp e'
  Rewrite.Tuple es -> do
    (is, es) <- desugarTuple pi es
    pure $ unify i (Tuple is <$ e) :*>: (Tuple es <$ e)
  Rewrite.Truth e' ->
    valM i (e $>) $ Truth <$> exists (desugarExp e')
  Rewrite.Int x ->
    pure $ i :=: (Int x <$ e)
  Rewrite.Float x ->
    pure $ i :=: (Float x <$ e)
  Rewrite.Name x ->
    pure $ i :=: (Name x <$ e)
  MixfixVarColonEqual x y e1 e2 -> do
    tellName x $ Var y
    e1 <- desugarExp e1
    e2 <- desugarExp' e2 pi i
    pure $ unify (Name <$> y) e1 :*>: unify (ArchetypeName <$> x) e2
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x Exists
    e <- desugarExp' e pi i
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    pure $ BracketInvoke e i
  MixfixArrowColonEqual x y e -> do
    tellName x Exists
    tellName y Exists
    e <- desugarExp' e pi i
    pure $ unify (ArchetypeName <$> y) e :*>: unify (name x) i
  Rewrite.IfArchetypeName x e1 e2 -> do
    y <- (e $>) . Ident.Label <$> supply
    xs <- get
    (e1, xs1) <- lift $ runStateT (desugarExp' e1 True (name y)) xs
    (e2, xs2) <- lift $ runStateT (desugarExp' e2 pi i) xs
    put $ xs1 <> xs2
    pure $ IfArchetypeName x y e1 e2
  e1 :|>: e2 ->
    desugarOfType e1 e2 pi i

valM
  :: Functor m
  => f (Exp f a)
  -> (b -> f (Exp f a))
  -> m b
  -> m (Exp f a)
valM i f m = (i :=:) . f <$> m

desugarNonEmpty
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> L (Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (L (Exp L Ident))
desugarNonEmpty pi i x = \ case
  [] -> desugarExp' x pi i
  y:xs -> desugarExp x `thenM` desugarNonEmpty pi i y xs

desugarNonEmpty'
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> L (Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (Exp L Ident)
desugarNonEmpty' pi i x = \ case
  [] -> desugarExp'' x pi i
  y:xs -> desugarExp x `thenM'` desugarNonEmpty pi i y xs

desugarTuple
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m ([L (Exp L Ident)], [L (Exp L Ident)])
desugarTuple pi = \ case
  [] -> pure ([], [])
  e:es -> do
    x <- name <$> freshIdent (loc e)
    e <- desugarExp' e pi x
    (xs, es) <- desugarTuple pi es
    pure (x:xs, e:es)

desugarOfType
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarOfType e1 e2 pi x = ask >>= \ case
  Execution -> do
    y <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 pi x
    e2 <- desugarExp e2
    pure $ unify y e1 :*>: bracketInvoke e2 y
  Verification ->
    name <$> freshIdent (loc e1) >>= \ y ->
    unify y <$> desugarExp' e1 pi x `thenM`
    verifyM (succeedsM do e2 <- desugarExp e2; pure $ bracketInvoke e2 y) `thenM'`
    (abstractM $ desugarExp e2)

desugarFun
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarFun e_domain e pi f = ask >>= \ case
  Execution -> desugarFunX e_domain e pi f
  Verification ->
    verifyFunM e_domain e pi f `thenM'`
    case extract e of
      _ :|>: e_range -> assumeFunM' e_domain e_range pi
      _ -> assumeFunM e_domain e pi f

desugarFunX
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarFunX e1 e2 pi f = do
  ((e1, j), xs) <- lift . runDesugarT $ do
    i <- name <$> freshIdent (loc e1)
    j <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 (not pi) i
    pure (unify j e1 `then'` i, j)
  e2 <- exists $
    name <$> freshIdent (loc e2) >>= \ z ->
    unify z <$> invokeM j pi f `thenM`
    desugarExp' e2 pi z
  pure $ Fun xs e1 e2

verifyFunM
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
verifyFunM e1 e2 pi f = do
  i <- freshIdent' $ loc e1
  verifyM $ forall' i <$> do
    j <- name <$> freshIdent (loc e1)
    unify j <$> desugarExp' e1 (not pi) (name i) `thenM` succeedsM do
      z <- name <$> freshIdent (loc e1)
      unify z <$> invokeM j pi f `thenM` desugarExp' e2 pi z

assumeFunM
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
assumeFunM e1 e2 pi f = do
  ((e1, j), xs) <- lift $ runDesugarT do
    i <- name <$> freshIdent (loc e1)
    j <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 (not pi) i
    pure (unify j e1 `then'` i, j)
  r <- freshIdent' $ loc e2
  fun xs e1 . forall' r <$> assumeM do
    z <- name <$> freshIdent (loc e2)
    unify z <$> invokeM j pi f `thenM` (unify (name r) <$> desugarExp' e2 pi z)

assumeFunM'
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> DesugarT m (L (Exp L Ident))
assumeFunM' e1 e2 pi = do
  (e1, xs) <- lift . runDesugarT $ desugarExp' e1 (not pi) . name =<< freshIdent (loc e1)
  r <- freshIdent' $ loc e2
  fun xs e1 . forall' r <$> assumeM (abstractM $ desugarExp e2)

abstractM
  :: MonadSupply Int m
  => DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
abstractM m = do
  r <- freshIdent''
  forall'' r <$> assumeM do
    e <- m
    unify (Name r <$ e) <$> prefixColonM e

invokeM
  :: MonadSupply Label m
  => L (Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
invokeM x pi f = case pi of
  False -> name <$> freshIdent (loc x <> loc f)
  True -> pure $ bracketInvoke f x

prefixColonM
  :: MonadSupply Label m
  => L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
prefixColonM e = bracketInvoke e . name <$> freshIdent (loc e)

verifyM
  :: (MonadSupply Label m)
  => DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
verifyM m = verify <$> exists m

succeedsM
  :: (MonadSupply Label m)
  => DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
succeedsM m = succeeds <$> exists m

assumeM
  :: (MonadSupply Label m)
  => DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
assumeM m = assume <$> exists m

infixl 4 `thenM`
thenM
  :: (Applicative m, Apply f)
  => m (f (Exp f a))
  -> m (f (Exp f a))
  -> m (f (Exp f a))
thenM = liftA2 then'

infixl 4 `thenM'`
thenM'
  :: Applicative m
  => m (f (Exp f a))
  -> m (f (Exp f a))
  -> m (Exp f a)
thenM' = liftA2 (:*>:)

forall'' :: Functor f => a -> f (Exp f a) -> f (Exp f a)
forall'' x e = Def Forall (x <$ e) e <$ e

freshIdent :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent loc = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, Exists)
  pure $ L loc x

freshIdent' :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent' loc = L loc <$> freshIdent''

freshIdent'' :: MonadSupply Label m => DesugarT m Ident
freshIdent'' = Ident.Label <$> supply

tellName :: MonadAbort Error m => L Ident -> Quantifier L Ident -> DesugarT m ()
tellName x y =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, y)
      Just (y, _) -> abort $ DefError y (loc x) (extract x))
  (extract x) =<<
  get

tellFunName :: Monad m => L Ident -> DesugarT m ()
tellFunName x =
  modify $ HashMap.insertWith (\ _ x -> x) (extract x) (loc x, Exists)

exists :: Monad m => DesugarT m (L (Exp L Ident)) -> DesugarT m (L (Exp L Ident))
exists m = do
  (e, xs) <- lift $ runDesugarT' m
  pure $ exists' xs e

exists' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, y) = Def y (L loc x) z <$ z
