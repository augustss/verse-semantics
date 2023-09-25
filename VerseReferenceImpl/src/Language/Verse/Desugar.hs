{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Abort
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Functor
import Data.Functor.Apply
import Data.HashMap.Strict (foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.Traversable

import Language.Verse.Desugar.Exp (Exp (..))
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Rewrite.Exp ( pattern List
                                  , pattern Where
                                  , pattern MixfixVarColonEqual
                                  , pattern InfixColonEqual
                                  , pattern PrefixColon
                                  , pattern MixfixArrowColonEqual
                                  , pattern (:|>:)
                                  )
import Language.Verse.Rewrite.Exp qualified as Rewrite

type DesugarT m = StateT Env m

type Env = IdentMap (Loc, Maybe (L Ident))

runDesugarT :: Functor m => DesugarT m a -> m (a, IdentMap (Maybe Ident))
runDesugarT = fmap (fmap (fmap (fmap extract . snd))) . runDesugarT'

runDesugarT' :: DesugarT m a -> m (a, Env)
runDesugarT' m = runStateT m mempty

desugar
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> m (L (Exp L Ident))
desugar e = do
  (e, xs) <- runDesugarT' $ desugarExp e
  pure $ exists' xs e

desugarExp
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp e = for e $ \ case
  (Rewrite.:=:) e1 e2 ->
    (:=:) <$> desugarExp e1 <*> desugarExp e2
  (Rewrite.:.:) e x ->
    desugarExp e <&> (:.: x)
  (Rewrite.:|:) e1 e2 ->
    (:|:) <$> desugarExp e1 <*> desugarExp e2
  List [] -> pure $ Tuple []
  List (e:es) -> extract <$> desugarNonEmpty e es
  e1 `Where` e2 -> do
    x <- freshIdent $ loc e1
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ unify (Name <$> x) e1 `then'` e2 :*>: (Name <$> x)
  Rewrite.Fail ->
    pure Fail
  Rewrite.One e -> do
    One <$> exists (desugarExp e)
  Rewrite.All e ->
    All <$> exists (desugarExp e)
  Rewrite.Not e ->
    Not <$> desugarExp e
  Rewrite.Module e -> do
    i <- supply
    (e, xs) <- lift . runDesugarT $ desugarExp e
    pure $ Module i xs e
  Rewrite.Enum xs -> do
    i <- supply
    pure $ Enum i xs
  Rewrite.Struct e -> do
    i <- supply
    (e, xs) <- lift . runDesugarT $ desugarExp e
    pure $ Struct i xs e
  Rewrite.Class e1 e2 -> do
    i <- supply
    e1 <- traverse desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Class i e1 xs e2
  Rewrite.Inst e1 e2 -> do
    e1 <- desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Inst e1 xs e2
  Rewrite.IfThenElse p t e -> do
    (p, xs) <- lift . runDesugarT $ desugarExp p
    IfThenElse xs p <$>
      exists (desugarExp t) <*>
      exists (desugarExp e)
  Rewrite.ForDo e1 e2 -> do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    ForDo xs e1 <$> exists (desugarExp e2)
  Rewrite.Block e ->
    extract <$> exists (desugarExp e)
  Rewrite.Exists x -> do
    tellName x Nothing
    pure $ Name $ extract x
  Rewrite.Set x e ->
    Set x <$> desugarExp e
  Rewrite.ParenInvoke e1 e2 ->
    ParenInvoke <$> desugarExp e1 <*> desugarExp e2
  Rewrite.BracketInvoke e1 e2 ->
    BracketInvoke <$> desugarExp e1 <*> desugarExp e2
  Rewrite.Tuple es ->
    Tuple <$> traverse desugarExp es
  Rewrite.Truth e ->
    Truth <$> exists (desugarExp e)
  Rewrite.Int x ->
    pure $ Int x
  Rewrite.Float x ->
    pure $ Float x
  Rewrite.Fun e_domain e -> do
    (e_domain, xs) <- lift . runDesugarT $ desugarDomain e_domain
    e <- exists $ desugarExp e
    pure $ Fun xs e_domain e
  MixfixVarColonEqual x y e1 e2 -> do
    tellName x $ Just y
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ unify (Name <$> y) e1 :*>: unify (Name <$> x) e2
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x Nothing
    e <- desugarExp e
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    x <- freshIdent $ loc e
    pure $ BracketInvoke e (Name <$> x)
  MixfixArrowColonEqual x y e -> do
    tellName x Nothing
    tellName y Nothing
    e <- desugarDomain' e $ Name <$> x
    pure $ (ArchetypeName <$> y) :=: e
  Rewrite.Name x ->
    pure $ Name x
  Rewrite.IfArchetypeName x e1 e2 -> do
    y <- (e $>) . Ident.Label <$> supply
    e1 <- desugarDomain' e1 $ Name <$> y
    e2 <- desugarExp e2
    pure $ IfArchetypeName x y e1 e2
  e1 :|>: e2 -> do
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ BracketInvoke e2 e1

desugarNonEmpty
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (L (Exp L Ident))
desugarNonEmpty e = \ case
  [] -> desugarExp e
  x:xs -> do
    e1 <- desugarExp e
    e2 <- desugarNonEmpty x xs
    pure $ e1 `then'` e2

desugarDomain
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarDomain e = do
  x <- fmap Name <$> freshIdent (loc e)
  e <- desugarDomain' e x
  pure $ e `then'` x

desugarDomain'
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarDomain' e i = for e $ \ case
  (Rewrite.:=:) e1 e2 -> do
    e1 <- desugarDomain' e1 i
    e2 <- desugarDomain' e2 i
    pure $ e1 :=: e2
  (Rewrite.:|:) e1 e2 -> do
    e1 <- desugarDomain' e1 i
    e2 <- desugarDomain' e2 i
    pure $ e1 :|: e2
  List [] -> pure $ i :=: (Tuple [] <$ e)
  List (e:es) -> extract <$> desugarDomainNonEmpty i e es
  e1 `Where` e2 -> do
    x <- fmap Name <$> freshIdent (loc e1)
    e1 <- desugarDomain' e1 i
    e2 <- desugarExp e2
    pure $ unify x e1 `then'` e2 :*>: x
  Rewrite.IfThenElse p t e -> do
    (p, xs) <- lift . runDesugarT $ desugarExp p
    IfThenElse xs p <$>
      exists (desugarDomain' t i) <*>
      exists (desugarDomain' e i)
  Rewrite.Tuple es -> do
    (is, es) <- desugarDomainTuple es
    pure $ (unify i $ Tuple is <$ e) :*>: (Tuple es <$ e)
  Rewrite.Name x ->
    pure $ i :=: (Name x <$ e)
  Rewrite.Fun e_domain e -> do
    ((e_domain, x), xs) <- lift . runDesugarT $ do
      x <- fmap Name <$> freshIdent (loc e_domain)
      j <- fmap Name <$> freshIdent (loc e_domain)
      e_domain <- desugarDomain' e_domain j
      pure (unify x e_domain `then'` j, x)
    e <- exists $ do
      y <- fmap Name <$> freshIdent (loc e)
      e <- desugarDomain' e y
      pure $ unify y (bracketInvoke i x) `then'` e
    pure $ Fun xs e_domain e
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x Nothing
    e <- desugarDomain' e i
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    pure $ BracketInvoke e i
  MixfixArrowColonEqual x y e -> do
    tellName x Nothing
    tellName y Nothing
    e <- desugarDomain' e i
    pure $ unify (ArchetypeName <$> y) e :*>: unify (Name <$> x) i
  Rewrite.IfArchetypeName x e1 e2 -> do
    y <- (e $>) . Ident.Label <$> supply
    e1 <- desugarDomain' e1 $ Name <$> y
    e2 <- desugarDomain' e2 i
    pure $ IfArchetypeName x y e1 e2
  e1 :|>: e2 -> do
    e1 <- desugarDomain' e1 i
    e2 <- desugarExp e2
    pure $ BracketInvoke e2 e1
  _ -> do
    e <- desugarExp e
    pure $ i :=: e

desugarDomainTuple
  :: (MonadAbort Error m, MonadSupply Label m)
  => [L (Rewrite.Exp L Ident)]
  -> DesugarT m ([L (Exp L Ident)], [L (Exp L Ident)])
desugarDomainTuple = \ case
  [] -> pure ([], [])
  e:es -> do
    x <- fmap Name <$> freshIdent (loc e)
    e <- desugarDomain' e x
    (xs, es) <- desugarDomainTuple es
    pure (x:xs, e:es)

desugarDomainNonEmpty
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (L (Exp L Ident))
desugarDomainNonEmpty i e = \ case
  [] -> desugarDomain' e i
  x:xs -> do
    e1 <- desugarExp e
    e2 <- desugarDomainNonEmpty i x xs
    pure $ e1 `then'` e2

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

then' :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
then' = liftL2 (:*>:)

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y

freshIdent :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent loc = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, Nothing)
  pure $ L loc x

tellName :: MonadAbort Error m => L Ident -> Maybe (L Ident) -> DesugarT m ()
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
  modify $ HashMap.insertWith (\ _ x -> x) (extract x) (loc x, Nothing)

exists :: Monad m => DesugarT m (L (Exp L Ident)) -> DesugarT m (L (Exp L Ident))
exists m = do
  (e, xs) <- lift $ runDesugarT' m
  pure $ exists' xs e

exists' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, y) = Exists (L loc x) y z <$ z
