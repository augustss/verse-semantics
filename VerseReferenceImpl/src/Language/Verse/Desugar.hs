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

import Language.Verse.Desugar.Exp
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Rewrite.Exp ( pattern List
                                  , pattern Where
                                  , pattern InfixColonEqual
                                  , pattern PrefixColon
                                  , pattern MixfixArrowColonEqual
                                  , pattern (:|>:)
                                  )
import Language.Verse.Rewrite.Exp qualified as Rewrite

type DesugarT m = StateT Env m

type Env = IdentMap (Loc, Bool)

runDesugarT :: Functor m => DesugarT m a -> m (a, IdentMap Bool)
runDesugarT = fmap (fmap (fmap snd)) . runDesugarT'

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
  Rewrite.Query e ->
    Query <$> desugarExp e
  Rewrite.Module e -> do
    i <- supply
    (e, xs) <- lift . runDesugarT $ desugarExp e
    pure $ Module i xs e
  Rewrite.Enum e -> do
    i <- supply
    ((L _ nes), xs) <- lift . runDesugarT $ desugarEnum i e
    let (ns, es) = Prelude.unzip $ map ( \ (L p (n,e)) -> (n, L p e)) nes
    pure $ Language.Verse.Desugar.Exp.Enum i xs ns es
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
    tellName x False
    pure $ Name $ extract x
  Rewrite.Var x -> do
    tellName x True
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
    Fun xs e_domain <$> exists (desugarExp e)
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x False
    e <- desugarExp e
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    x <- freshIdent $ loc e
    pure $ BracketInvoke e (Name <$> x)
  MixfixArrowColonEqual x y e -> do
    tellName x False
    tellName y False
    e <- desugarDomain' e $ Name <$> x
    pure $ (ArchetypeName <$> y) :=: e
  Rewrite.Name x ->
    pure $ Name x
  Rewrite.IfArchetypeName x y e1 e2 -> do
    e1 <- desugarExp e1
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
  List [] -> pure $ Tuple []
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
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x False
    e <- desugarDomain' e i
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    pure $ BracketInvoke e i
  MixfixArrowColonEqual x y e -> do
    tellName x False
    tellName y False
    e <- desugarDomain' e i
    pure $ unify (ArchetypeName <$> y) e :*>: unify (Name <$> x) i
  Rewrite.IfArchetypeName x y e1 e2 -> do
    e1 <- desugarDomain' e1 i
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

desugarEnum
  :: (MonadAbort Error m, MonadSupply Label m)
  => Label
  -> L (Rewrite.Exp L Ident)
  -> DesugarT m (L [L (Ident, Exp L Ident)])
desugarEnum i e = for e $ \ case
  Rewrite.List [] ->
    pure []
  Rewrite.List (e:es) -> do
    e <- desugarEnum' i (0, e)
    es <- traverse (desugarEnum' i) (zip [1 ..] es)
    pure $ e:es
  _ -> abort $ EnumError (loc e)

desugarEnum'
  :: (MonadAbort Error m, MonadSupply Label m)
  => Label
  -> (Integer, L (Rewrite.Exp L Ident))
  -> DesugarT m (L (Ident, Exp L Ident))
desugarEnum' i (index, e) = for e $ \ case
  Rewrite.Name x -> do
    tellName (x <$ e) False
    y <- freshIdent $ loc e
    let e' = EnumValue i index <$ e
    pure $ (x, (ArchetypeName x <$ e') :=: ifArchetypeName x (extract y) (Name <$> y) e')
  _ -> abort . EnumError $ loc e

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

then' :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
then' = liftL2 (:*>:)

ifArchetypeName :: Apply f => a -> a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ifArchetypeName x y = liftL2 $ IfArchetypeName x y

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y

freshIdent :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent loc = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, False)
  pure $ L loc x

tellName :: MonadAbort Error m => L Ident -> Bool -> DesugarT m ()
tellName x var =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, var)
      Just (y, _) -> abort $ DefError y (loc x) (extract x))
  (extract x) =<<
  get

tellFunName :: Monad m => L Ident -> DesugarT m ()
tellFunName x =
  modify $ HashMap.insertWith (\ _ x -> x) (extract x) (loc x, False)

exists :: Monad m => DesugarT m (L (Exp L Ident)) -> DesugarT m (L (Exp L Ident))
exists m = do
  (e, xs) <- lift $ runDesugarT' m
  pure $ exists' xs e

exists' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, var) = Exists var (L loc x) z <$ z
