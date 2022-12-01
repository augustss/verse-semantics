{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Language.Verse.Simplify
  ( simplify
  ) where

import Control.Comonad
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Except
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS

import Data.HashMap.Strict (HashMap, traverseWithKey)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List.Diff qualified as Diff
import Data.Traversable (for)

import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Loc (L, loc)
import Language.Verse.Name
import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Simplify.Exp

type Simplify = WriterT W (ReaderT R (SupplyT Word (Except Error)))

type W = Diff.List (Ident Name, Level)

data R = R { level :: !Level, env :: !Env }

type Env = HashMap Name (Ident Name, Level)

type Level = Word

simplify :: L (Desugar.Exp L Name) -> Either Error (L (Exp L (Ident Name)))
simplify =
  runExcept .
  runSupplyT .
  flip runReaderT (R minBound mempty) .
  evalWriterT .
  simplify'

simplify' :: L (Desugar.Exp L Name) -> Simplify (L (Exp L (Ident Name)))
simplify' e = for e $ \ case
  (Desugar.:*>:) e1 e2 ->
    (:*>:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:=:) e1 e2 ->
    (:=:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:|:) e1 e2 ->
    (:|:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:+:) e1 e2 ->
    (:+:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:-:) e1 e2 ->
    (:-:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:*:) e1 e2 ->
    (:*:) <$> simplify' e1 <*> simplify' e2
  (Desugar.:/:) e1 e2 ->
    (:/:) <$> simplify' e1 <*> simplify' e2
  Desugar.Fail ->
    pure Fail
  Desugar.One e ->
    One <$> simplify' e
  Desugar.All e ->
    All <$> simplify' e
  Desugar.Not e ->
    Not <$> simplify' e
  Desugar.Query e ->
    Query <$> simplify' e
  Desugar.IfThenElse xs p t e -> newEnv xs >>= \ xs ->
    IfThenElse (fromEnv xs) <$>
    localNames xs (simplify' p) <*>
    localNames xs (simplify' t) <*>
    simplify' e
  Desugar.ForDo xs e1 e2 -> newEnv xs >>= \ xs ->
    ForDo (fromEnv xs) <$>
    localNames xs (simplify' e1) <*>
    localNames xs (simplify' e2)
  Desugar.Exists x e -> do
    x' <- for x newIdent
    e' <- localName (extract x) (extract x') $ simplify' e
    pure $ Exists x' e'
  Desugar.Invoke e1 e2 ->
    Invoke <$> simplify' e1 <*> simplify' e2
  Desugar.Lambda x e -> do
    x' <- for x newIdent
    (e, xs) <- localLambda (extract x) (extract x') $ simplify' e
    pure $ Lambda x' xs e
  Desugar.Tuple exps ->
    Tuple <$> for exps simplify'
  Desugar.Truth e ->
    Truth <$> simplify' e
  Desugar.Int x ->
    pure $ Int x
  Desugar.Float x ->
    pure $ Float x
  Desugar.Name x -> lookupName x >>= \ case
    Nothing ->
      throwError $ NameError (loc e) x
    Just (x', level_x) -> do
      tellName x' level_x
      pure $ Name x'
  Desugar.Colon e -> do
    e1' <- simplify' e
    x' <- freshIdent
    let e2' = Exists (x' <$ e) <$> duplicate (Name x' <$ e)
    pure $ Invoke e1' e2'

newIdent :: MonadSupply Word m => a -> m (Ident a)
newIdent x = Ident <$> supply <*> pure (Just x)

freshIdent :: MonadSupply Word m => m (Ident a)
freshIdent = flip Ident Nothing <$> supply

localLambda :: Name -> Ident Name -> Simplify a -> Simplify (a, HashSet (Ident Name))
localLambda x x' m = do
  level <- (+ 1) <$> askLevel
  let f r = R { level, env = HashMap.insert x (x', level) r.env }
  (y, xs) <- lift . fmap (fmap Diff.toList) . runWriterT $ local f m
  tell $ Diff.fromList $ filter ((< level) . snd) xs
  pure (y, HashSet.fromList $ fst <$> xs)

tellName :: Ident Name -> Level -> Simplify ()
tellName x level_x = do
  level <- askLevel
  when (level_x < level) $ tell $ pure (x, level_x)

lookupName :: Name -> Simplify (Maybe (Ident Name, Level))
lookupName x = asks $ HashMap.lookup x . env

localNames :: Env -> Simplify a -> Simplify a
localNames = localEnv . (<>)

localName :: Name -> Ident Name -> Simplify a -> Simplify a
localName x y m = do
  level <- askLevel
  localEnv (HashMap.insert x (y, level)) $ m

localEnv :: (Env -> Env) -> Simplify a -> Simplify a
localEnv f = local (\ R {..} -> R { env = f env, .. })

newEnv :: HashSet Name -> Simplify Env
newEnv xs = do
  level <- askLevel
  traverseWithKey (\ x _ -> (, level) <$> newIdent x) $ HashSet.toMap xs

fromEnv :: Env -> HashSet (Ident Name)
fromEnv = HashSet.fromList . fmap fst . HashMap.elems

askLevel :: Simplify Level
askLevel = asks level

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT
