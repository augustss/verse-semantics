{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Except
import Control.Monad.State.Strict

import Data.Functor.Apply
import Data.HashMap.Strict (HashMap, foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.Traversable (for)

import Language.Verse.Desugar.Exp
import Language.Verse.Error
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Parse.Exp qualified as Parse

type Desugar = StateT (HashMap Name Loc) (Except Error)

runDesugar' :: Desugar a -> Except Error (a, HashMap Name Loc)
runDesugar' = flip runStateT mempty

desugar :: L (Parse.Exp L Name) -> Either Error (L (Exp L Name))
desugar e = runExcept $ do
  (e, xs) <- runDesugar' $ desugar' e
  pure $ exists' xs e

desugar' :: L (Parse.Exp L Name) -> Desugar (L (Exp L Name))
desugar' e = for e $ \ case
  (Parse.:*>:) e1 e2 ->
    (:*>:) <$> desugar' e1 <*> desugar' e2
  (Parse.:=:) e1 e2 ->
    (:=:) <$> desugar' e1 <*> desugar' e2
  (Parse.:<:) e1 e2 ->
    (:<:) <$> desugar' e1 <*> desugar' e2
  (Parse.:<=:) e1 e2 ->
    (:<=:) <$> desugar' e1 <*> desugar' e2
  (Parse.:>:) e1 e2 ->
    (:>:) <$> desugar' e1 <*> desugar' e2
  (Parse.:>=:) e1 e2 ->
    (:>=:) <$> desugar' e1 <*> desugar' e2
  (Parse.:|:) e1 e2 ->
    (:|:) <$> desugar' e1 <*> desugar' e2
  (Parse.:+:) e1 e2 ->
    (:+:) <$> desugar' e1 <*> desugar' e2
  (Parse.:-:) e1 e2 ->
    (:-:) <$> desugar' e1 <*> desugar' e2
  (Parse.:*:) e1 e2 ->
    (:*:) <$> desugar' e1 <*> desugar' e2
  (Parse.:/:) e1 e2 ->
    (:/:) <$> desugar' e1 <*> desugar' e2
  Parse.Fail ->
    pure Fail
  Parse.One e -> do
    One <$> exists (desugar' e)
  Parse.All e ->
    All <$> exists (desugar' e)
  Parse.Not e ->
    Not <$> desugar' e
  Parse.Query e ->
    Query <$> desugar' e
  Parse.If p -> do
    (p, xs) <- lift $ runDesugar' $ desugar' p
    pure $ IfThenElse (HashMap.keysSet xs) p (Tuple [] <$ p) (Tuple [] <$ p)
  Parse.IfThen p t -> do
    (p, xs) <- lift $ runDesugar' $ desugar' p
    IfThenElse (HashMap.keysSet xs) p <$>
      exists (desugar' t) <*>
      pure (Tuple [] <$ p <. t)
  Parse.IfThenElse p t e -> do
    (p, xs) <- lift $ runDesugar' $ desugar' p
    IfThenElse (HashMap.keysSet xs) p <$>
      exists (desugar' t) <*>
      exists (desugar' e)
  Parse.For e -> do
    (e, xs) <- lift $ runDesugar' $ desugar' e
    pure $ ForDo (HashMap.keysSet xs) e (Tuple [] <$ e)
  Parse.ForDo e1 e2 -> do
    (e1, xs) <- lift $ runDesugar' $ desugar' e1
    ForDo (HashMap.keysSet xs) e1 <$> exists (desugar' e2)
  Parse.Block e ->
    extract <$> exists (desugar' e)
  Parse.Exists x e ->
    Exists x <$> exists (desugar' e)
  Parse.Invoke e1 e2 ->
    Invoke <$> desugar' e1 <*> desugar' e2
  Parse.Lambda x e ->
    Lambda x <$> exists (desugar' e)
  Parse.Tuple exps ->
    Tuple <$> for exps desugar'
  Parse.Truth e ->
    Truth <$> exists (desugar' e)
  Parse.True ->
    pure $ Truth (Tuple [] <$ e)
  Parse.False ->
    pure $ Tuple []
  Parse.Int x ->
    pure $ Int x
  Parse.Float x ->
    pure $ Float x
  Parse.Name x ->
    pure $ Name x
  Parse.PrefixColon e ->
    Colon <$> desugar' e
  Parse.InfixColon x e -> do
    tellName x
    e <- desugar' e
    pure $ (Name <$> x) :=: (Colon <$> duplicate e)
  Parse.InfixColonEqual x e -> do
    tellName x
    e <- desugar' e
    pure $ (Name <$> x) :=: e

tellName :: L Name -> Desugar ()
tellName x = do
  s <- get
  case HashMap.lookup (extract x) s of
    Nothing -> put $ HashMap.insert (extract x) (loc x) s
    Just y -> throwError $ DefError y (loc x) (extract x)

exists :: Desugar (L (Exp L Name)) -> Desugar (L (Exp L Name))
exists m = lift $ do
  (e, xs) <- runDesugar' m
  pure $ exists' xs e

exists' :: HashMap Name Loc -> L (Exp L Name) -> L (Exp L Name)
exists' xs e = foldlWithKey' (\ z x y -> L y $ Exists (L y x) z) e xs
