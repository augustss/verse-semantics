{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.RST
import Control.Monad.State.Class
import Control.Monad.Supply
import Control.Monad.Trans.Class

import Data.Foldable
import Data.Functor
import Data.Functor.Apply
import Data.HashMap.Strict (HashMap, foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.Traversable

import Language.Verse.Desugar.Exp
import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Parse.Exp qualified as Parse

type Desugar = RST Bool Env (SupplyT Label (Except Error))

type Env = HashMap (Ident Name) (Loc, Bool)

runDesugar :: Desugar a -> SupplyT Label (Except Error) (a, Env)
runDesugar m = runRST m False mempty

desugar :: L (Parse.Exp L Name) -> Either Error (L (Exp L (Ident Name)))
desugar e = runExcept . runSupplyT $ do
  (e, xs) <- runDesugar $ desugar' e
  pure $ exists' xs e

desugar' :: L (Parse.Exp L Name) -> Desugar (L (Exp L (Ident Name)))
desugar' e = for e $ \ case
  (Parse.:=:) e1 e2 -> ask >>= \ case
    False -> (:=:) <$> desugar' e1 <*> desugar' e2
    True -> do
      (x, e1) <- (,) <$> getIdent e1 <*> desugar' e1
      e2 <- local (const False) $ desugar' e2
      pure $ Default x e1 e2
  (Parse.:<>:) e1 e2 -> do
    Not . (<$ e) <$> ((:=:) <$> desugar' e1 <*> desugar' e2)
  (Parse.:.:) e x ->
    (:.: x) <$> desugar' e
  (Parse.:..:) e1 e2 ->
    (:..:) <$> desugar' e1 <*> desugar' e2
  (Parse.:<:) e1 e2 ->
    desugarOperator2 "operator'<'" e1 e2
  (Parse.:<=:) e1 e2 ->
    desugarOperator2 "operator'<='" e1 e2
  (Parse.:>:) e1 e2 ->
    desugarOperator2 "operator'>'" e1 e2
  (Parse.:>=:) e1 e2 ->
    desugarOperator2 "operator'>='" e1 e2
  (Parse.:|:) e1 e2 ->
    (:|:) <$> desugar' e1 <*> desugar' e2
  (Parse.:+:) e1 e2 ->
    desugarOperator2 "operator'+'" e1 e2
  Parse.PrefixPlus e ->
    desugarOperator1 "prefix'+'" e
  (Parse.:-:) e1 e2 ->
    desugarOperator2 "operator'-'" e1 e2
  Parse.PrefixMinus e ->
    desugarOperator1 "prefix'-'" e
  (Parse.:*:) e1 e2 ->
    desugarOperator2 "operator'*'" e1 e2
  (Parse.:/:) e1 e2 ->
    desugarOperator2 "operator'/'" e1 e2
  Parse.List [] ->
    pure $ Tuple []
  Parse.List (e:es) -> extract <$> do
    e <- desugar' e
    es <- traverse desugar' es
    pure $ foldl' (\ z x -> (:*>:) <$> duplicate z <.> duplicate x) e es
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
  Parse.Module e -> do
    i <- supply
    (e, xs) <- lift $ runDesugar $ desugar' e
    pure $ Module i (snd <$> xs) e
  Parse.Struct e -> do
    i <- supply
    (e, xs) <- lift $ runDesugar $ local (const True) $ desugar' e
    pure $ Struct i (snd <$> xs) e
  Parse.Class e1 e2 -> do
    i <- supply
    e1 <- traverse desugar' e1
    (e2, xs) <- lift $ runDesugar $ local (const True) $ desugar' e2
    pure $ Class i e1 (snd <$> xs) e2
  Parse.Inst e1 e2 -> do
    e1 <- desugar' e1
    (e2, xs) <- lift $ runDesugar $ desugar' e2
    pure $ Inst e1 (snd <$> xs) e2
  Parse.If p -> do
    (p, xs) <- lift $ runDesugar $ desugar' p
    pure $ IfThenElse (snd <$> xs) p (Tuple [] <$ p) (Tuple [] <$ p)
  Parse.IfThen p t -> do
    (p, xs) <- lift $ runDesugar $ desugar' p
    IfThenElse (snd <$> xs) p <$>
      exists (desugar' t) <*>
      pure (Tuple [] <$ p <. t)
  Parse.IfThenElse p t e -> do
    (p, xs) <- lift $ runDesugar $ desugar' p
    IfThenElse (snd <$> xs) p <$>
      exists (desugar' t) <*>
      exists (desugar' e)
  Parse.For e ->
    All <$> exists (desugar' e)
  Parse.ForDo e1 e2 -> do
    (e1, xs) <- lift $ runDesugar $ desugar' e1
    ForDo (snd <$> xs) e1 <$> exists (desugar' e2)
  Parse.Block e ->
    extract <$> exists (desugar' e)
  Parse.Exists x -> do
    tellName x False
    pure . Name . Pure $ extract x
  Parse.Var x -> do
    tellName x True
    pure . Name . Pure $ extract x
  Parse.Set x e ->
    Set (Pure <$> x) <$> desugar' e
  Parse.Function e1 e2 -> do
    (e1, xs) <- lift $ runDesugar $ desugar' e1
    Function (snd <$> xs) e1 <$> exists (desugar' e2)
  Parse.Overload x e1 e2 -> do
    tellName' x False
    let x' = Pure <$> x
    (e1, xs) <- lift $ runDesugar $ desugar' e1
    e2 <- exists $ desugar' e2
    let e = Function (snd <$> xs) <$> duplicate e1 <.> duplicate e2
    ask <&> \ case
      False -> (Name <$> x') :=: e
      True -> Default x' (Name <$> x') e
  Parse.ParenInvoke e1 e2 ->
    Invoke <$> desugar' e1 <*> desugar' e2
  Parse.BracketInvoke e1 e2 ->
    Invoke <$> desugar' e1 <*> desugar' e2
  Parse.Tuple es ->
    Tuple <$> traverse desugar' es
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
    pure . Name $ Pure x
  Parse.PrefixColon e -> do
    e <- desugar' e
    e_i <- (e $>) . Name <$> freshIdent (loc e) False
    ask <&> \ case
      False -> Invoke e e_i
      True -> (Invoke <$> duplicate e <.> duplicate e_i) :*>: e_i
  Parse.InfixColon x e -> do
    tellName x False
    let e1 = Name . Pure <$> x
    e <- desugar' e
    e_i <- (e $>) . Name <$> freshIdent (loc e) False
    let e2 = Invoke <$> duplicate e <.> duplicate e_i
    pure $ ((:=:) <$> duplicate e1 <.> duplicate e2) :*>: e_i
  Parse.ArrowInfixColon x y e -> do
    tellName x False
    let e3 = Name . Pure <$> x
    tellName y False
    let e1 = Name . Pure <$> y
    e <- desugar' e
    let e2 = Invoke <$> duplicate e <.> duplicate e3
    pure $ ((:=:) <$> duplicate e1 <.> duplicate e2) :*>: e3
  Parse.InfixColonEqual x e -> do
    tellName x False
    let x' = Pure <$> x
    ask >>= \ case
      False -> desugar' e <&> ((Name <$> x') :=:)
      True -> local (const False) (desugar' e) <&> Default x' (Name <$> x')

desugarOperator1 :: Name ->
                    L (Parse.Exp L Name) ->
                    Desugar (Exp L (Ident Name))
desugarOperator1 x e =
  desugar' e <&> \ e ->
  Invoke (Name (Pure x) <$ e) e

desugarOperator2 :: Name ->
                    L (Parse.Exp L Name) ->
                    L (Parse.Exp L Name) ->
                    Desugar (Exp L (Ident Name))
desugarOperator2 x e1 e2 =
  (,) <$> desugar' e1 <*> desugar' e2 <&> \ (e1, e2) ->
  Invoke (Name (Pure x) <$ e1 <. e2) (Tuple [e1, e2] <$ e1 <. e2)

getIdent :: L (Parse.Exp L Name) -> Desugar (L (Ident Name))
getIdent e = case extract e of
  Parse.Exists x -> pure $ Pure <$> x
  Parse.InfixColon x _ -> pure $ Pure <$> x
  _ -> throwError $ AnonError $ loc e

freshIdent :: Loc -> Bool -> Desugar (Ident Name)
freshIdent loc var = do
  x <- Label <$> supply
  modify $ HashMap.insert x (loc, var)
  pure x

tellName :: L Name -> Bool -> Desugar ()
tellName x var =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, var)
      Just (y, _) -> throwError $ DefError y (loc x) (extract x))
  (Pure $ extract x) =<<
  get

tellName' :: L Name -> Bool -> Desugar ()
tellName' x var =
  modify $ HashMap.insertWith (flip const) (Pure $ extract x) (loc x, var)

exists :: Desugar (L (Exp L (Ident Name))) -> Desugar (L (Exp L (Ident Name)))
exists m = lift $ do
  (e, xs) <- runDesugar m
  pure $ exists' xs e

exists' :: Env -> L (Exp L (Ident Name)) -> L (Exp L (Ident Name))
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, var) =
      (if var then Var else Exists) <$>
      duplicate (L loc x) <.>
      duplicate z
