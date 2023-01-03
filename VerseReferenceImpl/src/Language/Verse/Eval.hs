{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Eval
  ( eval
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad (join)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Verse

import Data.Fix
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Ratio
import Data.Ref
import Data.Traversable (for)

import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Simplify.Exp (Exp ( (:*>:)
                                        , (:=:)
                                        , (:<:)
                                        , (:<=:)
                                        , (:>:)
                                        , (:>=:)
                                        , (:|:)
                                        , (:+:)
                                        , (:-:)
                                        , (:*:)
                                        , (:/:)
                                        ))
import Language.Verse.Simplify.Exp qualified as Exp
import Language.Verse.Name
import Language.Verse.Loc (Loc, L, loc)
import Language.Verse.Val (Val)
import Language.Verse.Val qualified as Val

type Env m = HashMap (Ident Name) (Var m Val)

type EvalT m = ReaderT (Env m) m

eval :: ( MonadError Error m
        , MonadFix m
        , MonadRef m
        , EqRef (Ref m)
        ) => L (Exp L (Ident Name)) -> m [Fix Val]
eval e = runSupplyT $ runVerseT $ runReaderT (eval' e) mempty >>= freeze >>= \ case
  Nothing -> throwError $ StuckError $ loc e
  Just x -> pure x

eval' :: ( MonadError Error m
         , MonadVerse m
         , EqRef (Ref m)
         ) => L (Exp L (Ident Name)) -> EvalT m (Var m Val)
eval' e = case extract e of
  e1 :*>: e2 -> do
    eval' e1 *> eval' e2
  e1 :=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    unify var1 var2
    pure var1
  e1 :<: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftOrd (loc e) (<) var1 var2
  e1 :<=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftOrd (loc e) (<=) var1 var2
  e1 :>: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftOrd (loc e) (>) var1 var2
  e1 :>=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftOrd (loc e) (>=) var1 var2
  e1 :|: e2 ->
    eval' e1 <|> eval' e2
  e1 :+: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftNum (loc e) (+) var1 var2
  e1 :-: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftNum (loc e) (-) var1 var2
  e1 :*: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    liftNum (loc e) (*) var1 var2
  e1 :/: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    div' (loc e) var1 var2
  Exp.Fail ->
    empty
  Exp.One e -> do
    var <- freshVar
    once' (eval' e) $ \ var_e -> unify var var_e
    pure var
  Exp.All e -> do
    var <- freshVar
    all' (eval' e) $ \ vars_e -> unify var =<< newVar (Val.Tuple vars_e)
    pure var
  Exp.Not e -> do
    lnot' $ eval' e
    newVar $ Val.Tuple []
  Exp.Query e -> do
    var <- freshVar
    join $ unify <$> newVar (Val.Truth var) <*> eval' e
    pure var
  Exp.IfThenElse xs p t e -> do
    var <- freshVar
    ifte'
      (do
          xs <- for (HashSet.toMap xs) $ const freshVar
          _ <- localNames xs $ eval' p
          pure xs)
      (\ xs ->
         unify var =<< localNames xs (eval' t))
      (unify var =<< eval' e)
    pure var
  Exp.ForDo xs e1 e2 -> do
    var <- freshVar
    for'
      (do
          xs <- for (HashSet.toMap xs) $ const freshVar
          _ <- localNames xs $ eval' e1
          pure xs)
      (\ xs ->
         localNames xs $ eval' e2)
      (\ vars ->
         unify var =<< newVar (Val.Tuple vars))
    pure var
  Exp.Exists x e -> do
    var <- freshVar
    localName (extract x) var $ eval' e
  Exp.Invoke e1 e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    var <- freshVar
    whenBound var1 $ \ case
      Val.Tuple xs ->
        foldr
        (\ (x, i) z -> ((unify var2 =<< newVar (Val.Int i)) *> unify var x) <|> z)
        empty
        (zip xs [0 ..])
      Val.Lambda x xs e ->
        unify var =<< local (const $ HashMap.insert x var2 xs) (eval' e)
      _ ->
        throwError $ DomainError $ loc e
    pure var
  Exp.Lambda x xs e -> do
    env <- ask
    newVar $ Val.Lambda (extract x) (HashMap.intersection env $ HashSet.toMap xs) e
  Exp.Tuple exps ->
    newVar =<< Val.Tuple <$> traverse eval' exps
  Exp.Truth e ->
    newVar =<< Val.Truth <$> eval' e
  Exp.Int x ->
    newVar $ Val.Int x
  Exp.Float x ->
    newVar $ Val.Float x
  Exp.Name x -> lookupName x >>= \ case
    Nothing -> throwError $ IdentError (loc e) x
    Just var -> pure var
  Exp.IsInt e -> do
    var <- eval' e
    isInt var

liftOrd :: (MonadError Error m, MonadVerse m) =>
           Loc ->
           (forall a . Ord a => a -> a -> Bool) ->
           Var m Val -> Var m Val -> m (Var m Val)
liftOrd loc f var_x var_y = do
  var <- freshVar
  whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
    unify var =<< case (val_x, val_y) of
      (Val.Int x, Val.Int y) ->
        newBool var_x $ f x y
      (Val.Int x, Val.Float y) ->
        newBool var_x $ f (fromInteger x) y
      (Val.Int x, Val.Rational y) ->
        newBool var_x $ f (fromInteger x) y
      (Val.Float x, Val.Int y) ->
        newBool var_x $ f x (fromInteger y)
      (Val.Float x, Val.Float y) ->
        newBool var_x $ f x y
      (Val.Float x, Val.Rational y) ->
        newBool var_x $ f (toRational x) y
      (Val.Rational x, Val.Int y) ->
        newBool var_x $ f x (fromInteger y)
      (Val.Rational x, Val.Float y) ->
        newBool var_x $ f x (toRational y)
      (Val.Rational x, Val.Rational y) ->
        newBool var_x $ f x y
      _ -> throwDomainError loc
  pure var
  where
    newBool var = \ case
      False -> empty
      True -> pure var

liftNum :: (MonadError Error m, MonadVerse m) =>
           Loc ->
           (forall a . Num a => a -> a -> a) ->
           Var m Val -> Var m Val -> m (Var m Val)
liftNum loc f var_x var_y = do
  var <- freshVar
  whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
    unify var =<< case (val_x, val_y) of
      (Val.Int x, Val.Int y) ->
        newVar . Val.Int $ f x y
      (Val.Int x, Val.Float y) ->
        newVar . Val.Float $ f (fromInteger x) y
      (Val.Int x, Val.Rational y) ->
        newVar . Val.Rational $ f (fromInteger x) y
      (Val.Float x, Val.Int y) ->
        newVar . Val.Float $ f x (fromInteger y)
      (Val.Float x, Val.Float y) ->
        newVar . Val.Float $ f x y
      (Val.Float x, Val.Rational y) ->
        newVar . Val.Float $ fromRational $ f (toRational x) y
      (Val.Rational x, Val.Int y) ->
        newVar . Val.Rational $ f x (fromInteger y)
      (Val.Rational x, Val.Float y) ->
        newVar . Val.Float $ fromRational $ f x (toRational y)
      (Val.Rational x, Val.Rational y) ->
        newVar . Val.Rational $ f x y
      _ -> throwDomainError loc
  pure var

div' :: ( MonadError Error m
        , MonadVerse m
        ) => Loc -> Var m Val -> Var m Val -> m (Var m Val)
div' loc var_x var_y = do
  var <- freshVar
  whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
    unify var =<< case (val_x, val_y) of
      (Val.Int _, Val.Int 0) ->
        empty
      (Val.Int x, Val.Int y) ->
        newVar $ Val.Rational $ x % y
      (Val.Int x, Val.Float y) ->
        newVar $ Val.Float $ fromInteger x / y
      (Val.Int x, Val.Rational y) ->
        newVar $ Val.Rational $ fromInteger x / y
      (Val.Float x, Val.Int y) ->
        newVar $ Val.Float $ x / fromInteger y
      (Val.Float x, Val.Float y) ->
        newVar $ Val.Float $ x / y
      (Val.Float x, Val.Rational y) ->
        newVar $ Val.Float $ fromRational $ toRational x / y
      (Val.Rational x, Val.Int y) ->
        newVar $ Val.Rational $ x / fromInteger y
      (Val.Rational x, Val.Float y) ->
        newVar $ Val.Float $ fromRational $ x / toRational y
      (Val.Rational _, Val.Rational 0) ->
        empty
      (Val.Rational x, Val.Rational y) ->
        newVar $ Val.Rational $ x / y
      _ -> throwDomainError loc
  pure var

isInt :: (MonadError Error m, MonadVerse m) =>
         Var m Val -> m (Var m Val)
isInt var_x = do
  var <- freshVar
  whenBound var_x $ \ case
    Val.Int _ -> unify var var_x
    _ -> empty
  pure var

lookupName :: Monad m => Ident Name -> EvalT m (Maybe (Var m Val))
lookupName = asks . HashMap.lookup

localName :: Monad m => Ident Name -> Var m Val -> EvalT m a -> EvalT m a
localName x = local . HashMap.insert x

localNames :: Monad m => Env m -> EvalT m a -> EvalT m a
localNames = local . (<>)

throwDomainError :: MonadError Error m => Loc -> m a
throwDomainError = throwError . DomainError
