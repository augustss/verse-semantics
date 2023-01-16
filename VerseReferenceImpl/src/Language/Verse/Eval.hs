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
import Control.Monad (guard, join)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Var
import Control.Monad.Verse (MonadVerse (..), runVerseT)

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Ratio
import Data.Ref
import Data.Traversable (for)

import Language.Verse.Error
import Language.Verse.Ident
import Language.Verse.Label
import Language.Verse.Simplify.Exp (Exp ( (:*>:)
                                        , (:=:)
                                        , (:.:)
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
         , MonadSupply Label m
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
  e :.: x -> do
    var_e <- eval' e
    var <- freshVar
    whenBound var_e $ \ case
      Val.Module _ xs
        | Just var_x <- HashMap.lookup x xs -> unify var var_x
        | otherwise -> throwNameError (loc e) x
      Val.StructInst _ xs
        | Just var_x <- HashMap.lookup x xs -> unify var var_x
        | otherwise -> throwNameError (loc e) x
      _ -> throwDomainError $ loc e
    pure var
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
  Exp.Module i xs e -> do
    xs <- for (HashSet.toMap xs) $ const freshVar
    _ <- localNames xs $ eval' e
    newVar . Val.Module i $ toNames xs
  Exp.Struct i ys xs e -> do
    env <- asks $ flip HashMap.intersection (HashSet.toMap ys)
    newVar $ Val.Struct i env xs e
  Exp.Inst e1 xs e2 -> do
    var1 <- eval' e1
    xs <- for (HashSet.toMap xs) $ const freshVar
    _ <- localNames xs $ eval' e2
    var <- freshVar
    whenBound var1 $ \ case
      Val.Struct i env ys e -> do
        ys <- for (HashSet.toMap ys) $ const freshVar
        _ <- local (const $ ys <> env) (eval' e)
        let ys' = toNames ys
        for_ (HashMap.intersectionWith (,) (toNames xs) ys') $ uncurry unify
        unify var =<< newVar (Val.StructInst i ys')
      _ -> throwDomainError $ loc e
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
  Exp.Function ys xs e1 e2 -> do
    i <- supply
    env <- asks $ flip HashMap.intersection (HashSet.toMap ys)
    newVar =<< Val.Overload (Val.Function i env xs e1 e2) <$> freshVar
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
      Val.Struct i env xs e -> do
        xs <- for (HashSet.toMap xs) $ const freshVar
        _ <- local (const $ xs <> env) (eval' e)
        unify var2 =<< newVar (Val.StructInst i $ toNames xs)
        unify var var2
      Val.Overload x var_xs -> fix (\ recur x var_xs ->
        let Val.Function _ env xs e_d e_r = x in
          ifte'
          (do
              xs <- for (HashSet.toMap xs) $ const freshVar
              let env' = xs <> env
              var_d <- local (const env') $ eval' e_d
              unify var2 var_d
              pure env')
          (\ env' -> unify var =<< local (const env') (eval' e_r)) $
          whenBound var_xs $ \ case
            Val.Overload x var_xs -> recur x var_xs
            _ -> throwDomainError $ loc e) x var_xs
      _ -> throwDomainError $ loc e
    pure var
  Exp.Tuple exps ->
    newVar . Val.Tuple =<< traverse eval' exps
  Exp.Truth e ->
    newVar . Val.Truth =<< eval' e
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
  whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
    case (val_x, val_y) of
      (Val.Int x, Val.Int y) ->
        guard $ f x y
      (Val.Int x, Val.Float y) ->
        guard $ f (fromInteger x) y
      (Val.Int x, Val.Rational y) ->
        guard $ f (fromInteger x) y
      (Val.Float x, Val.Int y) ->
        guard $ f x (fromInteger y)
      (Val.Float x, Val.Float y) ->
        guard $ f x y
      (Val.Float x, Val.Rational y) ->
        guard $ f (toRational x) y
      (Val.Rational x, Val.Int y) ->
        guard $ f x (fromInteger y)
      (Val.Rational x, Val.Float y) ->
        guard $ f x (toRational y)
      (Val.Rational x, Val.Rational y) ->
        guard $ f x y
      _ -> throwDomainError loc
  pure var_x

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

throwNameError :: MonadError Error m => Loc -> Name -> m a
throwNameError x = throwError . NameError x

toNames :: Hashable a => HashMap (Ident a) v -> HashMap a v
toNames =
  HashMap.fromList .
  HashMap.foldrWithKey
  (\ k x z ->
     case name k of
       Nothing -> z
       Just k -> (k, x) : z)
  []
