{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Eval
  ( Pure (..)
  , eval
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Except (MonadError (..))
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Lenient qualified as Lenient
import Control.Monad.Supply
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Unify
import Control.Monad.Var
import Control.Monad.Verse (MonadVerse (..), runVerseT)

import Data.Fix
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Ratio
import Data.Ref
import Data.Traversable
import Data.Unifiable

import Language.Verse.Error
import Language.Verse.Ident (Ident)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Desugar.Exp (Exp ( (:*>:)
                                       , (:=:)
                                       , (:.:)
                                       , (:..:)
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
import Language.Verse.Desugar.Exp qualified as Exp
import Language.Verse.Name
import Language.Verse.Loc (Loc, L, loc)
import Language.Verse.Val (Val, hoistVal)
import Language.Verse.Val qualified as Val

import Prettyprinter

type EvalT m = WriterT (Defaults m) (ReaderT (Env m) m)

type MonadEval m =
  ( MonadError Error m
  , MonadSupply Label m
  , MonadVerse m
  , EqRef (Lenient.Ref m)
  )

type Defaults m = HashMap (Ident Name) (Env m, L (Exp L (Ident Name)))

type Env m = HashMap (Ident Name) (Named m)

type Named m = Mut m (MutVar m)

type MutVar m = Var m (MutVal m)

type MutVal m = Val (Mut m)

data Mut m a
  = Ref (Lenient.Ref m (MutVar m))
  | Val a deriving (Functor, Foldable, Traversable)

instance EqRef (Lenient.Ref m) => Unifiable (Mut m)

instance EqRef (Lenient.Ref m) => Zippable (Mut m) where
  zipMatch = curry $ \ case
    (Ref x, Ref y) | eqRef x y -> Just []
    (Val x, Val y) -> Just [(x, y)]
    _ -> Nothing

data Pure a
  = Read
  | Pure a deriving (Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Pure a) where
  pretty = \ case
    Read -> pretty '^'
    Pure x -> pretty x

freeze' :: MonadVerse m => MutVar m -> m (Maybe (Fix (Val Pure)))
freeze' = freezeBy $ hoistVal $ \ case
  Ref _ -> Read
  Val x -> Pure x

runEvalT :: Functor m => EvalT m a -> m a
runEvalT = flip runReaderT mempty . evalWriterT

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

eval :: ( MonadError Error m
        , MonadFix m
        , MonadRef m
        , EqRef (Ref m)
        ) => L (Exp L (Ident Name)) -> m [Fix (Val Pure)]
eval e = runSupplyT $ runVerseT $ runEvalT (eval' e) >>= freeze' >>= \ case
  Nothing -> throwError $ StuckError $ loc e
  Just x -> pure x

eval' :: MonadEval m => L (Exp L (Ident Name)) -> EvalT m (MutVar m)
eval' e = case extract e of
  e1 :*>: e2 ->
    eval' e1 *> eval' e2
  e1 :=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    unify var1 var2
    pure var1
  e :.: x -> do
    var_e <- eval' e
    var <- freshVar
    lift $ whenBound var_e $ \ case
      Val.Module _ xs ->
        case HashMap.lookup x xs of
          Just (Ref ref_x) -> Lenient.readRef ref_x $ unify var
          Just (Val var_x) -> unify var var_x
          Nothing -> throwNameError (loc e) x
      Val.StructInst _ xs ->
        case HashMap.lookup x xs of
          Just (Ref ref_x) -> Lenient.readRef ref_x $ unify var
          Just (Val var_x) -> unify var var_x
          Nothing -> throwNameError (loc e) x
      Val.ClassInst _ _ xs ->
        case HashMap.lookup x xs of
          Just (Ref ref_x) -> Lenient.readRef ref_x $ unify var
          Just (Val var_x) -> unify var var_x
          Nothing -> throwNameError (loc e) x
      _ -> throwDomainError $ loc e
    pure var
  e1 :..: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    var <- freshVar
    lift $ whenBound var1 $ \ case
      Val.Int val1 -> whenBound var2 $ \ case
        Val.Int val2 ->
          unify var =<<
          foldr (\ x z -> newVar (Val.Int x) <|> z) empty [val1 .. val2]
        _ -> throwDomainError $ loc e2
      _ -> throwDomainError $ loc e1
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
    lift $ once' (evalWriterT $ eval' e) $ \ var_e ->
      unify var var_e
    pure var
  Exp.All e -> do
    var <- freshVar
    lift $ for' (evalWriterT $ eval' e) freshen $ \ vars_e ->
      unify var =<< newVar (Val.Tuple vars_e)
    pure var
  Exp.Not e -> do
    lift . lnot' . evalWriterT $ eval' e
    newVar $ Val.Tuple []
  Exp.Query e -> do
    var_e <- eval' e
    var <- freshVar
    unify var_e =<< newVar (Val.Truth var)
    pure var
  Exp.Module i xs e -> do
    xs <- for xs freshNamed
    _ <- localNames xs $ eval' e
    newVar . Val.Module i $ fromIdents xs
  Exp.Struct i xs e -> do
    env <- ask
    newVar $ Val.Struct i env xs e
  Exp.Class i e_super xs e_body -> do
    env <- ask
    var_super <- for e_super eval'
    newVar $ Val.Class i env var_super xs e_body
  Exp.Inst e1 xs e2 -> do
    var1 <- eval' e1
    xs <- for xs freshNamed
    _ <- localNames xs $ eval' e2
    let xs' = fromIdents xs
    var <- freshVar
    whenBound' var1 $ \ case
      Val.Struct i env ys e -> do
        ys <- for ys freshNamed
        defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
        let ys' = fromIdents ys
        for_ (HashMap.intersectionWith (,) ys' xs') $
          uncurry unifyNamed
        let defs' = fromIdents defs
        for_ (HashMap.intersectionWith (,) (ys' \\ xs') defs') $ \ (y, (env, e)) ->
          unifyNamed y . Val =<< local (const env) (eval' e)
        unify var =<< newVar (Val.StructInst i ys')
      Val.Class i env var_super ys e_body ->
        instSuper (loc e) var_super xs $ \ var_super defs_super ys_super -> do
          ys <-  (ys_super <>) <$> for (ys \\ ys_super) freshNamed
          defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e_body
          let ys' = fromIdents ys
          for_ (HashMap.intersectionWith (,) ys' xs') $
            uncurry unifyNamed
          let defs' = fromIdents $ defs <> defs_super
          for_ (HashMap.intersectionWith (,) (ys' \\ xs') defs') $ \ (y, (env, e)) ->
            unifyNamed y . Val =<< local (const env) (eval' e)
          unify var =<< newVar (Val.ClassInst i var_super ys')
      _ -> throwDomainError $ loc e
    pure var
  Exp.IfThenElse xs p t e -> do
    var <- freshVar
    ifte''
      (do
          xs <- for xs freshNamed
          _ <- localNames xs $ eval' p
          pure xs)
      (\ xs -> unify var =<< localNames xs (eval' t))
      (unify var =<< eval' e)
    pure var
  Exp.ForDo xs e1 e2 -> do
    var <- freshVar
    lift $ for'
      (evalWriterT $ do
          xs <- for xs freshNamed
          _ <- localNames xs $ eval' e1
          pure xs)
      (\ xs -> freshen =<< evalWriterT (localNames xs $ eval' e2))
      (\ vars -> unify var =<< newVar (Val.Tuple vars))
    pure var
  Exp.Exists x e -> do
    var <- freshVar
    localName (extract x) (Val var) $ eval' e
  Exp.Var x e -> do
    ref <- lift $ Lenient.newRef =<< freshVar
    localName (extract x) (Ref ref) $ eval' e
  Exp.Set x e -> lookupName' (extract x) >>= \ case
    Nothing -> throwIdentError (loc x) (extract x)
    Just (Val _) -> throwDomainError $ loc e
    Just (Ref ref) -> do
      var <- eval' e
      lift $ Lenient.writeRef ref =<< freshen var
      pure var
  Exp.Function xs e1 e2 -> do
    i <- supply
    env <- ask
    newVar . Val.Overload (Val.Function i env xs e1 e2) =<< freshVar
  Exp.Invoke e1 e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    var <- freshVar
    lift $ whenBound var1 $ \ case
      Val.Tuple xs ->
        foldr
        (\ (x, i) z -> ((unify var2 =<< newVar (Val.Int i)) *> unify var x) <|> z)
        empty
        (zip xs [0 ..])
      Val.Overload x var_xs -> fix (\ recur x var_xs ->
        let Val.Function _ env xs e_arg e_body = x in
          ifte'
          (evalWriterT $ do
              xs <- for xs freshNamed
              let env' = xs <> env
              var_arg <- local (const env') $ eval' e_arg
              unify var2 var_arg
              pure env')
          (\ env' -> unify var =<< evalWriterT (local (const env') $ eval' e_body)) $
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
    Nothing -> throwIdentError (loc e) x
    Just var -> pure var
  Exp.Default x e1 e2 -> do
    var1 <- eval' e1
    env <- ask
    tell $ HashMap.singleton (extract x) (env, e2)
    pure var1
  Exp.IsInt e ->
    isInt =<< eval' e

liftOrd :: (MonadError Error m, MonadVerse m) =>
           Loc ->
           (forall a . Ord a => a -> a -> Bool) ->
           MutVar m -> MutVar m -> EvalT m (MutVar m)
liftOrd loc f var_x var_y = lift $ do
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

liftNum :: (MonadError Error m, MonadVerse m, EqRef (Lenient.Ref m)) =>
           Loc ->
           (forall a . Num a => a -> a -> a) ->
           MutVar m -> MutVar m -> EvalT m (MutVar m)
liftNum loc f var_x var_y = lift $ do
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

div' :: (MonadError Error m, MonadVerse m, EqRef (Lenient.Ref m)) =>
        Loc ->
        MutVar m -> MutVar m -> EvalT m (MutVar m)
div' loc var_x var_y = lift $ do
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

isInt :: ( MonadError Error m
         , MonadVerse m
         , EqRef (Lenient.Ref m)
         ) => MutVar m -> EvalT m (MutVar m)
isInt var_x = do
  var <- freshVar
  lift $ whenBound var_x $ \ case
    Val.Int _ -> unify var var_x
    _ -> empty
  pure var

instSuper :: MonadEval m =>
             Loc ->
             Maybe (MutVar m) -> Env m ->
             (Maybe (MutVar m) -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instSuper loc var_super xs f = case var_super of
  Nothing -> f Nothing mempty mempty
  Just var_super -> instClass loc var_super xs $ f . Just

instClass :: MonadEval m =>
             Loc ->
             MutVar m -> Env m ->
             (MutVar m -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instClass loc var_class xs f = whenBound' var_class $ \ case
  Val.Class i env var_super ys e_body ->
    instSuper loc var_super xs $ \ var_super defs_super ys_super -> do
      ys <-  (ys_super <>) <$> for ys freshNamed
      defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e_body
      let ys' = fromIdents ys
      for_ (HashMap.intersectionWith (,) (fromIdents xs) ys') $
        uncurry unifyNamed
      var <- newVar (Val.ClassInst i var_super ys')
      f var (defs <> defs_super) ys
  _ -> throwDomainError loc

lookupName :: ( MonadVerse m
              , EqRef (Lenient.Ref m)
              ) => Ident Name -> EvalT m (Maybe (MutVar m))
lookupName = lookupName' >=> \ case
  Nothing -> pure Nothing
  Just (Ref ref) -> do
    var <- freshVar
    lift $ Lenient.readRef ref $ unify var
    pure $ Just var
  Just (Val x) -> pure $ Just x

lookupName' :: ( MonadVerse m
               , EqRef (Lenient.Ref m)
               ) => Ident Name -> EvalT m (Maybe (Named m))
lookupName' = asks . HashMap.lookup

localName :: Monad m => Ident Name -> Named m -> EvalT m a -> EvalT m a
localName x = local . HashMap.insert x

localNames :: (Semigroup r, MonadReader r m) => r -> m a -> m a
localNames = local . (<>)

throwDomainError :: MonadError Error m => Loc -> m a
throwDomainError = throwError . DomainError

throwIdentError :: MonadError Error m => Loc -> Ident Name -> m a
throwIdentError x = throwError . IdentError x

throwNameError :: MonadError Error m => Loc -> Name -> m a
throwNameError x = throwError . NameError x

fromIdents :: Hashable a => HashMap (Ident a) v -> HashMap a v
fromIdents =
  HashMap.fromList .
  HashMap.foldrWithKey
  (\ case
      Ident.Pure x -> \ y z -> (x, y) : z
      Ident.Label _ -> \ _ z -> z)
  []

unifyNamed :: ( MonadVerse m
              , EqRef (Lenient.Ref m)
              ) => Named m -> Named m -> EvalT m ()
unifyNamed = curry $ lift . \ case
  (Val var_x, Val var_y) ->
    unify var_x var_y
  (Ref ref_x, Val var_y) ->
    Lenient.readRef ref_x $ unify var_y
  (Val var_x, Ref ref_y) ->
    Lenient.readRef ref_y $ unify var_x
  (Ref ref_x, Ref ref_y) ->
    Lenient.readRef ref_x $ \ var_x ->
    Lenient.readRef ref_y $ \ var_y ->
    unify var_x var_y

freshNamed :: MonadVerse m => Bool -> EvalT m (Named m)
freshNamed = \ case
  False -> Val <$> freshVar
  True -> Ref <$> freshRef

freshRef :: MonadVerse m => EvalT m (Lenient.Ref m (Var m f))
freshRef = lift . Lenient.newRef =<< freshVar

whenBound' :: ( Monoid w
              , MonadVerse m
              ) => Var m f -> (f (Var m f) -> WriterT w m ()) -> WriterT w m ()
whenBound' x f = lift . whenBound x $ evalWriterT . f

ifte'' :: ( Monoid w
          , MonadVerse m
          ) => WriterT w m a -> (a -> WriterT w m ()) -> WriterT w m () -> WriterT w m ()
ifte'' p t e = lift $ ifte' (evalWriterT p) (evalWriterT . t) (evalWriterT e)

(\\) :: Hashable k => HashMap k a -> HashMap k b -> HashMap k a
(\\) = HashMap.difference
