{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Eval
  ( Pure (..)
  , eval
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Ref.Backtrack qualified as Backtrack
import Control.Monad.Supply
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Unify
import Control.Monad.Var
import Control.Monad.Verse (runVerseT)
import Control.Monad.Verse.Class

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
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Intrinsic qualified as Intrinsic
import Language.Verse.Label
import Language.Verse.Desugar.Exp (Exp ((:*>:), (:=:), (:.:), (:..:), (:|:)))
import Language.Verse.Desugar.Exp qualified as Exp
import Language.Verse.Loc (Loc, L, loc)
import Language.Verse.Name
import Language.Verse.Overload qualified as Overload
import Language.Verse.Val (Val)
import Language.Verse.Val qualified as Val

import Prettyprinter

type EvalT m = WriterT (Defaults m) (ReaderT (Env m) m)

type MonadEval m =
  ( MonadError Error m
  , MonadSupply Label m
  , MonadVerse m
  , Unifiable (World m)
  , EqRef (Backtrack.Ref m)
  )

type Defaults m = HashMap (Ident Name) (MutVar m, Env m, L (Exp L (Ident Name)))

type Env m = HashMap (Ident Name) (Named m)

type Named m = Mut m (MutVar m)

type MutVar m = Var m (MutVal m)

type MutVal m = Val (Mut m)

data Mut m a
  = Ref (Backtrack.Ref m (MutVar m))
  | Val a deriving (Functor, Foldable, Traversable)

instance EqRef (Backtrack.Ref m) => Unifiable (Mut m)

instance EqRef (Backtrack.Ref m) => Zippable (Mut m) where
  zipMatch = curry $ \ case
    (Ref x, Ref y) | eqRef x y -> Just []
    (Val x, Val y) -> Just [(x, y)]
    _ -> Nothing

data Pure a
  = Read
  | Pure a deriving (Show, Functor, Foldable, Traversable)

instance Pretty a => Pretty (Pure a) where
  pretty = \ case
    Read -> "ref"
    Pure x -> pretty x

freeze' :: ( Backtrack.MonadRef m
           , MonadVar m
           ) => MutVar m -> m (Maybe (Fix (Val Pure)))
freeze' = freezeBy $ Val.hoist $ \ case
  Ref _ -> Read
  Val var -> Pure var

runEvalT :: MonadVar m => EvalT m a -> m a
runEvalT m = runReaderT (evalWriterT m) =<< newEnv

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
          Just (Ref ref_x) -> readRef' ref_x $ unify var
          Just (Val var_x) -> unify var var_x
          Nothing -> throwNameError (loc e) x
      Val.StructInst _ xs ->
        case HashMap.lookup x xs of
          Just (Ref ref_x) -> readRef' ref_x $ unify var
          Just (Val var_x) -> unify var var_x
          Nothing -> throwNameError (loc e) x
      Val.ClassInst _ _ xs ->
        case HashMap.lookup x xs of
          Just (Ref ref_x) -> readRef' ref_x $ unify var
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
  e1 :|: e2 ->
    eval' e1 <|> eval' e2
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
    newVar . Val.Overloads (Overload.Struct i env xs e) =<< freshVar
  Exp.Class i e_super xs e -> do
    env <- ask
    var_super <- for e_super eval'
    newVar . Val.Overloads (Overload.Class i env var_super xs e) =<< freshVar
  Exp.Inst e1 xs e2 -> do
    var1 <- eval' e1
    xs <- for xs freshNamed
    _ <- localNames xs $ eval' e2
    let xs' = fromIdents xs
    var <- freshVar
    whenBound' var1 $ \ case
      Val.Overloads x var_xs -> fix (\ recur x var_xs -> case x of
        Overload.Struct i env ys e -> do
          ys <- for ys freshNamed
          defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
          let ys' = fromIdents ys
          for_ (HashMap.intersectionWith (,) ys' xs') $
            uncurry unifyNamed
          let defs' = fromIdents defs
          for_ (HashMap.intersection defs' $ ys' \\ xs') $ \ (var, env, e) ->
            unify var =<< local (const env) (eval' e)
          unify var =<< newVar (Val.StructInst i ys')
        Overload.Class i env var_super ys e ->
          instSuper (loc e) var_super xs' $ \ var_super defs_super ys_super -> do
            ys <- (ys_super <>) <$> for (ys \\ ys_super) freshNamed
            defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
            let ys' = fromIdents ys
            for_ (HashMap.intersectionWith (,) ys' xs') $
              uncurry unifyNamed
            let defs' = fromIdents $ defs <> defs_super
            for_ (HashMap.intersection defs' $ ys' \\ xs') $ \ (var, env, e) ->
              unify var =<< local (const env) (eval' e)
            unify var =<< newVar (Val.ClassInst i var_super ys')
        _ -> whenBound' var_xs $ \ case
          Val.Overloads x var_xs -> recur x var_xs
          _ -> throwDomainError $ loc e) x var_xs
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
    ref <- lift $ newRef' =<< freshVar
    localName (extract x) (Ref ref) $ eval' e
  Exp.Set x e -> lookupName' (extract x) >>= \ case
    Nothing -> throwIdentError (loc x) (extract x)
    Just (Val _) -> throwDomainError $ loc e
    Just (Ref ref) -> do
      var <- eval' e
      lift $ writeRef' ref =<< freshen var
      pure var
  Exp.Function xs e1 e2 -> do
    i <- supply
    env <- ask
    newVar . Val.Overloads (Overload.Function i env xs e1 e2) =<< freshVar
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
      Val.Overloads x var_xs -> fix (\ recur x var_xs -> case x of
        Overload.Function _ env xs e_arg e ->
          ifte'
          (evalWriterT $ do
              xs <- for xs freshNamed
              let env' = xs <> env
              unify var2 =<< local (const env') (eval' e_arg)
              pure env')
          (\ env' -> unify var =<< evalWriterT (local (const env') $ eval' e)) $
          whenBound var_xs $ \ case
            Val.Overloads x var_xs -> recur x var_xs
            _ -> throwDomainError $ loc e
        Overload.Struct i env xs e -> evalWriterT $ do
          unify var var2
          xs <- for xs freshNamed
          _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
          unify var2 =<< newVar (Val.StructInst i $ fromIdents xs)
        Overload.Class i env var_super xs e -> do
          unify var var2
          fix (\ recur var2 -> do
            whenBound var2 $ \ case
              Val.ClassInst j var_super' _
                | i == j ->
                  evalWriterT $ instSuper' (loc e) var_super $ \ var_super xs_super -> do
                    xs <- (xs_super <>) <$> for (xs \\ xs_super) freshNamed
                    _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
                    unify var2 =<< newVar (Val.ClassInst i var_super $ fromIdents xs)
                | Just var2 <- var_super' -> recur var2
              _ -> empty) var2
        Overload.Intrinsic x -> invokeIntrinsic x var2 $ \ case
          Just var' -> unify var var'
          Nothing -> whenBound var_xs $ \ case
            Val.Overloads x var_xs -> recur x var_xs
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
    tell $ HashMap.singleton (extract x) (var1, env, e2)
    pure var1

invokeIntrinsic :: (MonadVerse m, Zippable f) =>
                   Intrinsic ->
                   Var m (Val f) ->
                   (Maybe (Var m (Val f)) -> m ()) ->
                   m ()
invokeIntrinsic = \ case
  Intrinsic.Less -> liftOrd (<)
  Intrinsic.LessEqual -> liftOrd (<=)
  Intrinsic.Greater -> liftOrd (>)
  Intrinsic.GreaterEqual -> liftOrd (>=)
  Intrinsic.Plus -> liftNum (+)
  Intrinsic.PrefixPlus -> prefixPlus
  Intrinsic.Minus -> liftNum (-)
  Intrinsic.PrefixMinus -> prefixMinus
  Intrinsic.Multiply -> liftNum (*)
  Intrinsic.Divide -> div'
  Intrinsic.Int -> int

liftOrd :: (MonadVerse m, Zippable f) =>
           (forall a . Ord a => a -> a -> Bool) ->
           Var m (Val f) ->
           (Maybe (Var m (Val f)) -> m ()) ->
           m ()
liftOrd f var k =
  ifte'
  (do
      var_x <- freshVar
      var_y <- freshVar
      unify var =<< newVar (Val.Tuple [var_x, var_y])
      var_p <- freshVar
      whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
        unify var_p =<< case (val_x, val_y) of
          (Val.Int x, Val.Int y) -> newVar . Const $ f x y
          (Val.Int x, Val.Float y) -> newVar . Const $ f (fromInteger x) y
          (Val.Int x, Val.Rational y) -> newVar . Const $ f (fromInteger x) y
          (Val.Float x, Val.Int y) -> newVar . Const $ f x (fromInteger y)
          (Val.Float x, Val.Float y) -> newVar . Const $ f x y
          (Val.Float x, Val.Rational y) -> newVar . Const $ f (toRational x) y
          (Val.Rational x, Val.Int y) -> newVar . Const $ f x (fromInteger y)
          (Val.Rational x, Val.Float y) -> newVar . Const $ f x (toRational y)
          (Val.Rational x, Val.Rational y) -> newVar . Const $ f x y
          _ -> empty
      pure (var_x, var_p))
  (\ (var_x, var_p) -> whenBound var_p $ \ val_p -> do
      guard $ getConst val_p
      k $ Just var_x)
  (k Nothing)

liftNum :: (MonadVerse m, Zippable f) =>
           (forall a . Num a => a -> a -> a) ->
           Var m (Val f) ->
           (Maybe (Var m (Val f)) -> m ()) ->
           m ()
liftNum f var k =
  ifte'
  (do
      var_x <- freshVar
      var_y <- freshVar
      unify var =<< newVar (Val.Tuple [var_x, var_y])
      var' <- freshVar
      whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
        unify var' =<< case (val_x, val_y) of
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
          _ -> empty
      pure var')
  (k . Just)
  (k Nothing)

prefixPlus :: MonadVerse m =>
              Var m (Val f) ->
              (Maybe (Var m (Val f)) -> m ()) ->
              m ()
prefixPlus var k =
  ifte'
  (do
      whenBound var $ \ case
        Val.Int _ -> pure ()
        Val.Float _ -> pure ()
        Val.Rational _ -> pure ()
        _ -> empty
      pure var)
  (k . Just)
  (k Nothing)

prefixMinus :: (MonadVerse m, Zippable f) =>
               Var m (Val f) ->
               (Maybe (Var m (Val f)) -> m ()) ->
               m ()
prefixMinus var k =
  ifte'
  (do
      var' <- freshVar
      whenBound var $ \ val -> unify var' =<< case val of
        Val.Int x -> newVar . Val.Int $ negate x
        Val.Float x -> newVar . Val.Float $ negate x
        Val.Rational x -> newVar . Val.Rational $ negate x
        _ -> empty
      pure var')
  (k . Just)
  (k Nothing)

div' :: (MonadVerse m, Zippable f) =>
        Var m (Val f) ->
        (Maybe (Var m (Val f)) -> m ()) ->
        m ()
div' var k =
  ifte'
  (do
      var_x <- freshVar
      var_y <- freshVar
      unify var =<< newVar (Val.Tuple [var_x, var_y])
      var' <- freshVar
      whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
        unify var' =<< case (val_x, val_y) of
          (Val.Int _, Val.Int 0) -> do
            newVar $ Const Nothing
          (Val.Int x, Val.Int y) ->
            newVar . Const . Just <=< newVar $ Val.Rational $ x % y
          (Val.Int x, Val.Float y) ->
            newVar . Const . Just <=< newVar $ Val.Float $ fromInteger x / y
          (Val.Int x, Val.Rational y) ->
            newVar . Const . Just <=< newVar $ Val.Rational $ fromInteger x / y
          (Val.Float x, Val.Int y) ->
            newVar . Const . Just <=< newVar $ Val.Float $ x / fromInteger y
          (Val.Float x, Val.Float y) ->
            newVar . Const . Just <=< newVar $ Val.Float $ x / y
          (Val.Float x, Val.Rational y) ->
            newVar . Const . Just <=< newVar $ Val.Float $ fromRational $ toRational x / y
          (Val.Rational x, Val.Int y) ->
            newVar . Const . Just <=< newVar $ Val.Rational $ x / fromInteger y
          (Val.Rational x, Val.Float y) ->
            newVar . Const . Just <=< newVar $ Val.Float $ fromRational $ x / toRational y
          (Val.Rational _, Val.Rational 0) ->
            newVar $ Const Nothing
          (Val.Rational x, Val.Rational y) ->
            newVar . Const . Just <=< newVar $ Val.Rational $ x / y
          _ -> empty
      pure var')
  (\ var -> whenBound var $ getConst >>> \ case
      Nothing -> empty
      Just var -> k $ Just var)
  (k Nothing)

int :: MonadVerse m => Var m (Val f) -> (Maybe (Var m (Val f)) -> m ()) -> m ()
int var k = whenBound var $ \ case
  Val.Int _ -> k $ Just var
  Val.Rational x | denominator x == 1 -> k $ Just var
  _ -> empty

instSuper :: MonadEval m =>
             Loc ->
             Maybe (MutVar m) -> HashMap Name (Named m) ->
             (Maybe (MutVar m) -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instSuper loc var_super xs f = case var_super of
  Nothing -> f Nothing mempty mempty
  Just var_super -> instClass loc var_super xs $ f . Just

instSuper' :: MonadEval m =>
             Loc ->
             Maybe (MutVar m) ->
             (Maybe (MutVar m) -> Env m -> EvalT m ()) ->
             EvalT m ()
instSuper' loc var_super f = case var_super of
  Nothing -> f Nothing mempty
  Just var_super -> instClass' loc var_super $ f . Just

instClass :: MonadEval m =>
             Loc ->
             MutVar m -> HashMap Name (Named m) ->
             (MutVar m -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instClass loc var_class xs f = whenBound' var_class $ \ case
  Val.Overloads x var_xs -> case x of
    Overload.Class i env var_super ys e ->
      instSuper loc var_super xs $ \ var_super defs_super ys_super -> do
        ys <- (ys_super <>) <$> for ys freshNamed
        defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
        let ys' = fromIdents ys
        for_ (HashMap.intersectionWith (,) ys' xs) $
          uncurry unifyNamed
        var <- newVar (Val.ClassInst i var_super ys')
        f var (defs <> defs_super) ys
    _ -> instClass loc var_xs xs f
  _ -> throwDomainError loc

instClass' :: MonadEval m =>
              Loc ->
              MutVar m ->
              (MutVar m-> Env m -> EvalT m ()) ->
              EvalT m ()
instClass' loc var_class f = whenBound' var_class $ \ case
  Val.Overloads x var_xs -> case x of
    Overload.Class i env var_super ys e ->
      instSuper' loc var_super $ \ var_super ys_super -> do
        ys <- (ys_super <>) <$> for ys freshNamed
        _ <- local (const $ ys <> env) . lift . evalWriterT $ eval' e
        var <- newVar (Val.ClassInst i var_super $ fromIdents ys)
        f var ys
    _ -> instClass' loc var_xs f
  _ -> throwDomainError loc

newEnv :: MonadVar m => m (Env m)
newEnv = execWriterT $ do
  tell' "operator'<'" Intrinsic.Less
  tell' "operator'<='" Intrinsic.LessEqual
  tell' "operator'>'" Intrinsic.Greater
  tell' "operator'>='" Intrinsic.GreaterEqual
  tell' "operator'+'" Intrinsic.Plus
  tell' "prefix'+'" Intrinsic.PrefixPlus
  tell' "operator'-'" Intrinsic.Minus
  tell' "prefix'-'" Intrinsic.PrefixMinus
  tell' "operator'*'" Intrinsic.Multiply
  tell' "operator'/'" Intrinsic.Divide
  tell' "int" Intrinsic.Int
  where
    tell' x y =
      tell . HashMap.singleton x . Val =<<
      newVar . Val.Overloads (Overload.Intrinsic y) =<<
      freshVar

lookupName :: ( MonadVerse m
              , Unifiable (World m)
              , EqRef (Backtrack.Ref m)
              ) => Ident Name -> EvalT m (Maybe (MutVar m))
lookupName = lookupName' >=> \ case
  Nothing -> pure Nothing
  Just (Ref ref) -> do
    var <- freshVar
    lift $ readRef' ref $ unify var
    pure $ Just var
  Just (Val x) -> pure $ Just x

lookupName' :: Monad m => Ident Name -> EvalT m (Maybe (Named m))
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
              , Unifiable (World m)
              , EqRef (Backtrack.Ref m)
              ) => Named m -> Named m -> EvalT m ()
unifyNamed = curry $ lift . \ case
  (Val var_x, Val var_y) ->
    unify var_x var_y
  (Ref ref_x, Val var_y) ->
    readRef' ref_x $ unify var_y
  (Val var_x, Ref ref_y) ->
    readRef' ref_y $ unify var_x
  (Ref ref_x, Ref ref_y) ->
    readRef' ref_x $ \ var_x ->
    readRef' ref_y $ \ var_y ->
    unify var_x var_y

freshNamed :: ( Backtrack.MonadRef m
              , MonadVar m
              ) => Bool -> EvalT m (Named m)
freshNamed = \ case
  False -> Val <$> freshVar
  True -> Ref <$> freshRef

freshRef :: ( Backtrack.MonadRef m
            , MonadVar m
            ) => m (Backtrack.Ref m (Var m f))
freshRef = newRef' =<< freshVar

whenBound' :: ( Monoid w
              , MonadVerse m
              ) => Var m f -> (f (Var m f) -> WriterT w m ()) -> WriterT w m ()
whenBound' x f = lift . whenBound x $ evalWriterT . f

ifte'' :: (Monoid w, MonadVerse m) =>
          WriterT w m a ->
          (a -> WriterT w m ()) ->
          WriterT w m () ->
          WriterT w m ()
ifte'' p t e = lift $ ifte' (evalWriterT p) (evalWriterT . t) (evalWriterT e)

newRef' :: Backtrack.MonadRef m => a -> m (Backtrack.Ref m a)
newRef' = Backtrack.newRef

readRef' :: ( Backtrack.MonadRef m
            , MonadVerse m
            , Unifiable (World m)
            ) => Backtrack.Ref m a -> (a -> m ()) -> m ()
readRef' ref f = do
  world <- getWorld
  world' <- freshVar
  whenBound world $ \ _ -> do
    world'' <- getWorld
    putWorld world
    f =<< Backtrack.readRef ref
    unify world' =<< getWorld
    putWorld world''

writeRef' :: ( Backtrack.MonadRef m
             , MonadVerse m
             , Unifiable (World m)
             ) => Backtrack.Ref m a -> a -> m ()
writeRef' ref x = do
  world <- getWorld
  world' <- freshVar
  whenBound world $ \ _ -> do
    world'' <- getWorld
    putWorld world
    Backtrack.writeRef ref x
    unify world' =<< getWorld
    putWorld world''

(\\) :: Hashable k => HashMap k a -> HashMap k b -> HashMap k a
(\\) = HashMap.difference
