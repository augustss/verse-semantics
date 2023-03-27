{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Eval
  ( eval
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.RST
import Control.Monad.State.Class
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Unify
import Control.Monad.Var
import Control.Monad.Verse.Class

import Data.Fix
import Data.Foldable
import Data.Freshenable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Ratio
import Data.Traversable
import Data.Unifiable

import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Intrinsic qualified as Intrinsic
import Language.Verse.Label
import Language.Verse.Desugar.Exp (Exp ((:*>:), (:=:), (:.:), (:..:), (:|:)))
import Language.Verse.Desugar.Exp qualified as Exp
import Language.Verse.Loc (Loc, L, loc)
import Language.Verse.Name
import Language.Verse.Named (Named (..))
import Language.Verse.Overload qualified as Overload
import Language.Verse.Val (Val)
import Language.Verse.Val qualified as Val

type EvalT m = WriterT (Defaults m) (RST (Env m) (Var m StoreFree) m)

data StoreFree a = StoreFree deriving (Functor, Foldable, Traversable)

instance Unifiable StoreFree

instance Zippable StoreFree where
  zipMatch _ _ = Just []

type MonadEval m =
  ( MonadError Error m
  , MonadSupply Label m
  , MonadVerse m
  , EqVarRef (VarRef m)
  )

type Defaults m = HashMap Ident (Var m (Val m), Env m, L (Exp L Ident))

type Env m = HashMap Ident (Named m (Var m (Val m)))

newtype Env1 m f = Env1
  { unEnv1 :: HashMap Ident (Named m (f (Val m)))
  }

instance Freshenable (Env1 m) where
  freshen f xs = fmap Env1 . for (unEnv1 xs) $ \ case
    Val x -> Val <$> f x
    Ref x -> pure $ Ref x

runEvalT :: MonadVar m => EvalT m a -> m a
runEvalT m = do
  storeFree <- newVar StoreFree
  env <- newEnv
  evalRST (evalWriterT m) env storeFree

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

eval :: MonadEval m => L (Exp L Ident) -> m (Fix (Val m))
eval e = runEvalT (eval' e) >>= freezeVar >>= \ case
  Nothing -> throwError $ StuckError $ loc e
  Just x -> pure x

eval' :: forall m . MonadEval m => L (Exp L Ident) -> EvalT m (Var m (Val m))
eval' e = case extract e of
  e1 :*>: e2 ->
    eval' e1 *> eval' e2
  e1 :=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    unify var1 var2
    pure var1
  e1 :.: x ->
    evalDot (loc e) e1 x
  e1 :..: e2 ->
    evalDotDot e1 e2
  e1 :|: e2 ->
    eval' e1 <|> eval' e2
  Exp.Fail ->
    empty
  Exp.One e -> do
    var <- freshVar
    storeFree <- get
    storeFree' <- freshVar
    once' (Identity1 <$> eval' e) $ \ (Identity1 var_e) -> do
      unify var var_e
      unify storeFree storeFree'
    put storeFree'
    pure var
  Exp.All e -> do
    var <- freshVar
    storeFree <- get
    storeFree' <- freshVar
    all' (Identity1 <$> eval' e) $ \ vars_e -> do
      unify var =<< newVar (Val.Tuple $ runIdentity1 <$> vars_e)
      unify storeFree storeFree'
    put storeFree'
    pure var
  Exp.Not e -> do
    ifte''
      (Unit1 <$ eval' e)
      (const empty)
      (pure ())
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
  Exp.Inst e1 xs e2 ->
    evalInst (loc e) e1 xs e2
  Exp.IfThenElse xs p t e -> do
    var <- freshVar
    ifte''
      (do
          xs <- for xs freshNamed
          _ <- localNames xs $ eval' p
          pure $ Env1 xs)
      (\ (Env1 xs) ->
          unify var =<< localNames xs (eval' t))
      (unify var =<< eval' e)
    pure var
  Exp.ForDo xs e1 e2 -> do
    var <- freshVar
    storeFree <- get
    storeFree' <- freshVar
    for'
      (do
          xs <- for xs freshNamed
          _ <- localNames xs $ eval' e1
          pure $ Env1 xs)
      (\ (Env1 xs) ->
          fmap Identity1 . localNames xs $ eval' e2)
      (\ vars -> do
          unify var =<< newVar (Val.Tuple $ runIdentity1 <$> vars)
          unify storeFree storeFree')
    put storeFree'
    pure var
  Exp.Exists x e -> do
    var <- freshVar
    localName (extract x) (Val var) $ eval' e
  Exp.Var x e -> do
    ref <- lift freshVarRef
    localName (extract x) (Ref ref) $ eval' e
  Exp.Set x e -> lookupName' (extract x) >>= \ case
    Nothing -> throwIdentError (loc x) (extract x)
    Just (Val _) -> throwDomainError $ loc e
    Just (Ref ref) -> do
      var <- eval' e
      writeVarRef' ref var
      pure var
  Exp.Function xs e1 e2 -> do
    i <- supply
    env <- ask
    newVar . Val.Overloads (Overload.Function i env xs e1 e2) =<< freshVar
  Exp.Invoke e1 e2 ->
    evalInvoke (loc e) e1 e2
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

evalDot :: MonadEval m =>
           Loc ->
           L (Exp L Ident) ->
           Name ->
           EvalT m (Var m (Val m))
evalDot loc e x = do
  var_e <- eval' e
  var <- freshVar
  whenBound var_e $ \ case
    Val.Module _ xs ->
      case HashMap.lookup x xs of
        Just (Ref ref_x) -> readVarRef' ref_x $ unify var
        Just (Val var_x) -> unify var var_x
        Nothing -> throwNameError loc x
    Val.StructInst _ xs ->
      case HashMap.lookup x xs of
        Just (Ref ref_x) -> readVarRef' ref_x $ unify var
        Just (Val var_x) -> unify var var_x
        Nothing -> throwNameError loc x
    Val.ClassInst _ _ xs ->
      case HashMap.lookup x xs of
        Just (Ref ref_x) -> readVarRef' ref_x $ unify var
        Just (Val var_x) -> unify var var_x
        Nothing -> throwNameError loc x
    _ -> throwDomainError loc
  pure var

evalDotDot :: MonadEval m =>
              L (Exp L Ident) ->
              L (Exp L Ident) ->
              EvalT m (Var m (Val m))
evalDotDot e1 e2 = do
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

evalInst :: MonadEval m =>
            Loc ->
            L (Exp L Ident) ->
            IdentMap Bool ->
            L (Exp L Ident) ->
            EvalT m (Var m (Val m))
evalInst loc e1 xs e2 = do
  var1 <- eval' e1
  xs <- for xs freshNamed
  _ <- localNames xs $ eval' e2
  let xs' = fromIdents xs
  var <- freshVar
  whenBound var1 $ \ case
    Val.Overloads overload var1 -> fix (\ recur overload var1 ->
      case overload of
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
          instSuper loc var_super xs' $ \ var_super defs_super ys_super -> do
            ys <- (ys_super <>) <$> for (ys \\ ys_super) freshNamed
            defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
            let ys' = fromIdents ys
            for_ (HashMap.intersectionWith (,) ys' xs') $
              uncurry unifyNamed
            let defs' = fromIdents $ defs <> defs_super
            for_ (HashMap.intersection defs' $ ys' \\ xs') $ \ (var, env, e) ->
              unify var =<< local (const env) (eval' e)
            unify var =<< newVar (Val.ClassInst i var_super ys')
        _ -> whenBound var1 $ \ case
          Val.Overloads overload var1 -> recur overload var1
          _ -> throwDomainError loc) overload var1
    _ -> throwDomainError loc
  pure var

evalInvoke :: MonadEval m =>
              Loc ->
              L (Exp L Ident) ->
              L (Exp L Ident) ->
              EvalT m (Var m (Val m))
evalInvoke loc e1 e2 = do
  var1 <- eval' e1
  var2 <- eval' e2
  var <- freshVar
  storeFree <- get
  storeFree' <- freshVar
  whenBound var1 $ \ case
    Val.Tuple xs -> do
      foldr
        (\ (x, i) z -> ((unify var2 =<< newVar (Val.Int i)) *> unify var x) <|> z)
        empty
        (zip xs [0 ..])
      unify storeFree storeFree'
    Val.Overloads overload var1 ->
      fix (\ recur overload var1 -> case overload of
        Overload.Function _ env xs e_domain e ->
          ifte''
          (do
              xs <- for xs freshNamed
              let env' = xs <> env
              unify var2 =<< local (const env') (eval' e_domain)
              pure $ Env1 xs)
          (\ (Env1 xs) -> do
              let env' = xs <> env
              unify var =<< local (const env') (eval' e)
              unify storeFree storeFree') $
          whenBound var1 $ \ case
            Val.Overloads overload var1 -> recur overload var1
            _ -> throwDomainError loc
        Overload.Struct i env xs e -> do
          unify var var2
          xs <- for xs freshNamed
          _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
          unify var2 =<< newVar (Val.StructInst i $ fromIdents xs)
          unify storeFree storeFree'
        Overload.Class i env var_super xs e -> do
          unify var var2
          fix (\ recur var2 -> whenBound var2 $ \ case
            Val.ClassInst j _ _  | i == j ->
              instSuper' loc var_super $ \ var_super xs_super -> do
                xs <- (xs_super <>) <$> for (xs \\ xs_super) freshNamed
                _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
                unify var2 =<< newVar (Val.ClassInst i var_super $ fromIdents xs)
            Val.ClassInst _ (Just var2) _ -> recur var2
            _ -> empty) var2
          unify storeFree storeFree'
        Overload.Intrinsic intrinsic ->
          invokeIntrinsic intrinsic var2 $ \ case
            Just var' -> do
              unify var var'
              unify storeFree storeFree'
            Nothing -> whenBound var1 $ \ case
              Val.Overloads overload var1 -> recur overload var1
              _ -> throwDomainError loc) overload var1
    _ -> throwDomainError loc
  put storeFree'
  pure var

invokeIntrinsic :: (MonadVerse m, EqVarRef (VarRef m)) =>
                   Intrinsic ->
                   Var m (Val m) ->
                   (Maybe (Var m (Val m)) -> EvalT m ()) ->
                   EvalT m ()
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

liftOrd :: (MonadVerse m, EqVarRef (VarRef m)) =>
           (forall a . Ord a => a -> a -> Bool) ->
           Var m (Val m) ->
           (Maybe (Var m (Val m)) -> EvalT m ()) ->
           EvalT m ()
liftOrd f var k =
  ifte''
  (do
      var_x <- freshVar
      var_y <- freshVar
      unify var =<< newVar (Val.Tuple [var_x, var_y])
      var_p <- freshVar
      whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
        unify var_p =<< case (val_x, val_y) of
          (Val.Int x, Val.Int y) -> newBool $ f x y
          (Val.Int x, Val.Float y) -> newBool $ f (fromInteger x) y
          (Val.Int x, Val.Rational y) -> newBool $ f (fromInteger x) y
          (Val.Float x, Val.Int y) -> newBool $ f x (fromInteger y)
          (Val.Float x, Val.Float y) -> newBool $ f x y
          (Val.Float x, Val.Rational y) -> newBool $ f (toRational x) y
          (Val.Rational x, Val.Int y) -> newBool $ f x (fromInteger y)
          (Val.Rational x, Val.Float y) -> newBool $ f x (toRational y)
          (Val.Rational x, Val.Rational y) -> newBool $ f x y
          _ -> empty
      pure $ Sum1 (Identity1 var_p) (Identity1 var_x))
  (\ (Sum1 (Identity1 var_p) (Identity1 var_x)) ->
      whenBound var_p $ getConst >>> \ case
        True -> k $ Just var_x
        False -> empty)
  (k Nothing)

newBool :: MonadVar m => Bool -> EvalT m (Var m (Const Bool))
newBool = newVar . Const

liftNum :: (MonadVerse m, EqVarRef (VarRef m)) =>
           (forall a . Num a => a -> a -> a) ->
           Var m (Val m) ->
           (Maybe (Var m (Val m)) -> EvalT m ()) ->
           EvalT m ()
liftNum f var k =
  ifte''
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
      pure $ Identity1 var')
  (k . Just . runIdentity1)
  (k Nothing)

prefixPlus :: MonadVerse m =>
              Var m (Val f) ->
              (Maybe (Var m (Val f)) -> EvalT m ()) ->
              EvalT m ()
prefixPlus var k =
  ifte''
  (do
      whenBound var $ \ case
        Val.Int _ -> pure ()
        Val.Float _ -> pure ()
        Val.Rational _ -> pure ()
        _ -> empty
      pure $ Identity1 var)
  (k . Just . runIdentity1)
  (k Nothing)

prefixMinus :: (MonadVerse m, EqVarRef (VarRef m)) =>
               Var m (Val m) ->
               (Maybe (Var m (Val m)) -> EvalT m ()) ->
               EvalT m ()
prefixMinus var k =
  ifte''
  (do
      var' <- freshVar
      whenBound var $ \ val -> unify var' =<< case val of
        Val.Int x -> newVar . Val.Int $ negate x
        Val.Float x -> newVar . Val.Float $ negate x
        Val.Rational x -> newVar . Val.Rational $ negate x
        _ -> empty
      pure $ Identity1 var')
  (k . Just . runIdentity1)
  (k Nothing)

data Div = Int !Integer | Float !Double | Rational !Rational deriving Eq

div' :: (MonadVerse m, EqVarRef (VarRef m)) =>
        Var m (Val m) ->
        (Maybe (Var m (Val m)) -> EvalT m ()) ->
        EvalT m ()
div' var k =
  ifte''
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
            newVar . Const . Just . Rational $ x % y
          (Val.Int x, Val.Float y) ->
            newVar . Const . Just . Float $ fromInteger x / y
          (Val.Int x, Val.Rational y) ->
            newVar . Const . Just . Rational $ fromInteger x / y
          (Val.Float x, Val.Int y) ->
            newVar . Const . Just . Float $ x / fromInteger y
          (Val.Float x, Val.Float y) ->
            newVar . Const . Just . Float $ x / y
          (Val.Float x, Val.Rational y) ->
            newVar . Const . Just . Float $ fromRational $ toRational x / y
          (Val.Rational x, Val.Int y) ->
            newVar . Const . Just . Rational $ x / fromInteger y
          (Val.Rational x, Val.Float y) ->
            newVar . Const . Just . Float $ fromRational $ x / toRational y
          (Val.Rational _, Val.Rational 0) ->
            newVar $ Const Nothing
          (Val.Rational x, Val.Rational y) ->
            newVar . Const . Just . Rational $ x / y
          _ -> empty
      pure $ Identity1 var')
  (\ (Identity1 var) -> whenBound var $ getConst >>> \ case
      Nothing -> empty
      Just (Int x) -> k . Just =<< newVar (Val.Int x)
      Just (Float x) -> k . Just =<< newVar (Val.Float x)
      Just (Rational x) -> k . Just =<< newVar (Val.Rational x))
  (k Nothing)

int :: MonadVerse m =>
       Var m (Val m) ->
       (Maybe (Var m (Val m)) -> EvalT m ()) ->
       EvalT m ()
int var k =
  ifte''
  (do
      whenBound var $ \ case
        Val.Int _ -> pure ()
        Val.Rational x | denominator x == 1 -> pure ()
        _ -> empty
      pure $ Identity1 var)
  (k . Just . runIdentity1)
  (k Nothing)

instSuper :: MonadEval m =>
             Loc ->
             Maybe (Var m (Val m)) -> HashMap Name (Named m (Var m (Val m))) ->
             (Maybe (Var m (Val m)) -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instSuper loc var_super xs f = case var_super of
  Nothing -> f Nothing mempty mempty
  Just var_super -> instClass loc var_super xs $ f . Just

instSuper' :: MonadEval m =>
             Loc ->
             Maybe (Var m (Val m)) ->
             (Maybe (Var m (Val m)) -> Env m -> EvalT m ()) ->
             EvalT m ()
instSuper' loc var_super f = case var_super of
  Nothing -> f Nothing mempty
  Just var_super -> instClass' loc var_super $ f . Just

instClass :: MonadEval m =>
             Loc ->
             Var m (Val m) -> HashMap Name (Named m (Var m (Val m))) ->
             (Var m (Val m) -> Defaults m -> Env m -> EvalT m ()) ->
             EvalT m ()
instClass loc var xs f = whenBound var $ \ case
  Val.Overloads overload var -> case overload of
    Overload.Class i env var_super ys e ->
      instSuper loc var_super xs $ \ var_super defs_super ys_super -> do
        ys <- (ys_super <>) <$> for ys freshNamed
        defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
        let ys' = fromIdents ys
        for_ (HashMap.intersectionWith (,) ys' xs) $
          uncurry unifyNamed
        var' <- newVar $ Val.ClassInst i var_super ys'
        f var' (defs <> defs_super) ys
    _ -> instClass loc var xs f
  _ -> throwDomainError loc

instClass' :: MonadEval m =>
              Loc ->
              Var m (Val m) ->
              (Var m (Val m)-> Env m -> EvalT m ()) ->
              EvalT m ()
instClass' loc var f = whenBound var $ \ case
  Val.Overloads overload var -> case overload of
    Overload.Class i env var_super ys e ->
      instSuper' loc var_super $ \ var_super ys_super -> do
        ys <- (ys_super <>) <$> for ys freshNamed
        _ <- local (const $ ys <> env) . lift . evalWriterT $ eval' e
        var' <- newVar $ Val.ClassInst i var_super $ fromIdents ys
        f var' ys
    _ -> instClass' loc var f
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
              , EqVarRef (VarRef m)
              ) => Ident -> EvalT m (Maybe (Var m (Val m)))
lookupName = lookupName' >=> \ case
  Nothing -> pure Nothing
  Just (Ref ref) -> do
    var <- freshVar
    readVarRef' ref $ unify var
    pure $ Just var
  Just (Val x) -> pure $ Just x

lookupName' :: Monad m => Ident -> EvalT m (Maybe (Named m (Var m (Val m))))
lookupName' = asks . HashMap.lookup

localName :: Monad m => Ident -> Named m (Var m (Val m)) -> EvalT m a -> EvalT m a
localName x = local . HashMap.insert x

localNames :: (Semigroup r, MonadReader r m) => r -> m a -> m a
localNames = local . (<>)

throwDomainError :: MonadError Error m => Loc -> m a
throwDomainError = throwError . DomainError

throwIdentError :: MonadError Error m => Loc -> Ident -> m a
throwIdentError x = throwError . IdentError x

throwNameError :: MonadError Error m => Loc -> Name -> m a
throwNameError x = throwError . NameError x

fromIdents :: HashMap Ident a -> HashMap Name a
fromIdents =
  HashMap.fromList .
  HashMap.foldrWithKey
  (\ case
      Ident.Name x -> \ y z -> (x, y) : z
      Ident.Label _ -> \ _ z -> z)
  []

unifyNamed :: ( MonadVerse m
              , EqVarRef (VarRef m)
              ) => Named m (Var m (Val m)) -> Named m (Var m (Val m)) -> EvalT m ()
unifyNamed = curry $ lift . \ case
  (Val var_x, Val var_y) ->
    unify var_x var_y
  (Ref ref_x, Val var_y) ->
    readVarRef' ref_x $ unify var_y
  (Val var_x, Ref ref_y) ->
    readVarRef' ref_y $ unify var_x
  (Ref ref_x, Ref ref_y) ->
    readVarRef' ref_x $ \ var_x ->
    readVarRef' ref_y $ \ var_y ->
    unify var_x var_y

freshNamed :: MonadVarRef m => Bool -> EvalT m (Named m (Var m (Val m)))
freshNamed = \ case
  False -> Val <$> freshVar
  True -> Ref <$> freshVarRef

freshVarRef :: (MonadVarRef m, Traversable f) => m (VarRef m f)
freshVarRef = newVarRef =<< freshVar

ifte'' :: (MonadVerse m, Freshenable f) =>
          EvalT m (f (Var m)) ->
          (f (Var m) -> EvalT m ()) ->
          EvalT m () ->
          EvalT m ()
ifte'' p t e = do
  storeFree <- get
  storeFree' <- freshVar
  ifte'
    p
    (\ x -> t x *> unify storeFree storeFree')
    (e *> unify storeFree storeFree')
  put storeFree

readVarRef' :: ( MonadState (Var m StoreFree) m
               , MonadVarRef m
               , MonadVerse m
               ) => VarRef m f -> (Var m f -> m ()) -> m ()
readVarRef' ref f = do
  storeFree <- get
  storeFree' <- freshVar
  whenBound storeFree . const $ do
    f =<< readVarRef ref
    unify storeFree storeFree'
  put storeFree'

writeVarRef' :: ( Traversable f
                , MonadState (Var m StoreFree) m
                , MonadVarRef m
                , MonadVerse m
                ) => VarRef m f -> Var m f -> m ()
writeVarRef' ref x = do
  storeFree <- get
  storeFree' <- freshVar
  whenBound storeFree . const $ do
    writeVarRef ref x
    unify storeFree storeFree'
  put storeFree'

(\\) :: Hashable k => HashMap k a -> HashMap k b -> HashMap k a
(\\) = HashMap.difference
