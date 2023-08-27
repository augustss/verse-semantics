{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Language.Verse.Eval
  ( eval
  ) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad
import Control.Monad
import Control.Monad.Abort
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Ref
import Control.Monad.RS
import Control.Monad.State.Class
import Control.Monad.Supply
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Verse

import Data.Bool
import Data.Eq
import Data.Foldable (foldr)
import Data.Function
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int
import Data.Kind
import Data.List (zip)
import Data.Match
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Traversable (Traversable, traverse)
import Data.Tuple
import Data.Semigroup

import Language.Verse.Desugar.Exp (Exp ((:*>:), (:=:), (:.:), (:..:), (:|:)))
import Language.Verse.Desugar.Exp qualified as Exp
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Intrinsic qualified as Intrinsic
import Language.Verse.Label
import Language.Verse.Loc (Loc, L, loc)
import Language.Verse.Name
import Language.Verse.Val (Val, VarVal, FrozenVal, Named (..))
import Language.Verse.Val qualified as Val

import Prelude (Integer, Num (..), fromRational, toRational)

type EvalT m = WriterT (Defaults m) (RST (Env m) (S m) (VerseT m))

data S m = S
  { choiceFree :: IVar m ()
  , storeFree :: IVar m ()
  }

instance Monad m => Freshenable (S n) m where
  freshen = pure

type Defaults m = HashMap Ident (VarVal m, Env m, L (Exp L Ident))

type Env m = HashMap Ident (Named (VarRef m) (VarVal m))

evalEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> VerseT m a
evalEvalT m = do
  env <- newEnv
  choiceFree <- newIVar ()
  storeFree <- newIVar ()
  evalRST (evalWriterT m) env S {..}

runEvalT' :: EvalT m a -> Env m -> S m -> VerseT m (a, S m)
runEvalT' = runRST . evalWriterT

evalEvalT' :: EvalT m a -> Env m -> S m -> VerseT m a
evalEvalT' = evalRST . evalWriterT

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

eval :: ( MonadAbort Error m
        , MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , EqRef (Ref m)
        ) => L (Exp L Ident) -> VerseT m FrozenVal
eval = freeze' <=< evalEvalT . eval'

eval' :: ( MonadAbort Error m
         , MonadFix m
         , MonadRef m
         , MonadSupply Label m
         , EqRef (Ref m)
         ) => L (Exp L Ident) -> EvalT m (VarVal m)
eval' e = case extract e of
  e1 :*>: e2 ->
    eval' e1 *> eval' e2
  e1 :=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    lift' $ unify var1 var2
    pure var1
  -- e1 :.: x ->
  --   evalDot (loc e) e1 x
  e1 :..: e2 ->
    evalDotDot e1 e2
  e1 :|: e2 ->
    evalChoice e1 e2
  Exp.Fail ->
    empty
  Exp.One e -> do
    var <- lift' freshVar
    env <- ask
    s <- get
    storeFree <- lift' freshIVar
    put s { storeFree }
    lift' $ fork do
      x <- readIVar =<< one do
        choiceFree <- newIVar ()
        evalEvalT' (eval' e) env s { choiceFree }
      writeIVar storeFree ()
      unify var x
    pure var
  Exp.All e -> do
    var <- lift' freshVar
    env <- ask
    s <- get
    storeFree <- lift' freshIVar
    put s { storeFree }
    lift' $ fork do
      xs <- readIVar =<< all do
        choiceFree <- newIVar ()
        evalEvalT' (eval' e) env s { choiceFree }
      writeIVar storeFree ()
      unify var =<< newVar (Val.Tuple xs)
    pure var
--   Exp.Not e -> do
--     if''
--       (eval' e)
--       (const empty)
--       (pure ())
--     newVar $ Val.Tuple []
  Exp.Query e -> do
    var_e <- eval' e
    var <- lift' freshVar
    lift' $ unify var_e =<< newVar (Val.Truth var)
    pure var
  Exp.Module i xs e -> do
    xs <- lift' $ traverse freshNamed xs
    _ <- localNames xs $ eval' e
    lift' . newVar . Val.Module i $ filterNames xs
  Exp.Struct i xs e -> do
    env <- ask
    lift' $ newVar . Val.Overloads (Val.Struct i env xs e) =<< freshVar
  Exp.Class i e_sup xs e -> do
    env <- ask
    var_sup <- traverse eval' e_sup
    lift' $ newVar . Val.Overloads (Val.Class i env var_sup xs e) =<< freshVar
--   Exp.Inst e1 xs e2 ->
--     evalInst (loc e) e1 xs e2
--   Exp.IfThenElse xs p t e -> do
--     var <- freshVar
--     if''
--       do
--           xs <- for xs freshNamed
--           _ <- localNames xs $ eval' p
--           pure xs
--       \ xs -> unify var =<< localNames xs (eval' t)
--       (unify var =<< eval' e)
--     pure var
--   Exp.ForDo xs e1 e2 -> do
--     var <- freshVar
--     choiceFree <- getChoiceFree
--     choiceFree' <- freshVar
--     storeFree <- getStoreFree
--     storeFree' <- freshVar
--     for'
--       (do
--           putChoiceFree =<< newVar ChoiceFree
--           xs <- for xs freshNamed
--           _ <- localNames xs $ eval' e1
--           pure $ Many xs)
--       (\ (Many xs) ->
--           localNames xs $ One <$> eval' e2)
--       (\ vars -> do
--           unify var =<< newVar (Val.Tuple $ getOne <$> vars)
--           unify choiceFree choiceFree'
--           unify storeFree storeFree')
--     putChoiceFree choiceFree'
--     putStoreFree storeFree'
--     pure var
  Exp.Exists x e -> do
    var <- lift' freshVar
    localName (extract x) (Val var) $ eval' e
  Exp.Var x e -> do
    ref <- lift' $ newVarRef =<< freshVar
    localName (extract x) (Ref ref) $ eval' e
  Exp.Set x e -> lookupName' (extract x) >>= \ case
    Nothing -> abort $ IdentError (loc x) (extract x)
    Just (Val _) -> abort $ DomainError $ loc e
    Just (Ref ref) -> do
      var <- eval' e
      writeVarRef' ref var
      pure var
  Exp.Function xs e1 e2 -> do
    i <- supply
    env <- ask
    lift' $ newVar . Val.Overloads (Val.Function i env xs e1 e2) =<< freshVar
  Exp.ParenInvoke e1 e2 ->
    evalInvoke (loc e) e1 e2
  Exp.BracketInvoke e1 e2 ->
    evalInvoke (loc e) e1 e2
  Exp.Tuple xs ->
    lift' . newVar . Val.Tuple =<< traverse eval' xs
  Exp.Truth e ->
    lift' . newVar . Val.Truth =<< eval' e
  Exp.Int x ->
    lift' . newVar $ Val.Int x
  Exp.Float x ->
    lift' . newVar $ Val.Float x
  Exp.Name x -> lookupName x >>= \ case
    Nothing -> abort $ IdentError (loc e) x
    Just var -> pure var
  Exp.Default x e1 e2 -> do
    var1 <- eval' e1
    env <- ask
    tell $ HashMap.singleton (extract x) (var1, env, e2)
    pure var1

-- evalDot :: Loc -> L (Exp L Ident) -> Name -> EvalT m (VarVal m)
-- evalDot loc e x = do
--   var_e <- eval' e
--   var <- lift' freshVar
--   lift' . fork $ readVar var_e >>= \ case
--     Val.Module _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> abortWithNameError loc x
--     Val.StructInst _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> abortWithNameError loc x
--     Val.ClassInst _ _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> abortWithNameError loc x
--     _ -> abortWithDomainError loc
--   pure var

evalDotDot :: ( MonadAbort Error m
              , MonadFix m
              , MonadRef m
              , MonadSupply Int m
              , EqRef (Ref m)
              ) => L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalDotDot e1 e2 = do
  var1 <- eval' e1
  var2 <- eval' e2
  var <- lift' freshVar
  s <- get
  choiceFree <- lift' freshIVar
  put s { choiceFree }
  lift' . fork $ (,) <$> readVar var1 <*> readVar var2 >>= \ case
    (Int val1, Int val2) -> do
      unify var =<< foldr (\ x z -> newVar (Val.Int x) <|> z) empty [val1 .. val2]
      writeIVar choiceFree ()
    _ -> abort . DomainError $ loc e1 <> loc e2
  pure var

pattern Int :: Integer -> Val f a
pattern Int x <- (getInt -> Just x)

getInt :: Val f a -> Maybe Integer
getInt = \ case
  Val.Int x -> pure x
  Val.Rational x | denominator x == 1 -> pure $ numerator x
  _ -> empty

evalChoice :: ( MonadAbort Error m
              , MonadFix m
              , MonadRef m
              , MonadSupply Int m
              , EqRef (Ref m)
              ) => L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalChoice e1 e2 = do
  var <- lift' freshVar
  env <- ask
  s <- get
  choiceFree <- lift' freshIVar
  storeFree <- lift' freshIVar
  put S { choiceFree, storeFree }
  lift' $ fork do
    readIVar s.choiceFree
    (x, s') <- runEvalT' (eval' e1 <|> eval' e2) env s
    fork do
      readIVar s'.choiceFree
      writeIVar choiceFree ()
    fork do
      readIVar s'.storeFree
      writeIVar storeFree ()
    unify var x
  pure var

-- evalInst :: MonadEval m =>
--             Loc ->
--             L (Exp L Ident) ->
--             IdentMap Bool ->
--             L (Exp L Ident) ->
--             EvalT m (Var m (Val m))
-- evalInst loc e1 xs e2 = do
--   var1 <- eval' e1
--   xs <- for xs freshNamed
--   _ <- localNames xs $ eval' e2
--   let xs' = fromIdents xs
--   var <- freshVar
--   whenBound var1 $ \ case
--     Val.Overloads overload var1 -> fix (\ recur overload var1 ->
--       case overload of
--         Val.Struct i env ys e -> do
--           ys <- for ys freshNamed
--           defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
--           let ys' = fromIdents ys
--           for_ (HashMap.intersectionWith (,) ys' xs') $
--             uncurry unifyNamed
--           let defs' = fromIdents defs
--           for_ (HashMap.intersection defs' $ ys' \\ xs') $ \ (var, env, e) ->
--             unify var =<< local (const env) (eval' e)
--           unify var =<< newVar (Val.StructInst i ys')
--         Val.Class i env var_super ys e ->
--           instSuper loc var_super xs' $ \ var_super defs_super ys_super -> do
--             ys <- (ys_super <>) <$> for (ys \\ ys_super) freshNamed
--             defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
--             let ys' = fromIdents ys
--             for_ (HashMap.intersectionWith (,) ys' xs') $
--               uncurry unifyNamed
--             let defs' = fromIdents $ defs <> defs_super
--             for_ (HashMap.intersection defs' $ ys' \\ xs') $ \ (var, env, e) ->
--               unify var =<< local (const env) (eval' e)
--             unify var =<< newVar (Val.ClassInst i var_super ys')
--         _ -> whenBound var1 $ \ case
--           Val.Overloads overload var1 -> recur overload var1
--           _ -> abortWithDomainError loc) overload var1
--     _ -> abortWithDomainError loc
--   pure var

evalInvoke :: ( MonadAbort Error m
              , MonadFix m
              , MonadRef m
              , MonadSupply Int m
              , EqRef (Ref m)
              ) => Loc -> L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalInvoke loc e1 e2 = do
  var1 <- eval' e1
  var2 <- eval' e2
  var <- lift' freshVar
  s <- get
  s' <- S <$> lift' freshIVar <*> lift' freshIVar
  put s'
  lift' . fork $ readVar var1 >>= \ case
    Val.Tuple xs -> do
      fork do
        readIVar s.storeFree
        writeIVar s'.storeFree ()
      readIVar s.choiceFree
      unify var =<< invokeTuple xs var2
      writeIVar s'.choiceFree ()
    Val.Overloads head tail ->
      unify var =<< invokeOverloads loc head tail var2 s s'
    _ -> abort $ DomainError loc
  pure var

invokeTuple :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
            => [Var m f] -> VarVal m -> VerseT m (Var m f)
invokeTuple xs var = asum $ zip xs [0 ..] <&> \ (x, i) -> do
  unify var =<< newVar (Val.Int i)
  pure x

invokeOverloads
  :: (MonadFix m, MonadAbort Error m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (VarVal m)
invokeOverloads loc head tail arg s s' = invokeOverload head arg s s' >>= \ case
  Just result -> pure result
  Nothing -> readVar tail >>= \ case
    Val.Overloads head tail -> invokeOverloads loc head tail arg s s'
    _ -> abort $ DomainError loc

invokeOverload
  :: (MonadAbort Error m, MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeOverload overload arg s s' = case overload of
  Val.Function _ env xs e_domain e -> invokeFunction env xs e_domain e arg s s'
  Val.Intrinsic intrinsic -> invokeIntrinsic intrinsic arg >>= \ case
    Nothing -> pure Nothing
    x@Just {} -> do
      fork do
        readIVar s.choiceFree
        writeIVar s'.choiceFree ()
      fork do
        readIVar s.storeFree
        writeIVar s'.storeFree ()
      pure x

invokeFunction
  :: (MonadAbort Error m, MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => Env m
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeFunction env xs e_domain e arg s s' = readIVar =<< if'
  do
    xs <- traverse freshNamed xs
    unify arg =<< evalEvalT' (eval' e_domain) (xs <> env) s
    pure xs
  do
    \ xs -> do
      let env' = xs <> env
      (result, s'') <- runEvalT' (eval' e) env' s
      fork do
        readIVar s''.choiceFree
        writeIVar s'.choiceFree ()
      fork do
        readIVar s''.storeFree
        writeIVar s'.storeFree ()
      pure $ Just result
  do
    pure Nothing

-- invokeStruct :: MonadEval m =>
--                 Label ->
--                 IdentMap (Named m (Var m (Val m))) ->
--                 IdentMap Bool ->
--                 L (Exp L Ident) ->
--                 Var m (Val m) ->
--                 EvalT m ()
-- invokeStruct i env xs e var_domain = do
--   xs <- for xs freshNamed
--   _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
--   unify var_domain =<< newVar (Val.StructInst i $ fromIdents xs)

-- invokeClass :: MonadEval m =>
--                Loc ->
--                Label ->
--                IdentMap (Named m (Var m (Val m))) ->
--                Maybe (Var m (Val m)) ->
--                IdentMap Bool ->
--                L (Exp L Ident) ->
--                Var m (Val m) ->
--                EvalT m ()
-- invokeClass loc i env var_super xs e var_domain = whenBound var_domain $ \ case
--   Val.ClassInst j _ _  | i == j ->
--     instSuper' loc var_super $ \ var_super xs_super -> do
--       xs <- (xs_super <>) <$> for (xs \\ xs_super) freshNamed
--       _ <- local (const $ xs <> env) . lift . evalWriterT $ eval' e
--       unify var_domain =<< newVar (Val.ClassInst i var_super $ fromIdents xs)
--   Val.ClassInst _ (Just var_domain) _ -> invokeClass loc i env var_super xs e var_domain
--   _ -> empty

invokeIntrinsic :: (MonadRef m, MonadSupply Int m)
                => Intrinsic -> VarVal m -> VerseT m (Maybe (VarVal m))
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
  -- Intrinsic.Divide -> div'
  Intrinsic.Int -> int

liftOrd :: (MonadRef m, MonadSupply Int m)
        => (forall a . Ord a => a -> a -> Bool)
        -> VarVal m -> VerseT m (Maybe (VarVal m))
liftOrd f var = readVar var >>= \ case
  Val.Tuple [var_x, var_y] -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.Int x, Val.Int y) -> guard (f x y) $> Just var_x
    (Val.Int x, Val.Float y) -> guard (f (fromInteger x) y) $> Just var_x
    (Val.Int x, Val.Rational y) -> guard (f (fromInteger x) y) $> Just var_x
    (Val.Float x, Val.Int y) -> guard (f x (fromInteger y)) $> Just var_x
    (Val.Float x, Val.Float y) -> guard (f x y) $> Just var_x
    (Val.Float x, Val.Rational y) -> guard (f (toRational x) y) $> Just var_x
    (Val.Rational x, Val.Int y) -> guard (f x (fromInteger y)) $> Just var_x
    (Val.Rational x, Val.Float y) -> guard (f x (toRational y)) $> Just var_x
    (Val.Rational x, Val.Rational y) -> guard (f x y) $> Just var_x
    _ -> pure Nothing
  _ -> pure Nothing

liftNum :: (MonadRef m, MonadSupply Int m)
        => (forall a . Num a => a -> a -> a)
        -> VarVal m -> VerseT m (Maybe (VarVal m))
liftNum f var = readVar var >>= \ case
  Val.Tuple [var_x, var_y] -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.Int x, Val.Int y) ->
      fmap Just . newVar . Val.Int $ f x y
    (Val.Int x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ f (fromInteger x) y
    (Val.Int x, Val.Rational y) ->
      fmap Just . newVar . Val.Rational $ f (fromInteger x) y
    (Val.Float x, Val.Int y) ->
      fmap Just . newVar . Val.Float $ f x (fromInteger y)
    (Val.Float x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ f x y
    (Val.Float x, Val.Rational y) ->
      fmap Just . newVar . Val.Float $ fromRational $ f (toRational x) y
    (Val.Rational x, Val.Int y) ->
      fmap Just . newVar . Val.Rational $ f x (fromInteger y)
    (Val.Rational x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ fromRational $ f x (toRational y)
    (Val.Rational x, Val.Rational y) ->
      fmap Just . newVar . Val.Rational $ f x y
    _ -> pure Nothing
  _ -> pure Nothing

prefixPlus :: MonadRef m => VarVal m -> VerseT m (Maybe (VarVal m))
prefixPlus var = readVar var >>= \ case
  Val.Int _ -> pure $ Just var
  Val.Float _ -> pure $ Just var
  Val.Rational _ -> pure $ Just var
  _ -> pure Nothing

prefixMinus :: (MonadRef m, MonadSupply Int m)
            => VarVal m -> VerseT m (Maybe (VarVal m))
prefixMinus var = readVar var >>= \ case
  Val.Int x -> Just <$> newVar (Val.Int $ negate x)
  Val.Float x -> Just <$> newVar (Val.Float $ negate x)
  Val.Rational x -> Just <$> newVar (Val.Rational $ negate x)
  _ -> pure Nothing

-- data Div = Int !Integer | Float !Double | Rational !Rational deriving Eq

-- div' :: Var m (Val m) ->
--         (Maybe (Var m (Val m)) -> EvalT m ()) ->
--         EvalT m ()
-- div' var k =
--   ifte''
--   (do
--       var_x <- freshVar
--       var_y <- freshVar
--       unify var =<< newVar (Val.Tuple [var_x, var_y])
--       var' <- freshVar
--       whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
--         unify var' =<< case (val_x, val_y) of
--           (Val.Int _, Val.Int 0) -> do
--             newVar $ Const Nothing
--           (Val.Int x, Val.Int y) ->
--             newVar . Const . Just . Rational $ x % y
--           (Val.Int x, Val.Float y) ->
--             newVar . Const . Just . Float $ fromInteger x / y
--           (Val.Int x, Val.Rational y) ->
--             newVar . Const . Just . Rational $ fromInteger x / y
--           (Val.Float x, Val.Int y) ->
--             newVar . Const . Just . Float $ x / fromInteger y
--           (Val.Float x, Val.Float y) ->
--             newVar . Const . Just . Float $ x / y
--           (Val.Float x, Val.Rational y) ->
--             newVar . Const . Just . Float $ fromRational $ toRational x / y
--           (Val.Rational x, Val.Int y) ->
--             newVar . Const . Just . Rational $ x / fromInteger y
--           (Val.Rational x, Val.Float y) ->
--             newVar . Const . Just . Float $ fromRational $ x / toRational y
--           (Val.Rational _, Val.Rational 0) ->
--             newVar $ Const Nothing
--           (Val.Rational x, Val.Rational y) ->
--             newVar . Const . Just . Rational $ x / y
--           _ -> empty
--       pure $ One var')
--   (\ (One var) -> whenBound var $ getConst >>> \ case
--       Nothing -> empty
--       Just (Int x) -> k . Just =<< newVar (Val.Int x)
--       Just (Float x) -> k . Just =<< newVar (Val.Float x)
--       Just (Rational x) -> k . Just =<< newVar (Val.Rational x))
--   (k Nothing)

int :: MonadRef m => VarVal m -> VerseT m (Maybe (VarVal m))
int var = readVar var >>= \ case
  Int _ -> pure $ Just var
  _ -> pure Nothing

-- instSuper :: MonadEval m =>
--              Loc ->
--              Maybe (Var m (Val m)) -> HashMap Name (Named m (Var m (Val m))) ->
--              (Maybe (Var m (Val m)) -> Defaults m -> Env m -> EvalT m ()) ->
--              EvalT m ()
-- instSuper loc var_super xs f = case var_super of
--   Nothing -> f Nothing mempty mempty
--   Just var_super -> instClass loc var_super xs $ f . Just

-- instSuper' :: MonadEval m =>
--               Loc ->
--               Maybe (Var m (Val m)) ->
--               (Maybe (Var m (Val m)) -> Env m -> EvalT m ()) ->
--               EvalT m ()
-- instSuper' loc var_super f = case var_super of
--   Nothing -> f Nothing mempty
--   Just var_super -> instClass' loc var_super $ f . Just

-- instClass :: MonadEval m =>
--              Loc ->
--              Var m (Val m) -> HashMap Name (Named m (Var m (Val m))) ->
--              (Var m (Val m) -> Defaults m -> Env m -> EvalT m ()) ->
--              EvalT m ()
-- instClass loc var xs f = whenBound var $ \ case
--   Val.Overloads overload var -> case overload of
--     Val.Class i env var_super ys e ->
--       instSuper loc var_super xs $ \ var_super defs_super ys_super -> do
--         ys <- (ys_super <>) <$> for (ys \\ ys_super) freshNamed
--         defs <- local (const $ ys <> env) . lift . execWriterT $ eval' e
--         let ys' = fromIdents ys
--         for_ (HashMap.intersectionWith (,) ys' xs) $
--           uncurry unifyNamed
--         var' <- newVar $ Val.ClassInst i var_super ys'
--         f var' (defs <> defs_super) ys
--     _ -> instClass loc var xs f
--   _ -> abortWithDomainError loc

-- instClass' :: MonadEval m =>
--               Loc ->
--               Var m (Val m) ->
--               (Var m (Val m)-> Env m -> EvalT m ()) ->
--               EvalT m ()
-- instClass' loc var f = whenBound var $ \ case
--   Val.Overloads overload var -> case overload of
--     Val.Class i env var_super ys e ->
--       instSuper' loc var_super $ \ var_super ys_super -> do
--         ys <- (ys_super <>) <$> for (ys \\ ys_super) freshNamed
--         _ <- local (const $ ys <> env) . lift . evalWriterT $ eval' e
--         var' <- newVar $ Val.ClassInst i var_super $ fromIdents ys
--         f var' ys
--     _ -> instClass' loc var f
--   _ -> abortWithDomainError loc

newEnv :: (MonadRef m, MonadSupply Int m) => VerseT m (Env m)
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
      lift . newVar . Val.Overloads (Val.Intrinsic y) =<<
      lift freshVar

filterNames :: HashMap Ident a -> HashMap Name a
filterNames =
  HashMap.fromList .
  HashMap.foldrWithKey
  \ case
    Ident.Name x -> \ y z -> (x, y) : z
    Ident.Label _ -> \ _ z -> z
  []

-- if'' :: (MonadRef m, MonadSupply Int m, Freshenable a m)
--      => EvalT m a -> (a -> EvalT m b) -> EvalT m b -> EvalT m (IVar m b)
-- if'' p t e = do
--   env <- ask
--   s <- get
--   choiceFree <- freshIVar
--   storeFree <- freshIVar
--   put S { choiceFree, storeFree }
--   lift' $ if'
--     do
--       choiceFree <- newIVar ()
--       (x, choiceFree', storeFree') <- runEvalT' p env s { choiceFree }
--       fork do
--         readIVar storeFree'
--         writeIVar storeFree ()
--       pure x
--     \ x -> do
--       (y, choiceFree', storeFree') <- runEvalT' (t x) env s
--       fork do
--         readIVar choiceFree'
--         writeIVar choiceFree ()
--       fork do
--         readIVar storeFree'
--         writeIVar storeFree'
--     do
--       (y, choiceFree', storeFree') <- runEvalT' e env s
--       fork do
--         readIVar choiceFree'
--         writeIVar choiceFree ()
--       fork do
--         readIVar storeFree'
--         writeIVar storeFree ()

lookupName :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
           => Ident -> EvalT m (Maybe (VarVal m))
lookupName = lookupName' >=> \ case
  Nothing -> pure Nothing
  Just (Val x) -> pure $ Just x
  Just (Ref x) -> Just <$> readVarRef' x

lookupName' :: Ident -> EvalT m (Maybe (Named (VarRef m) (VarVal m)))
lookupName' x = HashMap.lookup x <$> ask

localName :: Ident -> Named (VarRef m) (VarVal m) -> EvalT m a -> EvalT m a
localName x = local . HashMap.insert x

localNames :: (Semigroup r, MonadReader r m) => r -> m a -> m a
localNames = local . (<>)

getChoiceFree :: Monad m => EvalT m (IVar m ())
getChoiceFree = gets choiceFree

putChoiceFree :: Monad m => IVar m () -> EvalT m ()
putChoiceFree choiceFree = modify $ \ s -> s { choiceFree }

getStoreFree :: Monad m => EvalT m (IVar m ())
getStoreFree = gets storeFree

putStoreFree :: Monad m => IVar m () -> EvalT m ()
putStoreFree storeFree = modify $ \ s -> s { storeFree }

lift' :: VerseT m a -> EvalT m a
lift' = lift . lift

freshNamed :: (MonadRef m, MonadSupply Int m)
           => Bool -> VerseT m (Named (VarRef m) (VarVal m))
freshNamed = \ case
  False -> Val <$> freshVar
  True -> Ref <$> freshVarRef

freshVarRef :: (MonadRef m, MonadSupply Int m) => VerseT m (VarRef m f)
freshVarRef = newVarRef =<< freshVar

readVarRef' :: (MonadRef m, MonadSupply Int m, RowMatchable f)
            => VarRef m f -> EvalT m (Var m f)
readVarRef' r = do
  x <- lift' freshVar
  s <- get
  storeFree <- lift' freshIVar
  put s { storeFree }
  lift' $ fork do
    readIVar s.storeFree
    unify x =<< readVarRef r
    writeIVar storeFree ()
  pure x

writeVarRef' :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
             => VarRef m f -> Var m f -> EvalT m ()
writeVarRef' r x = do
  s <- get
  storeFree <- lift' freshIVar
  put s { storeFree }
  lift' $ fork do
    readIVar s.storeFree
    writeVarRef r x
    writeIVar storeFree ()

(\\) :: Hashable k => HashMap k a -> HashMap k b -> HashMap k a
(\\) = HashMap.difference
