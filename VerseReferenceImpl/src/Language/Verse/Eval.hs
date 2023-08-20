{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Control.Monad.Fix
import Control.Monad.Reader.Class
import Control.Monad.Ref
import Control.Monad.RST
import Control.Monad.State.Class
import Control.Monad.Supply
import Control.Monad.Throw
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Verse

import Data.Bool
import Data.Eq
import Data.Fix
import Data.Function
import Data.Functor
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int
import Data.Kind
import Data.Maybe
import Data.Monoid
import Data.Ratio
import Data.Traversable (Traversable)
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
import Language.Verse.Val (Val)
import Language.Verse.Val qualified as Val

type EvalT m = WriterT (Defaults m) (RST (Env m) (S m) (VerseT m))

data S m = S
  { choiceFree :: IVar m ()
  , storeFree :: IVar m ()
  }

type Defaults m = HashMap Ident (VarVal m, Env m, L (Exp L Ident))

type Env m = HashMap Ident (Bool, VarVal m)

type VarVal m = Var m (Val (VarRef m))

type FrozenVal = Frozen (Val Frozen)

runEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> VerseT m a
runEvalT m = do
  env <- newEnv
  choiceFree <- newIVar ()
  storeFree <- newIVar ()
  evalRST (evalWriterT m) env S {..}

runEvalT' :: EvalT m a -> Env m -> S m -> VerseT m (a, S m)
runEvalT' = runRST . evalWriterT

evalWriterT :: (Monoid w, Functor m) => WriterT w m a -> m a
evalWriterT = fmap fst . runWriterT

eval :: ( MonadThrow Error m
        , MonadFix m
        , MonadRef m
        , MonadSupply Label m
        , Eq (Ref m (VarVal m))
        ) => L (Exp L Ident) -> VerseT m FrozenVal
eval = freeze <=< runEvalT . eval'

eval' :: ( MonadThrow Error m
         , MonadRef m
         , MonadSupply Label m
         , Eq (Ref m (Var m (Val m)))
         ) => L (Exp L Ident) -> EvalT m (Var m (Val m))
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
  -- e1 :..: e2 ->
  --   evalDotDot e1 e2
  e1 :|: e2 -> do
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
  Exp.Fail ->
    empty
  -- Exp.One e -> do
  --   var <- lift' freshVar
  --   env <- ask
  --   s@S { storeFree } <- get
  --   storeFree' <- lift' freshIVar
  --   lift' $ fork do
  --     (x, choiceFree, storeFree) <- readIVar =<< one do
  --       choiceFree' <- newIVar ()
  --       runEvalT' (eval' e) env s { choiceFree = choiceFree' }
  --     writeIVar storeFree' ()
  --     unify var x
  --   modify s { storeFree = storeFree' }
  --   pure var
--   Exp.All e -> do
--     var <- freshVar
--     storeFree' <- freshIVar
--     env <- ask
--     s <- get
--     lift' $ fork do
--       xs <- readIVar =<< all do
--         choiceFree' <- newIVar ()
--         runEvalT' (eval' e) env s { choiceFree = choiceFree' }
--       writeIVar storeFree' ()
--       unify var =<< newVar (Val.Tuple xs)
--     modify s { storeFree = storeFree' }
--     pure var
--   Exp.Not e -> do
--     if''
--       (eval' e)
--       (const empty)
--       (pure ())
--     newVar $ Val.Tuple []
--   Exp.Query e -> do
--     var_e <- eval' e
--     var <- freshVar
--     unify var_e =<< newVar (Val.Truth var)
--     pure var
--   Exp.Module i xs e -> do
--     xs <- for xs freshNamed
--     _ <- localNames xs $ eval' e
--     newVar . Val.Module i $ fromIdents xs
--   Exp.Struct i xs e -> do
--     env <- ask
--     newVar . Val.Overloads (Val.Struct i env xs e) =<< freshVar
--   Exp.Class i e_super xs e -> do
--     env <- ask
--     var_super <- for e_super eval'
--     newVar . Val.Overloads (Val.Class i env var_super xs e) =<< freshVar
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
--   Exp.Exists x e -> do
--     var <- freshVar
--     localName (extract x) (Val var) $ eval' e
--   Exp.Var x e -> do
--     ref <- lift freshVarRef
--     localName (extract x) (Ref ref) $ eval' e
--   Exp.Set x e -> lookupName' (extract x) >>= \ case
--     Nothing -> throwIdentError (loc x) (extract x)
--     Just (Val _) -> throwDomainError $ loc e
--     Just (Ref ref) -> do
--       var <- eval' e
--       writeVarRef' ref var
--       pure var
--   Exp.Function xs e1 e2 -> do
--     i <- supply
--     env <- ask
--     newVar . Val.Overloads (Val.Function i env xs e1 e2) =<< freshVar
--   Exp.ParenInvoke e1 e2 ->
--     evalInvoke (loc e) e1 e2
--   Exp.BracketInvoke e1 e2 ->
--     evalInvoke (loc e) e1 e2
--   Exp.Tuple exps ->
--     newVar . Val.Tuple =<< traverse eval' exps
--   Exp.Truth e ->
--     newVar . Val.Truth =<< eval' e
--   Exp.Int x ->
--     newVar $ Val.Int x
--   Exp.Float x ->
--     newVar $ Val.Float x
--   Exp.Name x -> lookupName x >>= \ case
--     Nothing -> throwIdentError (loc e) x
--     Just var -> pure var
--   Exp.Default x e1 e2 -> do
--     var1 <- eval' e1
--     env <- ask
--     tell $ HashMap.singleton (extract x) (var1, env, e2)
--     pure var1

-- evalDot :: MonadEval m =>
--            Loc ->
--            L (Exp L Ident) ->
--            Name ->
--            EvalT m (Var m (Val m))
-- evalDot loc e x = do
--   var_e <- eval' e
--   var <- freshVar
--   whenBound var_e $ \ case
--     Val.Module _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> throwNameError loc x
--     Val.StructInst _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> throwNameError loc x
--     Val.ClassInst _ _ xs ->
--       case HashMap.lookup x xs of
--         Just (Ref ref_x) -> readVarRef' ref_x $ unify var
--         Just (Val var_x) -> unify var var_x
--         Nothing -> throwNameError loc x
--     _ -> throwDomainError loc
--   pure var

-- evalDotDot :: MonadEval m =>
--               L (Exp L Ident) ->
--               L (Exp L Ident) ->
--               EvalT m (Var m (Val m))
-- evalDotDot e1 e2 = do
--   choiceFree <- getChoiceFree
--   choiceFree' <- freshVar
--   var1 <- eval' e1
--   var2 <- eval' e2
--   var <- freshVar
--   lift $ whenBound var1 $ \ case
--     Val.Int val1 -> whenBound var2 $ \ case
--       Val.Int val2 -> do
--         unify var =<< foldr (\ x z -> newVar (Val.Int x) <|> z) empty [val1 .. val2]
--         unify choiceFree choiceFree'
--       _ -> throwDomainError $ loc e2
--     _ -> throwDomainError $ loc e1
--   putChoiceFree choiceFree'
--   pure var

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
--           _ -> throwDomainError loc) overload var1
--     _ -> throwDomainError loc
--   pure var

-- evalInvoke :: MonadEval m =>
--               Loc ->
--               L (Exp L Ident) ->
--               L (Exp L Ident) ->
--               EvalT m (Var m (Val m))
-- evalInvoke loc e1 e2 = do
--   var1 <- eval' e1
--   var2 <- eval' e2
--   var <- freshVar
--   choiceFree <- getChoiceFree
--   choiceFree' <- freshVar
--   storeFree <- getStoreFree
--   storeFree' <- freshVar
--   whenBound var1 $ \ case
--     Val.Tuple xs -> do
--       unify var =<< invokeTuple xs var2
--       unify choiceFree choiceFree'
--       unify storeFree storeFree'
--     Val.Overloads overload var1 ->
--       fix (\ recur overload var1 -> case overload of
--         Val.Function _ env xs e_domain e ->
--           invokeFunction env xs e_domain e var2 $ \ case
--             Just var' -> do
--               unify var var'
--               unify choiceFree choiceFree'
--               unify storeFree storeFree'
--             Nothing -> whenBound var1 $ \ case
--               Val.Overloads overload var1 -> recur overload var1
--               _ -> throwDomainError loc
--         Val.Struct i env xs e -> do
--           unify var var2
--           invokeStruct i env xs e var2
--           unify choiceFree choiceFree'
--           unify storeFree storeFree'
--         Val.Class i env var_super xs e -> do
--           unify var var2
--           invokeClass loc i env var_super xs e var2
--           unify choiceFree choiceFree'
--           unify storeFree storeFree'
--         Val.Intrinsic intrinsic ->
--           invokeIntrinsic intrinsic var2 $ \ case
--             Just var' -> do
--               unify var var'
--               unify choiceFree choiceFree'
--               unify storeFree storeFree'
--             Nothing -> whenBound var1 $ \ case
--               Val.Overloads overload var1 -> recur overload var1
--               _ -> throwDomainError loc) overload var1
--     _ -> throwDomainError loc
--   putChoiceFree choiceFree'
--   putStoreFree storeFree'
--   pure var

-- invokeTuple :: ( MonadUnify m
--                , EqVarRef (VarRef m)
--                ) => [Var m f] -> Var m (Val m) -> EvalT m (Var m f)
-- invokeTuple xs var = do
--   asum $ zip xs [0 ..] <&> \ (x, i) -> do
--     unify var =<< newVar (Val.Int i)
--     pure x

-- invokeFunction :: MonadEval m =>
--                   Env m ->
--                   IdentMap Bool ->
--                   L (Exp L Ident) ->
--                   L (Exp L Ident) ->
--                   Var m (Val m) ->
--                   (Maybe (Var m (Val m)) -> EvalT m ()) ->
--                   EvalT m ()
-- invokeFunction env xs e_domain e var_domain k =
--   ifte''
--   (do
--       xs <- for xs freshNamed
--       let env' = xs <> env
--       unify var_domain =<< local (const env') (eval' e_domain)
--       pure $ Many xs)
--   (\ (Many xs) -> do
--       let env' = xs <> env
--       k . Just =<< local (const env') (eval' e))
--   (k Nothing)

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

-- invokeIntrinsic :: Intrinsic ->
--                    Var m (Val m) ->
--                    (Maybe (Var m (Val m)) -> EvalT m ()) ->
--                    EvalT m ()
-- invokeIntrinsic = \ case
--   Intrinsic.Less -> liftOrd (<)
--   Intrinsic.LessEqual -> liftOrd (<=)
--   Intrinsic.Greater -> liftOrd (>)
--   Intrinsic.GreaterEqual -> liftOrd (>=)
--   Intrinsic.Plus -> liftNum (+)
--   Intrinsic.PrefixPlus -> prefixPlus
--   Intrinsic.Minus -> liftNum (-)
--   Intrinsic.PrefixMinus -> prefixMinus
--   Intrinsic.Multiply -> liftNum (*)
--   Intrinsic.Divide -> div'
--   Intrinsic.Int -> int

-- liftOrd :: (forall a . Ord a => a -> a -> Bool) ->
--            Var m (Val m) ->
--            (Maybe (Var m (Val m)) -> EvalT m ()) ->
--            EvalT m ()
-- liftOrd f var k =
--   ifte''
--   (do
--       var_x <- freshVar
--       var_y <- freshVar
--       unify var =<< newVar (Val.Tuple [var_x, var_y])
--       var_p <- freshVar
--       whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
--         unify var_p =<< case (val_x, val_y) of
--           (Val.Int x, Val.Int y) -> newBool $ f x y
--           (Val.Int x, Val.Float y) -> newBool $ f (fromInteger x) y
--           (Val.Int x, Val.Rational y) -> newBool $ f (fromInteger x) y
--           (Val.Float x, Val.Int y) -> newBool $ f x (fromInteger y)
--           (Val.Float x, Val.Float y) -> newBool $ f x y
--           (Val.Float x, Val.Rational y) -> newBool $ f (toRational x) y
--           (Val.Rational x, Val.Int y) -> newBool $ f x (fromInteger y)
--           (Val.Rational x, Val.Float y) -> newBool $ f x (toRational y)
--           (Val.Rational x, Val.Rational y) -> newBool $ f x y
--           _ -> empty
--       pure (One var_p, One var_x))
--   (\ (One var_p, One var_x) ->
--       whenBound var_p $ getConst >>> \ case
--         True -> k $ Just var_x
--         False -> empty)
--   (k Nothing)

-- newBool :: MonadVar m => Bool -> EvalT m (Var m (Const Bool))
-- newBool = newVar . Const

-- liftNum :: (forall a . Num a => a -> a -> a) ->
--            Var m (Val m) ->
--            (Maybe (Var m (Val m)) -> EvalT m ()) ->
--            EvalT m ()
-- liftNum f var k =
--   ifte''
--   (do
--       var_x <- freshVar
--       var_y <- freshVar
--       unify var =<< newVar (Val.Tuple [var_x, var_y])
--       var' <- freshVar
--       whenBound var_x $ \ val_x -> whenBound var_y $ \ val_y ->
--         unify var' =<< case (val_x, val_y) of
--           (Val.Int x, Val.Int y) ->
--             newVar . Val.Int $ f x y
--           (Val.Int x, Val.Float y) ->
--             newVar . Val.Float $ f (fromInteger x) y
--           (Val.Int x, Val.Rational y) ->
--             newVar . Val.Rational $ f (fromInteger x) y
--           (Val.Float x, Val.Int y) ->
--             newVar . Val.Float $ f x (fromInteger y)
--           (Val.Float x, Val.Float y) ->
--             newVar . Val.Float $ f x y
--           (Val.Float x, Val.Rational y) ->
--             newVar . Val.Float $ fromRational $ f (toRational x) y
--           (Val.Rational x, Val.Int y) ->
--             newVar . Val.Rational $ f x (fromInteger y)
--           (Val.Rational x, Val.Float y) ->
--             newVar . Val.Float $ fromRational $ f x (toRational y)
--           (Val.Rational x, Val.Rational y) ->
--             newVar . Val.Rational $ f x y
--           _ -> empty
--       pure $ One var')
--   (k . Just . getOne)
--   (k Nothing)

-- prefixPlus :: Var m (Val f) ->
--               (Maybe (Var m (Val f)) -> EvalT m ()) ->
--               EvalT m ()
-- prefixPlus var k =
--   ifte''
--   (do
--       whenBound var $ \ case
--         Val.Int _ -> pure ()
--         Val.Float _ -> pure ()
--         Val.Rational _ -> pure ()
--         _ -> empty
--       pure $ One var)
--   (k . Just . getOne)
--   (k Nothing)

-- prefixMinus :: Var m (Val m) ->
--                (Maybe (Var m (Val m)) -> EvalT m ()) ->
--                EvalT m ()
-- prefixMinus var k =
--   ifte''
--   (do
--       var' <- freshVar
--       whenBound var $ \ val -> unify var' =<< case val of
--         Val.Int x -> newVar . Val.Int $ negate x
--         Val.Float x -> newVar . Val.Float $ negate x
--         Val.Rational x -> newVar . Val.Rational $ negate x
--         _ -> empty
--       pure $ One var')
--   (k . Just . getOne)
--   (k Nothing)

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

-- int :: Var m (Val m) ->
--        (Maybe (Var m (Val m)) -> EvalT m ()) ->
--        EvalT m ()
-- int var k =
--   ifte''
--   (do
--       whenBound var $ \ case
--         Val.Int _ -> pure ()
--         Val.Rational x | denominator x == 1 -> pure ()
--         _ -> empty
--       pure $ One var)
--   (k . Just . getOne)
--   (k Nothing)

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
--   _ -> throwDomainError loc

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
--   _ -> throwDomainError loc

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
      tell . HashMap.singleton x . (False,) =<<
      lift . newVar . Val.Overloads (Val.Intrinsic y) =<<
      lift freshVar

-- localNames :: (Semigroup r, MonadReader r m) => r -> m a -> m a
-- localNames = local . (<>)

-- throwDomainError :: MonadThrow Error m => Loc -> m a
-- throwDomainError = throwError . DomainError

-- throwIdentError :: MonadThrow Error m => Loc -> Ident -> m a
-- throwIdentError x = throwError . IdentError x

-- throwNameError :: MonadThrow Error m => Loc -> Name -> m a
-- throwNameError x = throwError . NameError x

-- fromIdents :: HashMap Ident a -> HashMap Name a
-- fromIdents =
--   HashMap.fromList .
--   HashMap.foldrWithKey
--   (\ case
--       Ident.Name x -> \ y z -> (x, y) : z
--       Ident.Label _ -> \ _ z -> z)
--   []

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

(\\) :: Hashable k => HashMap k a -> HashMap k b -> HashMap k a
(\\) = HashMap.difference
