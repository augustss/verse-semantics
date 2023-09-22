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
  ( MonadEval
  , eval
  ) where

import Control.Applicative
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
import Data.Foldable (Foldable, foldr, traverse_)
import Data.Function
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int
import Data.List (unzip, zip)
import Data.Match
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Traversable (Traversable, traverse)
import Data.Semigroup
import Data.String

import Language.Verse.Desugar.Exp (Exp ((:*>:), (:=:), (:.:), (:|:)))
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

import Prelude (Integer, Num (..), Fractional (..), fromRational, toRational)

type EvalT m = RST (R m) (S m) (VerseT m)

data R m = R
  { env :: !(Env m)
  , archetype :: !(Archetype m)
  , archetype' :: !(Archetype m)
  }

instance Semigroup (R m) where
  x <> y = R
    { env = x.env <> y.env
    , archetype = x.archetype <> y.archetype
    , archetype' = x.archetype' <> y.archetype'
    }

instance Monoid (R m) where
  mempty = R { env = mempty, archetype = mempty, archetype' = mempty }

data S m = S
  { choiceFree :: Var m ChoiceFree
  , storeFree :: Var m StoreFree
  }

freshS :: (MonadRef m, MonadSupply Int m) => VerseT m (S m)
freshS = S <$> freshVar <*> freshVar

data ChoiceFree a = ChoiceFree deriving (Functor, Foldable, Traversable)

instance RowMatchable ChoiceFree

instance ZipMatchable ChoiceFree where
  zipMatch _ _ = Just ChoiceFree

data StoreFree a = StoreFree deriving (Functor, Foldable, Traversable)

instance RowMatchable StoreFree

instance ZipMatchable StoreFree where
  zipMatch _ _ = Just StoreFree

instance Monad m => Freshenable (S n) m where
  freshen = pure

type Env m = Val.Env Ident (VarRef m) (VarVal m)

type Archetype m = Env m

evalEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> VerseT m a
evalEvalT m = do
  env <- newEnv
  choiceFree <- newVar ChoiceFree
  storeFree <- newVar StoreFree
  evalRST m R {..} S {..}
  where
    archetype = mempty
    archetype' = mempty

runEvalT' :: EvalT m a -> R m -> S m -> VerseT m (a, S m)
runEvalT' = runRST

evalEvalT' :: EvalT m a -> R m -> S m -> VerseT m a
evalEvalT' = evalRST

execEvalT' :: EvalT m a -> R m -> S m -> VerseT m (S m)
execEvalT' = execRST

type MonadEval m =
  ( MonadAbort Error m
  , MonadFix m
  , MonadRef m
  , MonadSupply Label m
  , EqRef (Ref m)
  )

eval :: MonadEval m => L (Exp L Ident) -> VerseT m FrozenVal
eval = freeze' <=< evalEvalT . evalExp

evalExp :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalExp e = case extract e of
  e1 :*>: e2 ->
    evalExp e1 *> evalExp e2
  e1 :=: e2 -> do
    var1 <- evalExp e1
    var2 <- evalExp e2
    lift $ unify var1 var2
    pure var1
  e1 :.: x ->
    evalDot (loc e) e1 x
  e1 :|: e2 ->
    evalChoice e1 e2
  Exp.Fail ->
    empty
  Exp.One e ->
    evalOne e
  Exp.All e ->
    evalAll e
  Exp.Not e ->
    evalNot e
  Exp.Query e -> do
    var_e <- evalExp e
    var <- lift freshVar
    lift $ unify var_e =<< newVar (Val.Truth var)
    pure var
  Exp.Module i xs e -> do
    xs <- lift $ traverse freshNamed xs
    _ <- localEnv xs $ evalExp e
    lift . newVar . Val.Module i $ filterNames xs
  Exp.Enum i xs ns es -> do
    xs <- lift $ traverse freshNamed xs
    es <- localEnv xs $ traverse evalExp es
    lift $ newVar $ Val.Enum i (filterNames xs) ns es
  Exp.EnumValue i c -> do
    lift . newVar $ Val.EnumValue i c
  Exp.Struct i xs e -> do
    r <- ask
    lift $ newVar . Val.Overloads (Val.Struct i r.env xs e) =<< freshVar
  Exp.Class i e_sup xs e -> do
    r <- ask
    var_sup <- traverse evalExp e_sup
    lift $ newVar . Val.Overloads (Val.Class i r.env var_sup xs e) =<< freshVar
  Exp.Inst e1 xs e2 ->
    evalInst (loc e) e1 xs e2
  Exp.IfThenElse xs p t e ->
    evalIfThenElse xs p t e
  Exp.ForDo xs e1 e2 ->
    evalForDo xs e1 e2
  Exp.Exists False x e -> do
    var <- lift freshVar
    localName (extract x) (Val var) $ evalExp e
  Exp.Exists True x e -> do
    ref <- lift $ freshVarRef
    localName (extract x) (Ref ref) $ evalExp e
  Exp.Set x e -> lookupNamed (extract x) >>= \ case
    Nothing -> abort $ IdentError (loc x) (extract x)
    Just (Val _) -> abort $ DomainError $ loc e
    Just (Ref ref) -> do
      var <- evalExp e
      writeVarRef' ref var
      pure var
  Exp.Fun xs e_domain e -> do
    i <- supply
    r <- ask
    lift $ newVar . Val.Overloads (Val.Fun i r.env xs e_domain e) =<< freshVar
  Exp.ParenInvoke e1 e2 -> do
    var1 <- evalExp e1
    var2 <- evalExp e2
    var <- lift freshVar
    s <- get
    s' <- lift freshS
    put s'
    lift . fork $ succeeds (freshS >>= invoke (loc e) var1 var2 s) >>= readIVar >>= \ case
      Nothing -> abort . SucceedsError $ loc e
      Just var' -> do
        unify var var'
        unify s.choiceFree s'.choiceFree
        unify s.storeFree s'.storeFree
    pure var
  Exp.BracketInvoke e1 e2 -> do
    var1 <- evalExp e1
    var2 <- evalExp e2
    evalInvoke (loc e) var1 var2
  Exp.Tuple xs ->
    lift . newVar . Val.Tuple =<< traverse evalExp xs
  Exp.Truth e ->
    lift . newVar . Val.Truth =<< evalExp e
  Exp.Int x ->
    lift . newVar $ Val.Int x
  Exp.Float x ->
    lift . newVar $ Val.Float x
  Exp.Name x ->
    evalIdent $ x <$ e
  Exp.IfArchetypeName x y e1 e2 -> asks archetype <&> HashMap.lookup x >>= \ case
    Nothing -> evalExp e2
    Just var -> local (\ r -> r { env = HashMap.insert y var r.env }) $ evalExp e1
  Exp.ArchetypeName x -> asks archetype' <&> HashMap.lookup x >>= \ case
    Nothing -> evalIdent $ x <$ e
    Just x -> readNamed x

evalDot :: MonadEval m => Loc -> L (Exp L Ident) -> Name -> EvalT m (VarVal m)
evalDot loc e x = do
  var_e <- evalExp e
  var <- lift freshVar
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    xs <- readVar var_e >>= \ case
      Val.Module _ xs -> pure xs
      Val.Enum _ xs _ _ -> pure xs
      Val.StructInst _ xs -> pure xs
      Val.ClassInst _ _ xs -> pure xs
      _ -> abort $ DomainError loc
    unify var =<< case HashMap.lookup x xs of
      Just y -> readNamed' s.storeFree storeFree y
      Nothing -> abort $ NameError loc x
  pure var

pattern Int :: Integer -> Val f a
pattern Int x <- (getInt -> Just x)

getInt :: Val f a -> Maybe Integer
getInt = \ case
  Val.Int x -> pure x
  Val.Rational x | denominator x == 1 -> pure $ numerator x
  _ -> empty

evalChoice :: MonadEval m => L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalChoice e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  s' <- lift freshS
  put s'
  lift $ fork do
    _ <- readVar s.choiceFree
    (x, s) <- runEvalT' (evalExp e1 <|> evalExp e2) r s
    unify s.choiceFree s'.choiceFree
    unify s.storeFree s'.storeFree
    unify var x
  pure var

evalOne :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalOne e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    unify var =<< readIVar =<< one do
      choiceFree <- newVar ChoiceFree
      evalEvalT' (evalExp e) mempty { env = r.env } s { choiceFree }
    unify storeFree s.storeFree
  pure var

evalAll :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalAll e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    unify var =<< newVar . Val.Tuple =<< readIVar =<< all do
      choiceFree <- newVar ChoiceFree
      evalEvalT' (evalExp e) mempty { env = r.env } s { choiceFree }
    unify storeFree s.storeFree
  pure var

evalNot :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalNot e = do
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift . fork $
    void $ readIVar =<< if'
    do
      choiceFree <- newVar ChoiceFree
      void $ runEvalT' (evalExp e) mempty { env = r.env } s { choiceFree }
    do
      const empty
    do
      unify storeFree s.storeFree
  lift . newVar $ Val.Tuple []

evalIfThenElse
  :: MonadEval m
  => IdentMap Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalIfThenElse xs p t e = do
  var <- lift freshVar
  r <- ask
  s <- get
  choiceFree <- lift freshVar
  storeFree <- lift freshVar
  put S { choiceFree, storeFree }
  lift $ fork do
    (var', s) <- readIVar =<< if'
      do
        xs <- traverse freshNamed xs
        choiceFree <- newVar ChoiceFree
        _ <- runEvalT' (evalExp p) mempty { env = xs <> r.env } s { choiceFree }
        pure xs
      do
        \ xs -> runEvalT' (evalExp t) mempty { env = xs <> r.env } s
      do
        runEvalT' (evalExp e) r s
    unify var var'
    unify choiceFree s.choiceFree
    unify storeFree s.storeFree
  pure var

evalForDo
  :: MonadEval m
  => IdentMap Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalForDo xs e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  s' <- lift freshS
  put s'
  lift $ fork do
    (vars, s) <- fmap unzip . readIVar =<< for
      do
        xs <- traverse freshNamed xs
        choiceFree <- newVar ChoiceFree
        _ <- runEvalT' (evalExp e1) mempty { env = xs <> r.env } s { choiceFree }
        pure xs
      do
        \ xs -> runEvalT' (evalExp e2) mempty { env = xs <> r.env } s
    unify var =<< newVar (Val.Tuple vars)
    traverse_ (unify s'.choiceFree . (.choiceFree)) s
    traverse_ (unify s'.storeFree . (.storeFree)) s
  pure var

evalInst
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalInst loc e1 xs e2 = do
  var1 <- evalExp e1
  xs <- lift $ traverse freshNamed xs
  _ <- localEnv xs $ evalExp e2
  var <- lift freshVar
  s <- get
  s' <- lift freshS
  put s'
  lift . fork $ readVar var1 >>= \ case
    Val.Overloads head tail ->
      unify var =<< instOverloads loc head tail xs s s'
    _ -> abort $ DomainError loc
  pure var

instOverloads
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (VarVal m)
instOverloads loc head tail archetype s s' = instOverload loc head archetype s s' >>= \ case
  Just result -> pure result
  Nothing -> readVar tail >>= \ case
    Val.Overloads head tail -> instOverloads loc head tail archetype s s'
    _ -> abort $ DomainError loc

instOverload
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
instOverload loc overload archetype s s' = case overload of
  Val.Struct i env xs e -> instStruct i env xs e archetype s s'
  Val.Class i env sup xs e -> instClass loc i env sup xs e archetype s s'
  _ -> pure Nothing

instStruct
  :: MonadEval m
  => Label
  -> Env m
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
instStruct i env xs e archetype s s' = do
  archetype' <- traverse freshNamed xs
  s <- execEvalT' (evalExp e) R { env = archetype' <> env, archetype, archetype' } s
  unify s.choiceFree s'.choiceFree
  unify s.storeFree s'.storeFree
  Just <$> newVar (Val.StructInst i $ filterNames archetype')

instClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
instClass loc i env sup xs e archetype s s' = do
  (var, _, initClass) <- allocClass loc i env sup xs e
  s <- initClass archetype s
  unify s.choiceFree s'.choiceFree
  unify s.storeFree s'.storeFree
  pure $ Just var

allocClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> VerseT m (VarVal m, Env m, Archetype m -> S m -> VerseT m (S m))
allocClass loc i env sup xs e = do
  (sup, vars_sup, initSup) <- allocSup loc sup
  archetype' <- traverse freshNamed xs
  let
    vars = vars_sup <> archetype'
    initClass archetype s = do
      s <- execEvalT' (evalExp e) R { env = vars <> env, archetype, archetype' } s
      initSup (archetype' <> archetype) s
  newVar (Val.ClassInst i sup $ filterNames vars) <&> (, vars, initClass)

allocSup
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> VerseT m (Maybe (VarVal m), Env m, Archetype m -> S m -> VerseT m (S m))
allocSup loc sup = case sup of
  Nothing -> pure (Nothing, mempty, const pure)
  Just sup -> do
    (i, env, sup, xs, e) <- readClass loc sup
    (sup, xs, initSup) <- allocClass loc i env sup xs e
    pure (Just sup, xs, initSup)

readClass
  :: (MonadAbort Error m, MonadRef m)
  => Loc
  -> VarVal m
  -> VerseT m (Label, Env m, Maybe (VarVal m), IdentMap Bool, L (Exp L Ident))
readClass loc = readVar >=> \ case
  Val.Overloads head tail -> case head of
    Val.Class i env sup xs e -> pure (i, env, sup, xs, e)
    _ -> readClass loc tail
  _ -> abort $ DomainError loc

findClassInst :: (MonadRef m, MonadSupply Int m)
              => Label -> VarVal m -> VerseT m (VarVal m)
findClassInst i var = readVar var >>= \ case
  Val.ClassInst j sup _
    | i == j -> pure var
    | Just var <- sup -> findClassInst i var
  _ -> empty

evalInvoke
  :: MonadEval m
  => Loc
  -> VarVal m
  -> VarVal m
  -> EvalT m (VarVal m)
evalInvoke loc var1 var2 = do
  var <- lift freshVar
  s <- get
  s' <- lift freshS
  put s'
  lift . fork $ unify var =<< invoke loc var1 var2 s s'
  pure var

invoke
  :: MonadEval m
  => Loc
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (VarVal m)
invoke loc var1 var2 s s' = readVar var1 >>= \ case
  Val.Tuple xs -> do
    unify s.storeFree s'.storeFree
    _ <- readVar s.choiceFree
    var <- invokeTuple xs var2
    unify s.choiceFree s'.choiceFree
    pure var
  Val.Enum i _xs _ns es -> do
    unify s.storeFree s'.storeFree
    _ <- readVar s.choiceFree
    var <- invokeEnum i es var2
    unify s.choiceFree s'.choiceFree
    pure var
  Val.Overloads head tail ->
    invokeOverloads loc head tail var2 s s'
  _ -> abort $ DomainError loc

invokeTuple :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
            => [Var m f] -> VarVal m -> VerseT m (Var m f)
invokeTuple xs var = asum $ zip xs [0 ..] <&> \ (x, i) -> do
  unify var =<< newVar (Val.Int i)
  pure x

invokeEnum :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
            => Label -> [Var m f] -> VarVal m -> VerseT m (Var m f)
invokeEnum i xs var = asum $ zip xs [0 ..] <&> \ (x, c) -> do
  unify var =<< newVar (Val.EnumValue i c)
  pure x

invokeOverloads
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (VarVal m)
invokeOverloads loc head tail arg s s' = invokeOverload loc head arg s s' >>= \ case
  Just result -> pure result
  Nothing -> readVar tail >>= \ case
    Val.Overloads head tail -> invokeOverloads loc head tail arg s s'
    _ -> abort $ DomainError loc

invokeOverload
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeOverload loc overload arg s s' = case overload of
  Val.Fun _ env xs e_domain e ->
    invokeFun env xs e_domain e arg s s'
  Val.Struct i env xs e ->
    invokeStruct i env xs e arg s s'
  Val.Class i env sup xs e ->
    invokeClass loc i env sup xs e arg s s'
  Val.Intrinsic intrinsic ->
    invokeIntrinsic intrinsic arg s s'

invokeFun
  :: MonadEval m
  => Env m
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeFun env xs e_domain e v_arg s s' = readIVar =<< if'
  do
    xs <- traverse freshNamed xs
    let r = mempty { env = xs <> env }
    unify v_arg =<< evalEvalT' (evalExp e_domain) r s
    pure xs
  do
    \ xs -> do
      let r = mempty { env = xs <> env }
      (var, s) <- runEvalT' (evalExp e) r s
      unify s.choiceFree s'.choiceFree
      unify s.storeFree s'.storeFree
      pure $ Just var
  do
    pure Nothing

invokeStruct
  :: MonadEval m
  => Label
  -> Env m
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeStruct i env xs e arg s s' = do
  archetype <- traverse freshNamed xs
  xs <- traverse freshNamed xs
  s <- execEvalT' (evalExp e) mempty { env = xs <> env, archetype } s
  unify arg =<< newVar (Val.StructInst i $ filterNames xs)
  unify s.choiceFree s'.choiceFree
  unify s.storeFree s'.storeFree
  pure $ Just arg

invokeClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeClass loc i sup env xs e arg s s' = do
  (inst, _, s) <- instEmptyClass loc i sup env xs e s
  unify inst =<< findClassInst i arg
  unify s.choiceFree s'.choiceFree
  unify s.storeFree s'.storeFree
  pure $ Just arg

instEmptyClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> S m
  -> VerseT m (VarVal m, Env m, S m)
instEmptyClass loc i env sup xs e s = do
  (sup, xs_sup, s) <- instEmptySup loc sup s
  archetype <- traverse freshNamed xs
  xs <- traverse freshNamed xs
  let xs' = xs_sup <> xs
  s <- execEvalT' (evalExp e) mempty { env = xs' <> env, archetype } s
  newVar (Val.ClassInst i sup $ filterNames xs') <&> (, xs', s)

instEmptySup
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> S m
  -> VerseT m (Maybe (VarVal m), Env m, S m)
instEmptySup loc sup s = case sup of
  Nothing -> pure (Nothing, mempty, s)
  Just sup -> do
    (i, env, sup, xs, e) <- readClass loc sup
    (sup, xs, s) <- instEmptyClass loc i env sup xs e s
    pure (Just sup, xs, s)

invokeIntrinsic
  :: (MonadRef m, MonadSupply Int m)
  => Intrinsic -> VarVal m -> S m -> S m -> VerseT m (Maybe (VarVal m))
invokeIntrinsic = \ case
  Intrinsic.Less -> liftPrim $ liftOrd (<)
  Intrinsic.LessEqual -> liftPrim $ liftOrd (<=)
  Intrinsic.Greater -> liftPrim $ liftOrd (>)
  Intrinsic.GreaterEqual -> liftPrim $ liftOrd (>=)
  Intrinsic.Plus -> liftPrim $ liftNum (+)
  Intrinsic.PrefixPlus -> liftPrim prefixPlus
  Intrinsic.Minus -> liftPrim $ liftNum (-)
  Intrinsic.PrefixMinus -> liftPrim prefixMinus
  Intrinsic.Multiply -> liftPrim $ liftNum (*)
  Intrinsic.Divide -> liftPrim div'
  Intrinsic.To -> to
  Intrinsic.Int -> liftPrim int
  Intrinsic.Float -> liftPrim float

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
    (Val.EnumValue x x', Val.EnumValue y y') | x == y -> guard (f x' y') $> Just var_x
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

div' :: (MonadRef m, MonadSupply Int m)
     => VarVal m -> VerseT m (Maybe (VarVal m))
div' var = readVar var >>= \ case
  Val.Tuple [var_x, var_y] -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.Int _, Val.Int 0) -> empty
    (Val.Rational _, Val.Rational 0) -> empty
    (Val.Int x, Val.Int y) ->
      fmap Just . newVar . Val.Rational $ x % y
    (Val.Int x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ fromInteger x / y
    (Val.Int x, Val.Rational y) ->
      fmap Just . newVar . Val.Rational $ fromInteger x / y
    (Val.Float x, Val.Int y) ->
      fmap Just . newVar . Val.Float $ x / fromInteger y
    (Val.Float x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ x / y
    (Val.Float x, Val.Rational y) ->
      fmap Just . newVar . Val.Float $ fromRational $ toRational x / y
    (Val.Rational x, Val.Int y) ->
      fmap Just . newVar . Val.Rational $ x / fromInteger y
    (Val.Rational x, Val.Float y) ->
      fmap Just . newVar . Val.Float $ fromRational $ x / toRational y
    (Val.Rational x, Val.Rational y) ->
      fmap Just . newVar . Val.Rational $ x / y
    _ -> pure Nothing
  _ -> pure Nothing

to :: (MonadRef m, MonadSupply Int m)
   => VarVal m
   -> S m
   -> S m
   -> VerseT m (Maybe (VarVal m))
to var s s' = readVar var >>= \ case
  Val.Tuple [var_x, var_y] -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Int val1, Int val2) -> do
      unify s.storeFree s'.storeFree
      var <- foldr (\ x z -> newVar (Val.Int x) <|> z) empty [val1 .. val2]
      unify s.choiceFree s'.choiceFree
      pure $ Just var
    _ -> pure Nothing
  _ -> pure Nothing

int :: (MonadRef m, MonadSupply Int m)
    => VarVal m -> VerseT m (Maybe (VarVal m))
int var = do
  fork $ readVar var >>= \ case
    Int _ -> pure ()
    _ -> empty
  pure $ Just var

float :: (MonadRef m, MonadSupply Int m)
      => VarVal m -> VerseT m (Maybe (VarVal m))
float var = do
  fork $ readVar var >>= \ case
    Val.Float _ -> pure ()
    _ -> empty
  pure $ Just var

liftPrim
  :: (MonadRef m, MonadSupply Int m)
  => (VarVal m -> VerseT m (Maybe (VarVal m)))
  -> VarVal m -> S m -> S m -> VerseT m (Maybe (VarVal m))
liftPrim f var s s' = f var >>= \ case
  Nothing -> pure Nothing
  x@Just {} -> do
    unify s.choiceFree s'.choiceFree
    unify s.storeFree s'.storeFree
    pure x

newEnv :: (MonadRef m, MonadSupply Int m) => VerseT m (Env m)
newEnv = execWriterT $ do
  tell' Intrinsic.Less
  tell' Intrinsic.LessEqual
  tell' Intrinsic.Greater
  tell' Intrinsic.GreaterEqual
  tell' Intrinsic.Plus
  tell' Intrinsic.PrefixPlus
  tell' Intrinsic.Minus
  tell' Intrinsic.PrefixMinus
  tell' Intrinsic.Multiply
  tell' Intrinsic.Divide
  tell' Intrinsic.To
  tell' Intrinsic.Int
  tell' Intrinsic.Float
  where
    tell' x =
      tell . HashMap.singleton (fromString $ Intrinsic.toString x) . Val =<<
      lift . newVar . Val.Overloads (Val.Intrinsic x) =<<
      lift freshVar

filterNames :: IdentMap a -> HashMap Name a
filterNames =
  HashMap.fromList .
  HashMap.foldrWithKey
  \ case
    Ident.Name k -> \ v z -> (k, v) : z
    Ident.Label _ -> \ _ z -> z
  []

evalIdent :: (MonadAbort Error m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
          => L Ident -> EvalT m (VarVal m)
evalIdent x = lookupVar (extract x) >>= \ case
  Nothing -> abort $ IdentError (loc x) (extract x)
  Just var -> pure var

lookupVar :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
          => Ident -> EvalT m (Maybe (VarVal m))
lookupVar = lookupNamed >=> \ case
  Nothing -> pure Nothing
  Just x -> Just <$> readNamed x

lookupNamed :: Ident -> EvalT m (Maybe (Named (VarRef m) (VarVal m)))
lookupNamed x = asks $ \ r -> HashMap.lookup x r.env

readNamed :: (MonadRef m, MonadSupply Int m, EqRef (Ref m))
          => Named (VarRef m) (VarVal m) -> EvalT m (VarVal m)
readNamed = \ case
  Ref x -> readVarRef' x
  Val x -> pure x

readNamed'
  :: (MonadRef m, MonadSupply Int m)
  => Var m StoreFree
  -> Var m StoreFree
  -> Named (VarRef m) (VarVal m)
  -> VerseT m (VarVal m)
readNamed' storeFree storeFree' = \ case
  Ref ref -> do
    _ <- readVar storeFree
    x <- readVarRef ref
    unify storeFree storeFree'
    pure x
  Val x -> do
    unify storeFree storeFree'
    pure x

localName :: Ident -> Named (VarRef m) (VarVal m) -> EvalT m a -> EvalT m a
localName k v = local $ \ r -> r { env = HashMap.insert k v r.env }

localEnv :: Env m -> EvalT m a -> EvalT m a
localEnv env = local $ \ r -> mempty { env = env <> r.env }

freshNamed :: (MonadFix m, MonadRef m, MonadSupply Int m)
           => Bool -> VerseT m (Named (VarRef m) (VarVal m))
freshNamed = \ case
  False -> Val <$> freshVar
  True -> Ref <$> freshVarRef

freshVarRef :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
            => VerseT m (VarRef m f)
freshVarRef = newVarRef =<< freshVar

readVarRef' :: (MonadRef m, MonadSupply Int m, RowMatchable f)
            => VarRef m f -> EvalT m (Var m f)
readVarRef' ref = do
  x <- lift freshVar
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    _ <- readVar s.storeFree
    unify x =<< readVarRef ref
    unify storeFree s.storeFree
  pure x

writeVarRef' :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
             => VarRef m f -> Var m f -> EvalT m ()
writeVarRef' ref x = do
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    _ <- readVar s.storeFree
    writeVarRef ref x
    unify storeFree s.storeFree
