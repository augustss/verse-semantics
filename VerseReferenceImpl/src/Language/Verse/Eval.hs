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
import Data.Foldable (foldr, traverse_)
import Data.Function
import Data.Functor ((<&>))
import Data.Hashable
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

import Prelude (Integer, Num (..), Fractional (..), fromRational, toRational)

type EvalT m = RST (R m) (S m) (VerseT m)

data R m = R
  { env :: Env m
  , archetype :: Archetype m
  }

instance Semigroup (R m) where
  x <> y = R { env = x.env <> y.env, archetype = x.archetype <> y.archetype }

instance Monoid (R m) where
  mempty = R { env = mempty, archetype = mempty }

data S m = S
  { choiceFree :: IVar m ()
  , storeFree :: IVar m ()
  }

instance Monad m => Freshenable (S n) m where
  freshen = pure

type Env m = Val.Env Ident (VarRef m) (VarVal m)

type Archetype m = Env m

evalEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> VerseT m a
evalEvalT m = do
  env <- newEnv
  let archetype = mempty
  choiceFree <- newIVar ()
  storeFree <- newIVar ()
  evalRST m R {..} S {..}

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
eval = freeze' <=< evalEvalT . eval'

eval' :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
eval' e = case extract e of
  e1 :*>: e2 ->
    eval' e1 *> eval' e2
  e1 :=: e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    lift $ unify var1 var2
    pure var1
  Exp.InfixColon x e -> do
    r <- ask
    var_x <- evalIdent x
    var_e <- eval' e
    var_arg <- case HashMap.lookup (extract x) r.archetype of
      Just named_arg -> readNamed named_arg
      Nothing -> lift freshVar
    lift . unify var_x =<< evalInvoke (loc e) var_e var_arg
    pure var_arg
  Exp.InfixColonEqual x e -> do
    r <- ask
    var_x <- evalIdent x
    var_y <- case HashMap.lookup (extract x) r.archetype of
      Just named_y -> readNamed named_y
      Nothing -> eval' e
    lift $ unify var_x var_y
    pure var_x
  Exp.MixfixColonEqual x e1 e2 -> do
    r <- ask
    var_x <- evalIdent x
    var_e <- eval' e1
    var_arg <- case HashMap.lookup (extract x) r.archetype of
      Just named_arg -> readNamed named_arg
      Nothing -> eval' e2
    lift . unify var_x =<< evalInvoke (loc e1 <> loc e2) var_e var_arg
    pure var_x
  e1 :.: x ->
    evalDot (loc e) e1 x
  e1 :..: e2 ->
    evalDotDot e1 e2
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
    var_e <- eval' e
    var <- lift freshVar
    lift $ unify var_e =<< newVar (Val.Truth var)
    pure var
  Exp.Module i xs e -> do
    xs <- lift $ traverse freshNamed xs
    _ <- localEnv xs $ eval' e
    lift . newVar . Val.Module i $ filterNames xs
  Exp.Struct i xs e -> do
    r <- ask
    lift $ newVar . Val.Overloads (Val.Struct i r.env xs e) =<< freshVar
  Exp.Class i e_sup xs e -> do
    r <- ask
    var_sup <- traverse eval' e_sup
    lift $ newVar . Val.Overloads (Val.Class i r.env var_sup xs e) =<< freshVar
  Exp.Inst e1 xs e2 ->
    evalInst (loc e) e1 xs e2
  Exp.IfThenElse xs p t e ->
    evalIfThenElse xs p t e
  Exp.ForDo xs e1 e2 ->
    evalForDo xs e1 e2
  Exp.Exists x e -> do
    var <- lift freshVar
    localName (extract x) (Val var) $ eval' e
  Exp.Var x e -> do
    ref <- lift $ newVarRef =<< freshVar
    localName (extract x) (Ref ref) $ eval' e
  Exp.Set x e -> lookupNamed (extract x) >>= \ case
    Nothing -> abort $ IdentError (loc x) (extract x)
    Just (Val _) -> abort $ DomainError $ loc e
    Just (Ref ref) -> do
      var <- eval' e
      writeVarRef' ref var
      pure var
  Exp.Function xs e1 e2 -> do
    i <- supply
    r <- ask
    lift $ newVar . Val.Overloads (Val.Function i r.env xs e1 e2) =<< freshVar
  Exp.ParenInvoke e1 e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    evalInvoke (loc e) var1 var2
  Exp.BracketInvoke e1 e2 -> do
    var1 <- eval' e1
    var2 <- eval' e2
    evalInvoke (loc e) var1 var2
  Exp.Tuple xs ->
    lift . newVar . Val.Tuple =<< traverse eval' xs
  Exp.Truth e ->
    lift . newVar . Val.Truth =<< eval' e
  Exp.Option e ->
    evalOption e
  Exp.Int x ->
    lift . newVar $ Val.Int x
  Exp.Float x ->
    lift . newVar $ Val.Float x
  Exp.Name x ->
    evalIdent $ x <$ e

evalDot :: MonadEval m => Loc -> L (Exp L Ident) -> Name -> EvalT m (VarVal m)
evalDot loc e x = do
  var_e <- eval' e
  var <- lift freshVar
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    xs <- readVar var_e >>= \ case
      Val.Module _ xs -> pure xs
      Val.StructInst _ xs -> pure xs
      Val.ClassInst _ _ xs -> pure xs
      _ -> abort $ DomainError loc
    unify var =<< case HashMap.lookup x xs of
      Just y -> readNamed' s.storeFree storeFree y
      Nothing -> abort $ NameError loc x
  pure var

evalDotDot :: MonadEval m => L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalDotDot e1 e2 = do
  var1 <- eval' e1
  var2 <- eval' e2
  var <- lift freshVar
  s <- get
  choiceFree <- lift freshIVar
  put s { choiceFree }
  lift . fork $ (,) <$> readVar var1 <*> readVar var2 >>= \ case
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

evalChoice :: MonadEval m => L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalChoice e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  choiceFree <- lift freshIVar
  storeFree <- lift freshIVar
  put S { choiceFree, storeFree }
  lift $ fork do
    readIVar s.choiceFree
    (x, s') <- runEvalT' (eval' e1 <|> eval' e2) r s
    fork do
      readIVar s'.choiceFree
      writeIVar choiceFree ()
    fork do
      readIVar s'.storeFree
      writeIVar storeFree ()
    unify var x
  pure var

evalOne :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalOne e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    unify var =<< readIVar =<< one do
      choiceFree <- newIVar ()
      evalEvalT' (eval' e) r s { choiceFree }
    writeIVar storeFree ()
  pure var

evalAll :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalAll e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    unify var =<< newVar . Val.Tuple =<< readIVar =<< all do
      choiceFree <- newIVar ()
      evalEvalT' (eval' e) r s { choiceFree }
    writeIVar storeFree ()
  pure var

evalNot :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalNot e = do
  r <- ask
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift . fork $
    void $ readIVar =<< if'
    do
      choiceFree <- newIVar ()
      void $ runEvalT' (eval' e) r s { choiceFree }
    do
      const empty
    do
      writeIVar storeFree ()
  lift . newVar $ Val.Tuple []

evalIfThenElse
  :: MonadEval m
  => HashMap Ident Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalIfThenElse xs p t e = do
  var <- lift freshVar
  r <- ask
  s <- get
  choiceFree <- lift freshIVar
  storeFree <- lift freshIVar
  put S { choiceFree, storeFree }
  lift $ fork do
    (var', s) <- readIVar =<< if'
      do
        xs <- traverse freshNamed xs
        choiceFree <- newIVar ()
        _ <- runEvalT' (eval' p) mempty { env = xs <> r.env } s { choiceFree }
        pure xs
      do
        \ xs -> runEvalT' (eval' t) mempty { env = xs <> r.env } s
      do
        runEvalT' (eval' e) r s
    unify var var'
    fork do
      readIVar s.choiceFree
      writeIVar choiceFree ()
    fork do
      readIVar s.storeFree
      writeIVar storeFree ()
  pure var

evalForDo
  :: MonadEval m
  => HashMap Ident Bool
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalForDo xs e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  choiceFree <- lift freshIVar
  storeFree <- lift freshIVar
  put S { choiceFree, storeFree }
  lift $ fork $ do
    (vars, s) <- fmap unzip . readIVar =<< for
      do
        xs <- traverse freshNamed xs
        choiceFree <- newIVar ()
        _ <- runEvalT' (eval' e1) mempty { env = xs <> r.env } s { choiceFree }
        pure xs
      do
        \ xs -> runEvalT' (eval' e2) mempty { env = xs <> r.env } s
    unify var =<< newVar (Val.Tuple vars)
    fork do
      traverse_ (readIVar . (.choiceFree)) s
      writeIVar choiceFree ()
    fork do
      traverse_ (readIVar . (.storeFree)) s
      writeIVar storeFree ()
  pure var

evalInst
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> HashMap Ident Bool
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalInst loc e1 xs e2 = do
  var1 <- eval' e1
  xs <- lift $ traverse freshNamed xs
  _ <- localEnv xs $ eval' e2
  var <- lift freshVar
  s <- get
  s' <- S <$> lift freshIVar <*> lift freshIVar
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
  xs <- traverse freshNamed xs
  s <- execEvalT' (eval' e) R { env = xs <> env, archetype } s
  fork do
    readIVar s.choiceFree
    writeIVar s'.choiceFree ()
  fork do
    readIVar s.storeFree
    writeIVar s'.storeFree ()
  Just <$> newVar (Val.StructInst i $ filterNames xs)

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
  (inst, _) <- instFreshClass loc i sup xs
  s <- evalClass loc env sup xs e archetype inst s
  fork do
    readIVar s.choiceFree
    writeIVar s'.choiceFree ()
  fork do
    readIVar s.storeFree
    writeIVar s'.storeFree ()
  pure $ Just inst

instFreshClass
  :: (MonadAbort Error m, MonadFix m, MonadRef m, MonadSupply Int m)
  => Loc
  -> Label
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> VerseT m (VarVal m, Env m)
instFreshClass loc i sup xs = do
  (sup, xs_sup) <- instFreshSup loc sup
  xs <- traverse freshNamed xs
  let xs' = xs_sup <> xs
  newVar (Val.ClassInst i sup $ filterNames xs') <&> (, xs')

instFreshSup
  :: (MonadAbort Error m, MonadFix m, MonadRef m, MonadSupply Int m)
  => Loc
  -> Maybe (VarVal m)
  -> VerseT m (Maybe (VarVal m), Env m)
instFreshSup loc sup = case sup of
  Nothing -> pure (Nothing, mempty)
  Just sup -> do
    (i, _, sup, xs, _) <- readClass loc sup
    (sup, xs) <- instFreshClass loc i sup xs
    pure (Just sup, xs)

evalClass
  :: MonadEval m
  => Loc
  -> Env m
  -> Maybe (VarVal m)
  -> IdentMap Bool
  -> L (Exp L Ident)
  -> Archetype m
  -> VarVal m
  -> S m
  -> VerseT m (S m)
evalClass loc env sup xs e archetype inst s = do
  (_, inst_sup, inst_xs) <- readClassInst loc inst
  let inst_xs' = toIdentMap inst_xs
  s <- execEvalT' (eval' e) R { env = inst_xs' <> env, archetype } s
  let archetype_sup = (inst_xs' /\ xs) <> archetype
  evalSup loc sup archetype_sup inst_sup s

evalSup
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> Archetype m
  -> Maybe (VarVal m)
  -> S m
  -> VerseT m (S m)
evalSup loc sup archetype inst_sup s = case (,) <$> sup <*> inst_sup of
  Nothing -> pure s
  Just (sup, inst_sup) -> do
    (_, env, sup, xs, e) <- readClass loc sup
    evalClass loc env sup xs e archetype inst_sup s

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

readClassInst
  :: (MonadAbort Error m, MonadRef m)
  => Loc
  -> VarVal m
  -> VerseT m (Label, Maybe (VarVal m), Val.Env Name (VarRef m) (VarVal m))
readClassInst loc = readVar >=> \ case
  Val.ClassInst i sup xs -> pure (i, sup, xs)
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
  s' <- S <$> lift freshIVar <*> lift freshIVar
  put s'
  lift . fork $ readVar var1 >>= \ case
    Val.Tuple xs -> do
      readIVar s.choiceFree
      unify var =<< invokeTuple xs var2
      writeIVar s'.choiceFree ()
      fork do
        readIVar s.storeFree
        writeIVar s'.storeFree ()
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
  Val.Function _ env xs e_domain e -> invokeFunction env xs e_domain e arg s s'
  Val.Struct i env xs e -> invokeStruct i env xs e arg s s'
  Val.Class i env sup xs e -> invokeClass loc i env sup xs e arg s s'
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
  :: MonadEval m
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
    unify arg =<< evalEvalT' (eval' e_domain) mempty { env = xs <> env } s
    pure xs
  do
    \ xs -> do
      (result, s) <- runEvalT' (eval' e) mempty { env = xs <> env } s
      fork do
        readIVar s.choiceFree
        writeIVar s'.choiceFree ()
      fork do
        readIVar s.storeFree
        writeIVar s'.storeFree ()
      pure $ Just result
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
  s <- execEvalT' (eval' e) mempty { env = xs <> env, archetype } s
  unify arg =<< newVar (Val.StructInst i $ filterNames xs)
  fork do
    readIVar s.choiceFree
    writeIVar s'.choiceFree ()
  fork do
    readIVar s.storeFree
    writeIVar s'.storeFree ()
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
  fork do
    readIVar s.choiceFree
    writeIVar s'.choiceFree ()
  fork do
    readIVar s.storeFree
    writeIVar s'.storeFree ()
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
  s <- execEvalT' (eval' e) mempty { env = xs' <> env, archetype } s
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
  Intrinsic.Divide -> div'
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

int :: MonadRef m => VarVal m -> VerseT m (Maybe (VarVal m))
int var = readVar var >>= \ case
  Int _ -> pure $ Just var
  _ -> pure Nothing

evalOption :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalOption e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    unify var =<< readIVar =<< if'
      do
        choiceFree <- newIVar ()
        evalEvalT' (eval' e) r s { choiceFree }
      do
        newVar . Val.Truth
      do
        newVar $ Val.Tuple []
    writeIVar storeFree ()
  pure var

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

toIdentMap :: HashMap Name a -> IdentMap a
toIdentMap =
  HashMap.fromList .
  HashMap.foldrWithKey
  (\ k v z -> (Ident.Name k, v) : z)
  []

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
  => IVar m ()
  -> IVar m ()
  -> Named (VarRef m) (VarVal m)
  -> VerseT m (VarVal m)
readNamed' storeFree storeFree' = \ case
  Ref ref -> do
    readIVar storeFree
    x <- readVarRef ref
    writeIVar storeFree' ()
    pure x
  Val x -> do
    fork do
      readIVar storeFree
      writeIVar storeFree' ()
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
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    readIVar s.storeFree
    unify x =<< readVarRef ref
    writeIVar storeFree ()
  pure x

writeVarRef' :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
             => VarRef m f -> Var m f -> EvalT m ()
writeVarRef' ref x = do
  s <- get
  storeFree <- lift freshIVar
  put s { storeFree }
  lift $ fork do
    readIVar s.storeFree
    writeVarRef ref x
    writeIVar storeFree ()

(/\) :: Hashable k => HashMap k v -> HashMap k w -> HashMap k v
(/\) = HashMap.intersection
