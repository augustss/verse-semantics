{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import Control.Monad.Ref
import Control.Monad.RS
import Control.Monad.Supply
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Verse

import Data.Bool
import Data.Eq
import Data.Foldable (Foldable, foldr, foldrM)
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
  zipMatch _ _ = Just []

data StoreFree a = StoreFree deriving (Functor, Foldable, Traversable)

instance RowMatchable StoreFree

instance ZipMatchable StoreFree where
  zipMatch _ _ = Just []

instance ( MonadFix m
         , MonadRef m
         , MonadSupply Int m
         ) => Freshenable (S m) m where
  freshen s = S <$> freshen s.choiceFree <*> freshen s.storeFree

type Env m = Val.Env Ident (VarRef m) (VarVal m)

type Archetype m = Env m

runEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> VerseT m a
runEvalT m = do
  env <- newEnv
  choiceFree <- newVar ChoiceFree
  storeFree <- newVar StoreFree
  evalRST m R {..} S {..}
  where
    archetype = mempty
    archetype' = mempty

type MonadEval m =
  ( MonadAbort Error m
  , MonadFix m
  , MonadRef m
  , MonadSupply Label m
  , EqRef (Ref m)
  )

eval :: MonadEval m => L (Exp L Ident) -> VerseT m FrozenVal
eval = freeze' <=< runEvalT . evalExp

evalExp :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalExp e = case extract e of
  e1 :*>: e2 ->
    evalExp e1 *> evalExp e2
  e1 :=: e2 -> do
    var1 <- evalExp e1
    var2 <- evalExp e2
    lift $ rowUnify' (loc e) var1 var2
    pure var1
  e1 :.: x ->
    evalDot (loc e) e1 x
  e1 :|: e2 ->
    evalChoice (loc e) e1 e2
  Exp.Fail ->
    empty
  Exp.One e' ->
    evalOne (loc e) e'
  Exp.All e' ->
    evalAll (loc e) e'
  Exp.Not e ->
    evalNot e
  Exp.Verify e ->
    evalVerify e
  Exp.Succeeds e' ->
    evalSucceeds (loc e) e'
  Exp.Fails e ->
    evalFails e
  Exp.Decides e' ->
    evalDecides (loc e) e'
  Exp.Assume e' ->
    evalAssume (loc e) e'
  Exp.Module i xs e -> do
    xs <- lift $ freshEnv xs
    _ <- localEnv xs $ evalExp e
    lift . newVar . Val.Module i $ filterNames xs
  Exp.Enum i xs -> lift $ do
    let foldrM' xs f = foldrM f mempty xs
    (xs, xs') <- foldrM' xs $ \ x (xs, xs') ->
      newVar (Val.EnumValue i x) <&> \ var ->
      (HashMap.insert x (Val var) xs, var:xs')
    newVar $ Val.Enum i xs xs'
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
    evalIfThenElse (loc e) xs p t e
  Exp.ForDo xs e1 e2 ->
    evalForDo (loc e) xs e1 e2
  Exp.Def Exp.Exists x e -> do
    var <- lift freshVar
    localName (extract x) (Val var) $ evalExp e
  Exp.Def Exp.Forall x e -> do
    var <- lift $ newVar Val.Any
    localName (extract x) (Val var) $ evalExp e
  Exp.Def (Exp.Var y) x e -> do
    ref <- lift freshVarRef
    var <- lift freshVar
    localName (extract x) (Ref ref var) $ localName (extract y) (Val var) $ evalExp e
  Exp.Set x e -> lookupNamed (extract x) >>= \ case
    Nothing -> abort $ IdentError (loc x) (extract x)
    Just (Val _) -> abort $ DomainError $ loc e
    Just (Ref ref var) -> do
      var <- evalInvoke (loc e) var =<< evalExp e
      writeVarRef' ref var
      pure var
  Exp.Fun xs e_domain e -> do
    i <- supply
    r <- ask
    lift $ newVar . Val.Overloads (Val.Fun i r.env xs e_domain e) =<< freshVar
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
  Exp.IfArchetypeName x y e1 e2 -> asks archetype <&> HashMap.lookup (extract x) >>= \ case
    Nothing -> evalExp e2
    Just var -> local (\ r -> r { env = HashMap.insert (extract y) var r.env }) $
      evalExp e1
  Exp.ArchetypeName x -> asks archetype' <&> HashMap.lookup x >>= \ case
    Nothing -> evalIdent $ x <$ e
    Just x -> readNamed (loc e) x

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
      Val.Enum _ xs _ -> pure xs
      Val.StructInst _ xs -> pure xs
      Val.ClassInst _ _ xs -> pure xs
      _ -> abort $ DomainError loc
    rowUnify' loc var =<< case HashMap.lookup x xs of
      Just y -> readNamed' s.storeFree storeFree y
      Nothing -> abort $ NameError loc x
  pure var

evalChoice
  :: MonadEval m
  => Loc -> L (Exp L Ident) -> L (Exp L Ident) -> EvalT m (VarVal m)
evalChoice loc e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  s' <- lift freshS
  put s'
  lift $ fork do
    _ <- readVar s.choiceFree
    (x, s) <- runRST (evalExp e1 <|> evalExp e2) r s
    zipUnify s.choiceFree s'.choiceFree
    zipUnify s.storeFree s'.storeFree
    rowUnify' loc var x
  pure var

evalOne :: MonadEval m => Loc -> L (Exp L Ident) -> EvalT m (VarVal m)
evalOne loc e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    (var', s) <- readIVar =<< one do
      choiceFree <- newVar ChoiceFree
      runRST (evalExp e) mempty { env = r.env } s { choiceFree }
    rowUnify' loc var var'
    zipUnify storeFree s.storeFree
  pure var

evalAll :: MonadEval m => Loc -> L (Exp L Ident) -> EvalT m (VarVal m)
evalAll loc e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    (xs, s') <- fmap unzip . readIVar =<< all do
      choiceFree <- newVar ChoiceFree
      runRST (evalExp e) mempty { env = r.env } s { choiceFree }
    rowUnify' loc var =<< newVar (Val.Tuple xs)
    zipUnify storeFree =<< getZipUnified (s' <&> (.storeFree)) s.storeFree
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
      execRST (evalExp e) mempty { env = r.env } s { choiceFree }
    do
      const empty
    do
      zipUnify storeFree s.storeFree
  lift . newVar $ Val.Tuple []

evalVerify :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalVerify e = do
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    readIVar =<< verify do
      choiceFree <- newVar ChoiceFree
      void $ evalRST (evalExp e) mempty { env = r.env } s { choiceFree }
    zipUnify storeFree s.storeFree
  lift . newVar $ Val.Tuple []

evalSucceeds :: MonadEval m => Loc -> L (Exp L Ident) -> EvalT m (VarVal m)
evalSucceeds loc' e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift . fork $ succeeds
    do
      choiceFree <- newVar ChoiceFree
      runRST (evalExp e) mempty { env = r.env } s { choiceFree }
    >>= readIVar >>= \ case
      Nothing -> abort . SucceedsError $ loc e
      Just (var', s) -> do
        rowUnify' loc' var var'
        zipUnify storeFree s.storeFree
  pure var

evalFails :: MonadEval m => L (Exp L Ident) -> EvalT m (VarVal m)
evalFails e = do
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork $ fails
    do
      choiceFree <- newVar ChoiceFree
      execRST (evalExp e) mempty { env = r.env } s { choiceFree }
    >>= readIVar >>= \ case
      False -> abort . FailsError $ loc e
      True -> empty
  lift freshVar

evalDecides :: MonadEval m => Loc -> L (Exp L Ident) -> EvalT m (VarVal m)
evalDecides loc' e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift . fork $ decides
    do
      choiceFree <- newVar ChoiceFree
      runRST (evalExp e) mempty { env = r.env } s { choiceFree }
    >>= readIVar >>= \ case
      Nothing -> abort . DecidesError $ loc e
      Just (var', s) -> do
        rowUnify' loc' var var'
        zipUnify storeFree s.storeFree
  pure var

evalAssume :: MonadEval m => Loc -> L (Exp L Ident) -> EvalT m (VarVal m)
evalAssume loc e = do
  var <- lift freshVar
  r <- ask
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    (var', s) <- readIVar =<< assume do
      choiceFree <- newVar ChoiceFree
      runRST (evalExp e) mempty { env = r.env } s { choiceFree }
    rowUnify' loc var var'
    zipUnify storeFree s.storeFree
  pure var

evalIfThenElse
  :: MonadEval m
  => Loc
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalIfThenElse loc xs p t e = do
  var <- lift freshVar
  r <- ask
  s <- get
  choiceFree <- lift freshVar
  storeFree <- lift freshVar
  put S { choiceFree, storeFree }
  lift $ fork do
    (var', s) <- readIVar =<< if'
      do
        xs <- freshEnv xs
        choiceFree <- newVar ChoiceFree
        s <- execRST (evalExp p) mempty { env = xs <> r.env } s { choiceFree }
        pure (xs, s)
      do
        \ (xs, s) -> runRST (evalExp t) mempty { env = xs <> r.env } s
      do
        runRST (evalExp e) r s
    rowUnify' loc var var'
    zipUnify choiceFree s.choiceFree
    zipUnify storeFree s.storeFree
  pure var

evalForDo
  :: MonadEval m
  => Loc
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalForDo loc xs e1 e2 = do
  var <- lift freshVar
  r <- ask
  s <- get
  s' <- lift freshS
  put s'
  lift $ fork do
    (vars, s'') <- fmap unzip . readIVar =<< for
      do
        xs <- freshEnv xs
        choiceFree <- newVar ChoiceFree
        s <- execRST (evalExp e1) mempty { env = xs <> r.env } s { choiceFree }
        pure (xs, s)
      do
        \ (xs, s) -> runRST (evalExp e2) mempty { env = xs <> r.env } s
    rowUnify' loc var =<< newVar (Val.Tuple vars)
    zipUnify s'.choiceFree =<< getZipUnified (s'' <&> (.choiceFree)) s.choiceFree
    zipUnify s'.storeFree =<< getZipUnified (s'' <&> (.storeFree)) s.storeFree
  pure var

evalInst
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> EvalT m (VarVal m)
evalInst loc e1 xs e2 = do
  var1 <- evalExp e1
  xs <- lift $ freshEnv xs
  _ <- localEnv xs $ evalExp e2
  var <- lift freshVar
  s <- get
  s' <- lift freshS
  put s'
  lift . fork $ readVar var1 >>= \ case
    Val.Overloads head tail ->
      rowUnify' loc var =<< instOverloads loc head tail xs s s'
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
instOverloads loc head tail archetype s s' =
  instOverload loc head archetype s s' >>= \ case
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
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
instStruct i env xs e archetype s s' = do
  archetype' <- freshEnv xs
  s <- execRST (evalExp e) R { env = archetype' <> env, archetype, archetype' } s
  zipUnify s.choiceFree s'.choiceFree
  zipUnify s.storeFree s'.storeFree
  Just <$> newVar (Val.StructInst i $ filterNames archetype')

instClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> Archetype m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
instClass loc i env sup xs e archetype s s' = do
  (var, _, initClass) <- allocClass loc i env sup xs e
  s <- initClass archetype s
  zipUnify s.choiceFree s'.choiceFree
  zipUnify s.storeFree s'.storeFree
  pure $ Just var

allocClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> VerseT m (VarVal m, Env m, Archetype m -> S m -> VerseT m (S m))
allocClass loc i env sup xs e = do
  (sup, vars_sup, initSup) <- allocSup loc sup
  archetype' <- freshEnv xs
  let
    vars = vars_sup <> archetype'
    initClass archetype s = do
      s <- execRST (evalExp e) R { env = vars <> env, archetype, archetype' } s
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
  -> VerseT m (Label, Env m, Maybe (VarVal m), Exp.Env L Ident, L (Exp L Ident))
readClass loc = readVar >=> \ case
  Val.Overloads head tail -> case head of
    Val.Class i env sup xs e -> pure (i, env, sup, xs, e)
    _ -> readClass loc tail
  _ -> abort $ DomainError loc

findClassInst
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
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
  lift . fork $ rowUnify' loc var =<< invoke loc var1 var2 s s'
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
    zipUnify s.storeFree s'.storeFree
    _ <- readVar s.choiceFree
    var <- invokeTuple loc xs var2
    zipUnify s.choiceFree s'.choiceFree
    pure var
  Val.Enum _ _ xs -> do
    zipUnify s.storeFree s'.storeFree
    _ <- readVar s.choiceFree
    var <- invokeEnum loc xs var2
    zipUnify s.choiceFree s'.choiceFree
    pure var
  Val.Overloads head tail ->
    invokeOverloads loc head tail var2 s s'
  _ -> abort $ DomainError loc

invokeTuple
  :: MonadEval m
  => Loc -> [Var m f] -> VarVal m -> VerseT m (Var m f)
invokeTuple loc xs var = asum $ zip xs [0 ..] <&> \ (x, i) -> do
  rowUnify' loc var =<< newVar (Val.Int i)
  pure x

invokeEnum
  :: MonadEval m
  => Loc -> [VarVal m] -> VarVal m -> VerseT m (VarVal m)
invokeEnum loc xs var = asum $ xs <&> \ x -> do
  rowUnify' loc var x
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
invokeOverloads loc head tail arg s s' =
  invokeOverload loc head arg s s' >>= \ case
    Just result -> pure result
    Nothing -> readVar tail >>= \ case
      Val.Overloads head tail -> invokeOverloads loc head tail arg s s'
      Val.AnyOverloads -> yield
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
    invokeFun loc env xs e_domain e arg s s'
  Val.Struct i env xs e ->
    invokeStruct loc i env xs e arg s s'
  Val.Class i env sup xs e ->
    invokeClass loc i env sup xs e arg s s'
  Val.Intrinsic intrinsic ->
    invokeIntrinsic loc intrinsic arg s s'

invokeFun
  :: MonadEval m
  => Loc
  -> Env m
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeFun loc env xs e_domain e v_arg s s' = readIVar =<< if'
  do
    xs <- freshEnv xs
    let r = mempty { env = xs <> env }
    (v_domain, s) <- runRST (evalExp e_domain) r s
    rowUnify' loc v_arg v_domain
    pure (xs, s)
  do
    \ (xs, s) -> do
      let r = mempty { env = xs <> env }
      (var, s) <- runRST (evalExp e) r s
      zipUnify s.choiceFree s'.choiceFree
      zipUnify s.storeFree s'.storeFree
      pure $ Just var
  do
    pure Nothing

invokeStruct
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeStruct loc i env xs e arg s s' = do
  archetype <- freshEnv xs
  xs <- freshEnv xs
  s <- execRST (evalExp e) mempty { env = xs <> env, archetype } s
  rowUnify' loc arg =<< newVar (Val.StructInst i $ filterNames xs)
  zipUnify s.choiceFree s'.choiceFree
  zipUnify s.storeFree s'.storeFree
  pure $ Just arg

invokeClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
invokeClass loc i sup env xs e arg s s' = do
  (inst, _, s) <- instEmptyClass loc i sup env xs e s
  rowUnify' loc inst =<< findClassInst i arg
  zipUnify s.choiceFree s'.choiceFree
  zipUnify s.storeFree s'.storeFree
  pure $ Just arg

instEmptyClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> S m
  -> VerseT m (VarVal m, Env m, S m)
instEmptyClass loc i env sup xs e s = do
  (sup, xs_sup, s) <- instEmptySup loc sup s
  archetype <- freshEnv xs
  xs <- freshEnv xs
  let xs' = xs_sup <> xs
  s <- execRST (evalExp e) mempty { env = xs' <> env, archetype } s
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
  :: MonadEval m
  => Loc -> Intrinsic -> VarVal m -> S m -> S m -> VerseT m (Maybe (VarVal m))
invokeIntrinsic loc = \ case
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
  Intrinsic.Int -> liftPrim $ int loc
  Intrinsic.Float -> liftPrim $ float loc
  Intrinsic.Function -> liftPrim $ function loc
  Intrinsic.Query -> liftPrim $ query loc

liftOrd
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => (forall a . Ord a => a -> a -> Bool)
  -> VarVal m -> VerseT m (Maybe (VarVal m))
liftOrd f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.AnyRational, AnyNumber) -> decide $> Just var_x
    (Val.Rational x, Val.Rational y) -> guard (f x y) $> Just var_x
    (Val.Rational x, Val.Int y) -> guard (f x (fromInteger y)) $> Just var_x
    (Val.Rational x, Val.Float y) -> guard (f (fromRational x) y) $> Just var_x
    (Val.Rational _, AnyNumber) -> decide $> Just var_x
    (Val.AnyInt, AnyNumber) -> decide $> Just var_x
    (Val.Int x, Val.Rational y) -> guard (f (fromInteger x) y) $> Just var_x
    (Val.Int x, Val.Int y) -> guard (f x y) $> Just var_x
    (Val.Int x, Val.Float y) -> guard (f (fromInteger x) y) $> Just var_x
    (Val.Int _, AnyNumber) -> decide $> Just var_x
    (Val.AnyFloat, AnyNumber) -> decide $> Just var_x
    (Val.Float x, Val.Rational y) -> guard (f (toRational x) y) $> Just var_x
    (Val.Float x, Val.Int y) -> guard (f x (fromInteger y)) $> Just var_x
    (Val.Float x, Val.Float y) -> guard (f x y) $> Just var_x
    (Val.Float _, AnyNumber) -> decide $> Just var_x
    _ -> pure Nothing
  _ -> pure Nothing

liftNum
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => (forall a . Num a => a -> a -> a)
  -> VarVal m -> VerseT m (Maybe (VarVal m))
liftNum f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.AnyRational, Val.AnyRational) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.Rational _) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.AnyInt) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.Int _) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyRational, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Rational _, Val.AnyRational) -> Just <$> newVar Val.AnyRational
    (Val.Rational x, Val.Rational y) -> fmap Just . newVar . Val.Rational $ f x y
    (Val.Rational _, Val.AnyInt) -> Just <$> newVar Val.AnyRational
    (Val.Rational x, Val.Int y) -> fmap Just . newVar . Val.Rational $ f x (fromInteger y)
    (Val.Rational _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Rational x, Val.Float y) -> fmap Just . newVar . Val.Float $ f (fromRational x) y
    (Val.AnyInt, Val.AnyRational) -> Just <$> newVar Val.AnyRational
    (Val.AnyInt, Val.Rational _) -> Just <$> newVar Val.AnyRational
    (Val.AnyInt, Val.AnyInt) -> Just <$> newVar Val.AnyInt
    (Val.AnyInt, Val.Int _) -> Just <$> newVar Val.AnyInt
    (Val.AnyInt, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyInt, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Int _, Val.AnyRational) -> Just <$> newVar Val.AnyRational
    (Val.Int x, Val.Rational y) -> fmap Just . newVar . Val.Rational $ f (fromInteger x) y
    (Val.Int _, Val.AnyInt) -> Just <$> newVar Val.AnyInt
    (Val.Int x, Val.Int y) -> fmap Just . newVar . Val.Int $ f x y
    (Val.Int _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Int x, Val.Float y) -> fmap Just . newVar . Val.Float $ f (fromInteger x) y
    (Val.AnyFloat, Val.AnyRational) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Rational _) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.AnyInt) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Int _) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Float _, Val.AnyRational) -> Just <$> newVar Val.AnyFloat
    (Val.Float x, Val.Rational y) -> fmap Just . newVar . Val.Float $ f x (fromRational y)
    (Val.Float _, Val.AnyInt) -> Just <$> newVar Val.AnyFloat
    (Val.Float x, Val.Int y) -> fmap Just . newVar . Val.Float $ f x (fromInteger y)
    (Val.Float _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Float x, Val.Float y) -> fmap Just . newVar . Val.Float $ f x y
    _ -> pure Nothing
  _ -> pure Nothing

prefixPlus :: MonadRef m => VarVal m -> VerseT m (Maybe (VarVal m))
prefixPlus var = readVar var >>= \ case
  AnyNumber -> pure $ Just var
  _ -> pure Nothing

prefixMinus
  :: (MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (Maybe (VarVal m))
prefixMinus var = readVar var >>= \ case
  Val.AnyRational -> Just <$> newVar Val.AnyRational
  Val.Rational x -> Just <$> newVar (Val.Rational $ negate x)
  Val.AnyInt -> Just <$> newVar Val.AnyInt
  Val.Int x -> Just <$> newVar (Val.Int $ negate x)
  Val.AnyFloat -> Just <$> newVar Val.AnyFloat
  Val.Float x -> Just <$> newVar (Val.Float $ negate x)
  _ -> pure Nothing

div'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m -> VerseT m (Maybe (VarVal m))
div' var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Val.AnyRational, Val.AnyRational) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.AnyRational, Val.Rational 0) -> empty
    (Val.AnyRational, Val.Rational _) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.AnyInt) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.AnyRational, Val.Int 0) -> empty
    (Val.AnyRational, Val.Int _) -> Just <$> newVar Val.AnyRational
    (Val.AnyRational, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyRational, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Rational _, Val.AnyRational) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.Rational _, Val.Rational 0) -> empty
    (Val.Rational x, Val.Rational y) -> fmap Just . newVar . Val.Rational $ x / y
    (Val.Rational _, Val.AnyInt) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.Rational _, Val.Int 0) -> empty
    (Val.Rational x, Val.Int y) -> fmap Just . newVar . Val.Rational $ x / fromInteger y
    (Val.Rational _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Rational x, Val.Float y) -> fmap Just . newVar . Val.Float $ fromRational x / y
    (Val.AnyInt, Val.AnyRational) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.AnyInt, Val.Rational 0) -> empty
    (Val.AnyInt, Val.Rational _) -> Just <$> newVar Val.AnyRational
    (Val.AnyInt, Val.AnyInt) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.AnyInt, Val.Int 0) -> empty
    (Val.AnyInt, Val.Int _) -> Just <$> newVar Val.AnyRational
    (Val.AnyInt, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyInt, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Int _, Val.AnyRational) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.Int _, Val.Rational 0) -> empty
    (Val.Int x, Val.Rational y) -> fmap Just . newVar . Val.Rational $ fromInteger x / y
    (Val.Int _, Val.AnyInt) -> decide *> (Just <$> newVar Val.AnyRational)
    (Val.Int _, Val.Int 0) -> empty
    (Val.Int x, Val.Int y) -> fmap Just . newVar . Val.Rational $ x % y
    (Val.Int _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Int x, Val.Float y) -> fmap Just . newVar . Val.Float $ fromInteger x / y
    (Val.AnyFloat, Val.AnyRational) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Rational _) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.AnyInt) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Int _) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.AnyFloat, Val.Float _) -> Just <$> newVar Val.AnyFloat
    (Val.Float _, Val.AnyRational) -> decide *> (Just <$> newVar Val.AnyFloat)
    (Val.Float x, Val.Rational y) -> fmap Just . newVar . Val.Float $ x / fromRational y
    (Val.Float _, Val.AnyInt) -> Just <$> newVar Val.AnyFloat
    (Val.Float x, Val.Int y) -> fmap Just . newVar . Val.Float $ x / fromInteger y
    (Val.Float _, Val.AnyFloat) -> Just <$> newVar Val.AnyFloat
    (Val.Float x, Val.Float y) -> fmap Just . newVar . Val.Float $ x / y
    _ -> pure Nothing
  _ -> pure Nothing

to
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
to var s s' = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar var_x <*> readVar var_y >>= \ case
    (Int val1, Int val2) -> to' val1 val2 s s'
    _ -> pure Nothing
  _ -> pure Nothing

to'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => Integer
  -> Integer
  -> S m
  -> S m
  -> VerseT m (Maybe (VarVal m))
to' val1 val2 s s' = do
  zipUnify s.storeFree s'.storeFree
  var <- foldr (\ x z -> newVar (Val.Int x) <|> z) empty [val1 .. val2]
  zipUnify s.choiceFree s'.choiceFree
  pure $ Just var

pattern AnyNumber :: Val f a
pattern AnyNumber <- (number -> True)

number :: Val f a -> Bool
number = \ case
  Val.AnyRational -> True
  Val.Rational _ -> True
  Val.AnyInt -> True
  Val.Int _ -> True
  Val.AnyFloat -> True
  Val.Float _ -> True
  _ -> False

pattern Int :: Integer -> Val f a
pattern Int x <- (getInt -> Just x)

getInt :: Val f a -> Maybe Integer
getInt = \ case
  Val.Rational x | denominator x == 1 -> pure $ numerator x
  Val.Int x -> pure x
  _ -> empty

int
  :: MonadEval m
  => Loc -> VarVal m -> VerseT m (Maybe (VarVal m))
int loc var = do
  fork $ readVar var >>= \ case
    Val.Any -> rowUnify' loc var =<< newVar Val.AnyInt
    Val.AnyRational -> rowUnify' loc var =<< newVar Val.AnyInt
    Val.Rational x | denominator x == 1 -> pure ()
    Val.AnyInt -> pure ()
    Val.Int _ -> pure ()
    _ -> empty
  pure $ Just var

float
  :: MonadEval m
  => Loc -> VarVal m -> VerseT m (Maybe (VarVal m))
float loc var = do
  fork $ readVar var >>= \ case
    Val.Any -> rowUnify' loc var =<< newVar Val.AnyFloat
    Val.AnyFloat -> pure ()
    Val.Float _ -> pure ()
    _ -> empty
  pure $ Just var

function
  :: MonadEval m
  => Loc -> VarVal m -> VerseT m (Maybe (VarVal m))
function loc var = do
  fork $ readVar var >>= \ case
    Val.Any -> rowUnify' loc var =<< newVar Val.AnyOverloads
    Val.Tuple _ -> pure ()
    Val.Enum {} -> pure ()
    Val.AnyOverloads -> pure ()
    Val.Overloads {} -> pure ()
    _ -> empty
  pure $ Just var

query
  :: MonadEval m
  => Loc -> VarVal m -> VerseT m (Maybe (VarVal m))
query loc var = do
  var' <- freshVar
  rowUnify' loc var =<< newVar (Val.Truth var')
  pure $ Just var'

liftPrim
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (VarVal m -> VerseT m (Maybe (VarVal m)))
  -> VarVal m -> S m -> S m -> VerseT m (Maybe (VarVal m))
liftPrim f var s s' = f var >>= \ case
  Nothing -> pure Nothing
  x@Just {} -> do
    zipUnify s.choiceFree s'.choiceFree
    zipUnify s.storeFree s'.storeFree
    pure x

readPair
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m
  -> VerseT m (Maybe (VarVal m, VarVal m))
readPair var = readVar var <&> \ case
  Val.Tuple [var1, var2] -> Just (var1, var2)
  _ -> Nothing

evalIdent
  :: MonadEval m
  => L Ident -> EvalT m (VarVal m)
evalIdent x = lookupVar (loc x) (extract x) >>= \ case
  Nothing -> abort $ IdentError (loc x) (extract x)
  Just var -> pure var

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
  tell' Intrinsic.Function
  tell' Intrinsic.Query
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

lookupVar
  :: MonadEval m
  => Loc -> Ident -> EvalT m (Maybe (VarVal m))
lookupVar loc = lookupNamed >=> \ case
  Nothing -> pure Nothing
  Just x -> Just <$> readNamed loc x

lookupNamed :: Ident -> EvalT m (Maybe (Named (VarRef m) (VarVal m)))
lookupNamed x = asks $ \ r -> HashMap.lookup x r.env

readNamed
  :: MonadEval m
  => Loc -> Named (VarRef m) (VarVal m) -> EvalT m (VarVal m)
readNamed loc = \ case
  Ref x _ -> readVarRef' loc x
  Val x -> pure x

readNamed'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Var m StoreFree
  -> Var m StoreFree
  -> Named (VarRef m) (VarVal m)
  -> VerseT m (VarVal m)
readNamed' storeFree storeFree' = \ case
  Ref ref _ -> do
    _ <- readVar storeFree
    x <- readVarRef ref
    zipUnify storeFree storeFree'
    pure x
  Val x -> do
    zipUnify storeFree storeFree'
    pure x

localName :: Ident -> Named (VarRef m) (VarVal m) -> EvalT m a -> EvalT m a
localName k v = local $ \ r -> r { env = HashMap.insert k v r.env }

localEnv :: Env m -> EvalT m a -> EvalT m a
localEnv env = local $ \ r -> mempty { env = env <> r.env }

freshEnv
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Exp.Env L Ident -> VerseT m (Env m)
freshEnv = getAp . HashMap.foldMapWithKey f
  where
    f k = \ case
      Exp.Exists -> Ap $ HashMap.singleton k . Val <$> freshVar
      Exp.Forall -> Ap $ HashMap.singleton k . Val <$> newVar Val.Any
      Exp.Var k' -> Ap $ do
        ref <- freshVarRef
        var <- freshVar
        pure $
          HashMap.singleton k (Ref ref var) <>
          HashMap.singleton (extract k') (Val var)

freshVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
  => VerseT m (VarRef m f)
freshVarRef = newVarRef =<< freshVar

readVarRef'
  :: (MonadEval m, RowMatchable f)
  => Loc -> VarRef m f -> EvalT m (Var m f)
readVarRef' loc ref = do
  x <- lift freshVar
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    _ <- readVar s.storeFree
    rowUnify' loc x =<< readVarRef ref
    zipUnify storeFree s.storeFree
  pure x

writeVarRef'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Traversable f)
  => VarRef m f -> Var m f -> EvalT m ()
writeVarRef' ref x = do
  s <- get
  storeFree <- lift freshVar
  put s { storeFree }
  lift $ fork do
    _ <- readVar s.storeFree
    writeVarRef ref x
    zipUnify storeFree s.storeFree

rowUnify'
  :: (MonadEval m, RowMatchable f)
  => Loc -> Var m f -> Var m f -> VerseT m ()
rowUnify' loc = rowUnify $ abort $ UndecidableError loc

getZipUnified
  :: (MonadFix m, MonadRef m, MonadSupply Int m, ZipMatchable f)
  => [Var m f] -> Var m f -> VerseT m (Var m f)
getZipUnified = \ case
  [] -> pure
  x:xs -> const $ getZipUnified1 x xs

getZipUnified1
  :: (MonadFix m, MonadRef m, MonadSupply Int m, ZipMatchable f)
  => Var m f -> [Var m f] -> VerseT m (Var m f)
getZipUnified1 x = \ case
  [] -> pure x
  y:xs -> do
    zipUnify x y
    getZipUnified1 x xs
