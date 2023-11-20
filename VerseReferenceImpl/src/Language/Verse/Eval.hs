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
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Verse

import Data.Bool
import Data.Coerce
import Data.Eq
import Data.Fix
import Data.Foldable (foldr, foldrM, for_)
import Data.Function
import Data.Functor ((<&>))
import Data.Functor.Compose
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int
import Data.List (zip)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Tuple
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
import Language.Verse.Mode
import Language.Verse.Name
import Language.Verse.Val
  ( FrozenVal
  , Named (..)
  , Val
  , VarEnv
  , VarNamed
  , VarRefVal
  , VarVal
  , forVal_
  )
import Language.Verse.Val qualified as Val

import Prelude
  ( Double
  , Integer
  , Num (..)
  , Fractional (..)
  , fromRational
  , isNaN
  , toRational
  )

type EvalT m = ReaderT (R m) (VerseT m)

data R m = R
  { mode :: !Mode
  , env :: !(Env m)
  , assumed :: !Bool
  , archetype :: !(Archetype m)
  , archetype' :: !(Archetype m)
  }

data S m = S
  { choiceFree :: Var m ChoiceFree
  , storeFree :: Var m StoreFree
  }

newS :: (MonadRef m, MonadSupply Int m) => VerseT m (S m)
newS = do
  choiceFree <- newVar ChoiceFree
  storeFree <- newVar StoreFree
  pure S { choiceFree, storeFree }

freshS :: (MonadRef m, MonadSupply Int m) => VerseT m (S m)
freshS = do
  choiceFree <- freshVar
  storeFree <- freshVar
  pure S { choiceFree, storeFree }

unifyS
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => S m
  -> S m
  -> VerseT m ()
unifyS s s' = do
  unifyEq s.choiceFree s'.choiceFree
  unifyEq s.storeFree s'.storeFree

data ChoiceFree = ChoiceFree deriving Eq

instance Monad m => Freshenable ChoiceFree m where
  freshen = pure

data StoreFree = StoreFree deriving Eq

instance Monad m => Freshenable StoreFree m where
  freshen = pure

type Env m = Val.VarEnv Ident m

type Archetype m = Env m

runEvalT :: (MonadRef m, MonadSupply Int m) => EvalT m a -> Mode -> VerseT m a
runEvalT m mode = do
  env <- newEnv
  runReaderT m R {..}
  where
    assumed = False
    archetype = mempty
    archetype' = mempty

type MonadEval m =
  ( MonadAbort Error m
  , MonadFix m
  , MonadRef m
  , MonadSupply Label m
  , EqRef (Ref m)
  )

eval :: MonadEval m => Mode -> L (Exp L Ident) -> VerseT m FrozenVal
eval mode e = do
  s <- newS
  s' <- freshS
  freeze' =<< runEvalT (evalExp e s s') mode

evalExp :: MonadEval m => L (Exp L Ident) -> S m -> S m -> EvalT m (VarVal m)
evalExp e = case extract e of
  e1 :*>: e2 -> \ s s' -> do
    s'' <- lift freshS
    _ <- evalExp e1 s s''
    evalExp e2 s'' s'
  e1 :=: e2 -> \ s s' -> do
    s'' <- lift freshS
    var1 <- evalExp e1 s s''
    var2 <- evalExp e2 s'' s'
    unify' (loc e) var1 var2
    pure var1
  e1 :.: x ->
    evalDot (loc e) e1 x
  e1 :|: e2 ->
    evalChoice (loc e) e1 e2
  Exp.Fail -> \ _ _ ->
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
  Exp.Module i xs e -> \ s s' -> do
    xs <- lift $ freshEnv xs
    _ <- localNames xs $ evalExp e s s'
    lift . newVar' . Val.Module i $ filterNames xs
  Exp.Enum i xs -> \ s s' -> lift $ do
    let foldrM' xs f = foldrM f mempty xs
    (xs, xs') <- foldrM' xs $ \ x (xs, xs') ->
      newVar' (Val.EnumValue i x) <&> \ var ->
      (HashMap.insert x (Val var) xs, var:xs')
    unifyS s s'
    newVar' $ Val.Enum i xs xs'
  Exp.Struct i xs e -> \ s s' -> ask >>= \ r -> lift $ do
    unifyS s s'
    newVar' . Val.Overloads (Val.Struct i r.env xs e) =<< freshVar'
  Exp.Class i e_sup xs e -> \ s s' -> do
    r <- ask
    var_sup <- case e_sup of
      Nothing -> lift $ Nothing <$ unifyS s s'
      Just e_sup -> Just <$> evalExp e_sup s s'
    lift $ newVar' . Val.Overloads (Val.Class i r.env var_sup xs e) =<< freshVar'
  Exp.Inst e1 xs e2 ->
    evalInst (loc e) e1 xs e2
  Exp.IfThenElse xs p t e ->
    evalIfThenElse (loc e) xs p t e
  Exp.ForDo xs e1 e2 ->
    evalForDo (loc e) xs e1 e2
  Exp.Def Exp.Exists x e -> \ s s' -> do
    var <- lift freshVar'
    localName (extract x) (Val var) $ evalExp e s s'
  Exp.Def Exp.Forall x e -> \ s s' -> do
    var <- lift $ newVar' Val.Any
    localName (extract x) (Val var) $ evalExp e s s'
  Exp.Def (Exp.Var y) x e -> \ s s' -> do
    ref <- lift freshVarRef
    var <- lift freshVar'
    localName (extract x) (Ref ref var) $ localName (extract y) (Val var) $ evalExp e s s'
  Exp.Set x e -> \ s s' -> lookupNamed (extract x) >>= \ case
    Nothing -> abort $ IdentError (loc x) (extract x)
    Just (Val _) -> abort $ DomainError $ loc e
    Just (Ref ref var) -> do
      s'' <- lift freshS
      var_e <- evalExp e s s''
      s''' <- lift freshS
      var <- invoke (loc e) var var_e s'' s'''
      writeVarRef' ref (coerce var) s''' s'
      pure var
  Exp.Fun xs e_domain e -> \ s s' -> ask >>= \ r -> lift $ do
    unifyS s s'
    newVar' . Val.Overloads (Val.Fun r.env xs e_domain e) =<< freshVar'
  Exp.BracketInvoke e1 e2 -> \ s s' -> do
    s'' <- lift freshS
    var1 <- evalExp e1 s s''
    s''' <- lift freshS
    var2 <- evalExp e2 s'' s'''
    invoke (loc e) var1 var2 s''' s'
  Exp.Tuple xs -> \ s s' ->
    lift . newVar' . Val.Tuple =<< evalExpList xs s s'
  Exp.Truth e -> \ s s' ->
    lift . newVar' . Val.Truth =<< evalExp e s s'
  Exp.Int x -> \ s s' -> lift $ do
    unifyS s s'
    newVar' $ Val.Int x
  Exp.Float x -> \ s s' -> lift $ do
    unifyS s s'
    newVar' $ Val.Float x
  Exp.Name x ->
    evalIdent $ x <$ e
  Exp.IfArchetypeName x y e1 e2 -> \ s s' ->
    asks archetype <&> HashMap.lookup (extract x) >>= \ case
      Nothing -> evalExp e2 s s'
      Just var -> localName (extract y) var $ evalExp e1 s s'
  Exp.ArchetypeName x -> \ s s' -> asks archetype' <&> HashMap.lookup x >>= \ case
    Nothing -> evalIdent (x <$ e) s s'
    Just x -> evalNamed (loc e) x s s'

evalDot
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> Name
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalDot loc e x s s' = do
  s'' <- lift freshS
  var_e <- evalExp e s s''
  var <- lift freshVar'
  fork' $ lift (readVar' var_e) >>= getNameEnv loc <&> HashMap.lookup x >>= \ case
    Nothing -> abort $ NameError loc x
    Just x -> unify' loc var =<< evalNamed' loc x s'' s'
  pure var

evalChoice
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalChoice loc e1 e2 s s' = do
  var <- lift freshVar'
  fork' do
    _ <- lift $ readVar s.choiceFree
    unify' loc var =<<  evalExp e1 s s' <|> evalExp e2 s s'
  pure var

evalOne
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalOne loc e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' do
    var' <- lift . readIVar =<< one' do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      evalExp e s { choiceFree } s'
    lift $ unifyEq s.storeFree s'.storeFree
    unify' loc var var'
  pure var

evalAll
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalAll loc e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' $ do
    vars <- lift . readIVar =<< all' do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      evalExp e s { choiceFree } s'
    lift $ unifyEq s.storeFree s'.storeFree
    unify' loc var =<< lift (newVar' $ Val.Tuple vars)
  pure var

evalNot
  :: MonadEval m
  => L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalNot e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  fork' $ lift . readIVar =<< if''
    do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      void $ evalExp e s { choiceFree } s'
    do
      const empty
    do
      lift $ unifyEq s.storeFree s'.storeFree
  lift . newVar' $ Val.Tuple []

evalIfThenElse
  :: MonadEval m
  => Loc
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalIfThenElse loc xs p t e s s' = do
  var <- lift freshVar'
  fork' do
    var' <- lift . readIVar =<< if''
      do
        xs <- lift $ freshEnv xs
        choiceFree <- lift $ newVar ChoiceFree
        s' <- lift freshS
        _ <- localNames xs $ evalExp p s { choiceFree } s'
        pure xs
      do
        \ xs -> localNames xs $ evalExp t s s'
      do
        evalExp e s s'
    unify' loc var var'
  pure var

evalForDo
  :: MonadEval m
  => Loc
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalForDo loc xs e1 e2 s s' = do
  var <- lift freshVar'
  fork' $ do
    s'' <- lift freshS
    vars <- lift . readIVar =<< for'
      do
        xs <- lift $ freshEnv xs
        choiceFree <- lift $ newVar ChoiceFree
        s' <- lift freshS
        _ <- localNames xs $ evalExp e1 s { choiceFree } s'
        pure xs
      do
        \ xs -> localNames xs $ evalExp e2 s s''
    lift $ unifyS s'' s'
    unify' loc var =<< lift (newVar' $ Val.Tuple vars)
  pure var

evalVerify
  :: MonadEval m
  => L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalVerify e s s' = do
  whenM (asks $ not . (.assumed)) $ do
    lift $ unifyS s s'
    fork' $ lift . readIVar =<< verify' do
      s <- lift newS
      s' <- lift freshS
      void . local (\ r -> r { assumed = True }) $ evalExp e s s'
  lift . newVar' $ Val.Tuple []

evalAssume
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalAssume loc e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' $ do
    var' <- lift . readIVar =<< assume' do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      local (\ r -> r { assumed = True }) $ evalExp e s { choiceFree } s'
    lift $ unifyEq s.storeFree s'.storeFree
    unify' loc var var'
  pure var

evalSucceeds
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalSucceeds loc' e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' $ succeeds'
    do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      local (\ r -> r { assumed = False }) $ evalExp e s { choiceFree } s'
    >>= lift . readIVar >>= \ case
      Nothing -> abort . SucceedsError $ loc e
      Just var' -> do
        lift $ unifyEq s.storeFree s'.storeFree
        unify' loc' var var'
  pure var

evalFails
  :: MonadEval m
  => L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalFails e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  fork' $ fails'
    do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      local (\ r -> r { assumed = False }) $ evalExp e s { choiceFree } s'
    >>= lift . readIVar >>= \ case
      False -> abort . FailsError $ loc e
      True -> empty
  lift freshVar'

evalDecides
  :: MonadEval m
  => Loc -> L (Exp L Ident) -> S m -> S m -> EvalT m (VarVal m)
evalDecides loc' e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' $ decides'
    do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      local (\ r -> r { assumed = False }) $ evalExp e s { choiceFree} s'
    >>= lift . readIVar >>= \ case
      Nothing -> abort . DecidesError $ loc e
      Just var' -> do
        lift $ unifyEq s.storeFree s'.storeFree
        unify' loc' var var'
  pure var

evalInst
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalInst loc e1 xs e2 s s' = do
  s'' <- lift freshS
  var1 <- evalExp e1 s s''
  s''' <- lift freshS
  xs <- lift $ freshEnv xs
  _ <- localNames xs $ evalExp e2 s'' s'''
  var <- lift freshVar'
  fork' $ lift (readVar' var1) >>= \ case
    Val.Overloads head tail ->
      unify' loc var =<< instOverloads loc head tail xs s''' s'
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
  -> EvalT m (VarVal m)
instOverloads loc head tail archetype s s' =
  instOverload loc head archetype s s' >>= \ case
    Just result -> pure result
    Nothing -> lift (readVar' tail) >>= \ case
      Val.Overloads head tail -> instOverloads loc head tail archetype s s'
      _ -> abort $ DomainError loc

instOverload
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> Archetype m
  -> S m
  -> S m
  -> EvalT m (Maybe (VarVal m))
instOverload loc overload archetype = case overload of
  Val.Struct i env xs e -> instStruct i env xs e archetype
  Val.Class i env sup xs e -> instClass loc i env sup xs e archetype
  _ -> \ _ _ -> pure Nothing

instStruct
  :: MonadEval m
  => Label
  -> Env m
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> Archetype m
  -> S m
  -> S m
  -> EvalT m (Maybe (VarVal m))
instStruct i env xs e archetype s s' = do
  archetype' <- lift $ freshEnv xs
  _ <- local (\ r -> r { env = archetype' <> env, archetype, archetype' }) $ evalExp e s s'
  Just <$> lift (newVar' $ Val.StructInst i $ filterNames archetype')

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
  -> EvalT m (Maybe (VarVal m))
instClass loc i env sup xs e archetype s s' = do
  (var, _, initClass) <- allocClass loc i env sup xs e
  initClass archetype s s'
  pure $ Just var

allocClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> EvalT m (VarVal m, Env m, Archetype m -> S m -> S m -> EvalT m ())
allocClass loc i env sup xs e = do
  (sup, vars_sup, initSup) <- allocSup loc sup
  archetype' <- lift $ freshEnv xs
  let
    vars = vars_sup <> archetype'
    initClass archetype s s' = do
      s'' <- lift freshS
      _ <- local (\ r -> r { env = vars <> env, archetype, archetype' }) $ evalExp e  s s''
      initSup (archetype' <> archetype) s'' s'
  lift $ newVar' (Val.ClassInst i sup $ filterNames vars) <&> (, vars, initClass)

allocSup
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> EvalT m (Maybe (VarVal m), Env m, Archetype m -> S m -> S m -> EvalT m ())
allocSup loc sup = case sup of
  Nothing -> pure (Nothing, mempty, \ _ s s' -> lift $ unifyS s s')
  Just sup -> do
    (i, env, sup, xs, e) <- lift $ readClass loc sup
    (sup, xs, initSup) <- allocClass loc i env sup xs e
    pure (Just sup, xs, initSup)

invoke
  :: MonadEval m
  => Loc
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invoke loc var1 var2 s s' = do
  var <- lift freshVar'
  fork' $ lift (readVar' var1) >>= \ case
    Val.Tuple xs -> do
      lift $ unifyEq s.storeFree s'.storeFree
      _ <- lift $ readVar s.choiceFree
      unify' loc var =<< invokeTuple loc xs var2
      lift $ unifyEq s.choiceFree s'.choiceFree
    Val.Enum _ _ xs -> do
      lift $ unifyEq s.storeFree s'.storeFree
      _ <- lift $ readVar s.choiceFree
      unify' loc var =<< invokeEnum loc xs var2
      lift $ unifyEq s.choiceFree s'.choiceFree
    Val.Overloads head tail ->
      unify' loc var =<< invokeOverloads loc head tail var2 s s'
    _ -> abort $ DomainError loc
  pure var

invokeTuple
  :: MonadEval m
  => Loc
  -> [VarVal m]
  -> VarVal m
  -> EvalT m (VarVal m)
invokeTuple loc xs var = asum $ zip xs [0 ..] <&> \ (x, i) -> do
  unify' loc var =<< lift (newVar' $ Val.Int i)
  pure x

invokeEnum
  :: MonadEval m
  => Loc
  -> [VarVal m]
  -> VarVal m
  -> EvalT m (VarVal m)
invokeEnum loc xs var = asum $ xs <&> \ x -> do
  unify' loc var x
  pure x

invokeOverloads
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeOverloads loc head tail arg s s' = lift . readIVar =<< if''
  do
    invokeOverload loc head arg s s'
  do
    runDomMatch
  do
    lift (readVar' tail) >>= \ case
      Val.Overloads head tail -> invokeOverloads loc head tail arg s s'
      _ -> abort $ DomainError loc

invokeOverloadDom
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> EvalT m ()
invokeOverloadDom loc f arg = do
  s <- lift newS
  s' <- lift freshS
  void $ invokeOverload loc f arg s s'

invokeOverload
  :: MonadEval m
  => Loc
  -> Val.Overload (VarRef m) (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
invokeOverload loc = \ case
  Val.Fun env xs e_domain e -> invokeFun loc env xs e_domain e
  Val.Struct i env xs e -> invokeStruct loc i env xs e
  Val.Class i env sup xs e -> invokeClass loc i env sup xs e
  Val.Intrinsic intrinsic -> invokeIntrinsic loc intrinsic

data DomMatch m = forall a . Freshenable a m => DomMatch
  a
  (a -> EvalT m (VarVal m))

runDomMatch :: DomMatch m -> EvalT m (VarVal m)
runDomMatch (DomMatch x f) = f x

domMatch_ :: Monad m => EvalT m (VarVal m) -> EvalT m (DomMatch m)
domMatch_ m = pure $ DomMatch () $ \ () -> m

domMatch_' :: Monad m => VerseT m (VarVal m) -> VerseT m (DomMatch m)
domMatch_' m = pure $ DomMatch () $ \ () -> lift m

instance Monad m => Freshenable (DomMatch m) m where
  freshen (DomMatch x f) = freshen x <&> \ x -> DomMatch x f

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
  -> EvalT m (DomMatch m)
invokeFun loc env xs e_domain e v_arg s s' = do
  xs <- lift $ freshEnv xs
  choiceFree <- lift $ newVar ChoiceFree
  s'' <- lift freshS
  v_domain <- localEnv (xs <> env) $ evalExp e_domain s { choiceFree } s''
  unify' loc v_arg v_domain
  pure . DomMatch xs $ \ xs -> localEnv (xs <> env) $ evalExp e s s'

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
  -> EvalT m (DomMatch m)
invokeStruct loc i env xs e arg s s' = pure . DomMatch () $ \ () -> do
  archetype <- lift $ freshEnv xs
  let archetype' = mempty
  xs <- lift $ freshEnv xs
  _ <- local (\ r -> r { env = xs <> env, archetype, archetype' }) $ evalExp e s s'
  unify' loc arg =<< lift (newVar' . Val.StructInst i $ filterNames xs)
  pure arg

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
  -> EvalT m (DomMatch m)
invokeClass loc i sup env xs e arg s s' = domMatch_ $ do
  (inst, _) <- instEmptyClass loc i sup env xs e s s'
  unify' loc inst =<< lift (findClassInst i arg)
  pure arg

instEmptyClass
  :: MonadEval m
  => Loc
  -> Label
  -> Env m
  -> Maybe (VarVal m)
  -> Exp.Env L Ident
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m, Env m)
instEmptyClass loc i env sup xs e s s' = do
  s'' <- lift freshS
  (sup, xs_sup) <- instEmptySup loc sup s s''
  archetype <- lift $ freshEnv xs
  let archetype' = mempty
  xs <- lift $ freshEnv xs
  let xs' = xs_sup <> xs
  _ <- local (\ r -> r { env = xs' <> env, archetype, archetype' }) $ evalExp e s'' s'
  lift $ newVar' (Val.ClassInst i sup $ filterNames xs') <&> (, xs')

instEmptySup
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> S m
  -> S m
  -> EvalT m (Maybe (VarVal m), Env m)
instEmptySup loc sup s s' = case sup of
  Nothing -> lift $ do
    unifyS s s'
    pure (Nothing, mempty)
  Just sup -> do
    (i, env, sup, xs, e) <- lift $ readClass loc sup
    (sup, xs) <- instEmptyClass loc i env sup xs e s s'
    pure (Just sup, xs)

readClass
  :: (MonadAbort Error m, MonadRef m)
  => Loc
  -> VarVal m
  -> VerseT m (Label, Env m, Maybe (VarVal m), Exp.Env L Ident, L (Exp L Ident))
readClass loc = readVar' >=> \ case
  Val.Overloads head tail -> case head of
    Val.Class i env sup xs e -> pure (i, env, sup, xs, e)
    _ -> readClass loc tail
  _ -> abort $ DomainError loc

findClassInst
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Label -> VarVal m -> VerseT m (VarVal m)
findClassInst i var = readVar' var >>= \ case
  Val.ClassInst j sup _
    | i == j -> pure var
    | Just var <- sup -> findClassInst i var
  _ -> empty

invokeIntrinsic
  :: MonadEval m
  => Loc
  -> Intrinsic
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
invokeIntrinsic loc = \ case
  Intrinsic.Less -> liftPrim $ lift . liftOrd (<)
  Intrinsic.LessEqual -> liftPrim $ lift . liftOrd (<=)
  Intrinsic.Greater -> liftPrim $ lift . liftOrd (>)
  Intrinsic.GreaterEqual -> liftPrim $ lift . liftOrd (>=)
  Intrinsic.Plus -> liftPrim $ lift . liftNum (+)
  Intrinsic.PrefixPlus -> liftPrim $ lift . prefixPlus
  Intrinsic.Minus -> liftPrim $ lift . liftNum (-)
  Intrinsic.PrefixMinus -> liftPrim $ lift . prefixMinus
  Intrinsic.Multiply -> liftPrim $ lift . liftNum (*)
  Intrinsic.Divide -> liftPrim $ lift . div'
  Intrinsic.To -> to
  Intrinsic.Int -> liftPrim $ int loc
  Intrinsic.Float -> liftPrim $ float loc
  Intrinsic.Function -> liftPrim $ function loc
  Intrinsic.Query -> liftPrim $ query loc

liftOrd
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => (forall a . Ord a => a -> a -> Bool)
  -> VarVal m -> VerseT m (DomMatch m)
liftOrd f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.AnyRational, AnyNumber) -> domMatch_' $ decide $> var_x
    (Val.Rational x, Val.Rational y) -> domMatch_' $ guard (f x y) $> var_x
    (Val.Rational x, Val.Int y) -> domMatch_' $ guard (f x (fromInteger y)) $> var_x
    (Val.Rational x, Val.Float y) -> domMatch_' $ guard (f (fromRational x) y) $> var_x
    (Val.Rational _, AnyNumber) -> domMatch_' $ decide $> var_x
    (Val.AnyInt, AnyNumber) -> domMatch_' $ decide $> var_x
    (Val.Int x, Val.Rational y) -> domMatch_' $ guard (f (fromInteger x) y) $> var_x
    (Val.Int x, Val.Int y) -> domMatch_' $ guard (f x y) $> var_x
    (Val.Int x, Val.Float y) -> domMatch_' $ guard (f (fromInteger x) y) $> var_x
    (Val.Int _, AnyNumber) -> domMatch_' $ decide $> var_x
    (Val.AnyFloat, AnyNumber) -> domMatch_' $ decide $> var_x
    (Val.Float x, Val.Rational y) -> domMatch_' $ guard (f (toRational x) y) $> var_x
    (Val.Float x, Val.Int y) -> domMatch_' $ guard (f x (fromInteger y)) $> var_x
    (Val.Float x, Val.Float y) -> domMatch_' $ guard (f x y) $> var_x
    (Val.Float _, AnyNumber) -> domMatch_' $ decide $> var_x
    _ -> empty
  _ -> empty

liftNum
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => (forall a . Num a => a -> a -> a)
  -> VarVal m -> VerseT m (DomMatch m)
liftNum f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.AnyRational, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.Rational _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.Int _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyRational, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Rational _, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyRational
    (Val.Rational x, Val.Rational y) -> domMatch_' . newVar' . Val.Rational $ f x y
    (Val.Rational _, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyRational
    (Val.Rational x, Val.Int y) -> domMatch_' . newVar' . Val.Rational $ f x (fromInteger y)
    (Val.Rational _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Rational x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ f (fromRational x) y
    (Val.AnyInt, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyInt, Val.Rational _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyInt, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyInt
    (Val.AnyInt, Val.Int _) -> domMatch_' $ newVar' Val.AnyInt
    (Val.AnyInt, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyInt, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Int _, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyRational
    (Val.Int x, Val.Rational y) -> domMatch_' . newVar' . Val.Rational $ f (fromInteger x) y
    (Val.Int _, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyInt
    (Val.Int x, Val.Int y) -> domMatch_' . newVar' . Val.Int $ f x y
    (Val.Int _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Int x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ f (fromInteger x) y
    (Val.AnyFloat, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Rational _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Int _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float _, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float x, Val.Rational y) -> domMatch_' . newVar' . Val.Float $ f x (fromRational y)
    (Val.Float _, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float x, Val.Int y) -> domMatch_' . newVar' . Val.Float $ f x (fromInteger y)
    (Val.Float _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ f x y
    _ -> empty
  _ -> empty

prefixPlus
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (DomMatch m)
prefixPlus var = readVar' var >>= \ case
  AnyNumber -> domMatch_' $ pure var
  _ -> empty

prefixMinus
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (DomMatch m)
prefixMinus var = readVar' var >>= \ case
  Val.AnyRational -> domMatch_' $ newVar' Val.AnyRational
  Val.Rational x -> domMatch_' $ newVar' (Val.Rational $ negate x)
  Val.AnyInt -> domMatch_' $ newVar' Val.AnyInt
  Val.Int x -> domMatch_' $ newVar' (Val.Int $ negate x)
  Val.AnyFloat -> domMatch_' $ newVar' Val.AnyFloat
  Val.Float x -> domMatch_' $ newVar' (Val.Float $ negate x)
  _ -> empty

div'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m -> VerseT m (DomMatch m)
div' var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.AnyRational, Val.AnyRational) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.AnyRational, Val.Rational 0) -> domMatch_' empty
    (Val.AnyRational, Val.Rational _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.AnyInt) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.AnyRational, Val.Int 0) -> domMatch_' empty
    (Val.AnyRational, Val.Int _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyRational, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyRational, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Rational _, Val.AnyRational) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.Rational _, Val.Rational 0) -> domMatch_' empty
    (Val.Rational x, Val.Rational y) -> domMatch_' $ newVar' . Val.Rational $ x / y
    (Val.Rational _, Val.AnyInt) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.Rational _, Val.Int 0) -> domMatch_' empty
    (Val.Rational x, Val.Int y) -> domMatch_' . newVar' . Val.Rational $ x / fromInteger y
    (Val.Rational _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Rational x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ fromRational x / y
    (Val.AnyInt, Val.AnyRational) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.AnyInt, Val.Rational 0) -> domMatch_' empty
    (Val.AnyInt, Val.Rational _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyInt, Val.AnyInt) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.AnyInt, Val.Int 0) -> domMatch_' empty
    (Val.AnyInt, Val.Int _) -> domMatch_' $ newVar' Val.AnyRational
    (Val.AnyInt, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyInt, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Int _, Val.AnyRational) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.Int _, Val.Rational 0) -> domMatch_' empty
    (Val.Int x, Val.Rational y) -> domMatch_' . newVar' . Val.Rational $ fromInteger x / y
    (Val.Int _, Val.AnyInt) -> domMatch_' $ decide *> newVar' Val.AnyRational
    (Val.Int _, Val.Int 0) -> domMatch_' empty
    (Val.Int x, Val.Int y) -> domMatch_' . newVar' . Val.Rational $ x % y
    (Val.Int _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Int x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ fromInteger x / y
    (Val.AnyFloat, Val.AnyRational) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Rational _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Int _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.AnyFloat, Val.Float _) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float _, Val.AnyRational) -> domMatch_' $ decide *> newVar' Val.AnyFloat
    (Val.Float x, Val.Rational y) -> domMatch_' . newVar' . Val.Float $ x / fromRational y
    (Val.Float _, Val.AnyInt) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float x, Val.Int y) -> domMatch_' . newVar' . Val.Float $ x / fromInteger y
    (Val.Float _, Val.AnyFloat) -> domMatch_' $ newVar' Val.AnyFloat
    (Val.Float x, Val.Float y) -> domMatch_' . newVar' . Val.Float $ x / y
    _ -> empty
  _ -> empty

to
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
to var s s' = lift $ readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Int val1, Int val2) -> domMatch_' $ to' val1 val2 s s'
    _ -> empty
  _ -> empty

to'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => Integer
  -> Integer
  -> S m
  -> S m
  -> VerseT m (VarVal m)
to' val1 val2 s s' = do
  unifyEq s.storeFree s'.storeFree
  _ <- readVar s.choiceFree
  var <- foldr (\ x z -> newVar' (Val.Int x) <|> z) empty [val1 .. val2]
  unifyEq s.choiceFree s'.choiceFree
  pure var

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
  => Loc -> VarVal m -> EvalT m (DomMatch m)
int loc var = domMatch_ $ do
  fork' $ lift (readVar' var) >>= \ case
    Val.Any -> unify' loc var =<< lift (newVar' Val.AnyInt)
    Val.AnyRational -> unify' loc var =<< lift (newVar' Val.AnyInt)
    Val.Rational x | denominator x == 1 -> pure ()
    Val.AnyInt -> pure ()
    Val.Int _ -> pure ()
    _ -> empty
  pure var

float
  :: MonadEval m
  => Loc -> VarVal m -> EvalT m (DomMatch m)
float loc var = domMatch_ $ do
  fork' $ lift (readVar' var) >>= \ case
    Val.Any -> unify' loc var =<< lift (newVar' Val.AnyFloat)
    Val.AnyFloat -> pure ()
    Val.Float _ -> pure ()
    _ -> empty
  pure var

function
  :: MonadEval m
  => Loc -> VarVal m -> EvalT m (DomMatch m)
function loc var = domMatch_ $ do
  fork' $ lift (readVar' var) >>= \ case
    Val.Any -> unify' loc var =<< lift (newVar' Val.AnyOverloads)
    Val.Tuple _ -> pure ()
    Val.Enum {} -> pure ()
    Val.AnyOverloads -> pure ()
    Val.Overloads {} -> pure ()
    _ -> empty
  pure var

query
  :: MonadEval m
  => Loc -> VarVal m -> EvalT m (DomMatch m)
query loc var = domMatch_ $ do
  var' <- lift freshVar'
  unify' loc var =<< lift (newVar' $ Val.Truth var')
  pure var'

liftPrim
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (VarVal m -> EvalT m (DomMatch m))
  -> VarVal m -> S m -> S m -> EvalT m (DomMatch m)
liftPrim f var s s' = f var <&> \ (DomMatch x f) ->
  DomMatch x $ \ x -> f x <* lift (unifyS s s')

readPair
  :: (MonadFix m, MonadRef m, MonadSupply Int m, EqRef (Ref m))
  => VarVal m
  -> VerseT m (Maybe (VarVal m, VarVal m))
readPair var = readVar' var <&> \ case
  Val.Tuple [var1, var2] -> Just (var1, var2)
  _ -> Nothing

evalIdent
  :: MonadEval m
  => L Ident
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalIdent x s s' = lookupNamed (extract x) >>= \ case
  Nothing -> abort $ IdentError (loc x) (extract x)
  Just y -> evalNamed (loc x) y s s'

evalNamed
  :: MonadEval m
  => Loc
  -> VarNamed m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalNamed loc x s s' = case x of
  Val var -> lift $ unifyS s s' $> var
  Ref ref var -> do
    var' <- lift freshVar'
    fork' $ unify' loc var' =<< readRef' loc ref var s s'
    pure var'

evalNamed'
  :: MonadEval m
  => Loc
  -> VarNamed m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalNamed' loc x s s' = case x of
  Val var -> lift $ unifyS s s' $> var
  Ref ref var -> readRef' loc ref var s s'

readRef'
  :: MonadEval m
  => Loc
  -> VarRefVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
readRef' loc ref var s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  _ <- lift $ readVar s.storeFree
  x <- asks (.mode) >>= \ case
    Execution -> lift $ coerce <$> readVarRef ref
    Verification -> do
      i <- lift $ newVar' Val.Any
      lift . readIVar <=< assume' . local (\ r -> r { assumed = True }) $ do
        s <- lift newS
        s' <- lift freshS
        invoke loc var i s s'
  lift $ unifyEq s.storeFree s'.storeFree
  pure x

evalExpList
  :: MonadEval m
  => [L (Exp L Ident)]
  -> S m
  -> S m
  -> EvalT m [VarVal m]
evalExpList xs s s' = case xs of
  [] -> do
    lift $ unifyS s s'
    pure []
  x:xs -> do
    s'' <- lift freshS
    var <- evalExp x s s''
    vars <- evalExpList xs s'' s'
    pure $ var:vars

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
      lift . newVar' . Val.Overloads (Val.Intrinsic x) =<<
      lift freshVar'

filterNames :: IdentMap a -> HashMap Name a
filterNames =
  HashMap.fromList .
  HashMap.foldrWithKey
  \ case
    Ident.Name k -> \ v z -> (k, v) : z
    Ident.Label _ -> \ _ z -> z
  []

lookupNamed :: Ident -> EvalT m (Maybe (Named (VarRef m) (VarVal m)))
lookupNamed x = asks $ \ r -> HashMap.lookup x r.env

localName :: Ident -> Named (VarRef m) (VarVal m) -> EvalT m a -> EvalT m a
localName k v = local $ \ r -> r { env = HashMap.insert k v r.env }

localEnv :: Env m -> EvalT m a -> EvalT m a
localEnv env = local $ \ r ->
  r { env, archetype = mempty, archetype' = mempty }

localNames :: Env m -> EvalT m a -> EvalT m a
localNames env = local $ \ r ->
  r { env = env <> r.env, archetype = mempty, archetype' = mempty }

freshEnv
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Exp.Env L Ident -> VerseT m (Env m)
freshEnv = getAp . HashMap.foldMapWithKey f
  where
    f k = \ case
      Exp.Exists -> Ap $ HashMap.singleton k . Val <$> freshVar'
      Exp.Forall -> Ap $ HashMap.singleton k . Val <$> newVar' Val.Any
      Exp.Var k' -> Ap $ do
        ref <- freshVarRef
        var <- freshVar'
        pure $
          HashMap.singleton k (Ref ref var) <>
          HashMap.singleton (extract k') (Val var)

freshVarRef
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m (VarRef m a)
freshVarRef = newVarRef =<< freshVar

writeVarRef'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => VarRef m a
  -> Var m a
  -> S m
  -> S m
  -> EvalT m ()
writeVarRef' ref x s s' = lift $ do
  unifyEq s.choiceFree s'.choiceFree
  fork $ do
    _ <- readVar s.storeFree
    writeVarRef ref x
    unifyEq s.storeFree s'.storeFree

unify'
  :: MonadEval m
  => Loc -> VarVal m -> VarVal m -> EvalT m ()
unify' loc x y = do
  r <- ask
  lift $ unify
    (\ x y -> runReaderT (match loc x y) r <&> \ (x, m) -> (x, runReaderT m r))
    (coerce x)
    (coerce y)

match
  :: MonadEval m
  => Loc
  -> Val (VarRef m) (VarVal m)
  -> Val (VarRef m) (VarVal m)
  -> EvalT m (Match, EvalT m ())
match loc x y = ask >>= \ r -> case (x, y) of
  (Val.Any, Val.Overloads _ ys)
    | r.assumed -> pure $ (GE,) do
        xs <- lift $ newVar' Val.AnyOverloads
        unify' loc xs ys
    | otherwise -> abort $ UndecidableError loc
  (Val.Any, y)
    | r.assumed -> pure . (GE,) . forVal_ y $ \ y -> do
        x <- lift $ newVar' Val.Any
        unify' loc x y
    | otherwise -> abort $ UndecidableError loc
  (Val.Comparable, Val.Any)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> abort $ UndecidableError loc
  (Val.Comparable, Val.AnyOverloads) -> abort $ UndecidableError loc
  (Val.Comparable, Val.Overloads {}) -> abort $ UndecidableError loc
  (Val.Comparable, y) -> pure . (GE,) . forVal_ y $ \ y -> do
    x <- lift $ newVar' Val.Comparable
    unify' loc x y
  (Val.AnyRational, Val.AnyRational) -> pure (LE, pure ())
  (Val.AnyRational, Val.Rational _) -> pure (GE, pure ())
  (Val.AnyRational, Val.AnyInt) -> pure (GE, pure ())
  (Val.AnyRational, Val.Int _) -> pure (GE, pure ())
  (Val.Rational _, Val.AnyRational) -> pure (LE, pure ())
  (Val.Rational x, Val.Rational y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.Rational x, Val.AnyInt) ->
    guard (denominator x == 1) $> (LE, pure ())
  (Val.Rational x, Val.Int y) ->
    guard (denominator x == 1 && numerator x == y) $> (SEQ, pure ())
  (Val.AnyInt, Val.AnyRational) -> pure (LE, pure ())
  (Val.AnyInt, Val.Rational y) ->
    guard (1 == denominator y) $> (GE, pure ())
  (Val.AnyInt, Val.AnyInt) -> pure (LE, pure ())
  (Val.AnyInt, Val.Int _) -> pure (GE, pure ())
  (Val.Int _, Val.AnyRational) -> pure (LE, pure ())
  (Val.Int x, Val.Rational y) ->
    guard (1 == denominator y && x == numerator y) $> (SEQ, pure ())
  (Val.Int _, Val.AnyInt) -> pure (LE, pure ())
  (Val.Int x, Val.Int y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.AnyFloat, Val.AnyFloat) -> pure (LE, pure ())
  (Val.AnyFloat, Val.Float _) -> pure (GE, pure ())
  (Val.Float _, Val.AnyFloat) -> pure (LE, pure ())
  (Val.Float x, Val.Float y) ->
    guard (eqFloat x y) $> (SEQ, pure ())
  (Val.Truth x, Val.Truth y) -> pure . (SEQ,) $ unify' loc x y
  (Val.Tuple xs, Val.Tuple ys) -> pure . (SEQ,) $ unifyList loc xs ys
  (Val.Enum i xs xs', Val.Enum j ys ys') -> guard (i == j) $> (SEQ,) do
    unifyEnv loc xs ys
    unifyList loc xs' ys'
  (Val.EnumValue i x, Val.EnumValue j y) ->
    guard (i == j && x == y) $> (SEQ, pure ())
  (Val.StructInst i xs, Val.StructInst j ys) -> do
    guard (i == j) $> (SEQ, unifyEnv loc xs ys)
  (Val.ClassInst i x xs, Val.ClassInst j y ys) -> guard (i == j) $> (SEQ,) do
    unifyMaybe loc x y
    unifyEnv loc xs ys
  (Val.AnyOverloads, Val.Any)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> abort $ UndecidableError loc
  (Val.AnyOverloads, Val.Comparable) -> abort $ UndecidableError loc
  (Val.AnyOverloads, Val.AnyOverloads)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> abort $ UndecidableError loc
  (Val.AnyOverloads, Val.Overloads _ ys)
    | r.assumed -> pure . (GE,) $ do
        xs <- lift $ newVar' Val.AnyOverloads
        unify' loc xs ys
    | otherwise -> abort $ UndecidableError loc
  (Val.Overloads _ xs, Val.Any)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.AnyOverloads
        unify' loc xs ys
    | otherwise -> abort $ UndecidableError loc
  (Val.Overloads {}, Val.Comparable) -> abort $ UndecidableError loc
  (Val.Overloads _ xs, Val.AnyOverloads)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.AnyOverloads
        unify' loc xs ys
    | otherwise -> abort $ UndecidableError loc
  (Val.Overloads x xs, Val.Overloads y ys) -> pure $ (SEQ,) do
    asks (.mode) >>= \ case
      Execution -> pure ()
      Verification -> do
        fork' . lift . readIVar <=< verify' . local (\ r -> r { assumed = True }) $ do
          i <- lift $ newVar' Val.Any
          _ <- invokeOverloadDom loc x i
          fails' (invokeOverloadDom loc y i) >>= lift . readIVar >>= \ case
            False -> abort $ FailsError loc
            True -> empty
    zs <- lift freshVar'
    unify' loc xs =<< lift (newVar' $ Val.Overloads y zs)
    unify' loc ys =<< lift (newVar' $ Val.Overloads x zs)
  (x, Val.Any)
    | r.assumed -> pure . (LE,) $ forVal_ x $ \ x -> do
        y <- lift $ newVar' Val.Any
        unify' loc x y
    | otherwise -> abort $ UndecidableError loc
  (x, Val.Comparable) -> pure . (LE,) . forVal_ x $ \ x -> do
    y <- lift $ newVar' Val.Comparable
    unify' loc x y
  _ -> empty

unifyMaybe
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> Maybe (VarVal m)
  -> EvalT m ()
unifyMaybe loc = curry $ \ case
  (Nothing, Nothing) -> pure ()
  (Just x, Just y) -> unify' loc x y
  _ -> empty

unifyList
  :: MonadEval m
  => Loc
  -> [VarVal m]
  -> [VarVal m]
  -> EvalT m ()
unifyList loc = curry $ \ case
  ([], []) -> pure ()
  (x:xs, y:ys) -> unify' loc x y *> unifyList loc xs ys
  _ -> empty

unifyEnv
  :: (MonadEval m, Eq k)
  => Loc
  -> VarEnv k m
  -> VarEnv k m
  -> EvalT m ()
unifyEnv loc xs ys = for_ (HashMap.intersectionWith (,) xs ys) $ \ (x, y) ->
  unifyNamed loc x y

unifyNamed
  :: MonadEval m
  => Loc
  -> VarNamed m
  -> VarNamed m
  -> EvalT m ()
unifyNamed loc = curry $ \ case
  (Val x, Val y) -> unify' loc x y
  (Ref ref_x x, Ref ref_y y) -> do
    guard $ ref_x == ref_y
    unify' loc x y
  _ -> empty

eqFloat :: Double -> Double -> Bool
eqFloat x y = if isNaN x then isNaN y else x == y

getNameEnv :: MonadAbort Error m => Loc -> Val ref a -> m (Val.Env Name ref a)
getNameEnv loc = \ case
  Val.Module _ xs -> pure xs
  Val.Enum _ xs _ -> pure xs
  Val.StructInst _ xs -> pure xs
  Val.ClassInst _ _ xs -> pure xs
  _ -> abort $ DomainError loc

fork'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => ReaderT r (VerseT m) ()
  -> ReaderT r (VerseT m) ()
fork' m = ReaderT $ fork . runReaderT m

newVar'
  :: (MonadRef m, MonadSupply Int m)
  => f (Fix (Compose (Var m) f))
  -> VerseT m (Fix (Compose (Var m) f))
newVar' = fmap coerce . newVar

freshVar'
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m (Fix (Compose (Var m) f))
freshVar' = coerce <$> freshVar

readVar'
  :: MonadRef m
  => Fix (Compose (Var m) f)
  -> VerseT m (f (Fix (Compose (Var m) f)))
readVar' = readVar . coerce

one'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (IVar m a)
one' m = ReaderT $ one . runReaderT m

if''
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a -> (a -> EvalT m b)
  -> EvalT m b
  -> EvalT m (IVar m b)
if'' p t e = ReaderT $ \ r ->
  if' (runReaderT p r) (\ x -> runReaderT (t x) r) (runReaderT e r)

all'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (IVar m [a])
all' m = ReaderT $ all . runReaderT m

for'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> (a -> EvalT m b)
  -> EvalT m (IVar m [b])
for' m f = ReaderT $ \ r -> for (runReaderT m r) (\ x -> runReaderT (f x) r)

verify'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => EvalT m ()
  -> EvalT m (IVar m ())
verify' m = ReaderT $ verify . runReaderT m

assume'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (IVar m a)
assume' m = ReaderT $ assume . runReaderT m

succeeds'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (IVar m (Maybe a))
succeeds' m = ReaderT $ succeeds . runReaderT m

fails'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => EvalT m a
  -> EvalT m (IVar m Bool)
fails' m = ReaderT $ fails . runReaderT m

decides'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (IVar m (Maybe a))
decides' m = ReaderT $ decides . runReaderT m

whenM :: Monad m => m Bool -> m () -> m ()
whenM m n = m >>= \ case
  False -> pure ()
  True -> n
