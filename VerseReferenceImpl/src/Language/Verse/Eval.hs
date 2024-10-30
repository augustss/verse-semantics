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
import Control.Monad.Extras
  ( Monad (..)
  , (=<<)
  , (>=>)
  , (<=<)
  , guard
  , when
  , whenM
  )
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Reader
import Control.Monad.Supply
import Control.Monad.Verse
import Control.Monad.Wrong

import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty (NonEmpty(..), (<|))

import Data.List qualified as List
import Data.Bool
import Data.Coerce
import Data.Eq
import Data.Foldable (foldr, foldrM, for_)
import Data.Function
import Data.Functor ((<&>), void)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Int
import Data.List (zip, map)
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Ratio
import Data.Tuple

import Language.Verse.Access
import Language.Verse.Contract (Contract)
import Language.Verse.Contract qualified as Contract
import Language.Verse.Desugar.Exp
  ( Exp
  , pattern (:*>:)
  , pattern (:>>:)
  , pattern (:=:)
  , pattern (:.:)
  , pattern (:|:)
  )
import Language.Verse.Desugar.Exp qualified as Exp
import Language.Verse.Effect.Split qualified as Split (Effect)
import Language.Verse.Effect.Split qualified as Effect
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Intrinsic (Intrinsic)
import Language.Verse.Intrinsic qualified as Intrinsic
import Language.Verse.Label
import Language.Verse.Loc (Loc, L (..), loc)
import Language.Verse.Mode
import Language.Verse.Path
import Language.Verse.SimpleName
import Language.Verse.Val
  ( FrozenVal
  , List (..)
  , Named (..)
  , RefVarVal
  , Val
  , VarEnv
  , VarList
  , VarNamed
  , VarVal
  , forVal_
  )
import Language.Verse.Val qualified as Val

import Prelude
  ( Double
  , Integer
  , Num (..)
  , Enum (..)
  , Fractional (..)
  , fromRational
  , isNaN
  , toRational
  )

type EvalT m = ReaderT (R m) (VerseT m)

data R m = R
  { mode :: !Mode
  , env :: !(Env m)
  , top :: !(Env m)
  -- stack of all currently active scopes, head is innermost scope
  , scopes :: NonEmpty Val.Scope
  , assumed :: !Bool
  , sign :: !Val.Sign
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

runEvalT
  :: (MonadRef m, MonadSupply Int m)
  => EvalT m a
  -> Mode
  -> NonEmpty Val.Scope
  -> VerseT m a
runEvalT m mode scopes = runReaderT m R {..}
  where
    env = mempty
    top = env
    assumed = False
    sign = True
    archetype = mempty
    archetype' = mempty

type MonadEval m =
  ( MonadWrong Error m
  , MonadFix m
  , MonadRef m
  , MonadSupply Label m
  )

eval :: MonadEval m => Mode -> L (Exp L Ident) -> VerseT m FrozenVal
eval mode e = do
  s <- newS
  s' <- freshS
  i <- supply
  let scopes = NonEmpty.singleton $ Val.Scope i [] i
  freeze' =<< runEvalT (evalExp e s s') mode scopes

evalExp :: MonadEval m => L (Exp L Ident) -> S m -> S m -> EvalT m (VarVal m)
evalExp e = case extract e of
  e1 :*>: e2 -> \ s s' -> do
    s'' <- lift freshS
    _ <- evalExp e1 s s''
    evalExp e2 s'' s'
  e1 :>>: e2 -> \ s s' -> do
    var <- lift freshVar'
    fork' $ do
      s'' <- lift freshS
      join'' . void $ evalExp e1 s s''
      unify' (loc e) var =<< evalExp e2 s'' s'
    pure var
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
  Exp.Check Effect.Fails e ->
    evalFails e
  Exp.Check Effect.Succeeds e' ->
    evalSucceeds (loc e) e'
  Exp.Check Effect.Decides e' ->
    evalDecides (loc e) e'
  Exp.Assume eff e' ->
    evalAssume (loc e) eff e'
  Exp.Module i xs e -> \ s s' ->
    pushModuleScope i $ do
      xs <- freshEnv xs
      _ <- localNames xs $ evalExp e s s'
      lift . newVar' . Val.Module i $ filterNames xs
  Exp.Enum i xs -> \ s s' -> do
    scopes <- getScopes
    lift $ do
      let foldrM' xs f = foldrM f mempty xs
      (env, xs) <- foldrM' xs $ \ x (env, xs) ->
        newVar' (Val.EnumValue i x) <&> \ var ->
        (HashMap.insert x (accessScope Public scopes, Val var) env, var:xs)
      unifyS s s'
      newVar' $ Val.Enum i env xs
  Exp.Struct expLabel members exp -> \ s s' -> ask >>= \ R {..} -> lift $ do
    unifyS s s'
    label <- supply
    newVar' $ Val.Struct Val.MkStruct {..}
  Exp.Class expLabel super members exp -> \ s s' -> do
    label <- supply
    R {..} <- ask
    super <- case super of
      Nothing -> lift $ Nothing <$ unifyS s s'
      Just e_super -> Just <$> evalExp e_super s s'
    lift . newVar' $ Val.Class Val.MkClass {..}
  Exp.Inst e1 xs e2 ->
    evalInst (loc e) e1 xs e2
  Exp.IfThenElse xs p t e ->
    evalIfThenElse (loc e) xs p t e
  Exp.ForDo xs e1 e2 ->
    evalForDo (loc e) xs e1 e2
  Exp.Def access Exp.Exists (extract -> x) e -> \ s s' -> do
    var <- lift freshVar'
    localName access x (Val var) $ evalExp e s s'
  Exp.Def access Exp.Forall x e -> \ s s' -> do
    var <- lift $ newVar' Val.SomeAny
    localName access (extract x) (Val var) $ evalExp e s s'
  Exp.Def access Exp.Var (extract -> x) e -> \ s s' -> do
    var <- lift freshVar'
    localName access x (Ref var) $ evalExp e s s'
  Exp.Alloc x e1 e2 ->
    evalAlloc (loc e) x e1 e2
  Exp.Set x e -> \ s s' -> lookupNamed (extract x) >>= \ case
    Nothing -> wrong $ IdentError (loc x) (extract x)
    Just (Val _) -> wrong $ RefError $ loc e
    Just (Ref var) -> do
      s'' <- lift freshS
      var_e <- evalExp e s s''
      fork' $ lift (readVar' var) >>= \ case
        Val.Ptr ref var -> writeRef' (loc x) ref var var_e s'' s'
        _ -> wrong . RefError $ loc e
      pure var_e
  Exp.Lam param exp -> \ s s' -> ask >>= \ R {..} -> lift $ do
    unifyS s s'
    newVar' $ Val.Lam Val.MkLam {..}
  Exp.OLam f params domain range -> \ s s' -> ask >>= \ R {..} -> do
    var_f <- evalExp f s s'
    lift . newVar' $ Val.OLam Val.MkOLam {..} var_f
  Exp.Intrinsic x -> \ s s' -> lift $ do
    unifyS s s'
    newVar' . Val.Intrinsic x =<< freshVar'
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
  Exp.Char x -> \ s s' -> lift $ do
    unifyS s s'
    newVar' $ Val.Char x
  Exp.Char32 x -> \ s s' -> lift $ do
    unifyS s s'
    newVar' $ Val.Char32 x
  Exp.Name x ->
    evalIdent $ x <$ e
  Exp.QualName x y ->
    evalQualName (loc e) x y
  Exp.Path (Path label pathIdents) -> \ s s' -> lift $ do
    unifyS s s'
    -- Ignore nested labels for now
    newVar' . Val.Path $ label : map snd pathIdents
  Exp.IfArchetypeName x y e1 e2 -> \ s s' -> do
    scopes <- getScopes
    asks archetype <&> lookupEnv scopes (extract x) >>= \ case
      Nothing -> evalExp e2 s s'
      Just x' -> localName Private (extract y) x' $ evalExp e1 s s'
  Exp.ArchetypeName x -> \ s s' -> do
    r <- ask
    case lookupEnv r.scopes x r.archetype' <|> lookupEnv r.scopes x r.env of
      Nothing -> wrong $ IdentError (loc e) x
      Just (Ref _) -> wrong . ValError $ loc e
      Just (Val var) -> lift $ unifyS s s' $> var
  Exp.TopLevel xs e -> \ s s' -> do
    xs <- freshEnv xs
    local ( \ r -> let env = xs <> r.env in r { top = env, env }) $ evalExp e s s'
  Exp.Domain e -> \ s s' ->
    local (\ r -> r { sign = not r.sign }) $ evalExp e s s'

evalDot
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> SimpleName
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalDot loc e x s s' = do
  s'' <- lift freshS
  var <- evalExp e s s''
  dotM loc var x s'' s'

dots1M'
  :: MonadEval m
  => Loc
  -> VarVal m
  -> [SimpleName]
  -> SimpleName
  -> S m
  -> S m
  -> EvalT m (VarVal m)
dots1M' loc var xs x s s' = case xs of
  [] -> dotM' loc var x s s'
  y:xs -> do
    s'' <- lift freshS
    var <- dotM' loc var y s s''
    dots1M' loc var xs x s'' s'

dotM
  :: MonadEval m
  => Loc
  -> VarVal m
  -> SimpleName
  -> S m
  -> S m
  -> EvalT m (VarVal m)
dotM loc var x s s' = do
  var' <- lift freshVar'
  fork' $ unify' loc var' =<< dotM' loc var x s s'
  pure var'

dotM'
  :: MonadEval m
  => Loc
  -> VarVal m
  -> SimpleName
  -> S m
  -> S m
  -> EvalT m (VarVal m)
dotM' loc var x s s' = do
  scopes <- getScopes
  lift (readVar' var) >>= getNameEnv loc <&> lookupEnv scopes x >>= \ case
    Nothing -> wrong $ NameError loc x
    Just x -> evalNamed' loc x s s'

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
    var' <- one' do
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
    vars <- all'' do
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
  fork' $ if''
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
  -> Exp.Env Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalIfThenElse loc xs p t e s s' = do
  var <- lift freshVar'
  fork' do
    var' <- if''
      do
        xs <- freshEnv xs
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
  -> Exp.Env Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalForDo loc xs e1 e2 s s' = do
  var <- lift freshVar'
  fork' $ do
    s'' <- lift freshS
    vars <- for''
      do
        xs <- freshEnv xs
        choiceFree <- lift $ newVar ChoiceFree
        s' <- lift freshS
        _ <- localNames xs $ evalExp e1 s { choiceFree } s'
        pure xs
      do
        \ xs -> localNames xs $ evalExp e2 s s''
    lift $ unifyS s'' s'
    unify' loc var =<< lift (newVar' $ Val.Tuple vars)
  pure var

evalAlloc
  :: MonadEval m
  => Loc
  -> L Ident
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalAlloc loc' x e1 e2 s s' = do
  r <- ask
  case lookupEnv r.scopes (extract x) r.archetype' <|>
       lookupEnv r.scopes (extract x) r.env of
    Nothing -> wrong $ IdentError (loc x) (extract x)
    Just (Val _) -> wrong . RefError $ loc x
    Just (Ref var) -> do
      s'' <- lift freshS
      var1 <- evalExp e1 s s''
      s''' <- lift freshS
      var2 <- evalExp e2 s'' s'''
      var' <- lift freshVar'
      fork' do
        join'' $ unify' loc' var' =<< invoke loc' var1 var2 s''' s'
        ref <- lift . newVerseRef $ coerce var'
        unify' loc' var =<< lift (newVar' $ Val.Ptr ref var1)
      pure var'

evalVerify
  :: MonadEval m
  => L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalVerify e s s' = do
  whenM (asks $ not . (.assumed)) $ do
    lift $ unifyS s s'
    fork' $ verify' do
      s <- lift newS
      s' <- lift freshS
      void . local (\ r -> r { assumed = True }) $ evalExp e s s'
  lift . newVar' $ Val.Tuple []

evalFails
  :: MonadEval m
  => L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalFails e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  fork' $ do
    choiceFree <- lift $ newVar ChoiceFree
    s' <- lift freshS
    _ <- local (\ r -> r { assumed = False }) $ evalExp e s { choiceFree } s'
    wrong . FailsError $ loc e
  lift freshVar'

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
    >>= \ case
      Nothing -> wrong . SucceedsError $ loc e
      Just var' -> do
        lift $ unifyEq s.storeFree s'.storeFree
        unify' loc' var var'
  pure var

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
    >>= \ case
      Nothing -> wrong . DecidesError $ loc e
      Just var' -> do
        lift $ unifyEq s.storeFree s'.storeFree
        unify' loc' var var'
  pure var

evalAssume
  :: MonadEval m
  => Loc
  -> Split.Effect
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalAssume loc eff e s s' = case eff of
  Effect.Fails -> empty
  Effect.Succeeds -> evalAssume' loc e s s'
  Effect.Decides -> lift decide *> evalAssume' loc e s s'

evalAssume'
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalAssume' loc e s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  var <- lift freshVar'
  fork' $ do
    var' <- assume' do
      choiceFree <- lift $ newVar ChoiceFree
      s' <- lift freshS
      local (\ r -> r { assumed = True }) $ evalExp e s { choiceFree } s'
    lift $ unifyEq s.storeFree s'.storeFree
    unify' loc var var'
  pure var

evalInst
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> Exp.Env Ident
  -> L (Exp L Ident)
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalInst loc e1 xs e2 s s' = do
  s'' <- lift freshS
  var1 <- evalExp e1 s s''
  s''' <- lift freshS
  xs <- freshEnv xs
  _ <- localNames xs $ evalExp e2 s'' s'''
  var <- lift freshVar'
  fork' $ lift (readVar' var1) >>= \ case
    Val.Struct x -> unify' loc var =<< instStruct x xs s''' s'
    Val.Class x -> unify' loc var =<< instClass loc x xs s''' s'
    _ -> wrong $ InstError loc
  pure var

instStruct
  :: MonadEval m
  => Val.Struct (VarVal m)
  -> Archetype m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
instStruct Val.MkStruct {..} archetype s s' =
  localScopes (addScope expLabel scopes) $ do
    archetype' <- freshEnv members
    _ <- local (\ r -> r { env = archetype' <> env, archetype, archetype' }) $
      evalExp exp s s'
    let members = filterNames archetype'
    lift . newVar' $ Val.StructInst Val.MkStructInst {..}

instClass
  :: MonadEval m
  => Loc
  -> Val.Class (VarVal m)
  -> Archetype m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
instClass loc x archetype s s' = do
    (var, _, _, initClass) <- allocClass loc x
    initClass archetype s s'
    pure var

allocClass
  :: MonadEval m
  => Loc
  -> Val.Class (VarVal m)
  -> EvalT m (VarVal m, [Label], Env m, Archetype m -> S m -> S m -> EvalT m ())
allocClass loc Val.MkClass {..} = do
   (super, super_labels, vars_super, initSuper) <- allocSuper loc super
   let scopes' = addScopeWithSuper expLabel super_labels scopes
   localScopes scopes' $ do
     archetype' <- freshEnv members
     let
       vars = vars_super <> archetype'
       initClass archetype s s' = localScopes scopes' $ do
         s'' <- lift freshS
         _ <- local (\ r -> r { env = vars <> env, archetype, archetype'}) $
           evalExp exp s s''
         initSuper (archetype' <> archetype) s'' s'
     let members = filterNames vars
     lift $ newVar' (Val.ClassInst Val.MkClassInst {..}) <&>
       (, expLabel:super_labels, vars, initClass)

allocSuper
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> EvalT m (Maybe (VarVal m), [Label], Env m, Archetype m -> S m -> S m -> EvalT m ())
allocSuper loc super = case super of
  Nothing -> pure (Nothing, [], mempty, \ _ s s' -> lift $ unifyS s s')
  Just super -> do
    x <- lift $ readClass loc super
    (super, labels, xs, initSuper) <- allocClass loc x
    pure (Just super, labels, xs, initSuper)

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
  fork' $ unify' loc var =<< invoke' loc var1 var2 s s'
  pure var

invoke'
  :: MonadEval m
  => Loc
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invoke' loc var1 var2 s s' = lift (readVar' var1) >>= \ case
  Val.Tuple xs -> do
    lift $ unifyEq s.storeFree s'.storeFree
    _ <- lift $ readVar s.choiceFree
    var <- invokeTuple loc xs var2
    lift $ unifyEq s.choiceFree s'.choiceFree
    pure var
  Val.Truth x -> do
    lift $ unifyEq s.storeFree s'.storeFree
    lift $ unifyEq s.choiceFree s'.choiceFree
    unify' loc x var2
    pure var2
  Val.Enum _ _ xs -> do
    lift $ unifyEq s.storeFree s'.storeFree
    _ <- lift $ readVar s.choiceFree
    var <- invokeEnum loc xs var2
    lift $ unifyEq s.choiceFree s'.choiceFree
    pure var
  Val.Lam x -> invokeLam x var2 s s'
  Val.OLam x xs -> invokeOLam loc x xs var2 s s'
  Val.Struct x -> invokeStruct loc x var2 s s'
  Val.Class x -> invokeClass loc x var2 s s'
  Val.Intrinsic x xs -> invokeIntrinsic loc x xs var2 s s'
  Val.Type assumed sign xs -> asks (.sign) <&> (== sign) >>= \ case
    True -> invokeNegList loc assumed xs var2 s s'
    False -> invokePosList loc xs var2 s s'
  Val.SomeFunction -> wrong $ UnknownInvokeError loc
  _ -> wrong $ InvokeError loc

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

invokeLam
  :: MonadEval m
  => Val.Lam (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeLam Val.MkLam {..} arg s s' =
  localScopes scopes .
  localSign sign .
  localEnv env .
  localName Private param (Val arg) $ evalExp exp s s'

invokeOLam
  :: MonadEval m
  => Loc
  -> Val.OLam (VarVal m)
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeOLam loc x xs arg s s' =
  if''
  do
    invokeOLamDom loc x arg s s'
  do
    runDomMatch
  do
    lift (readVar' xs) >>= \ case
      Val.OLam x xs -> invokeOLam loc x xs arg s s'
      Val.Intrinsic x xs -> invokeIntrinsic loc x xs arg s s'
      Val.SomeFunction -> wrong $ UnknownInvokeError loc
      _ -> wrong $ InvokeError loc

invokeOLamDom_
  :: MonadEval m
  => Loc
  -> Val.OLam (VarVal m)
  -> VarVal m
  -> EvalT m ()
invokeOLamDom_ loc x arg = do
  s <- lift newS
  s' <- lift freshS
  void $ invokeOLamDom loc x arg s s'

data DomMatch m = forall a . Freshenable a m => DomMatch
  a
  (a -> EvalT m (VarVal m))

runDomMatch :: DomMatch m -> EvalT m (VarVal m)
runDomMatch (DomMatch x f) = f x

anyDom :: Monad m => EvalT m (VarVal m) -> EvalT m (DomMatch m)
anyDom m = pure $ DomMatch () $ \ () -> m

anyDom' :: Monad m => VerseT m (VarVal m) -> VerseT m (DomMatch m)
anyDom' m = pure $ DomMatch () $ \ () -> lift m

instance Monad m => Freshenable (DomMatch m) m where
  freshen (DomMatch x f) = freshen x <&> \ x -> DomMatch x f

invokeOLamDom
  :: MonadEval m
  => Loc
  -> Val.OLam (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
invokeOLamDom loc Val.MkOLam {..} arg s s' =
  localScopes scopes .
  localSign sign $ do
    params <- freshEnv params
    choiceFree <- lift $ newVar ChoiceFree
    s'' <- lift freshS
    unify' loc arg <=<
      localEnv (params <> env) $
      evalExp domain s { choiceFree } s''
    pure . DomMatch params $ \ params ->
      localScopes scopes .
      localSign sign .
      localEnv (params <> env) $
      evalExp range s s'

invokeStruct
  :: MonadEval m
  => Loc
  -> Val.Struct (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeStruct loc x@Val.MkStruct {..} arg s s' = do
  localScopes (addScope expLabel scopes) . fork' $ lift (readVar' arg) >>= \ case
    Val.StructInst y | x.expLabel == y.expLabel -> when (x.label /= y.label) $ do
      archetype <- freshEnv members
      let archetype' = mempty
      xs <- freshEnv members
      _ <- local (\ r -> r { env = xs <> env, archetype, archetype' }) $
        evalExp exp s s'
      let members = filterNames xs
      unify' loc arg <=< lift . newVar' $ Val.StructInst Val.MkStructInst {..}
    _ -> empty
  pure arg

invokeClass
  :: MonadEval m
  => Loc
  -> Val.Class (VarVal m)
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeClass loc x@Val.MkClass {..} arg s s' = do
  localScopes (addScope expLabel scopes) . fork' $ do
    (inst_y, y) <- lift (findClassInst expLabel arg)
    when (x.label /= y.label) $ do
      (inst_x, _) <- instEmptyClass loc x s s'
      unify' loc inst_x inst_y
  pure arg

instEmptyClass
  :: MonadEval m
  => Loc
  -> Val.Class (VarVal m)
  -> S m
  -> S m
  -> EvalT m (VarVal m, Env m)
instEmptyClass loc Val.MkClass {..} s s' = do
  s'' <- lift freshS
  (super, xs_super) <- instEmptySuper loc super s s''
  archetype <- freshEnv members
  let archetype' = mempty
  xs <- freshEnv members
  let xs' = xs_super <> xs
  _ <- local (\ r -> r { env = xs' <> env, archetype, archetype' }) $
    evalExp exp s'' s'
  let members = filterNames xs'
  lift $ newVar' (Val.ClassInst Val.MkClassInst {..}) <&> (, xs')

instEmptySuper
  :: MonadEval m
  => Loc
  -> Maybe (VarVal m)
  -> S m
  -> S m
  -> EvalT m (Maybe (VarVal m), Env m)
instEmptySuper loc super s s' = case super of
  Nothing -> lift $ do
    unifyS s s'
    pure (Nothing, mempty)
  Just super -> do
    x <- lift $ readClass loc super
    (super, xs) <- instEmptyClass loc x s s'
    pure (Just super, xs)

readClass
  :: (MonadWrong Error m, MonadRef m)
  => Loc
  -> VarVal m
  -> VerseT m (Val.Class (VarVal m))
readClass loc = readVar' >=> \ case
  Val.Class x -> pure x
  _ -> wrong $ ClassError loc

findClassInst
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Label -> VarVal m -> VerseT m (VarVal m, Val.ClassInst (VarVal m))
findClassInst i var = readVar' var >>= \ case
  Val.ClassInst x
    | x.expLabel == i -> pure (var, x)
    | Just super <- x.super -> findClassInst i super
  _ -> empty

invokeIntrinsic
  :: MonadEval m
  => Loc
  -> Intrinsic
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeIntrinsic loc x xs arg s s' = if''
  do
    invokeIntrinsicDom loc x arg s s'
  do
    runDomMatch
  do
    lift (readVar' xs) >>= \ case
      Val.OLam x xs -> invokeOLam loc x xs arg s s'
      Val.Intrinsic x xs -> invokeIntrinsic loc x xs arg s s'
      Val.SomeFunction -> wrong $ UnknownInvokeError loc
      _ -> wrong $ InvokeError loc

invokeIntrinsicDom_
  :: MonadEval m
  => Loc
  -> Intrinsic
  -> VarVal m
  -> EvalT m ()
invokeIntrinsicDom_ loc x arg = do
  s <- lift newS
  s' <- lift freshS
  void $ invokeIntrinsicDom loc x arg s s'

invokeIntrinsicDom
  :: MonadEval m
  => Loc
  -> Intrinsic
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
invokeIntrinsicDom loc = \ case
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
  Intrinsic.Any -> liftPrim $ lift . any
  Intrinsic.Int -> liftPrim $ int loc
  Intrinsic.Rational -> liftPrim $ rational loc
  Intrinsic.Float -> liftPrim $ float loc
  Intrinsic.Char -> liftPrim $ char loc
  Intrinsic.Char32 -> liftPrim $ char32 loc
  Intrinsic.Function -> liftPrim $ function loc
  Intrinsic.Type -> liftPrim $ type' loc
  Intrinsic.Query -> liftPrim $ query loc

invokeContract
  :: MonadEval m
  => Loc
  -> Contract
  -> VarVal m
  -> EvalT m ()
invokeContract loc = \ case
  Contract.Any -> lift . anyC
  Contract.Rational -> rationalC loc
  Contract.Int -> intC loc
  Contract.Float -> floatC loc
  Contract.Char -> charC loc
  Contract.Char32 -> char32C loc
  Contract.Function -> functionC loc

liftOrd
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (forall a . Ord a => a -> a -> Bool)
  -> VarVal m -> VerseT m (DomMatch m)
liftOrd f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.SomeRational, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.Rational x, Val.Rational y) -> anyDom' $ guard (f x y) $> var_x
    (Val.Rational x, Val.Int y) -> anyDom' $ guard (f x (fromInteger y)) $> var_x
    (Val.Rational x, Val.Float y) -> anyDom' $ guard (f (fromRational x) y) $> var_x
    (Val.Rational _, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.SomeInt, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.Int x, Val.Rational y) -> anyDom' $ guard (f (fromInteger x) y) $> var_x
    (Val.Int x, Val.Int y) -> anyDom' $ guard (f x y) $> var_x
    (Val.Int x, Val.Float y) -> anyDom' $ guard (f (fromInteger x) y) $> var_x
    (Val.Int _, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.SomeFloat, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.Float x, Val.Rational y) -> anyDom' $ guard (f (toRational x) y) $> var_x
    (Val.Float x, Val.Int y) -> anyDom' $ guard (f x (fromInteger y)) $> var_x
    (Val.Float x, Val.Float y) -> anyDom' $ guard (f x y) $> var_x
    (Val.Float _, SomeNumber) -> anyDom' $ decide $> var_x
    (Val.Char x, Val.Char y) -> anyDom' $ guard (f x y) $> var_x
    (Val.Char _, Val.SomeChar) -> anyDom' $ decide $> var_x
    (Val.SomeChar, Val.Char _) -> anyDom' $ decide $> var_x
    (Val.Char32 x, Val.Char32 y) -> anyDom' $ guard (f x y) $> var_x
    (Val.Char32 _, Val.SomeChar32) -> anyDom' $ decide $> var_x
    (Val.SomeChar32, Val.Char32 _) -> anyDom' $ decide $> var_x
    _ -> empty
  _ -> empty

liftNum
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (forall a . Num a => a -> a -> a)
  -> VarVal m -> VerseT m (DomMatch m)
liftNum f var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.SomeRational, Val.SomeRational) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.Rational _) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.SomeInt) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.Int _) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeRational, Val.Float _) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Rational _, Val.SomeRational) -> anyDom' $ newVar' Val.SomeRational
    (Val.Rational x, Val.Rational y) -> anyDom' . newVar' . Val.Rational $ f x y
    (Val.Rational _, Val.SomeInt) -> anyDom' $ newVar' Val.SomeRational
    (Val.Rational x, Val.Int y) -> anyDom' . newVar' . Val.Rational $ f x (fromInteger y)
    (Val.Rational _, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Rational x, Val.Float y) -> anyDom' . newVar' . Val.Float $ f (fromRational x) y
    (Val.SomeInt, Val.SomeRational) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeInt, Val.Rational _) -> anyDom' $ newVar' Val.SomeRational
    (Val.SomeInt, Val.SomeInt) -> anyDom' $ newVar' Val.SomeInt
    (Val.SomeInt, Val.Int _) -> anyDom' $ newVar' Val.SomeInt
    (Val.SomeInt, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeInt, Val.Float _) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Int _, Val.SomeRational) -> anyDom' $ newVar' Val.SomeRational
    (Val.Int x, Val.Rational y) -> anyDom' . newVar' . Val.Rational $ f (fromInteger x) y
    (Val.Int _, Val.SomeInt) -> anyDom' $ newVar' Val.SomeInt
    (Val.Int x, Val.Int y) -> anyDom' . newVar' . Val.Int $ f x y
    (Val.Int _, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Int x, Val.Float y) -> anyDom' . newVar' . Val.Float $ f (fromInteger x) y
    (Val.SomeFloat, Val.SomeRational) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Rational _) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.SomeInt) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Int _) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Float _) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Float _, Val.SomeRational) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Float x, Val.Rational y) -> anyDom' . newVar' . Val.Float $ f x (fromRational y)
    (Val.Float _, Val.SomeInt) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Float x, Val.Int y) -> anyDom' . newVar' . Val.Float $ f x (fromInteger y)
    (Val.Float _, Val.SomeFloat) -> anyDom' $ newVar' Val.SomeFloat
    (Val.Float x, Val.Float y) -> anyDom' . newVar' . Val.Float $ f x y
    _ -> empty
  _ -> empty

prefixPlus
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (DomMatch m)
prefixPlus var = readVar' var >>= \ case
  SomeNumber -> anyDom' $ pure var
  _ -> empty

prefixMinus
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (DomMatch m)
prefixMinus var = readVar' var >>= \ case
  Val.SomeRational -> anyDom' $ newVar' Val.SomeRational
  Val.Rational x -> anyDom' $ newVar' (Val.Rational $ negate x)
  Val.SomeInt -> anyDom' $ newVar' Val.SomeInt
  Val.Int x -> anyDom' $ newVar' (Val.Int $ negate x)
  Val.SomeFloat -> anyDom' $ newVar' Val.SomeFloat
  Val.Float x -> anyDom' $ newVar' (Val.Float $ negate x)
  _ -> empty

div'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m -> VerseT m (DomMatch m)
div' var = readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Val.SomeRational, Val.SomeRational) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.SomeRational, Val.Rational 0) ->
      anyDom' empty
    (Val.SomeRational, Val.Rational _) ->
      anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.SomeInt) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.SomeRational, Val.Int 0) ->
      anyDom' empty
    (Val.SomeRational, Val.Int _) ->
      anyDom' $ newVar' Val.SomeRational
    (Val.SomeRational, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeRational, Val.Float _) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Rational _, Val.SomeRational) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.Rational _, Val.Rational 0) ->
      anyDom' empty
    (Val.Rational x, Val.Rational y) ->
      anyDom' $ newVar' . Val.Rational $ x / y
    (Val.Rational _, Val.SomeInt) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.Rational _, Val.Int 0) ->
      anyDom' empty
    (Val.Rational x, Val.Int y) ->
      anyDom' . newVar' . Val.Rational $ x / fromInteger y
    (Val.Rational _, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Rational x, Val.Float y) ->
      anyDom' . newVar' . Val.Float $ fromRational x / y
    (Val.SomeInt, Val.SomeRational) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.SomeInt, Val.Rational 0) ->
      anyDom' empty
    (Val.SomeInt, Val.Rational _) ->
      anyDom' $ newVar' Val.SomeRational
    (Val.SomeInt, Val.SomeInt) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.SomeInt, Val.Int 0) ->
      anyDom' empty
    (Val.SomeInt, Val.Int _) ->
      anyDom' $ newVar' Val.SomeRational
    (Val.SomeInt, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeInt, Val.Float _) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Int _, Val.SomeRational) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.Int _, Val.Rational 0) ->
      anyDom' empty
    (Val.Int x, Val.Rational y) ->
      anyDom' . newVar' . Val.Rational $ fromInteger x / y
    (Val.Int _, Val.SomeInt) ->
      anyDom' $ decide *> newVar' Val.SomeRational
    (Val.Int _, Val.Int 0) ->
      anyDom' empty
    (Val.Int x, Val.Int y) ->
      anyDom' . newVar' . Val.Rational $ x % y
    (Val.Int _, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Int x, Val.Float y) ->
      anyDom' . newVar' . Val.Float $ fromInteger x / y
    (Val.SomeFloat, Val.SomeRational) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Rational _) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.SomeInt) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Int _) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.SomeFloat, Val.Float _) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Float _, Val.SomeRational) ->
      anyDom' $ decide *> newVar' Val.SomeFloat
    (Val.Float x, Val.Rational y) ->
      anyDom' . newVar' . Val.Float $ x / fromRational y
    (Val.Float _, Val.SomeInt) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Float x, Val.Int y) ->
      anyDom' . newVar' . Val.Float $ x / fromInteger y
    (Val.Float _, Val.SomeFloat) ->
      anyDom' $ newVar' Val.SomeFloat
    (Val.Float x, Val.Float y) ->
      anyDom' . newVar' . Val.Float $ x / y
    _ -> empty
  _ -> empty

to
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m
  -> S m
  -> S m
  -> EvalT m (DomMatch m)
to var s s' = lift $ readPair var >>= \ case
  Just (var_x, var_y) -> (,) <$> readVar' var_x <*> readVar' var_y >>= \ case
    (Int val1, Int val2) ->
      anyDom' $ to' Val.Int val1 val2 s s'
    (Val.Char val1, Val.Char val2) ->
      anyDom' $ to' Val.Char val1 val2 s s'
    (Val.Char32 val1, Val.Char32 val2) ->
      anyDom' $ to' Val.Char32 val1 val2 s s'
    _ -> empty
  _ -> empty

to'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Enum a)
  => (a -> Val (VerseRef m) (VarList m) (VarVal m))
  -> a
  -> a
  -> S m
  -> S m
  -> VerseT m (VarVal m)
to' f val1 val2 s s' = do
  unifyEq s.storeFree s'.storeFree
  _ <- readVar s.choiceFree
  var <- foldr (\ x z -> newVar' (f x) <|> z) empty [val1 .. val2]
  unifyEq s.choiceFree s'.choiceFree
  pure var

pattern SomeNumber :: Val f a b
pattern SomeNumber <- (number -> True)

number :: Val f a b -> Bool
number = \ case
  Val.SomeRational -> True
  Val.Rational _ -> True
  Val.SomeInt -> True
  Val.Int _ -> True
  Val.SomeFloat -> True
  Val.Float _ -> True
  _ -> False

pattern Int :: Integer -> Val f a b
pattern Int x <- (getInt -> Just x)

getInt :: Val f a b -> Maybe Integer
getInt = \ case
  Val.Rational x | denominator x == 1 -> pure $ numerator x
  Val.Int x -> pure x
  _ -> empty

any :: MonadEval m => VarVal m -> VerseT m (DomMatch m)
any var = anyDom' $ do
  fork $ anyC var
  pure var

anyC :: MonadRef m => VarVal m -> VerseT m ()
anyC var = void $ readVar' var

rational :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
rational loc var = anyDom $ do
  fork' $ rationalC loc var
  pure var

rationalC :: MonadEval m => Loc -> VarVal m -> EvalT m ()
rationalC loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeRational)
  Val.SomeRational -> pure ()
  Val.Rational _ -> pure ()
  Val.SomeInt -> pure ()
  Val.Int _ -> pure ()
  _ -> empty

int :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
int loc var = anyDom $ do
  fork' $ intC loc var
  pure var

intC :: MonadEval m => Loc -> VarVal m -> EvalT m ()
intC loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeInt)
  Val.SomeRational -> unify' loc var =<< lift (newVar' Val.SomeInt)
  Val.Rational x | denominator x == 1 -> pure ()
  Val.SomeInt -> pure ()
  Val.Int _ -> pure ()
  _ -> empty

float :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
float loc var = anyDom $ do
  fork' $ floatC loc var
  pure var

floatC :: MonadEval m => Loc -> VarVal m -> EvalT m ()
floatC loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeFloat)
  Val.SomeFloat -> pure ()
  Val.Float _ -> pure ()
  _ -> empty

char :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
char loc var = anyDom $ do
  fork' $ charC loc var
  pure var

charC :: MonadEval m => Loc -> VarVal m -> EvalT m ()
charC loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeChar)
  Val.SomeChar -> pure ()
  Val.Char _ -> pure ()
  _ -> empty

char32 :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
char32 loc var = anyDom $ do
  fork' $ char32C loc var
  pure var

char32C :: MonadEval m => Loc -> VarVal m -> EvalT m ()
char32C loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeChar32)
  Val.SomeChar32 -> pure ()
  Val.Char32 _ -> pure ()
  _ -> empty

function :: MonadEval m => Loc -> VarVal m -> EvalT m (DomMatch m)
function loc var = anyDom $ do
  fork' $ functionC loc var
  pure var

functionC :: MonadEval m => Loc -> VarVal m -> EvalT m ()
functionC loc var = lift (readVar' var) >>= \ case
  Val.SomeAny -> unify' loc var =<< lift (newVar' Val.SomeFunction)
  Val.Tuple _ -> pure ()
  Val.Enum {} -> pure ()
  Val.Lam _ -> pure ()
  Val.SomeFunction -> pure ()
  Val.OLam {} -> pure ()
  Val.Intrinsic {} -> pure ()
  _ -> empty

type'
  :: MonadEval m
  => Loc -> VarVal m -> EvalT m (DomMatch m)
type' loc var = anyDom $ do
  R {..} <- ask
  unify' loc var <=< lift $ newVar' . Val.Type assumed sign =<< freshDGVar' Nil
  pure var

query
  :: MonadEval m
  => Loc -> VarVal m -> EvalT m (DomMatch m)
query loc var = anyDom $ do
  var' <- lift freshVar'
  unify' loc var =<< lift (newVar' $ Val.Truth var')
  pure var'

liftPrim
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => (VarVal m -> EvalT m (DomMatch m))
  -> VarVal m -> S m -> S m -> EvalT m (DomMatch m)
liftPrim f var s s' = f var <&> \ (DomMatch x f) ->
  DomMatch x $ \ x -> f x <* lift (unifyS s s')

invokeNegList
  :: MonadEval m
  => Loc
  -> Val.Assumed
  -> VarList m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokeNegList loc assumed xs var s s' = do
  unifyG' loc xs <=< lift $ newList var
  lift $ unifyS s s'
  pure var
  where
    newList var = readVar' var >>= \ case
      Val.Some x | not assumed -> newGVar' . Contract x =<< freshGVar'
      _ -> newGVar' . Var var =<< freshGVar'

invokePosList
  :: MonadEval m
  => Loc
  -> VarList m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
invokePosList loc xs var s s' = do
  local (\ r -> r { assumed = True }) . fork' . one' $ loop xs
  lift $ unifyS s s'
  pure var
  where
    loop = lift . readGVar' >=> \ case
      Nil -> empty
      Var x xs -> unify' loc x var <|> loop xs
      Contract x xs -> invokeContract loc x var <|> loop xs

readPair
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
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
  Nothing -> wrong $ IdentError (loc x) (extract x)
  Just y -> evalNamed (loc x) y s s'

evalQualName
  :: MonadEval m
  => Loc
  -> L (Exp L Ident)
  -> SimpleName
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalQualName loc e x s s' = do
  s'' <- lift freshS
  var_e <- evalExp e s s''
  var <- lift freshVar'
  fork' $ lift (readVar' var_e) >>= getPath loc >>= \ case
    Nothing -> unify' loc var =<< evalTopIdent' (L loc $ Ident.Name x) s'' s'
    Just (p, ps) -> do
      s''' <- lift freshS
      var_p <- evalTopIdent' (L loc $ Ident.Name p) s'' s'''
      unify' loc var =<< dots1M' loc var_p ps x s''' s'
  pure var

-- Ignore root for now since I don't know what it should be.  In the
-- future we want to be able to support several packages, selecting
-- the correct one depending on the root.
getPath :: MonadWrong Error m => Loc -> Val ref a b -> m (Maybe (SimpleName, [SimpleName]))
getPath loc = \ case
  Val.Path (_root:p:ps) -> return $ Just (p, ps)
  Val.Path [_root] -> return Nothing
  _ -> wrong $ EnvError loc

evalTopIdent'
  :: MonadEval m
  => L Ident
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalTopIdent' x s s' = lookupTopNamed (extract x) >>= \ case
  Nothing -> wrong $ IdentError (loc x) (extract x)
  Just y -> evalNamed' (loc x) y s s'

evalNamed
  :: MonadEval m
  => Loc
  -> VarNamed m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
evalNamed loc x s s' = case x of
  Val var -> lift $ unifyS s s' $> var
  Ref var -> do
    var' <- lift freshVar'
    fork' $ lift (readVar' var) >>= \ case
      Val.Ptr ref var -> unify' loc var' =<< readRef' loc ref var s s'
      _ -> wrong $ RefError loc
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
  Ref var -> lift (readVar' var) >>= \ case
    Val.Ptr ref var -> readRef' loc ref var s s'
    _ -> wrong $ RefError loc

writeRef'
  :: MonadEval m
  => Loc
  -> RefVarVal m
  -> VarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m ()
writeRef' loc ref var x s s' = asks (.mode) >>= \ case
  Execution -> do
    s'' <- lift freshS
    y <- lift freshVar'
    join'' $ unify' loc y =<< invoke loc var x s s''
    lift $ do
      unifyEq s''.choiceFree s'.choiceFree
      fork $ do
        _ <- readVar s''.storeFree
        writeVerseRef ref $ coerce y
        unifyEq s''.storeFree s'.storeFree
  Verification -> do
    s'' <- lift freshS
    join'' . void $ invoke loc var x s s''
    lift $ do
      unifyEq s''.choiceFree s'.choiceFree
      fork $ do
        _ <- readVar s''.storeFree
        unifyEq s''.storeFree s'.storeFree

readRef'
  :: MonadEval m
  => Loc
  -> RefVarVal m
  -> VarVal m
  -> S m
  -> S m
  -> EvalT m (VarVal m)
readRef' loc ref var s s' = do
  lift $ unifyEq s.choiceFree s'.choiceFree
  _ <- lift $ readVar s.storeFree
  x <- asks (.mode) >>= \ case
    Execution -> lift $ readVerseRef ref
    Verification -> do
      i <- lift $ newVar' Val.SomeAny
      assume' . local (\ r -> r { assumed = True }) $ do
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

filterNames :: IdentMap a -> HashMap SimpleName a
filterNames =
  HashMap.fromList .
  HashMap.foldrWithKey
  \ case
    Ident.Name k -> \ v z -> (k, v) : z
    Ident.Label _ -> \ _ z -> z
  []

accessOk :: Access -> Label -> Label -> NonEmpty Val.Scope -> Bool
accessOk access scope mod scopes =
  case access of
    Public -> True
    Protected -> List.any (matchProtected scope) scopes
    Private -> List.any (matchScope scope) scopes
    Internal -> List.any (matchScope mod) scopes

matchScope :: Label -> Val.Scope -> Bool
matchScope label (Val.Scope label' _ _) = label == label'

matchProtected :: Label -> Val.Scope -> Bool
matchProtected label (Val.Scope label' labels' _) =
  label == label' || List.elem label labels'

lookupNamed :: Ident -> EvalT m (Maybe (VarNamed m))
lookupNamed x = do
  asks $ \ r -> lookupEnv r.scopes x r.env

lookupTopNamed :: Ident -> EvalT m (Maybe (VarNamed m))
lookupTopNamed x = asks $ \ r -> lookupEnv r.scopes x r.top

lookupEnv
  :: (Hashable k, Eq k)
  => NonEmpty Val.Scope
  -> k
  -> HashMap k (Val.AccessScope, VarNamed m)
  -> Maybe (VarNamed m)
lookupEnv scopes x env =
  case HashMap.lookup x env of
    Just (Val.AccessScope access qScope qModule, named)
      | accessOk access qScope qModule scopes -> Just named
    _ -> Nothing

localName :: Access -> Ident -> VarNamed m -> EvalT m a -> EvalT m a
localName access k v = local $ \ r ->
  r { env = HashMap.insert k (accessScope access r.scopes, v) r.env }

localEnv :: Env m -> EvalT m a -> EvalT m a
localEnv env = local $ \ r ->
  r { env, archetype = mempty, archetype' = mempty }

localNames :: Env m -> EvalT m a -> EvalT m a
localNames env = local $ \ r ->
  r { env = env <> r.env, archetype = mempty, archetype' = mempty }

localSign :: Val.Sign -> EvalT m a -> EvalT m a
localSign sign = local $ \ r -> r { sign }

localScopes :: NonEmpty Val.Scope -> EvalT m a -> EvalT m a
localScopes scopes = local $ \ r -> r { scopes }

getScopes :: EvalT m (NonEmpty Val.Scope)
getScopes = do
  r <- ask
  pure r.scopes

accessScope :: Access -> NonEmpty Val.Scope -> Val.AccessScope
accessScope access (Val.Scope label _ moduleLabel :| _) =
  Val.AccessScope access label moduleLabel

addScope :: Label -> NonEmpty Val.Scope -> NonEmpty Val.Scope
addScope label = addScopeWithSuper label []

addScopeWithSuper :: Label -> [Label] -> NonEmpty Val.Scope -> NonEmpty Val.Scope
addScopeWithSuper label labels scopes =
  Val.Scope label labels (moduleFromScopes scopes) <| scopes

moduleFromScopes :: NonEmpty Val.Scope -> Label
moduleFromScopes (Val.Scope _ _ moduleLabel :| _) = moduleLabel

pushModuleScope :: Label -> EvalT m a -> EvalT m a
pushModuleScope i = local $ \ r -> r { scopes = Val.Scope i [] i <| r.scopes }

freshEnv
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Exp.Env Ident -> EvalT m (Env m)
freshEnv xs = do
  R { scopes } <- ask
  lift . getAp $ HashMap.foldMapWithKey (f scopes) xs
  where
    f scopes k = Ap . \ case
      (access, Exp.Exists) ->
        HashMap.singleton k . (accessScope access scopes,) . Val <$>
        freshVar'
      (access, Exp.Forall) ->
        HashMap.singleton k . (accessScope access scopes,) . Val <$>
        newVar' Val.SomeAny
      (access, Exp.Var) ->
        HashMap.singleton k . (accessScope access scopes,) . Ref <$>
        freshVar'

unify'
  :: MonadEval m
  => Loc -> VarVal m -> VarVal m -> EvalT m ()
unify' loc = unify'' (match loc) `on` coerce

match
  :: MonadEval m
  => Loc
  -> Val (VerseRef m) (VarList m) (VarVal m)
  -> Val (VerseRef m) (VarList m) (VarVal m)
  -> EvalT m (Match, EvalT m ())
match loc' x y = ask >>= \ r -> case (x, y) of
  (Val.SomeAny, Val.Lam _)
    | r.assumed -> pure (GE, pure ())
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeAny, Val.OLam _ ys)
    | r.assumed -> pure $ (GE,) do
        xs <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeAny, Val.Intrinsic _ ys)
    | r.assumed -> pure $ (GE,) do
        xs <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeAny, y)
    | r.assumed -> pure . (GE,) . forVal_ y $ \ y -> do
        x <- lift $ newVar' Val.SomeAny
        unify' loc' x y
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeRational, Val.SomeRational) ->
    pure (GE, pure ())
  (Val.SomeRational, Val.Rational _) ->
    pure (GE, pure ())
  (Val.SomeRational, Val.SomeInt) ->
    pure (GE, pure ())
  (Val.SomeRational, Val.Int _) ->
    pure (GE, pure ())
  (Val.Rational _, Val.SomeRational) ->
    pure (LE, pure ())
  (Val.Rational x, Val.Rational y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.Rational x, Val.SomeInt) ->
    guard (denominator x == 1) $> (LE, pure ())
  (Val.Rational x, Val.Int y) ->
    guard (denominator x == 1 && numerator x == y) $> (SEQ, pure ())
  (Val.SomeInt, Val.SomeRational) ->
    pure (LE, pure ())
  (Val.SomeInt, Val.Rational y) ->
    guard (1 == denominator y) $> (GE, pure ())
  (Val.SomeInt, Val.SomeInt) ->
    pure (GE, pure ())
  (Val.SomeInt, Val.Int _) ->
    pure (GE, pure ())
  (Val.Int _, Val.SomeRational) ->
    pure (LE, pure ())
  (Val.Int x, Val.Rational y) ->
    guard (1 == denominator y && x == numerator y) $> (SEQ, pure ())
  (Val.Int _, Val.SomeInt) ->
    pure (LE, pure ())
  (Val.Int x, Val.Int y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.SomeFloat, Val.SomeFloat) ->
    pure (GE, pure ())
  (Val.SomeFloat, Val.Float _) ->
    pure (GE, pure ())
  (Val.Float _, Val.SomeFloat) ->
    pure (LE, pure ())
  (Val.Float x, Val.Float y) ->
    guard (eqFloat x y) $> (SEQ, pure ())
  (Val.SomeChar, Val.SomeChar) ->
    pure (GE, pure ())
  (Val.SomeChar, Val.Char _) ->
    pure (GE, pure ())
  (Val.Char _, Val.SomeChar) ->
    pure (LE, pure ())
  (Val.Char x, Val.Char y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.SomeChar32, Val.SomeChar32) ->
    pure (GE, pure ())
  (Val.SomeChar32, Val.Char32 _) ->
    pure (GE, pure ())
  (Val.Char32 _, Val.SomeChar32) ->
    pure (LE, pure ())
  (Val.Char32 x, Val.Char32 y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.Path x, Val.Path y) ->
    guard (x == y) $> (SEQ, pure ())
  (Val.Truth x, Val.Truth y) ->
    pure . (SEQ,) $ unify' loc' x y
  (Val.Tuple xs, Val.Tuple ys) ->
    pure . (SEQ,) $ unifyList loc' xs ys
  (Val.Ptr ref_x x, Val.Ptr ref_y y) ->
    guard (ref_x == ref_y) $> (SEQ,) do
      unify' loc' x y
  (Val.Enum i env_x xs, Val.Enum j env_y ys) ->
    guard (i == j) $> (SEQ,) do
      unifyEnv loc' env_x env_y
      unifyList loc' xs ys
  (Val.EnumValue i x, Val.EnumValue j y) ->
    guard (i == j && x == y) $> (SEQ, pure ())
  (Val.StructInst x, Val.StructInst y) ->
    guard (x.expLabel == y.expLabel) $> (SEQ,) do
      unifyEnv loc' x.members y.members
  (Val.ClassInst x, Val.ClassInst y) ->
    guard (x.expLabel == y.expLabel) $> (SEQ,) do
      unifyMaybe loc' x.super y.super
      unifyEnv loc' x.members y.members
  (Val.Lam _, Val.SomeAny)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> wrong $ UndecidableError loc'
  (Val.Lam _, Val.Lam _) ->
    wrong $ UndecidableError loc'
  (Val.Lam _, Val.SomeFunction) -> wrong $ UndecidableError loc'
  (Val.Lam _, Val.OLam {}) -> wrong $ UndecidableError loc'
  (Val.SomeFunction, Val.SomeAny)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeFunction, Val.Lam _) -> wrong $ UndecidableError loc'
  (Val.SomeFunction, Val.SomeFunction)
    | r.assumed -> pure (LE, pure ())
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeFunction, Val.OLam _ ys)
    | r.assumed -> pure . (GE,) $ do
        xs <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.SomeFunction, Val.Intrinsic _ ys)
    | r.assumed -> pure . (GE,) $ do
        xs <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.OLam _ xs, Val.SomeAny)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.OLam {}, Val.Lam _) ->
    wrong $ UndecidableError loc'
  (Val.OLam _ xs, Val.SomeFunction)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.OLam x xs, Val.OLam y ys) ->
    pure $ (SEQ,) do
      whenVerifying . fork' . verify' . local (\ r -> r { assumed = True }) $
        lift (newVar' Val.SomeAny) >>= \ i ->
        invokeOLamDom_ loc' x i >>
        invokeOLamDom_ loc' y i >>
        lift (freeze' i) >>=
        wrong . OLamDomError loc' (loc x.domain) (loc y.domain)
      zs <- lift freshVar'
      unify' loc' xs <=< lift . newVar' $ Val.OLam y zs
      unify' loc' ys <=< lift . newVar' $ Val.OLam x zs
  (Val.OLam x xs, Val.Intrinsic y ys) ->
    pure $ (SEQ,) do
      whenVerifying . fork' . verify' . local (\ r -> r { assumed = True }) $
        lift (newVar' Val.SomeAny) >>= \ i ->
        invokeOLamDom_ loc' x i >>
        invokeIntrinsicDom_ loc' y i >>
        lift (freeze' i) >>=
        wrong . DomError loc' (loc x.domain)
      zs <- lift freshVar'
      unify' loc' xs <=< lift . newVar' $ Val.Intrinsic y zs
      unify' loc' ys <=< lift . newVar' $ Val.OLam x zs
  (Val.Intrinsic _ xs, Val.SomeAny)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.Intrinsic _ xs, Val.SomeFunction)
    | r.assumed -> pure $ (LE,) do
        ys <- lift $ newVar' Val.SomeFunction
        unify' loc' xs ys
    | otherwise -> wrong $ UndecidableError loc'
  (Val.Intrinsic x xs, Val.OLam y ys) ->
    pure $ (SEQ,) do
      whenVerifying . fork' . verify' . local (\ r -> r { assumed = True }) $
        lift (newVar' Val.SomeAny) >>= \ i ->
        invokeIntrinsicDom_ loc' x i >>
        invokeOLamDom_ loc' y i >>
        lift (freeze' i) >>=
        wrong . DomError loc' (loc y.domain)
      zs <- lift freshVar'
      unify' loc' xs <=< lift . newVar' $ Val.OLam y zs
      unify' loc' ys <=< lift . newVar' $ Val.Intrinsic x zs
  (Val.Intrinsic x xs, Val.Intrinsic y ys) ->
    pure $ (SEQ,) do
      whenVerifying . verify' . local (\ r -> r { assumed = True }) $
        lift (newVar' Val.SomeAny) >>= \ i ->
        invokeIntrinsicDom_ loc' x i >>
        invokeIntrinsicDom_ loc' y i >>
        lift (freeze' i) >>=
        wrong . IntrinsicDomError loc'
      zs <- lift freshVar'
      unify' loc' xs =<< lift (newVar' $ Val.Intrinsic y zs)
      unify' loc' ys =<< lift (newVar' $ Val.Intrinsic x zs)
  (x, Val.SomeAny)
    | r.assumed -> pure . (LE,) $ forVal_ x $ \ x -> do
        y <- lift $ newVar' Val.SomeAny
        unify' loc' x y
    | otherwise -> wrong $ UndecidableError loc'
  _ -> empty

unifyG'
  :: MonadEval m
  => Loc
  -> VarList m
  -> VarList m
  -> EvalT m ()
unifyG' loc x y = do
  r <- ask
  lift $ unifyG
    (\ x y -> runReaderT (matchG loc x y) r <&> \ m -> runReaderT m r)
    (coerce x)
    (coerce y)

matchG
  :: MonadEval m
  => Loc
  -> List (VarVal m) (VarList m)
  -> List (VarVal m) (VarList m)
  -> EvalT m (EvalT m ())
matchG loc = curry $ \ case
  (Nil, Nil) -> pure $ pure ()
  (Var x xs, Var y ys) -> pure $
    if'' (unifyHead loc x y)
    do
      const $ unifyG' loc xs ys
    do
      zs <- lift freshGVar'
      unifyG' loc xs <=< lift . newGVar' $ Var y zs
      unifyG' loc ys <=< lift . newGVar' $ Var x zs
  (Var x xs, Contract y ys) -> pure $ do
     zs <- lift freshGVar'
     unifyG' loc xs <=< lift . newGVar' $ Contract y zs
     unifyG' loc ys <=< lift . newGVar' $ Var x zs
  (Contract x xs, Var y ys) -> pure $ do
     zs <- lift freshGVar'
     unifyG' loc xs <=< lift . newGVar' $ Var y zs
     unifyG' loc ys <=< lift . newGVar' $ Contract x zs
  (Contract x xs, Contract y ys) -> pure $
    if x == y
    then unifyG' loc xs ys
    else do
      zs <- lift freshGVar'
      unifyG' loc xs <=< lift . newGVar' $ Contract y zs
      unifyG' loc ys <=< lift . newGVar' $ Contract x zs
  _ -> empty

unifyHead
  :: MonadEval m
  => Loc -> VarVal m -> VarVal m -> EvalT m ()
unifyHead loc = unify'' (matchHead loc) `on` coerce

matchHead
  :: MonadEval m
  => Loc
  -> Val (VerseRef m) (VarList m) (VarVal m)
  -> Val (VerseRef m) (VarList m) (VarVal m)
  -> EvalT m (Match, EvalT m ())
matchHead loc x y = match loc x y >>= \ case
  (SEQ, m) -> pure (SEQ, m)
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
unifyEnv loc xs ys =
  for_ (HashMap.intersectionWith (,) xs ys) $ \ ((_, x), (_, y)) ->
    unifyNamed loc x y

unifyNamed
  :: MonadEval m
  => Loc
  -> VarNamed m
  -> VarNamed m
  -> EvalT m ()
unifyNamed loc = curry $ \ case
  (Val x, Val y) -> unify' loc x y
  (Ref x, Ref y) -> unify' loc x y
  _ -> empty

unify''
  :: MonadEval m
  => (a -> a -> EvalT m (Match, EvalT m ()))
  -> Var m a -> Var m a -> EvalT m ()
unify'' f x y = ask >>= \ r -> lift $
  unify (\ x y -> runReaderT (f x y) r <&> fmap (\ m -> runReaderT m r)) x y

eqFloat :: Double -> Double -> Bool
eqFloat x y = if isNaN x then isNaN y else x == y

getNameEnv
  :: MonadWrong Error m
  => Loc
  -> Val ref a b
  -> m (Val.Env SimpleName b)
getNameEnv loc = \ case
  Val.Module _ xs -> pure xs
  Val.Enum _ xs _ -> pure xs
  Val.StructInst x -> pure x.members
  Val.ClassInst x -> pure x.members
  _ -> wrong $ EnvError loc

fork'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => ReaderT r (VerseT m) ()
  -> ReaderT r (VerseT m) ()
fork' m = ReaderT $ fork . runReaderT m

join''
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => ReaderT r (VerseT m) ()
  -> ReaderT r (VerseT m) ()
join'' m = ReaderT $ join' . runReaderT m

newVar'
  :: (MonadRef m, MonadSupply Int m)
  => Val (VerseRef m) (VarList m) (VarVal m)
  -> VerseT m (VarVal m)
newVar' = fmap coerce . newVar

freshVar'
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m (VarVal m)
freshVar' = coerce <$> freshVar

readVar'
  :: MonadRef m
  => VarVal m
  -> VerseT m (Val (VerseRef m) (VarList m) (VarVal m))
readVar' = readVar . coerce

freshGVar'
  :: (MonadRef m, MonadSupply Int m)
  => VerseT m (VarList m)
freshGVar' = coerce <$> freshGVar

newGVar'
  :: (MonadRef m, MonadSupply Int m)
  => List (VarVal m) (VarList m)
  -> VerseT m (VarList m)
newGVar' = fmap coerce . newGVar

freshDGVar'
  :: (MonadRef m, MonadSupply Int m)
  => List (VarVal m) (VarList m)
  -> VerseT m (VarList m)
freshDGVar' = fmap coerce . freshDGVar . coerce

readGVar'
  :: MonadRef m
  => VarList m
  -> VerseT m (List (VarVal m) (VarList m))
readGVar' = readGVar . coerce

one'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m a
one' m = ReaderT $ one . runReaderT m

if''
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a -> (a -> EvalT m b)
  -> EvalT m b
  -> EvalT m b
if'' p t e = ReaderT $ \ r ->
  if' (runReaderT p r) (\ x -> runReaderT (t x) r) (runReaderT e r)

all''
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m [a]
all'' m = ReaderT $ all' . runReaderT m

for''
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> (a -> EvalT m b)
  -> EvalT m [b]
for'' m f = ReaderT $ \ r -> for' (runReaderT m r) (\ x -> runReaderT (f x) r)

verify'
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => EvalT m ()
  -> EvalT m ()
verify' m = ReaderT $ verify . runReaderT m

assume'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m a
assume' m = ReaderT $ assume . runReaderT m

succeeds'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (Maybe a)
succeeds' m = ReaderT $ succeeds . runReaderT m

decides'
  :: (MonadFix m, MonadRef m, MonadSupply Int m, Freshenable a m)
  => EvalT m a
  -> EvalT m (Maybe a)
decides' m = ReaderT $ decides . runReaderT m

whenVerifying :: EvalT m () -> EvalT m ()
whenVerifying m = asks (.mode) >>= \ case
  Execution -> pure ()
  Verification -> m

succeeds
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (Maybe a)
succeeds m = split m >>= \ case
  Done -> pure Nothing
  Step x m -> m >>= \ case
    Done -> pure $ Just x
    Step _ _ -> pure Nothing

decides
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> VerseT m (Maybe a)
decides m = split m >>= \ case
  Done -> empty
  Step x m -> m >>= \ case
    Done -> pure $ Just x
    Step _ _ -> pure Nothing

decide :: VerseT m ()
decide = pure () <?> empty

if'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> (a -> VerseT m b)
  -> VerseT m b
  -> VerseT m b
if' p t e = split p >>= \ case
  Done -> e
  Step x _ -> t x

for'
  :: (MonadRef m, MonadSupply Int m, Freshenable a m)
  => VerseT m a
  -> (a -> VerseT m b)
  -> VerseT m [b]
for' m k = loop $ split m
  where
    loop m = m >>= \ case
      Done -> pure []
      Step x m -> k x >>= \ y -> (y:) <$> loop m
