{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply
import Control.Monad.Wrong

import Data.Foldable
import Data.Functor ((<&>))
import Data.Functor.Apply
import Data.HashMap.Strict (foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.String
import Data.Traversable

import Language.Verse.Access
import Language.Verse.Desugar.Exp
  ( Exp (..)
  , Quantifier (..)
  , assume
  , bracketInvoke
  , check
  , domain
  , forall'
  , name
  , seq'
  , then'
  , unify
  , verify
  )
import Language.Verse.Effect.Split qualified as Split (Effect)
import Language.Verse.Effect.Split qualified as Effect
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Intrinsic qualified as Intrinsic
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Mode
import Language.Verse.Rewrite.Exp
  ( pattern Alloc2
  , pattern Alloc3
  , pattern InfixColonEqual
  , pattern List
  , pattern OfType
  , pattern MixfixArrowColonEqual
  , pattern PrefixColon
  , pattern Where
  , OC (..)
  )
import Language.Verse.Rewrite.Exp qualified as Rewrite
import Language.Verse.SimpleName

type DesugarT m = StateT Env (ReaderT R m)

type Env = IdentMap (Loc, Access, Quantifier)

data R = Exec | Neg | Pos

data Pi = E | P !(L Ident)

fromMode :: Mode -> R
fromMode = \ case
  Execution -> Exec
  Verification -> Pos

runDesugarT
  :: Functor m
  => DesugarT m a
  -> ReaderT R m (a, IdentMap (Loc, Access, Quantifier))
runDesugarT =  runDesugarT'

runDesugarT' :: DesugarT m a -> ReaderT R m (a, Env)
runDesugarT' m = runStateT m mempty

desugar
  :: (MonadWrong Error m, MonadSupply Label m)
  => Mode
  -> L (Rewrite.Exp L Ident)
  -> m (L (Exp L Ident))
desugar mode e = flip runReaderT (fromMode mode) $ case mode of
  Execution -> desugarTop e
  Verification -> verify . check Effect.Succeeds <$> desugarTop e

desugarTop
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> ReaderT R m (L (Exp L Ident))
desugarTop e = do
  (e, xs) <- runDesugarT $ unifyEnv (loc e) `thenM` desugarExp e
  pure $ TopLevel (dropLoc xs) <$> duplicate e

desugarExp
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp e = desugarExp' e E

desugarExp'
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (L (Exp L Ident))
desugarExp' e pi = (e $>) <$> desugarExp'' e pi

desugarExp''
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
desugarExp'' e pi = case extract e of
  (Rewrite.:=:) e1 e2 ->
    (:=:) <$> desugarExp' e1 pi <*> desugarExp' e2 pi
  (Rewrite.:.:) e' x ->
    valM pi (e $>) $ desugarExp e' <&> (:.: x)
  (Rewrite.:|:) e1 e2 ->
    (:|:) <$> desugarExp' e1 pi <*> desugarExp' e2 pi
  List [] ->
    pure . val pi (e $>) $ Tuple []
  List (e:es) ->
    desugarNonEmpty' pi e es
  e1 `Where` e2 -> do
    x <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 pi
    e2 <- posM $ desugarExp e2
    pure $ unify x e1 `then'` e2 :*>: x
  Rewrite.Fail ->
    pure $ val pi (e $>) Fail
  Rewrite.One e' ->
    valM pi (e $>) $ One <$> exists (desugarExp e')
  Rewrite.All e' ->
    valM pi (e $>) $ All <$> exists (desugarExp e')
  Rewrite.Not e' ->
    valM pi (e $>) $ Not <$> exists (desugarExp e')
  Rewrite.Verify e' ->
    valM pi (e $>) $ Verify <$> exists (desugarExp e')
  Rewrite.Check eff e' ->
    valM pi (e $>) $ Check eff <$> exists (desugarExp e')
  OfType e1 e2 ->
    desugarOfType e1 e2 pi
  Rewrite.Assume e' ->
    valM pi (e $>) $ Assume Effect.Succeeds <$> exists (desugarExp e')
  Rewrite.Module e' -> valM pi (e $>) $ do
    i <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    checkNoneOf [Private] xs
    pure $ Module i (dropLoc xs) e'
  Rewrite.Enum xs -> valM pi (e $>) $ do
    j <- supply
    pure $ Enum j xs
  Rewrite.Struct e' -> valM pi (e $>) $ do
    j <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    pure $ Struct j (dropLoc xs) e'
  Rewrite.Class e1 e2 -> valM pi (e $>) $ do
    j <- supply
    e1 <- traverse desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Class j e1 (dropLoc xs) e2
  Rewrite.Inst e1 e2 -> valM pi (e $>) $ do
    e1 <- desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Inst e1 (dropLoc xs) e2
  Rewrite.IfThenElse e1 e2 e3 -> do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    IfThenElse (dropLoc xs) e1 <$>
      exists (desugarExp' e2 pi) <*>
      exists (desugarExp' e3 pi)
  Rewrite.ForDo e1 e2 -> valM pi (e $>) $ do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    e2 <- exists $ desugarExp e2
    pure $ ForDo (dropLoc xs) e1 e2
  Rewrite.Block e' ->
    valM pi (e $>) $ extract <$> exists (desugarExp e')
  Rewrite.BracketInvoke e1 e2 -> valM pi (e $>) $ do
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ BracketInvoke e1 e2
  Rewrite.Exists x -> do
    tellExistsName Internal x
    pure . val pi (e $>) . Name $ extract x
  Rewrite.Forall x -> do
    tellForallName Internal x
    pure . val pi (e $>) . Name $ extract x
  Alloc2 access x e -> do
    tellVarName access x
    i <- case pi of
      E -> freshIdent $ loc e
      P i -> pure i
    e <- desugarExp e
    pure . Alloc x e $ name i
  Alloc3 access x e1 e2 -> do
    tellVarName access x
    e1 <- desugarExp e1
    e2 <- desugarExp' e2 pi
    pure $ Alloc x e1 e2
  Rewrite.Set x e' ->
    valM pi (e $>) $ Set x <$> desugarExp e'
  Rewrite.Tuple es -> case pi of
    E -> Tuple <$> for es desugarExp
    P i ->
      let
        unfold e = do
          x <- freshIdent $ loc e
          (name x,) <$> desugarExp' e (P x)
        fold (xs, es) =
          unify (name i) (Tuple xs <$ e) :*>: (Tuple es <$ e)
      in fold . unzip <$> traverse unfold es
  Rewrite.Truth e' ->
    valM pi (e $>) $ Truth <$> exists (desugarExp e')
  Rewrite.Int x ->
    pure . val pi (e $>) $ Int x
  Rewrite.Float x ->
    pure . val pi (e $>) $ Float x
  Rewrite.Char x ->
    pure . val pi (e $>) $ Char x
  Rewrite.Char32 x ->
    pure . val pi (e $>) $ Char32 x
  Rewrite.Lam e1 oc eff e2 -> case oc of
    O -> desugarOLam (loc e) e1 eff e2 pi
    C -> desugarLam (loc e) e1 eff e2 pi
  Rewrite.Name x ->
    pure . val pi (e $>) $ Name x
  Rewrite.QualName x y ->
    valM pi (e $>) $ desugarQualName x y
  Rewrite.Path x ->
    pure . val pi (e $>) $ Path x
  InfixColonEqual access t x e -> do
    tellName access t x
    e <- desugarExp' e pi
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> case pi of
    E -> ask >>= \ case
      Neg -> do
        z <- name <$> freshIdent (loc e)
        e <- unify z <$> desugarExp e
        r <- freshIdent' $ loc e
        pure $
          e :*>:
          forall' Internal r (assume Effect.Succeeds . bracketInvoke z $ name r)
      _ -> do
        e <- desugarExp e
        BracketInvoke e . name <$> freshIdent (loc e)
    P i -> do
      e <- desugarExp e
      pure . BracketInvoke e $ name i
  MixfixArrowColonEqual x y e -> do
    tellExistsName Internal x
    tellExistsName Internal y
    i <- case pi of
      E -> freshIdent $ loc e
      P i -> pure i
    e <- desugarExp' e $ P i
    pure $ unify (name x) (name i) :*>: unify (ArchetypeName <$> y) e
  Rewrite.IfArchetypeName x e1 e2 -> do
    y <- freshIdent' $ loc e
    xs <- get
    (e1, xs1) <- lift $ runStateT (desugarExp' e1 $ P y) xs
    (e2, xs2) <- lift $ runStateT (desugarExp' e2 pi) xs
    put $ xs1 <> xs2
    pure $ IfArchetypeName x y e1 e2
  Rewrite.Domain e ->
    valM pi (e $>) $ Domain <$> desugarExp e

desugarQualName
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> SimpleName
  -> DesugarT m (Exp L Ident)
desugarQualName e x = do
  e <- desugarExp e
  pure $ QualName e x

desugarNonEmpty
  :: (MonadWrong Error m, MonadSupply Label m)
  => Pi
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (L (Exp L Ident))
desugarNonEmpty pi x = \ case
  [] -> desugarExp' x pi
  y:xs -> posM (desugarExp x) `thenM` desugarNonEmpty pi y xs

desugarNonEmpty'
  :: (MonadWrong Error m, MonadSupply Label m)
  => Pi
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (Exp L Ident)
desugarNonEmpty' pi x = \ case
  [] -> desugarExp'' x pi
  y:xs -> posM (desugarExp x) `thenM'` desugarNonEmpty pi y xs

desugarLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
desugarLam loc e1 eff e2 pi = ask >>= \ case
  Exec -> execLam loc e1 e2 pi
  Neg -> negLam loc e1 eff e2 pi
  Pos -> posLam loc e1 eff e2 pi

execLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
execLam loc' e1 e2 = \ case
  E -> do
    a <- freshIdent' $ loc e1
    Lam (extract a) <$> exists do
      e1 <- domainM . desugarExp' e1 $ P a
      e2 <- desugarExp e2
      pure $ e1 `then'` e2
  P f -> do
    a <- freshIdent' $ loc e1
    e <- Lam (extract a) <$> exists do
      b <- name <$> freshIdent (loc e1)
      e1 <- unify b <$> domainM (desugarExp' e1 $ P a)
      c <- freshIdent $ loc e2
      let e_f = unify (name c) $ bracketInvoke (name f) b
      e2 <- desugarExp' e2 $ P c
      pure $ e1 `seq'` (e_f `then'` e2)
    pure $ bracketInvoke (L loc' $ Name "function") (name f) :*>: L loc' e

negLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
negLam loc' e1 eff e2 = \ case
  E -> do
    a <- freshIdent' $ loc e1
    Lam (extract a) <$> exists do
      e1 <- posM . domainM . desugarExp' e1 $ P a
      e2 <- assumeM eff $ desugarExp e2
      pure $ e1 `seq'` e2
  P f -> do
    a <- freshIdent' $ loc e1
    e <- Lam (extract a) <$> exists do
      b <- name <$> freshIdent (loc e1)
      e1 <- unify b <$> posM (domainM . desugarExp' e1 $ P a)
      e2 <- desugarExp e2
      pure $ e1 `seq'` e2
    pure $ bracketInvoke (L loc' $ Name "function") (name f) :*>: L loc' e

posLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
posLam loc e1 eff e2 pi =
  verifyPosLam' loc e1 eff e2 pi `thenM'`
  L loc <$> negM (negLam loc e1 eff e2 pi)

desugarOLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
desugarOLam loc e1 eff e2 pi = ask >>= \ case
  Exec -> execOLam loc e1 e2 pi
  Neg -> negOLam loc e1 eff e2 pi
  Pos -> posOLam loc e1 eff e2 pi

execOLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
execOLam loc' e1 e2 = \ case
  E -> do
    f <- name <$> freshIdent (loc e1)
    (e1, xs) <- lift . runDesugarT $ do
      b <- freshIdent (loc e1)
      e1 <- domainM . desugarExp' e1 $ P b
      pure $ e1 `then'` name b
    e2 <- exists (desugarExp e2)
    pure $ OLam f (dropLoc xs) e1 e2
  P f -> do
    ((e1, b), xs) <- lift . runDesugarT $ do
      a <- freshIdent (loc e1)
      b <- name <$> freshIdent (loc e1)
      e1 <- domainM . desugarExp' e1 $ P a
      pure (unify b e1 `then'` name a, b)
    e <- OLam (name f) (dropLoc xs) e1 <$> exists do
      c <- freshIdent (loc e1)
      let e_f = unify (name c) (bracketInvoke (name f) b)
      e2 <- exists . desugarExp' e2 $ P c
      pure $ e_f `then'` e2
    pure $ bracketInvoke (L loc' $ Name "function") (name f) :*>: L loc' e

negOLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
negOLam loc' e1 eff e2 = \ case
  E -> do
    f <- name <$> freshIdent (loc e1)
    (e1, xs) <- lift . runDesugarT $ do
      b <- freshIdent $ loc e1
      e1 <- posM . domainM . desugarExp' e1 $ P b
      pure $ e1 `then'` name b
    e2 <- assumeM eff $ desugarExp e2
    pure $ OLam f (dropLoc xs) e1 e2
  P f -> do
    (e1, xs) <- lift . runDesugarT $ do
      a <- freshIdent $ loc e1
      e1 <- posM . domainM . desugarExp' e1 $ P a
      pure $ e1 `then'` name a
    e <- OLam (name f) (dropLoc xs) e1 <$> exists (desugarExp e2)
    pure $ bracketInvoke (L loc' $ Name "function") (name f) :*>: L loc' e

posOLam
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
posOLam loc e1 eff e2 pi =
  verifyPosLam' loc e1 eff e2 pi `thenM'`
  L loc <$> negM (negOLam loc e1 eff e2 pi)

verifyPosLam'
  :: (MonadWrong Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (L (Exp L Ident))
verifyPosLam' loc' e1 eff e2 = \ case
  E -> verifyM $
    negM (domainM $ desugarExp e1) `seqM`
    checkM eff (desugarExp e2)
  P f -> (bracketInvoke (L loc' $ Name "function") (name f) `then'`) <$> verifyM do
    b <- name <$> freshIdent (loc e1)
    unify b <$> negM (domainM $ desugarExp e1) `thenM` checkM eff do
      c <- freshIdent $ loc e2
      (unify (name c) (bracketInvoke (name f) b) `then'`) <$> desugarExp' e2 (P c)

desugarOfType
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
desugarOfType e1 e2 pi = ask >>= \ case
  Exec -> execOfType e1 e2 pi
  Neg -> negOfType e2
  Pos -> posOfType e1 e2 pi

execOfType
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
execOfType e1 e2 pi = do
  y <- name <$> freshIdent (loc e1)
  e1 <- desugarExp' e1 pi
  e2 <- desugarExp e2
  pure $ unify y e1 :>>: bracketInvoke e2 y

negOfType
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (Exp L Ident)
negOfType e2 = do
  z <- name <$> freshIdent (loc e2)
  e2 <- unify z <$> desugarExp e2
  r <- freshIdent' $ loc e2
  pure $
    e2 :>>:
    forall' Internal r (assume Effect.Succeeds . bracketInvoke z $ name r)

posOfType
  :: (MonadWrong Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Pi
  -> DesugarT m (Exp L Ident)
posOfType e1 e2 pi = do
  y <- name <$> freshIdent (loc e1)
  e1 <- unify y <$> desugarExp' e1 pi
  z <- name <$> freshIdent (loc e2)
  e2 <- unify z <$> desugarExp e2
  r <- freshIdent' $ loc e2
  pure $
    e1 `then'`
    e2 :*>:
    (check Effect.Succeeds (bracketInvoke z y) `seq'`
     forall' Internal r (assume Effect.Succeeds . bracketInvoke z $ name r))

valM
  :: Functor m
  => Pi
  -> (b -> L (Exp L Ident))
  -> m b
  -> m (Exp L Ident)
valM pi f m = val pi f <$> m

val
  :: Pi
  -> (b -> L (Exp L Ident))
  -> b
  -> Exp L Ident
val pi f e = case pi of
  E -> extract $ f e
  P i -> (name i :=:) $ f e

unifyEnv :: Monad m => Loc -> DesugarT m (L (Exp L Ident))
unifyEnv loc =
  list <$> traverse f
  [ Intrinsic.Less
  , Intrinsic.LessEqual
  , Intrinsic.Greater
  , Intrinsic.GreaterEqual
  , Intrinsic.Plus
  , Intrinsic.PrefixPlus
  , Intrinsic.Minus
  , Intrinsic.PrefixMinus
  , Intrinsic.Multiply
  , Intrinsic.Divide
  , Intrinsic.To
  , Intrinsic.Any
  , Intrinsic.Int
  , Intrinsic.Rational
  , Intrinsic.Float
  , Intrinsic.Char
  , Intrinsic.Char32
  , Intrinsic.Function
  , Intrinsic.Type
  , Intrinsic.Query
  ]
  where
    list = \ case
      [] -> L loc $ Tuple []
      x:xs -> foldr then' x xs
    f x = do
      let x' = L loc . Ident.Name . fromString $ Intrinsic.toString x
      tellFunName Internal x'
      pure $ unify (name x') (L loc $ Intrinsic x)

negM :: MonadReader R m => m a -> m a
negM = local (const Neg)

posM :: MonadReader R m => m a -> m a
posM = local $ \ case
  Exec -> Exec
  Neg -> Pos
  Pos -> Pos

verifyM
  :: (MonadSupply Label m)
  => DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
verifyM m = verify <$> exists m

assumeM
  :: (MonadSupply Label m)
  => Split.Effect
  -> DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
assumeM eff m = assume eff <$> exists m

checkM
  :: (MonadSupply Label m)
  => Split.Effect
  -> DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
checkM eff m = check eff <$> exists m

thenM
  :: (Applicative m, Apply f)
  => m (f (Exp f a))
  -> m (f (Exp f a))
  -> m (f (Exp f a))
thenM = liftA2 then'
infixl 1 `thenM`

thenM'
  :: Applicative m
  => m (f (Exp f a))
  -> m (f (Exp f a))
  -> m (Exp f a)
thenM' = liftA2 (:*>:)
infixl 1 `thenM'`

seqM
  :: (Applicative m, Apply f)
  => m (f (Exp f a))
  -> m (f (Exp f a))
  -> m (f (Exp f a))
seqM = liftA2 seq'
infixl 1 `seqM`

domainM :: (Functor m, Functor f) => m (f (Exp f a)) -> m (f (Exp f a))
domainM = fmap domain

freshIdent :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent loc = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, Internal, Exists)
  pure $ L loc x

freshIdent' :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent' loc = L loc . Ident.Label <$> supply

tellValName :: MonadWrong Error m => Access -> L Ident -> DesugarT m ()
tellValName access = tellName' access Exists

tellFunName :: Monad m => Access -> L Ident -> DesugarT m ()
tellFunName access x =
  modify $ HashMap.insertWith (\ _ x -> x) (extract x) (loc x, access, Exists)

tellVarName :: MonadWrong Error m => Access -> L Ident -> DesugarT m ()
tellVarName access = tellName' access Var

tellExistsName :: MonadWrong Error m => Access -> L Ident -> DesugarT m ()
tellExistsName access = tellName' access Exists

tellForallName :: MonadWrong Error m => Access -> L Ident -> DesugarT m ()
tellForallName access = tellName' access Forall

tellName
  :: MonadWrong Error m
  => Access
  -> Rewrite.Quantifier
  -> L Ident
  -> DesugarT m ()
tellName access = \ case
  Rewrite.Val -> tellValName access
  Rewrite.Fun -> tellFunName access
  Rewrite.Var -> tellVarName access

tellName' :: MonadWrong Error m => Access -> Quantifier -> L Ident -> DesugarT m ()
tellName' access t x =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, access, t)
      Just (y, _, _) -> wrong $ DefError y (loc x) (extract x))
  (extract x) =<<
  get

exists :: Monad m => DesugarT m (L (Exp L Ident)) -> DesugarT m (L (Exp L Ident))
exists m = do
  (e, xs) <- lift $ runDesugarT' m
  pure $ exists' xs e

exists' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, access, y) = Def access y (L loc x) z <$ z

dropLoc :: Env -> HashMap.HashMap Ident (Access, Quantifier)
dropLoc = fmap (\ (_loc, access, x) -> (access, x))

checkNoneOf :: MonadWrong Error m => [Access] -> Env -> m ()
checkNoneOf notAllowed env = for_ env $ \ (loc, access, _) ->
  when (access `elem` notAllowed) . wrong $ AccessError loc access
