{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Abort
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Functor
import Data.Functor.Apply
import Data.HashMap.Strict (foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.String

import Language.Verse.Desugar.Exp
  ( Exp (..)
  , Quantifier (..)
  , unify
  , verify
  , check
  , assume
  , forall'
  , bracketInvoke
  , name
  , then'
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
  ( pattern InfixColonEqual
  , pattern List
  , pattern OfType
  , pattern MixfixArrowColonEqual
  , pattern MixfixVarColonEqual
  , pattern PrefixColon
  , pattern Where
  , OC (..)
  )
import Language.Verse.Rewrite.Exp qualified as Rewrite

type DesugarT m = StateT Env (ReaderT R m)

type Env = IdentMap (Loc, Quantifier L Ident)

data R = Exec | Neg | Pos

fromMode :: Mode -> R
fromMode = \ case
  Execution -> Exec
  Verification -> Pos

runDesugarT
  :: Functor m
  => DesugarT m a
  -> ReaderT R m (a, IdentMap (Quantifier L Ident))
runDesugarT = fmap (fmap (fmap snd)) . runDesugarT'

runDesugarT' :: DesugarT m a -> ReaderT R m (a, Env)
runDesugarT' m = runStateT m mempty

desugar
  :: (MonadAbort Error m, MonadSupply Label m)
  => Mode
  -> L (Rewrite.Exp L Ident)
  -> m (L (Exp L Ident))
desugar mode e =
  runReaderT (runDesugarT' (desugar' e) <&> \ (e, xs) -> exists' xs e) $
  fromMode mode

desugar'
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugar' e = ask >>= \ case
  Exec -> desugar'' e
  Pos -> verifyM . checkM Effect.Succeeds . posM $ desugar'' e
  Neg -> desugar'' e

desugar''
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugar'' e = unifyEnv (loc e) `thenM` desugarExp e

desugarExp
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp e = do
  x <- name <$> freshIdent (loc e)
  desugarExp' e False x

desugarExp'
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
desugarExp' e pi x = (e $>) <$> desugarExp'' e pi x

desugarExp''
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarExp'' e pi i = case extract e of
  (Rewrite.:=:) e1 e2 -> do
    e1 <- desugarExp e1
    e2 <- desugarExp' e2 pi i
    pure $ e1 :=: e2
  (Rewrite.:.:) e' x ->
    valM i (e $>) $ desugarExp e' <&> (:.: x)
  (Rewrite.:|:) e1 e2 -> do
    e1 <- desugarExp' e1 pi i
    e2 <- desugarExp' e2 pi i
    pure $ e1 :|: e2
  List [] ->
    pure $ i :=: (Tuple [] <$ e)
  List (e:es) ->
    desugarNonEmpty' pi i e es
  e1 `Where` e2 -> do
    x <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 pi i
    e2 <- desugarExp e2
    pure $ unify x e1 `then'` e2 :*>: x
  Rewrite.Fail ->
    pure Fail
  Rewrite.One e' ->
    valM i (e $>) $ One <$> exists (desugarExp e')
  Rewrite.All e' ->
    valM i (e $>) $ All <$> exists (desugarExp e')
  Rewrite.Not e' ->
    valM i (e $>) $ Not <$> exists (desugarExp e')
  Rewrite.Verify e' ->
    valM i (e $>) $ Verify <$> exists (desugarExp e')
  Rewrite.Check eff e' ->
    valM i (e $>) $ Check eff <$> exists (desugarExp e')
  OfType e1 e2 ->
    desugarOfType e1 e2 pi i
  Rewrite.Assume e' ->
    valM i (e $>) $ Assume Effect.Succeeds <$> exists (desugarExp e')
  Rewrite.Module e' -> valM i (e $>) $ do
    i <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    pure $ Module i xs e'
  Rewrite.Enum xs -> valM i (e $>) $ do
    j <- supply
    pure $ Enum j xs
  Rewrite.Struct e' -> valM i (e $>) $ do
    j <- supply
    (e', xs) <- lift . runDesugarT $ desugarExp e'
    pure $ Struct j xs e'
  Rewrite.Class e1 e2 -> valM i (e $>) $ do
    j <- supply
    e1 <- traverse desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Class j e1 xs e2
  Rewrite.Inst e1 e2 -> valM i (e $>) $ do
    e1 <- desugarExp e1
    (e2, xs) <- lift . runDesugarT $ desugarExp e2
    pure $ Inst e1 xs e2
  Rewrite.IfThenElse e1 e2 e3 -> do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    IfThenElse xs e1 <$>
      exists (desugarExp' e2 pi i) <*>
      exists (desugarExp' e3 pi i)
  Rewrite.ForDo e1 e2 -> valM i (e $>) $ do
    (e1, xs) <- lift . runDesugarT $ desugarExp e1
    e2 <- exists $ desugarExp e2
    pure $ ForDo xs e1 e2
  Rewrite.Block e' ->
    (i :=:) <$> exists (desugarExp e')
  Rewrite.BracketInvoke e1 e2 -> valM i (e $>) $ do
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    pure $ BracketInvoke e1 e2
  Rewrite.Exists x -> do
    tellName x Exists
    pure $ i :=: name x
  Rewrite.Forall x -> do
    tellName x Forall
    pure $ i :=: name x
  Rewrite.Set x e' ->
    valM i (e $>) $ Set x <$> desugarExp e'
  Rewrite.Tuple es -> do
    (is, es) <- desugarTuple pi es
    pure $ unify i (Tuple is <$ e) :*>: (Tuple es <$ e)
  Rewrite.Truth e' ->
    valM i (e $>) $ Truth <$> exists (desugarExp e')
  Rewrite.Int x ->
    pure $ i :=: (Int x <$ e)
  Rewrite.Float x ->
    pure $ i :=: (Float x <$ e)
  Rewrite.Char x ->
    pure $ i :=: (Char x <$ e)
  Rewrite.Char32 x ->
    pure $ i :=: (Char32 x <$ e)
  Rewrite.Lam e1 oc eff e2 e3 -> case oc of
    O -> desugarOLam (loc e) e1 eff e2 e3 pi i
    C -> desugarLam (loc e) e1 eff e2 e3 pi i
  Rewrite.Name x ->
    pure $ i :=: (Name x <$ e)
  MixfixVarColonEqual x y e1 e2 -> do
    tellName x $ Var y
    e1 <- unify (name y) <$> desugarExp e1
    e2 <- desugarExp' e2 pi i
    ask >>= \ case
      Exec -> pure $ e1 :*>: unify (ArchetypeName <$> x) e2
      _ -> pure $ e1 :*>: e2
  InfixColonEqual funName x e -> do
    if funName then tellFunName x else tellName x Exists
    e <- desugarExp' e pi i
    pure $ (ArchetypeName <$> x) :=: e
  PrefixColon e -> do
    e <- desugarExp e
    pure $ BracketInvoke e i
  MixfixArrowColonEqual x y e -> do
    tellName x Exists
    tellName y Exists
    e <- desugarExp' e pi i
    pure $ unify (name x) i :*>: unify (ArchetypeName <$> y) e
  Rewrite.IfArchetypeName x e1 e2 -> do
    y <- (e $>) . Ident.Label <$> supply
    xs <- get
    (e1, xs1) <- lift $ runStateT (desugarExp' e1 True $ name y) xs
    (e2, xs2) <- lift $ runStateT (desugarExp' e2 pi i) xs
    put $ xs1 <> xs2
    pure $ IfArchetypeName x y e1 e2

desugarNonEmpty
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> L (Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (L (Exp L Ident))
desugarNonEmpty pi i x = \ case
  [] -> desugarExp' x pi i
  y:xs -> desugarExp x `thenM` desugarNonEmpty pi i y xs

desugarNonEmpty'
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> L (Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m (Exp L Ident)
desugarNonEmpty' pi i x = \ case
  [] -> desugarExp'' x pi i
  y:xs -> desugarExp x `thenM'` desugarNonEmpty pi i y xs

desugarTuple
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> [L (Rewrite.Exp L Ident)]
  -> DesugarT m ([L (Exp L Ident)], [L (Exp L Ident)])
desugarTuple pi = \ case
  [] -> pure ([], [])
  e:es -> do
    x <- name <$> freshIdent (loc e)
    e <- desugarExp' e pi x
    (xs, es) <- desugarTuple pi es
    pure (x:xs, e:es)

desugarLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarLam loc e1 eff e2 e3 pi f = ask >>= \ case
  Exec -> execLam loc e1 e2 e3 pi f
  Neg -> negLam e1 eff e2 e3
  Pos -> posLam loc e1 eff e2 e3 pi f

execLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
execLam loc' e1 e2 e3 pi f = do
  a <- freshIdent' $ loc e1
  function' loc' pi f . Lam (extract a) <$> exists case e2 of
    Nothing -> do
      b <- name <$> freshIdent (loc e1)
      c <- name <$> freshIdent (loc e1)
      unify b <$> desugarExp' e1 True (name a) `thenM`
        unify c <$> invokeM b pi f `thenM`
        desugarExp' e3 pi c
    Just e2 -> do
      b <- name <$> freshIdent (loc e1)
      c <- name <$> freshIdent (loc e1)
      d <- name <$> freshIdent (loc e3)
      e <- name <$> freshIdent (loc e2)
      unify b <$> desugarExp' e1 True (name a) `thenM`
        unify c <$> invokeM b pi f `thenM`
        unify d <$> desugarExp' e3 pi c `thenM`
        unify e <$> desugarExp e2 `thenM`
        pure (bracketInvoke e d)

posLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
posLam loc e1 eff e2 e3 pi f =
  verifyPosLam' loc e1 eff e2 e3 pi f `thenM'`
  L loc <$> assumePosLam e1 eff e2 e3 pi f

assumePosLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
assumePosLam e1 eff e2 e3 pi f = do
  a <- freshIdent' $ loc e1
  Lam (extract a) <$> exists case e2 of
    Just e2 ->
      desugarExp' e1 True (name a) `thenM`
      negM (abstractD eff e2)
    Nothing -> do
      b <- name <$> freshIdent (loc e1)
      c <- name <$> freshIdent (loc e1)
      unify b <$> desugarExp' e1 True (name a) `thenM`
        unify c <$> invokeM b pi f `thenM`
        negM (desugarExp' e3 pi c)

negLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> DesugarT m (Exp L Ident)
negLam = assumeNegLam

assumeNegLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> DesugarT m (Exp L Ident)
assumeNegLam e1 eff e2 e3 = do
  i <- freshIdent' $ loc e1
  Lam (extract i) <$> exists case e2 of
    Just e2 ->
      posM (desugarExp' e1 True $ name i) `thenM`
      abstractD eff e2
    Nothing ->
      posM (desugarExp' e1 True $ name i) `thenM`
      concreteD eff e3

desugarOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarOLam loc e1 eff e2 e3 pi f = ask >>= \ case
  Exec -> execOLam loc e1 e2 e3 pi f
  Neg -> negOLam loc e1 eff e2 e3 pi f
  Pos -> posOLam loc e1 eff e2 e3 pi f

execOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
execOLam loc' e1 e2 e3 pi f = do
  ((e1, a), xs) <- lift . runDesugarT $ do
    a <- name <$> freshIdent (loc e1)
    b <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 True b
    pure (unify a e1 `then'` b, a)
  function' loc' pi f . OLam f xs e1 <$> exists do
    b <- name <$> freshIdent (loc e3)
    unify b <$> invokeM a pi f `thenM` case e2 of
      Just e2 -> ofTypeD e3 e2 pi b
      Nothing -> desugarExp' e3 pi b

posOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
posOLam loc e1 eff e2 e3 pi f =
  verifyPosLam' loc e1 eff e2 e3 pi f `thenM'`
  L loc <$> assumePosOLam e1 eff e2 e3 pi f

assumePosOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
assumePosOLam e1 eff e2 e3 pi f = do
  ((e1, a), xs) <- lift $ runDesugarT $ do
    a <- name <$> freshIdent (loc e1)
    b <- name <$> freshIdent (loc e1)
    e1 <- desugarExp' e1 True b
    pure (unify a e1 `then'` b, a)
  OLam f xs e1 <$> case e2 of
    Just e2 -> negM $ abstractD eff e2
    Nothing -> exists do
      b <- name <$> freshIdent (loc e3)
      unify b <$> invokeM a pi f `thenM` negM (desugarExp' e3 pi b)

negOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
negOLam = assumeNegOLam

assumeNegOLam
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
assumeNegOLam loc' e1 eff e2 e3 pi f = do
  (e1, xs) <- lift $ runDesugarT $ do
    i <- name <$> freshIdent (loc e1)
    e1 <- posM $ desugarExp' e1 True i
    pure $ e1 `then'` i
  function' loc' pi f . OLam f xs e1 <$> case e2 of
    Just e2 -> abstractD eff e2
    Nothing -> concreteD eff e3

verifyPosLam'
  :: (MonadAbort Error m, MonadSupply Label m)
  => Loc
  -> L (Rewrite.Exp L Ident)
  -> Split.Effect
  -> Maybe (L (Rewrite.Exp L Ident))
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
verifyPosLam' loc' e1 eff e2 e3 pi f = do
  a <- freshIdent' $ loc e1
  functionM loc' pi f . verifyM $ forall' a <$> do
    b <- name <$> freshIdent (loc e1)
    unify b <$> negM (desugarExp' e1 True $ name a) `thenM` checkM eff do
      c <- name <$> freshIdent (loc e3)
      case e2 of
        Just e2 -> unify c <$> invokeM b pi f `thenM` checkOfTypeD e3 e2 pi c
        Nothing -> unify c <$> invokeM b pi f `thenM` desugarExp' e3 pi c

desugarOfType
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
desugarOfType e1 e2 pi x = ask >>= \ case
  Exec -> execOfType e1 e2 pi x
  Neg -> negOfType e2
  Pos -> posOfType e1 e2 pi x

execOfType
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
execOfType e1 e2 pi x = do
  y <- name <$> freshIdent (loc e1)
  e1 <- desugarExp' e1 pi x
  e2 <- desugarExp e2
  pure $ unify y e1 :*>: bracketInvoke e2 y

negOfType
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> DesugarT m (Exp L Ident)
negOfType e2 = do
  z <- name <$> freshIdent (loc e2)
  e2 <- unify z <$> desugarExp e2
  r <- freshIdent' $ loc e2
  pure $ e2 :*>: forall' r (assume Effect.Succeeds $ bracketInvoke z (name r))

posOfType
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (Exp L Ident)
posOfType e1 e2 pi x = do
  y <- name <$> freshIdent (loc e1)
  e1 <- unify y <$> desugarExp' e1 pi x
  z <- name <$> freshIdent (loc e2)
  e2 <- unify z <$> desugarExp e2
  r <- freshIdent' $ loc e2
  pure $
    e1 `then'`
    e2 `then'`
    check Effect.Succeeds (bracketInvoke z y) :*>:
    forall' r (assume Effect.Succeeds $ bracketInvoke z (name r))

checkOfTypeD
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
checkOfTypeD e1 e2 pi x = do
  i <- name <$> freshIdent (loc e1)
  j <- name <$> freshIdent (loc e2)
  e1 <- desugarExp' e1 pi x
  e2 <- desugarExp e2
  pure $
    unify i e1 `then'`
    unify j e2 `then'`
    check Effect.Succeeds (bracketInvoke j i)

ofTypeD
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Rewrite.Exp L Ident)
  -> L (Rewrite.Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
ofTypeD e1 e2 pi x = do
  i <- name <$> freshIdent (loc e1)
  j <- name <$> freshIdent (loc e2)
  e1 <- desugarExp' e1 pi x
  e2 <- desugarExp e2
  pure $
    unify i e1 `then'`
    unify j e2 `then'`
    bracketInvoke j i

abstractD
  :: (MonadAbort Error m, MonadSupply Label m)
  => Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
abstractD eff e = do
  r <- freshIdent' $ loc e
  forall' r <$> assumeM eff do
    i <- name <$> freshIdent (loc e)
    unify i <$> desugarExp e `thenM`
      pure (assume Effect.Succeeds . bracketInvoke i $ name r)

concreteD
  :: (MonadAbort Error m, MonadSupply Label m)
  => Split.Effect
  -> L (Rewrite.Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
concreteD eff e = do
  r <- freshIdent' $ loc e
  forall' r <$> assumeM eff (negM . desugarExp' e True $ name r)

valM
  :: Functor m
  => f (Exp f a)
  -> (b -> f (Exp f a))
  -> m b
  -> m (Exp f a)
valM i f m = (i :=:) . f <$> m

invokeM
  :: MonadSupply Label m
  => L (Exp L Ident)
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
invokeM x pi f = case pi of
  False -> name <$> freshIdent (loc x <> loc f)
  True -> pure $ bracketInvoke f x

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
  , Intrinsic.Query
  ]
  where
    list = \ case
      [] -> L loc $ Tuple []
      x:xs -> foldr then' x xs
    f x = do
      let x' = L loc . Ident.Name . fromString $ Intrinsic.toString x
      tellFunName x'
      pure $ unify (name x') (L loc $ Intrinsic x)

negM :: MonadReader R m => m a -> m a
negM = local (const Neg)

posM :: MonadReader R m => m a -> m a
posM = local (const Pos)

functionM
  :: Functor m
  => Loc
  -> Bool
  -> L (Exp L Ident)
  -> DesugarT m (L (Exp L Ident))
  -> DesugarT m (L (Exp L Ident))
functionM loc pi f m = case pi of
  False -> m
  True -> (bracketInvoke (L loc $ Name "function") f `then'`) <$> m

function'
  :: Loc
  -> Bool
  -> L (Exp L Ident)
  -> Exp L Ident
  -> Exp L Ident
function' loc pi f e = case pi of
  False -> e
  True -> bracketInvoke (L loc $ Name "function") f :*>: L loc e

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

freshIdent :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent loc = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, Exists)
  pure $ L loc x

freshIdent' :: MonadSupply Label m => Loc -> DesugarT m (L Ident)
freshIdent' loc = L loc <$> freshIdent''

freshIdent'' :: MonadSupply Label m => DesugarT m Ident
freshIdent'' = Ident.Label <$> supply

tellName :: MonadAbort Error m => L Ident -> Quantifier L Ident -> DesugarT m ()
tellName x y =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, y)
      Just (y, _) -> abort $ DefError y (loc x) (extract x))
  (extract x) =<<
  get

tellFunName :: Monad m => L Ident -> DesugarT m ()
tellFunName x =
  modify $ HashMap.insertWith (\ _ x -> x) (extract x) (loc x, Exists)

exists :: Monad m => DesugarT m (L (Exp L Ident)) -> DesugarT m (L (Exp L Ident))
exists m = do
  (e, xs) <- lift $ runDesugarT' m
  pure $ exists' xs e

exists' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists' xs e = foldlWithKey' f e xs
  where
    f z x (loc, y) = Def y (L loc x) z <$ z
