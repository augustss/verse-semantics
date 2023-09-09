{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Desugar
  ( desugar
  ) where

import Control.Comonad
import Control.Monad.Abort
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Supply

import Data.Foldable
import Data.Functor
import Data.Functor.Apply
import Data.HashMap.Strict (HashMap, foldlWithKey')
import Data.HashMap.Strict qualified as HashMap
import Data.Traversable

import Language.Verse.Desugar.Exp
import Language.Verse.Error
import Language.Verse.Ident (Ident, IdentMap)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Parse.Exp ( pattern (:<>:)
                                , pattern (:<:)
                                , pattern (:<=:)
                                , pattern (:>:)
                                , pattern (:>=:)
                                , pattern (:+:)
                                , pattern (:-:)
                                , pattern (:*:)
                                , pattern (:/:)
                                , pattern (:->:)
                                )
import Language.Verse.Parse.Exp qualified as Parse

type Desugar = StateT Env (SupplyT Label (Except Error))

type Env = IdentMap (Loc, Bool)

runDesugar :: Desugar a -> SupplyT Label (Except Error) (a, IdentMap Bool)
runDesugar = fmap (fmap (fmap snd)) . runDesugar'

runDesugar' :: Desugar a -> SupplyT Label (Except Error) (a, Env)
runDesugar' m = runStateT m mempty

desugar :: L (Parse.Exp L Name) -> Either Error (L (Exp L Ident))
desugar e = runExcept . runSupplyT $ do
  (e, xs) <- runDesugar' $ desugarExp e
  pure $ exists'' xs e

desugarExp :: L (Parse.Exp L Name) -> Desugar (L (Exp L Ident))
desugarExp e = for e $ \ case
  Parse.InfixColonEqual p e ->
    extract <$> desugarDef p (desugarExp e)
  (Parse.:=:) e1@(extract -> Parse.Pat p@Parse.InfixColon {}) e2 ->
    extract <$> desugarDef (p <$ e1) (desugarExp e2)
  (Parse.:=:) e1 e2 ->
    (:=:) <$> desugarExp e1 <*> desugarExp e2
  Parse.Pat p ->
    extract <$> desugarPat (p <$ e)
  Parse.PrefixColon e -> do
    e <- desugarExp e
    x <- freshIdent (loc e) False
    pure $ BracketInvoke e $ Name x <$ e
  e1 :<>: e2 -> do
    Not . (<$ e) <$> ((:=:) <$> desugarExp e1 <*> desugarExp e2)
  (Parse.:.:) e x ->
    (:.: x) <$> desugarExp e
  (Parse.:..:) e1 e2 ->
    (:..:) <$> desugarExp e1 <*> desugarExp e2
  (Parse.:|:) e1 e2 ->
    (:|:) <$> desugarExp e1 <*> desugarExp e2
  e1 :<: e2 ->
    desugarOperator2 "operator'<'" e1 e2
  e1 :<=: e2 ->
    desugarOperator2 "operator'<='" e1 e2
  e1 :>: e2 ->
    desugarOperator2 "operator'>'" e1 e2
  e1 :>=: e2 ->
    desugarOperator2 "operator'>='" e1 e2
  e1 :+: e2 ->
    desugarOperator2 "operator'+'" e1 e2
  Parse.PrefixPlus e ->
    desugarOperator1 "prefix'+'" e
  e1 :-: e2 ->
    desugarOperator2 "operator'-'" e1 e2
  Parse.PrefixMinus e ->
    desugarOperator1 "prefix'-'" e
  e1 :*: e2 ->
    desugarOperator2 "operator'*'" e1 e2
  e1 :/: e2 ->
    desugarOperator2 "operator'/'" e1 e2
  Parse.List [] ->
    pure $ Tuple []
  Parse.List (e:es) -> extract <$> do
    e <- desugarExp e
    es <- traverse desugarExp es
    pure $ foldl' (\ z x -> z :*>: x <$ z <. x) e es
  Parse.Where e1 e2 -> do
    x <- freshIdent (loc e1) False
    e1 <- desugarExp e1
    e2 <- desugarExp e2
    let x' = Name x <$ e1
    pure $ ((x' :=: e1 <$ e1) :*>: e2 <$ e1 <. e2) :*>: x'
  Parse.Fail ->
    pure Fail
  Parse.One e -> do
    One <$> exists (desugarExp e)
  Parse.All e ->
    All <$> exists (desugarExp e)
  Parse.Not e ->
    Not <$> desugarExp e
  Parse.PrefixBracket e ->
    desugarOperator1 "prefix'[]'" e
  Parse.PrefixQuery e ->
    desugarOperator1 "prefix'?'" e
  Parse.Query e ->
    Query <$> desugarExp e
  Parse.Module e -> do
    i <- supply
    (e, xs) <- lift . runDesugar $ desugarExp e
    pure $ Module i xs e
  Parse.Struct e -> do
    i <- supply
    (e, xs) <- lift . runDesugar $ desugarExp e
    pure $ Struct i xs e
  Parse.Class e1 e2 -> do
    i <- supply
    e1 <- traverse desugarExp e1
    (e2, xs) <- lift . runDesugar $ desugarExp e2
    pure $ Class i e1 xs e2
  Parse.Inst e1 e2 -> do
    e1 <- desugarExp e1
    (e2, xs) <- lift . runDesugar $ desugarExp e2
    pure $ Inst e1 xs e2
  Parse.If p -> do
    (p, xs) <- lift . runDesugar $ desugarExp p
    pure $ IfThenElse xs p (Tuple [] <$ p) (Tuple [] <$ p)
  Parse.IfThen p t -> do
    (p, xs) <- lift . runDesugar $ desugarExp p
    IfThenElse xs p <$>
      exists (desugarExp t) <*>
      pure (Tuple [] <$ p <. t)
  Parse.IfThenElse p t e -> do
    (p, xs) <- lift . runDesugar $ desugarExp p
    IfThenElse xs p <$>
      exists (desugarExp t) <*>
      exists (desugarExp e)
  Parse.For e ->
    All <$> exists (desugarExp e)
  Parse.ForDo e1 e2 -> do
    (e1, xs) <- lift . runDesugar $ desugarExp e1
    ForDo xs e1 <$> exists (desugarExp e2)
  Parse.Block e ->
    extract <$> exists (desugarExp e)
  Parse.Exists x -> do
    tellName x False
    pure . Name . Ident.Name $ extract x
  Parse.Var x -> do
    tellName x True
    pure . Name . Ident.Name $ extract x
  Parse.Set x e ->
    Set (Ident.Name <$> x) <$> desugarExp e
  Parse.Function e_domain e -> do
    (e_domain, xs) <- lift . runDesugar $ desugarExp e_domain
    Fun xs e_domain <$> exists (desugarExp e)
  Parse.ParenInvoke e1 e2 ->
    ParenInvoke <$> desugarExp e1 <*> desugarExp e2
  Parse.BracketInvoke e1 e2 ->
    BracketInvoke <$> desugarExp e1 <*> desugarExp e2
  Parse.Tuple es ->
    Tuple <$> traverse desugarExp es
  Parse.Truth e ->
    Truth <$> exists (desugarExp e)
  Parse.Option e ->
    Option <$> exists (desugarExp e)
  Parse.True ->
    pure $ Truth (Tuple [] <$ e)
  Parse.False ->
    pure $ Tuple []
  Parse.Int x ->
    pure $ Int x
  Parse.Float x ->
    pure $ Float x

desugarDef
  :: L (Parse.Pat L Name)
  -> Desugar (L (Exp L Ident))
  -> Desugar (L (Exp L Ident))
desugarDef p i = do
  e_def <- desugarDef' False p $ (, id) <$> i
  pure $ e_def <$ p

desugarDef'
  :: Bool
  -> L (Parse.Pat L Name)
  -> Desugar (L (Exp L Ident), L (Exp L Ident) -> L (Exp L Ident))
  -> Desugar (Exp L Ident)
desugarDef' funName p m_i = case extract p of
  Parse.Name x -> do
    (if funName then tellFunName else tellName) (x <$ p) False
    let x' = Ident.Name x
    y <- freshIdent (loc p) False
    (e_i, check_i) <- m_i
    pure $ (ArchetypeName x' <$ p) :=: ifArchetypeName x' y (check_i $ Name y <$ p) e_i
  Parse.InfixColon (extract -> Parse.Invoke p e_domain) e_range -> do
    (e_domain, xs) <- lift . runDesugar $ desugarExp e_domain
    e_range <- exists $ desugarExp e_range
    (e_i, check_i) <- exists' m_i
    y <- Ident.Label <$> supply
    let
      xs' = HashMap.insert y False xs
      e_domain' = unify (Name y <$ e_domain) e_domain
      e_i' =
        fun xs e_domain $
        parenInvoke e_range e_i
      check_i' e =
        fun xs' e_domain' .
        parenInvoke e_range .
        check_i .
        parenInvoke e $ Name y <$ e
    desugarDef' True p $ pure (e_i', check_i')
  Parse.InfixColon p e ->
    desugarDef' funName p $ do
      (e_i, check_i) <- m_i
      e <- desugarExp e
      pure (bracketInvoke e e_i, bracketInvoke e . check_i)
  Parse.Invoke p e_domain -> do
    (e_domain, xs) <- lift . runDesugar $ desugarExp e_domain
    (e_i, check_i) <- exists' m_i
    y <- Ident.Label <$> supply
    let
      e_domain' = unify (Name y <$ e_domain) e_domain
      e_i' =
        fun xs e_domain e_i
      check_i' e =
        fun (HashMap.insert y False xs) e_domain' .
        check_i .
        parenInvoke e $ Name y <$ e
    desugarDef' True p $ pure (e_i', check_i')
  p1 :->: p2 ->
    desugarDef' funName p2 $ do
      (e_i, check_i) <- m_i
      x <- freshIdent (loc p1) False
      e1 <- desugarDef p1 (pure $ Name x <$ p1)
      pure (bracketInvoke e_i e1, check_i)

desugarPat :: L (Parse.Pat L Name) -> Desugar (L (Exp L Ident))
desugarPat p = for p $ \ case
  Parse.Name x -> pure . Name $ Ident.Name x
  Parse.InfixColon (extract -> p1 :->: p2) e ->
    desugarDef' False p2 $ do
      e <- desugarExp e
      x <- freshIdent (loc p1) False
      e1 <- desugarDef p1 (pure $ Name x <$ p1)
      pure (bracketInvoke e e1, bracketInvoke e)
  Parse.InfixColon p e -> do
    x <- freshIdent (loc e) False
    e_def <- desugarDef' False p $ do
      e <- desugarExp e
      pure (bracketInvoke e $ Name x <$ e, bracketInvoke e)
    pure $ (e_def <$ p <. e) :*>: (Name x <$ e)
  Parse.Invoke p e -> do
    p <- desugarPat p
    e <- desugarExp e
    pure $ ParenInvoke p e
  p1 :->: p2 -> do
    p1 <- desugarPat p1
    p2 <- desugarPat p2
    pure $ bracketInvoke2 "operator'->'" p1 p2

desugarOperator1 :: Name
                 -> L (Parse.Exp L Name)
                 -> Desugar (Exp L Ident)
desugarOperator1 x e =
  desugarExp e <&> \ e ->
  BracketInvoke (Name (Ident.Name x) <$ e) e

desugarOperator2 :: Name
                 -> L (Parse.Exp L Name)
                 -> L (Parse.Exp L Name)
                 -> Desugar (Exp L Ident)
desugarOperator2 x e1 e2 = bracketInvoke2 x <$> desugarExp e1 <*> desugarExp e2

bracketInvoke2 :: Apply f => Name -> f (Exp f Ident) -> f (Exp f Ident) -> Exp f Ident
bracketInvoke2 x e1 e2 =
  BracketInvoke (Name (Ident.Name x) <$ e1 <. e2) (Tuple [e1, e2] <$ e1 <. e2)

ifArchetypeName :: Apply f => a -> a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ifArchetypeName x y = liftL2 $ IfArchetypeName x y

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

parenInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
parenInvoke = liftL2 ParenInvoke

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

fun :: Apply f => HashMap a Bool -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
fun = liftL2 . Fun

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y

freshIdent :: Loc -> Bool -> Desugar Ident
freshIdent loc var = do
  x <- Ident.Label <$> supply
  modify $ HashMap.insert x (loc, var)
  pure x

tellName :: L Name -> Bool -> Desugar ()
tellName x var =
  put =<<
  HashMap.alterF
  (\ case
      Nothing -> pure $ Just (loc x, var)
      Just (y, _) -> abort $ DefError y (loc x) (extract x))
  (Ident.Name $ extract x) =<<
  get

tellFunName :: L Name -> Bool -> Desugar ()
tellFunName x var =
  modify $ HashMap.insertWith (\ _ x -> x) (Ident.Name $ extract x) (loc x, var)

exists :: Desugar (L (Exp L Ident)) -> Desugar (L (Exp L Ident))
exists m = lift $ do
  (e, xs) <- runDesugar' m
  pure $ exists'' xs e

exists' :: Desugar (L (Exp L Ident), L (Exp L Ident) -> L (Exp L Ident))
         -> Desugar (L (Exp L Ident), L (Exp L Ident) -> L (Exp L Ident))
exists' m = lift $ do
  ((e, f), xs) <- runDesugar' m
  pure (exists'' xs e, exists'' xs . f)

exists'' :: Env -> L (Exp L Ident) -> L (Exp L Ident)
exists'' xs e = foldlWithKey' f e xs
  where
    f z x (loc, var) =
      (if var then Var else Exists) (L loc x) z <$ z
