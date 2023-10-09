{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Rewrite
  ( rewrite
  ) where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Supply

import Data.Bool
import Data.Function
import Data.Functor
import Data.Functor.Apply
import Data.Traversable

import Language.Verse.Ident (Ident)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Name
import Language.Verse.Parse.Exp ( pattern (:<>:)
                                , pattern (:..:)
                                , pattern (:<:)
                                , pattern (:<=:)
                                , pattern (:>:)
                                , pattern (:>=:)
                                , pattern PrefixPlus
                                , pattern (:+:)
                                , pattern PrefixMinus
                                , pattern (:-:)
                                , pattern (:*:)
                                , pattern (:/:)
                                , pattern (:->:)
                                , pattern PrefixBracket
                                , pattern PrefixQuery
                                , pattern PostfixQuery
                                , pattern If
                                , pattern IfThen
                                , pattern IfElse
                                , pattern For
                                , pattern Pat
                                , Pat
                                , pattern Var
                                , pattern InfixColon
                                , pattern InfixArrow
                                , pattern Invoke
                                )
import Language.Verse.Parse.Exp qualified as Parse
import Language.Verse.Rewrite.Exp
import Prelude(error)

rewrite
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => f (Parse.Exp f Name)
  -> m (f (Exp f Ident))
rewrite = rewriteExp

rewriteExp
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => f (Parse.Exp f Name)
  -> m (f (Exp f Ident))
rewriteExp e = for e $ \ case
  (Parse.:=:) e1@(extract -> Pat p@Parse.Var {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) e1@(extract -> Pat p@Parse.PrefixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) e1@(extract -> Pat p@InfixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) e1 e2 ->
    (:=:) <$> rewriteExp e1 <*> rewriteExp e2
  e1 :<>: e2 ->
    Not . (e $>) <$> ((:=:) <$> rewriteExp e1 <*> rewriteExp e2)
  (Parse.:|:) e1 e2 ->
    (:|:) <$> rewriteExp e1 <*> rewriteExp e2
  (Parse.:.:) e x ->
    rewriteExp e <&> (:.: x)
  e1 :..: e2 ->
    rewriteOperator2 "operator'..'" e1 e2
  e1 :<: e2 ->
    rewriteOperator2 "operator'<'" e1 e2
  e1 :<=: e2 ->
    rewriteOperator2 "operator'<='" e1 e2
  e1 :>: e2 ->
    rewriteOperator2 "operator'>'" e1 e2
  e1 :>=: e2 ->
    rewriteOperator2 "operator'>='" e1 e2
  PrefixPlus e ->
    rewriteOperator1 "prefix'+'" e
  e1 :+: e2 ->
    rewriteOperator2 "operator'+'" e1 e2
  PrefixMinus e ->
    rewriteOperator1 "prefix'-'" e
  e1 :-: e2 ->
    rewriteOperator2 "operator'-'" e1 e2
  e1 :*: e2 ->
    rewriteOperator2 "operator'*'" e1 e2
  e1 :/: e2 ->
    rewriteOperator2 "operator'/'" e1 e2
  e1 :->: e2 ->
    rewriteOperator2 "operator'->'" e1 e2
  PrefixBracket e ->
    rewriteOperator1 "prefix'[]'" e
  PrefixQuery e ->
    rewriteOperator1 "prefix'?'" e
  PostfixQuery e ->
    rewriteOperator1 "postfix'?'" e
  Parse.List es ->
    List <$> traverse rewriteExp es
  Parse.Where e1 e2 ->
    Where <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Fail ->
    pure Fail
  Parse.One e ->
    One <$> rewriteExp e
  Parse.All e ->
    All <$> rewriteExp e
  Parse.Not e ->
    Not <$> rewriteExp e
  Parse.Module e ->
    Module <$> rewriteExp e
  Parse.Struct e ->
    Struct <$> rewriteExp e
  Parse.Class e1 e2 ->
    Class <$> traverse rewriteExp e1 <*> rewriteExp e2
  Parse.Inst e1 e2 ->
    Inst <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Enum xs ->
    pure $ Enum xs
  If e' -> do
    e' <- rewriteExp e'
    pure $ IfThenElse e' (Tuple [] <$ e) (Tuple [] <$ e)
  IfThen e1 e2 -> do
    e1 <- rewriteExp e1
    e2 <- rewriteExp e2
    pure $ IfThenElse e1 e2 (Tuple [] <$ e)
  IfElse e1 e2 -> do
    e1 <- rewriteExp e1
    e2 <- rewriteExp e2
    pure $ IfThenElse e1 (Tuple [] <$ e) e2
  Parse.IfThenElse e1 e2 e3 ->
    IfThenElse <$> rewriteExp e1 <*> rewriteExp e2 <*> rewriteExp e3
  For e ->
    All <$> rewriteExp e
  Parse.ForDo e1 e2 ->
    ForDo <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Block e ->
    Block <$> rewriteExp e
  Parse.ParenInvoke e1 e2 ->
    ParenInvoke <$> rewriteExp e1 <*> rewriteExp e2
  Parse.BracketInvoke e1 e2 ->
    BracketInvoke <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Exists x ->
    pure . Exists $ Ident.Name <$> x
  Parse.Set p@(extract -> Parse.Name x) e2 -> -- Only Pat.Name implemented
    Set (Ident.Name x <$ p) <$> rewriteExp e2
  Parse.Tuple es ->
    Tuple <$> traverse rewriteExp es
  Parse.Truth e ->
    Truth <$> rewriteExp e
  Parse.Option e' -> do
    x <- (e $>) . Ident.Label <$> supply
    e' <- rewriteExp e'
    pure $ IfThenElse (infixColonEqual False x e') (Name <$> x) (Tuple [] <$ e)
  Parse.True ->
    pure $ Truth (Tuple [] <$ e)
  Parse.False ->
    pure $ Tuple []
  Parse.Int x ->
    pure $ Int x
  Parse.Float x ->
    pure $ Float x
  Parse.InfixColonEqual p e ->
    rewriteDef p =<< rewriteExp e
  Parse.Fun e1 e2 ->
    Fun <$> rewriteExp e1 <*> rewriteExp e2
  Pat p ->
    rewritePat p
  _ -> error "rewriteExp FIXME"

rewritePat
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => Pat f Name
  -> m (Exp f Ident)
rewritePat = \ case
  Parse.Name x -> pure . Name $ Ident.Name x
  Var x e -> do
    let x' = Ident.Name <$> x
    y <- (e $>) . Ident.Label <$> supply
    e <- rewriteExp e
    let e' = prefixColon $ Name <$> y
    pure $
      MixfixVarColonEqual x' y e $
      ifArchetypeName x' e' e'
  Parse.PrefixColon e -> PrefixColon <$> rewriteExp e
  InfixColon p e -> do
    e <- rewriteExp e
    rewriteDef p $ prefixColon e
  InfixArrow p1 p2 -> do
    p1 <- traverse rewritePat p1
    p2 <- traverse rewritePat p2
    pure $ bracketInvoke2 "operator'->'" p1 p2
  Invoke p e -> do
    p <- traverse rewritePat p
    e <- rewriteExp e
    pure $ ParenInvoke p e
  _ -> error "rewritePat FIXME"

rewriteDef
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => f (Pat f Name)
  -> f (Exp f Ident)
  -> m (Exp f Ident)
rewriteDef p e = case extract p of
  Parse.Name x -> do
    let x' = Ident.Name x <$ p
    pure $
      InfixColonEqual False x' $
      ifArchetypeName x' e e
  Var x e' -> do
    let x' = Ident.Name <$> x
    y <- (e' $>) . Ident.Label <$> supply
    e' <- rewriteExp e'
    pure $
      MixfixVarColonEqual x' y e' $
      ifArchetypeName x' (prefixColon $ Name <$> y) (e `ofType` (Name <$> y))
  Parse.PrefixColon e' -> (e :|>:) <$> rewriteExp e'
  InfixColon (extract -> Invoke p e_domain) e_range -> do
    e_domain <- rewriteExp e_domain
    e_range <- rewriteExp e_range
    rewriteDef' True p
      (fun e_domain $ prefixColon e_range)
      (fun e_domain $ e `ofType` e_range)
  InfixColon p e' -> do
    e' <- rewriteExp e'
    rewriteDef' False p
      (prefixColon e')
      (e `ofType` e')
  InfixArrow p1 p2 -> do
    x1 <- (p1 $>) . Ident.Label <$> supply
    x2 <- (p2 $>) . Ident.Label <$> supply
    e1 <- rewriteDef p1 $ Name <$> x1
    e2 <- rewriteDef p2 $ Name <$> x2
    pure $ List [e1 <$ p1, e2 <$ p2, mixfixArrowColonEqual x1 x2 e]
  Invoke p e_domain -> do
    e_domain <- rewriteExp e_domain
    let e' = fun e_domain e
    rewriteDef' True p e' e'
  _ -> error "rewriteDef FIXME"

rewriteDef'
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => Bool
  -> f (Pat f Name)
  -> f (Exp f Ident)
  -> f (Exp f Ident)
  -> m (Exp f Ident)
rewriteDef' funName p e1 e2 = case extract p of
  Parse.Name x -> do
    let x' = Ident.Name x <$ p
    pure $
      InfixColonEqual funName x' $
      ifArchetypeName x' e1 e2
  Var x e' -> do
    let x' = Ident.Name <$> x
    y <- (e' $>) . Ident.Label <$> supply
    e' <- rewriteExp e'
    pure $
      MixfixVarColonEqual x' y e' $
      ifArchetypeName x' (e1 `ofType` (Name <$> y)) (e2 `ofType` (Name <$> y))
  Parse.PrefixColon e' -> (e2 :|>:) <$> rewriteExp e'
  InfixColon (extract -> Invoke p e_domain) e_range -> do
    e_domain <- rewriteExp e_domain
    e_range <- rewriteExp e_range
    rewriteDef' True p
      (fun e_domain $ e1 `ofType` e_range)
      (fun e_domain $ e2 `ofType` e_range)
  InfixColon p e' -> do
    e' <- rewriteExp e'
    rewriteDef' funName p (e1 `ofType` e') (e2 `ofType` e')
  InfixArrow p1 p2 -> do
    x1 <- (p1 $>) . Ident.Label <$> supply
    x2 <- (p2 $>) . Ident.Label <$> supply
    e1' <- rewriteDef p1 $ Name <$> x1
    let x2' = Name <$> x2
    e2' <- rewriteDef' funName p2 x2' x2'
    pure $ List [e1' <$ p1, e2' <$ p2, mixfixArrowColonEqual x1 x2 e2]
  Invoke p e_domain -> do
    e_domain <- rewriteExp e_domain
    rewriteDef' True p (fun e_domain e1) (fun e_domain e2)
  _ -> error "rewriteDef' FIXME"

rewriteOperator1
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => Name
  -> f (Parse.Exp f Name)
  -> m (Exp f Ident)
rewriteOperator1 x e =
  rewriteExp e <&> \ e ->
  BracketInvoke (Name (Ident.Name x) <$ e) e

rewriteOperator2
  :: (MonadSupply Label m, Apply f, Traversable f, Comonad f)
  => Name
  -> f (Parse.Exp f Name)
  -> f (Parse.Exp f Name)
  -> m (Exp f Ident)
rewriteOperator2 x e1 e2 = bracketInvoke2 x <$> rewriteExp e1 <*> rewriteExp e2

bracketInvoke2 :: Apply f => Name -> f (Exp f Ident) -> f (Exp f Ident) -> Exp f Ident
bracketInvoke2 x e1 e2 =
  BracketInvoke (Name (Ident.Name x) <$ e1 <. e2) (Tuple [e1, e2] <$ e1 <. e2)

infixColonEqual :: Apply f => Bool -> f a -> f (Exp f a) -> f (Exp f a)
infixColonEqual funName = liftL2 $ InfixColonEqual funName

prefixColon :: Apply f => f (Exp f a) -> f (Exp f a)
prefixColon e = PrefixColon e <$ e

mixfixArrowColonEqual
  :: Apply f
  => f a
  -> f a
  -> f (Exp f a)
  -> f (Exp f a)
mixfixArrowColonEqual = liftL3 MixfixArrowColonEqual

fun :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
fun = liftL2 Fun

ifArchetypeName :: Apply f => f a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ifArchetypeName = liftL3 IfArchetypeName

ofType :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ofType = liftL2 (:|>:)

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y

liftL3 :: Apply f => (f a -> f b -> f c -> d) -> f a -> f b -> f c -> f d
liftL3 f x y z = f x y z <$ x <. y <. z
