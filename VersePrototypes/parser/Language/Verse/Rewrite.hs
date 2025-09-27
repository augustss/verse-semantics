{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- TODO: {-# OPTIONS_GHC -Werror -Wall #-}

module Language.Verse.Rewrite
  ( rewrite
  , parenInvokeM
  ) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Comonad
import Control.Monad
import Control.Monad.Supply
import Control.Monad.Wrong

import Data.Bool
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal (c2w)
import Data.Foldable (foldlM)
import Data.Function
import Data.Functor
import Data.Functor.Apply
import Data.Maybe (maybe)
import Data.Text.Encoding qualified as Text
import Data.Traversable
import Data.Tuple
import Data.List

import Language.Verse.Access
import Language.Verse.Effect.Split qualified as Split (Effect)
import Language.Verse.Effect.Split qualified as Effect
import Language.Verse.Error
import Language.Verse.Ident (Ident)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc (L (..), Loc (..), liftL1, liftL2, loc, mkL)
import Language.Verse.Path (Path)
import Language.Verse.Path qualified as Path
import Language.Verse.SimpleName
import Language.Verse.Exp
  ( pattern (:<>:)
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
  , pattern ExpSpecs
  , pattern Pat
  , Pat
  , pattern InfixColon
  , pattern InfixArrow
  , pattern Invoke
  , pattern Specs
  , expToPat
  )
import Language.Verse.Exp qualified as Parse
import Language.Verse.Pos (Pos (..))
import Language.Verse.Rewrite.Exp

import Prelude (Maybe (..), Show (..), String, (==), (+), ($!), error)

import Debug.Trace (trace)

-- TODO: in general this module needs:
-- - a bunch of cleanup. Many cases can be handled recursively rather than directly
-- - more documentation. Where do we rewrite and generate fresh names and why?
rewrite
  :: ( MonadWrong  Error m
     , MonadSupply Label m
     )
  => L (Parse.Exp SimpleName)
  -> m (L (Exp L Ident))
rewrite = rewriteExp

rewriteExp
  :: ( MonadWrong Error m
     , MonadSupply Label m
     )
  => L (Parse.Exp SimpleName)
  -> m (L (Exp L Ident))
rewriteExp expr = for expr $ \case
  (Parse.:=:) e1@(extract -> Pat p@Parse.PrefixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) e1@(extract -> Pat p@InfixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) (extract -> Parse.ExpInfixColon (expToPat -> Just p1) e1) e2 ->
    rewriteDef (Parse.InfixColon <$> duplicate p1 <.> duplicate e1) =<< rewriteExp e2
  (Parse.:=:) (extract -> Parse.ExpSet e@(extract -> IdentName x)) e2 ->
    Set (Ident.Name x <$ e) <$> rewriteExp e2
  Parse.Set e1@(extract -> IdentName x) e2 -> -- Only unqualified names are implemented
    Set (Ident.Name x <$ e1) <$> rewriteExp e2
  (Parse.:=:) e1 e2 ->
    (:=:) <$> rewriteExp e1 <*> rewriteExp e2
  e1 :<>: e2 -> do
    x <- freshIdent $ loc e1
    e1' <- rewriteExp e1
    y <- freshIdent $ loc e2
    e2' <- rewriteExp e2
    pure $ List
      [ infixColonEqual Val x e1'
      , infixColonEqual Val y e2'
      , not' $ unify (name x) (name y)
      ]
  (Parse.:|:) e1 e2 ->
    (:|:) <$> rewriteExp e1 <*> rewriteExp e2
  (Parse.:.:) e (extract -> Parse.IdentName x) ->
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
  PrefixBracket _s e ->
    rewriteOperator1 "prefix'[]'" e
  PrefixQuery e ->
    rewriteOperator1 "prefix'?'" e
  PostfixQuery e ->
    rewriteOperator1 "postfix'?'" e
  Parse.List es ->
    List <$> traverse rewriteExp es
  Parse.Paren e -> do
    e' <- rewriteExp e
    pure $ extract e'
  Parse.Brace e -> do
    e' <- rewriteExp e
    pure $ extract e'
  Parse.Where e1 e2 ->
    Where <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Fail ->
    pure Fail
  Parse.Not e ->
    Not <$> rewriteExp e
  Parse.Struct e ->
    Struct <$> rewriteExp e
  Parse.Class e1 e2 ->
    Class <$> traverse rewriteExp e1 <*> rewriteExp e2
  -- Ignore attributes for now
  Parse.Inst (getMacroParensBraces "class" -> Just (arg, _specs)) e2 ->
    Class <$> traverse rewriteExp arg <*> rewriteExp e2
  -- Ignore attributes for now
  Parse.Inst (getMacroParensBraces "struct" -> Just (Nothing, _specs)) e2 ->
    Struct <$> rewriteExp e2
  Parse.Inst (getMacroParensBraces "function" -> Just (Just e1, specs)) e2 -> do
    e1' <- rewriteExp e1
    (oc, eff) <- getLamSpecs specs
    e2' <- rewriteExp e2
    pure $ Lam e1' oc eff e2'
  -- Ignore attributes for now
  Parse.Inst (getMacroParensBraces "module" -> Just (Nothing, _specs)) e2 ->
    Module <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "type" e1 -> do
    x <- freshIdent $ loc e2
    e2' <- rewriteExp e2
    pure . Lam (infixColonEqual Val x e2') C Effect.Succeeds $ Name <$> x
  Parse.Inst e1 e2 | isPredefined "assume" e1 ->
    Assume <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "fails" e1 ->
    Check Effect.Fails <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "succeeds" e1 ->
    Check Effect.Succeeds <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "decides" e1 ->
    Check Effect.Decides <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "verify" e1 ->
    Verify <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "for" e1 ->
    All <$> rewriteExp e2
  Parse.Inst (extract -> Parse.ParenInvoke e1 e2) e3 | isPredefined "for" e1 ->
    ForDo <$> rewriteExp e2 <*> rewriteExp e3
  Parse.Do (extract -> Parse.Inst e1 e2) e3 | isPredefined "for" e1 ->
    ForDo <$> rewriteExp e2 <*> rewriteExp e3

  Parse.Inst e1 e2 | isPredefined "array" e1 ->
    case e2 of
      L _ (Parse.List es)  -> Array <$> traverse rewriteExp es  -- array{1;2;3}
      L _ (Parse.Tuple es) -> Array <$> traverse rewriteExp es  -- array{1,2,3}
      _                    -> Array . (:[]) <$> rewriteExp e2

  Parse.Inst e1 e2 | isPredefined "one" e1 ->
    One <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "all" e1 ->
    All <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "truth" e1 ->
    Truth <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "domain" e1 ->
    Domain <$> rewriteExp e2

  -- Parse generates these
  Parse.One e2 ->
    One <$> rewriteExp e2
  Parse.All e2 ->
    All <$> rewriteExp e2
  Parse.Option Nothing  -> pure $ Option Nothing
  Parse.Option (Just e) -> Option . Just <$> rewriteExp e

  Parse.Inst (extract -> ExpSpecs a bs) e2 | isPredefined "check" a -> do
    let find_eff x | isPredefined "succeeds" x = Effect.Succeeds
                   | isPredefined "fails"    x = Effect.Fails
                   | isPredefined "decides"  x = Effect.Decides
                   | otherwise = error $ "expSpecs with unknown effect: " ++ show x
        eff = head $ find_eff <$> bs
    Check eff <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "option" e1 -> do
    x <- freshIdent $ loc expr
    e' <- rewriteExp e2
    pure $ IfThenElse (infixColonEqual Val x e') (Truth (Name <$> x) <$ x) (Tuple [] <$ expr)
  Parse.Inst e1 e2 ->
    Inst <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Enum _attributes xs -> -- Ignore attributes
    pure $ Enum (map (extract . snd) xs) -- Ignore attributes
  If e -> do
    e' <- rewriteExp e
    pure $ IfThenElse e' (Tuple [] <$ expr) (Tuple [] <$ expr)
  IfThen e1 e2 -> do
    e1' <- rewriteExp e1
    e2' <- rewriteExp e2
    pure $ IfThenElse e1' e2' (Tuple [] <$ expr)
  IfElse e1 e2 -> do
    e1' <- rewriteExp e1
    e2' <- rewriteExp e2
    pure $ IfThenElse e1' (Tuple [] <$ expr) e2'
  Parse.IfThenElse e1 e2 e3 ->
    IfThenElse <$> rewriteExp e1 <*> rewriteExp e2 <*> rewriteExp e3
  For e ->
    All <$> rewriteExp e
  Parse.Do (extract -> For e1) e2 ->
    ForDo <$> rewriteExp e1 <*> rewriteExp e2
  Parse.ForDo e1 e2 ->
    ForDo <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Block e ->
    Block <$> rewriteExp e
  Parse.ParenInvoke e1 e2 ->
    ParenInvoke <$> rewriteExp e1 <*> rewriteExp e2
  Parse.BracketInvoke e1 e2 ->
    BracketInvoke <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Exists nms' body' -> do
    let nms = fmap Ident.Name <$> nms'
    body <- rewriteExp body'
    pure $ Exists nms body
  Parse.Forall x ->
    pure . Forall $ Ident.Name <$> x
  Parse.Tuple es ->
    Tuple <$> traverse rewriteExp es
  Parse.True ->
    pure $ Truth (Tuple [] <$ expr)
  Parse.False ->
    pure $ Tuple []
  Parse.Int x ->
    pure $ Int x
  Parse.Float x ->
    pure $ Float x
  Parse.Char x ->
    pure $ Char $ c2w x
  Parse.Char32 x ->
    pure $ Char32 x
  Parse.String txt [] -> case loc expr of
    Loc p _ ->
      pure .
      Tuple .
      map (\ (i, x) -> L (Loc p { column = column p + i } p { column = column p + i + 1 }) (Char x)) .
      zip [1 ..] .
      ByteString.unpack $
      Text.encodeUtf8 txt
  e@(Parse.String _txt _txts) ->
    notImplemented "rewriteExp on string with {}" e

  -- TODO: special case for tuples
  -- remove this and handle this case through recursion
  Parse.InfixColonEqual
    (expToPat -> Just l@(extract -> Parse.PatTuple{}))
    r@(extract -> Parse.Tuple{}) -> do
      r' <- rewriteExp r
      rewriteDef l r'
  Parse.InfixColonEqual l r ->
    case expToPat l of
      Just p  -> rewrite r >>= rewriteDef p
      -- HACK: See #86: This matches (a,b,c) := (1,2,3) which shows up in the
      -- test cases in versetest. This case is a hack because according to
      -- verse-spec a Tuple should be a pattern. So the ast should be
      -- ((InfixColonEqual (Tuple ...) (Tuple ...)). But that would mean adding
      -- 'Tuple' to Parser.Exp.Pat and then modifying the parser to accomodate
      -- that pattern. Instead of doing that (the real fix) we use this case for
      -- the time being.

      -- Followup: this is implementing the same mechanims as the expToPat just
      -- badly by not defining the Pat. So expToPat defines the pats allowable
      -- on the lhs. Well here I didn't properly define the tuple pat so
      -- expToPat returns an Nothing. Meaning that its not a legal pat but then
      -- I catch this special case with tuples. So this case should really be
      -- the body of this case in expToPat.

      -- more followup. I was tracing the rewrite of this expr '{i->x := 2; (i,
      -- x)}', we end up in this case due to the tuples that now include splicing.
      Nothing -> case (extract l, extract r) of
        -- we expect ls to all be idents
        (Parse.Tuple ls, Parse.Tuple rs) -> do
          -- START: Its wrong here. expToPat should be catching the tuple
          let -- get_idents (Parse.Pat (Parse.Name (Parse.IdentName n))) = n
              binders :: [L (Parse.Exp SimpleName)]
              binders = ls
              inner :: [L (Parse.Exp SimpleName)]
              inner = zipWith (\a b -> mkL (loc expr) $ Parse.InfixColonEqual a b) binders rs
              new :: L (Parse.Exp SimpleName)
              new = mkL (loc expr) $ Parse.List inner
          res <- rewrite new
          return $ extract res
        -- TODO: see #86
        -- TODO: ifArchetypeName for the splice
        -- implment this one
        (Parse.PrefixDotDot (extract -> Pat p), _) -> do
          nme <- rewritePat p
          let go = case nme of
                Name n -> n
                _      -> error "prefixDotDot not before ident"
          r'  <- rewriteExp r
          return $ InfixColonEqual Public Val (go <$ l) r'
        (Parse.PrefixDotDot _, _) -> Splice <$> rewriteExp l
        s -> notImplemented "rewriteEXXX" s

  Parse.Lam e1 e2 -> do
    e1' <- rewriteExp e1
    e2' <- rewriteExp e2
    pure $ Lam e1' C Effect.Succeeds e2'
  -- Try to fix Parse2 so that we can get rid of this
  Parse.ExpInfixColon (Parse.expToPat -> Just pat) e2 ->
    rewritePat $ Parse.InfixColon pat e2
  -- TODO: not sure if this is needed?
  Parse.PrefixDotDot e ->
    Splice <$> rewriteExp e
  Pat p ->
    rewritePat p

  -- Not implemented yet
  e@Parse.And{}                   -> notImplemented "rewriteExp" e
  e@Parse.Or{}                    -> notImplemented "rewriteExp" e
  e@Parse.Array{}                 -> notImplemented "rewriteExp" e
  e@Parse.Break{}                 -> notImplemented "rewriteExp" e
  e@Parse.Catch{}                 -> notImplemented "rewriteExp" e
  e@Parse.Continue{}              -> notImplemented "rewriteExp" e
  e@Parse.Fails{}                 -> notImplemented "rewriteExp" e
  e@Parse.Units{}                 -> notImplemented "rewriteExp" e
  e@Parse.InfixDivideEqual{}      -> notImplemented "rewriteExp" e
  e@Parse.InfixMinusEqual{}       -> notImplemented "rewriteExp" e
  e@Parse.InfixMultiplyEqual{}    -> notImplemented "rewriteExp" e
  e@Parse.InfixPlusEqual{}        -> notImplemented "rewriteExp" e
  e@Parse.Module{}                -> notImplemented "rewriteExp" e
  e@Parse.PostfixCaret{}          -> notImplemented "rewriteExp" e
  e@Parse.PrefixCaret{}           -> notImplemented "rewriteExp" e
  e@Parse.Option{}                -> notImplemented "rewriteExp" e
  e@Parse.PrefixMultiply{}        -> notImplemented "rewriteExp" e
  e@Parse.PrefixAmpersand{}       -> notImplemented "rewriteExp" e
  e@Parse.Return{}                -> notImplemented "rewriteExp" e
  e@Parse.ExpVar{}                -> notImplemented "rewriteExp" e
  e@Parse.ExpSet{}                -> notImplemented "rewriteExp" e
  e@Parse.ExpRef{}                -> notImplemented "rewriteExp" e
  e@Parse.ExpAlias{}              -> notImplemented "rewriteExp" e
  e@Parse.SetInfixDivideEqual{}   -> notImplemented "rewriteExp" e
  e@Parse.SetInfixMinusEqual{}    -> notImplemented "rewriteExp" e
  e@Parse.SetInfixMultiplyEqual{} -> notImplemented "rewriteExp" e
  e@Parse.AtSpec{}                -> notImplemented "rewriteExp" e
  e@Parse.SpecAt{}                -> notImplemented "rewriteExp" e
  e@Parse.Truth{}                 -> notImplemented "rewriteExp" e
  e@Parse.SetInfixPlusEqual{}     -> notImplemented "rewriteExp" e
  e@Parse.Until{}                 -> notImplemented "rewriteExp" e
  e@Parse.Yield{}                 -> notImplemented "rewriteExp" e
  e@Parse.Next{}                  -> notImplemented "rewriteExp" e
  e@Parse.Over{}                  -> notImplemented "rewriteExp" e
  e@Parse.When{}                  -> notImplemented "rewriteExp" e
  e@Parse.While{}                 -> notImplemented "rewriteExp" e
  e@Parse.Is{}                    -> notImplemented "rewriteExp" e
  e@Parse.Set{}                   -> notImplemented "rewriteExp" e
  e@(_ Parse.:.: _)               -> notImplemented "rewriteExp" e
  e@Parse.Do{}                    -> notImplemented "rewriteExp" e
  e@Parse.ExpInfixColon{}         -> notImplemented "rewriteExp" e
  e@ExpSpecs{}                    -> notImplemented "rewriteExp" e

-- Rewrite a pattern into the Rewrite.Exp language. Rewrite.Exp does not have
-- any notion of pattern so this does some desugaring as well.
rewritePat
  :: (MonadWrong Error m, MonadSupply Label m)
  => Pat SimpleName
  -> m (Exp L Ident)
rewritePat = \ case
  Parse.Name (Parse.IdentName x) -> pure . Name $ Ident.Name x
  Parse.Name (Parse.IdentQualName [e] (extract -> y)) -> do
    e' <- rewriteExp e
    pure $ QualName e' y
  Parse.Name (Parse.IdentPath path) -> pure . Path $ rewritePath path
  -- Dropping specs after var for now
  InfixColon (extract -> Parse.Var _e1 i@(extract -> Parse.IdentName x) e2) e -> do
    access <- getDefSpecs e2
    Alloc2 access (Ident.Name x <$ i) <$> rewriteExp e
  Parse.PrefixColon e -> PrefixColon <$> rewriteExp e
  Parse.PatTuple xs  -> do
    xs' <- traverse (\(L l x) -> L l <$> rewritePat x) xs
    return $ Tuple xs'
  Parse.PatSplice p@(extract -> e) -> do
    e' <- rewritePat e
    pure . Splice $ e' <$ p
  InfixColon p e -> do
    e' <- rewriteExp e
    rewriteDef p $ prefixColon e'
  InfixArrow p1 p2 -> do
    p1' <- traverse rewritePat p1
    p2' <- traverse rewritePat p2
    pure $ bracketInvoke2 "operator'->'" p1' p2'
  Invoke p e -> do
    e1 <- traverse rewritePat p
    e2 <- rewriteExp e
    parenInvokeM e1 e2
  e -> notImplemented "rewritePat" e

rewritePath
  :: Parse.Path SimpleName
  -> Path
rewritePath = \ case
  Parse.Path label pathIdents ->
    Path.Path (extract label) $ pathIdents <&> \ (qualPath, ident) ->
      (rewritePath <$> qualPath, extract ident)

-- START: start adding more connectives
-- | rewrite a definition form. These are terms like t0 := t1. The binding
-- connective is assumed by passing the lhs (called 'pat') and the rhs to this
-- function.
rewriteDef
  :: ( MonadWrong Error m
     , MonadSupply Label m
     )
  => L (Pat SimpleName)
  -> L (Exp L Ident)
  -> m (Exp L Ident)
rewriteDef pat rhs = case extract pat of
  (stripSpecs -> (Parse.Name (Parse.IdentName x), specs)) -> do
    let x' = Ident.Name x <$ pat
    access <- getDefSpecs specs
    pure $
      InfixColonEqual access Val x' $
      ifArchetypeName x' rhs rhs
  InfixColon (extract -> Parse.Var [] i@(extract -> Parse.IdentName x) specs) e -> do
    let x' = Ident.Name x <$ i
    access <- getDefSpecs specs
    e' <- rewriteExp e
    pure $ IfArchetypeName x' (alloc2 access x' e') (alloc3 access x' e' rhs)
  Parse.PrefixColon e' -> (rhs `OfType`) <$> rewriteExp e'
  Parse.PatSplice (extract -> Parse.Name (Parse.IdentName x)) -> do
    let x' = Ident.Name x <$ pat
    pure $ InfixColonEqual Public Var x' rhs
  Parse.PatSplice ss -> rewriteDef ss rhs
  Parse.PatTuple es -> do
    let
      -- TODO: Better note
      -- we know that the lhs is <= the cardinality of the rhs but also we must
      -- be able to match things like (a,..b,c) := (1,2,3,4,5) and yield a := 1;
      -- b:= (2,3,4); c:= 5 this means we scan from left to right when we have
      -- an Ident like 'a', we assign it to the rhs corresponding element. When
      -- we have a pattern like b, we pause, reverse the ls and the rs and start
      -- processing again until we just have singletons that is the pattern b
      -- and the tuple it should be bound to

      splice_zip ls_ rs_ = go ls_ rs_
        where
          mk_binder a b = InfixColonEqual Public Val a b <$ rhs

          go [] [] = return []
          go [(extract -> Parse.PatSplice i@(extract -> Parse.Name (Parse.IdentName x)))] ys = -- base
            pure [mk_binder (Ident.Name x <$ i) (Tuple ys <$ rhs)]
          go [i@(extract -> Parse.PatSplice{} )] ys = do -- base 2
            res <- rewriteDef i (List ys <$ rhs)
            return $ [res <$ i]
          go xs@((extract -> Parse.PatSplice{}):_) ys  = -- flip case
            go (reverse xs) (reverse ys)
          go (x:xs)  (y:ys) = do                         -- something else
            res <- rewriteDef x y
            rest <- splice_zip xs ys
            return $ (res <$ x) : rest

      -- TODO: remove this unsafe match
      (Tuple rhss) = extract rhs
    List <$> splice_zip es rhss

  InfixColon (extract -> stripSpecs -> (Invoke p e_domain, specs)) e_range -> do
    e_domain' <- rewriteExp e_domain
    (oc, eff) <- getLamSpecs specs
    e_range' <- rewriteExp e_range
    rewriteDef' Fun p
      (lam e_domain' oc eff $ prefixColon e_range')
      (lam e_domain' oc eff $ rhs `ofType` e_range')
  InfixColon p e -> do
    e' <- rewriteExp e
    rewriteDef' Val p
      (prefixColon e')
      (rhs `ofType` e')
  InfixArrow p1 p2 -> do
    x1 <- freshIdent $ loc p1
    x2 <- freshIdent $ loc p2
    e1 <- rewriteDef p1 $ Name <$> x1
    e2 <- rewriteDef p2 $ Name <$> x2
    pure $ List [e1 <$ p1, e2 <$ p2, mixfixArrowColonEqual x1 x2 rhs]
  (stripSpecs -> (Invoke p e_domain, specs)) -> do
    e_domain' <- rewriteExp e_domain
    (oc, eff) <- getLamSpecs specs
    let e' = lam e_domain' oc eff rhs
    rewriteDef' Fun p e' e'
  e -> notImplemented "rewriteDef" e

rewriteDef'
  :: (MonadWrong Error m, MonadSupply Label m)
  => Quantifier
  -> L (Pat SimpleName)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> m (Exp L Ident)
rewriteDef' q pat e1 e2 = case extract pat of
  (stripSpecs -> (Parse.Name (Parse.IdentName x), specs)) -> do
    let x' = Ident.Name x <$ pat
    access <- getDefSpecs specs
    pure $
      InfixColonEqual access q x' $
      ifArchetypeName x' e1 e2
  InfixColon (extract -> Parse.Var [] i@(extract -> Parse.IdentName x) specs) e -> do
    let x' = Ident.Name x <$ i
    access <- getDefSpecs specs
    e' <- rewriteExp e
    pure $ IfArchetypeName x' (alloc2 access x' e1) (alloc3 access x' e2 e')
  Parse.PrefixColon e' -> (e2 `OfType`) <$> rewriteExp e'
  InfixColon (extract -> stripSpecs -> (Invoke p e_domain, specs)) e_range -> do
    e_domain' <- rewriteExp e_domain
    (oc, eff) <- getLamSpecs specs
    e_range' <- rewriteExp e_range
    rewriteDef' Fun p
      (lam e_domain' oc eff $ e1 `ofType` e_range')
      (lam e_domain' oc eff $ e2 `ofType` e_range')
  InfixColon p e -> do
    e' <- rewriteExp e
    rewriteDef' q p
      (e1 `ofType` e')
      (e2 `ofType` e')
  InfixArrow p1 p2 -> do
    x1 <- freshIdent $ loc p1
    x2 <- freshIdent $ loc p2
    e1' <- rewriteDef p1 $ Name <$> x1
    let x2' = Name <$> x2
    e2' <- rewriteDef' q p2 x2' x2'
    pure $ List [e1' <$ p1, e2' <$ p2, mixfixArrowColonEqual x1 x2 e2]
  (stripSpecs -> (Invoke p e_domain, specs)) -> do
    e_domain' <- rewriteExp e_domain
    (oc, eff) <- getLamSpecs specs
    rewriteDef' Fun p
      (lam e_domain' oc eff e1)
      (lam e_domain' oc eff e2)
  e -> notImplemented "rewriteDef'" e

notImplemented :: (MonadWrong Error m, Show a) => String -> a -> m b
notImplemented fun e = wrong $ NotImplemented $ fun ++ " on: " ++ show e

getMacroParensBraces
  :: SimpleName
  -> L (Parse.Exp SimpleName)
  -> Maybe (Maybe (L (Parse.Exp SimpleName)), [L (Parse.Exp SimpleName)])
getMacroParensBraces macro = \ case
  (extract -> stripExpSpecs -> (Parse.ParenInvoke (extract -> IdentName nme) arg, specs))
    | nme == macro -> Just (Just arg, specs)
  (extract -> stripExpSpecs -> (IdentName nme, specs))
    | nme == macro -> Just (Nothing, specs)
  _ -> Nothing

isPredefined :: SimpleName -> L (Parse.Exp SimpleName)  -> Bool
isPredefined predefined (extract -> IdentName nme) = nme == predefined
isPredefined _predefined _ = False

stripExpSpecs :: Parse.Exp SimpleName -> (Parse.Exp SimpleName, [L (Parse.Exp SimpleName)])
stripExpSpecs = \ case
  ExpSpecs (extract -> stripExpSpecs -> (exp', specs')) specs -> (exp', specs ++ specs')
  Pat (stripSpecs -> (pat, specs)) -> (Pat pat, specs)
  exp -> (exp, [])

stripSpecs :: Parse.Pat SimpleName -> (Parse.Pat SimpleName, [L (Parse.Exp SimpleName)])
stripSpecs = \ case
  Specs (extract -> stripSpecs -> (pat', specs')) specs -> (pat', specs ++ specs')
  pat -> (pat, [])

rewriteOperator1
  :: (MonadWrong Error m, MonadSupply Label m)
  => SimpleName
  -> L (Parse.Exp SimpleName)
  -> m (Exp L Ident)
rewriteOperator1 x e =
  rewriteExp e <&> \ e' ->
  BracketInvoke (Name (Ident.Name x) <$ e') e'

rewriteOperator2
  :: (MonadWrong Error m, MonadSupply Label m)
  => SimpleName
  -> L (Parse.Exp SimpleName)
  -> L (Parse.Exp SimpleName)
  -> m (Exp L Ident)
rewriteOperator2 x e1 e2 = bracketInvoke2 x <$> rewriteExp e1 <*> rewriteExp e2

getLamSpecs
  :: MonadWrong Error m
  => [L (Parse.Exp SimpleName)]
  -> m (OC, Split.Effect)
getLamSpecs = wrap $ \ case
  ((Nothing, z), y@(extract -> IdentName "open")) ->
    pure (Just $ O <$ y, z)
  ((Just x, _), y@(extract -> IdentName "open")) ->
    wrong $ OpenClosedError (loc x) (loc y)
  ((Nothing, z), y@(extract -> IdentName "closed")) ->
    pure (Just $ C <$ y, z)
  ((Just x, _), y@(extract -> IdentName "closed")) ->
    wrong $ OpenClosedError (loc x) (loc y)
  ((z, Nothing), y@(extract -> IdentName "fails")) ->
    pure (z, Just $ Effect.Fails <$ y)
  ((_, Just x), y@(extract -> IdentName "fails")) ->
    wrong $ SplitEffectError (loc x) (loc y)
  ((z, Nothing), y@(extract -> IdentName "succeeds")) ->
    pure (z, Just $ Effect.Succeeds <$ y)
  ((_, Just x), y@(extract -> IdentName "succeeds")) ->
    wrong $ SplitEffectError (loc x) (loc y)
  ((z, Nothing), y@(extract -> IdentName "decides")) ->
    pure (z, Just $ Effect.Decides <$ y)
  ((_, Just x), y@(extract -> IdentName "decides")) ->
    wrong $ SplitEffectError (loc x) (loc y)
  (_, y) -> wrong $ SpecError $ loc y
  where
    wrap f =
      fmap (maybe O extract *** maybe Effect.Succeeds extract) .
      foldlM (curry f) (Nothing, Nothing)

getDefSpecs
  :: MonadWrong Error m
  => [L (Parse.Exp SimpleName)]
  -> m Access
getDefSpecs = wrap $ \ case
  (x, y@(extract -> IdentName "public")) -> add x Public y
  (x, y@(extract -> IdentName "protected")) -> add x Protected y
  (x, y@(extract -> IdentName "private")) -> add x Private y
  (x, y@(extract -> IdentName "internal")) -> add x Internal y
  (_, y) -> wrong $ SpecError $ loc y
  where
    wrap f =
      fmap (maybe Internal extract) .
      foldlM (curry f) Nothing
    add Nothing access y = pure $! (Just $! access <$ y)
    add (Just x) _access y = wrong $ MultipleAccessError (loc x) (loc y)

pattern IdentName :: a -> Parse.Exp a
pattern IdentName x = Parse.Pat (Parse.Name (Parse.IdentName x))

parenInvokeM
  :: (MonadSupply Label m)
  => L (Exp L Ident)
  -> L (Exp L Ident)
  -> m (Exp L Ident)
parenInvokeM e1 e2 = do
  x1 <- freshIdent $ loc e1
  x2 <- freshIdent $ loc e2
  pure $ List
    [ infixColonEqual Val x1 e1
    , infixColonEqual Val x2 e2
    , check Effect.Succeeds $ bracketInvoke (Name <$> x1) (Name <$> x2)
    ]

freshIdent :: MonadSupply Label m => Loc -> m (L Ident)
freshIdent lc = L lc . Ident.Label <$> supply

unify :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
unify = liftL2 (:=:)

not' :: Functor f => f (Exp f a) -> f (Exp f a)
not' = liftL1 Not

check :: Functor f => Split.Effect -> f (Exp f a) -> f (Exp f a)
check = liftL1 . Check

bracketInvoke2
  :: Apply f
  => SimpleName
  -> f (Exp f Ident)
  -> f (Exp f Ident)
  -> Exp f Ident
bracketInvoke2 x e1 e2 =
  BracketInvoke (Name (Ident.Name x) <$ e1 <. e2) (Tuple [e1, e2] <$ e1 <. e2)

lam
  :: Apply f
  => f (Exp f a)
  -> OC
  -> Split.Effect
  -> f (Exp f a)
  -> f (Exp f a)
lam e1 oc eff e2 = Lam e1 oc eff e2 <$ e1 <. e2
