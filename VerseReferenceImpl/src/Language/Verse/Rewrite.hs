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
import Control.Monad.Abort
import Control.Monad.Supply

import Data.Bool
import Data.Function
import Data.Functor
import Data.Functor.Apply
import Data.Traversable
import Data.Text.Encoding qualified as Text
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal(w2c)

import Language.Verse.Error
import Language.Verse.Ident (Ident)
import Language.Verse.Ident qualified as Ident
import Language.Verse.Label
import Language.Verse.Loc(L(..), Loc(..), loc)
import Language.Verse.Pos(Pos(..))
import Language.Verse.Name
import Language.Verse.Parse.Exp
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
  , pattern Pat
  , Pat
  , pattern InfixColon
  , pattern InfixArrow
  , pattern Invoke
  , expToPat
  )
import Language.Verse.Parse.Exp qualified as Parse
import Language.Verse.Rewrite.Exp

import Prelude (Maybe (..), Show (..), String, (==), (++), map, snd, (+), zip)

rewrite
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Parse.Exp Name)
  -> m (L (Exp L Ident))
rewrite = rewriteExp

rewriteExp
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Parse.Exp Name)
  -> m (L (Exp L Ident))
rewriteExp e = for e $ \ case
  (Parse.:=:) e1@(extract -> Pat p@Parse.PrefixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) e1@(extract -> Pat p@InfixColon {}) e2 ->
    rewriteDef (p <$ e1) =<< rewriteExp e2
  (Parse.:=:) (extract -> Parse.ExpInfixColon (expToPat -> Just p1) e1) e2 ->
    rewriteDef (Parse.InfixColon <$> duplicate p1 <.> duplicate e1) =<< rewriteExp e2
  (Parse.:=:) (extract -> Parse.ExpSet e@(extract -> Parse.Pat (Parse.Name (Parse.IdentName x)))) e2 ->
    Set (Ident.Name x <$ e) <$> rewriteExp e2
  Parse.Set e@(extract -> Parse.Pat (Parse.Name (Parse.IdentName x))) e2 -> -- Only unqualified names are implemented
    Set (Ident.Name x <$ e) <$> rewriteExp e2
  (Parse.:=:) e1 e2 ->
    (:=:) <$> rewriteExp e1 <*> rewriteExp e2
  e1 :<>: e2 ->
    Not . (e $>) <$> ((:=:) <$> rewriteExp e1 <*> rewriteExp e2)
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
    e <- rewriteExp e
    pure $ extract e
  Parse.Brace e -> do
    e <- rewriteExp e
    pure $ extract e
  Parse.Where e1 e2 ->
    Where <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Fail ->
    pure Fail
  Parse.Not e ->
    Not <$> rewriteExp e
  Parse.Fails e ->
    Fails <$> rewriteExp e
  Parse.Struct e ->
    Struct <$> rewriteExp e
  Parse.Class e1 e2 ->
    Class <$> traverse rewriteExp e1 <*> rewriteExp e2
  Parse.Inst _e1@(isMacroParensBraces "class" -> Just (arg, _attributes)) e2 ->   -- Ignore attributes for now
    Class <$> traverse rewriteExp arg <*> rewriteExp e2
  Parse.Inst _e1@(isMacroParensBraces "struct"  -> Just (Nothing, _attributes)) e2 ->   -- Ignore attributes for now
    Struct <$> rewriteExp e2
  Parse.Inst _e1@(isMacroParensBraces "function" -> Just (Just e1, [])) e2 ->
    Fun <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Inst _e1@(isMacroParensBraces "module"  -> Just (Nothing, _attributes)) e2 ->   -- Ignore attributes for now
    Module <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "decides" e1 ->
    Decides <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "assume" e1 ->
    Assume <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "succeeds" e1 ->
    Succeeds <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "fails" e1 ->
    Fails <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "verify" e1 ->
    Verify <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "for" e1 ->
    All <$> rewriteExp e2
  Parse.Inst (extract -> Parse.ParenInvoke e1 e2) e3 | isPredefined "for" e1 ->
    ForDo <$> rewriteExp e2 <*> rewriteExp e3
  Parse.Do (extract -> Parse.Inst e1 e2) e3 | isPredefined "for" e1 ->
    ForDo <$> rewriteExp e2 <*> rewriteExp e3

  -- Parse.Inst e1 e2 | isPredefined "array" e1 ->  -- Map array to tuple
  --   Tuple <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "one" e1 ->
    One <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "all" e1 ->
    All <$> rewriteExp e2
  Parse.Inst e1 e2 | isPredefined "truth" e1 ->
    Truth <$> rewriteExp e2

  -- Parse generates these
  Parse.One e2 ->
    One <$> rewriteExp e2
  Parse.All e2 ->
    All <$> rewriteExp e2

  Parse.Inst e1 e2 | isPredefined "option" e1 -> do
    x <- (e $>) . Ident.Label <$> supply
    e' <- rewriteExp e2
    pure $ IfThenElse (infixColonEqual False x e') (Name <$> x) (Tuple [] <$ e)
  Parse.Inst e1 e2 ->
    Inst <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Enum _attributes xs -> -- Ignore attributes
    pure $ Enum (map (extract . snd) xs) -- Ignore attributes
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
  Parse.Do (extract -> For e1) e2 ->
    ForDo <$> rewriteExp e1 <*> rewriteExp e2
  Parse.ForDo e1 e2 ->
    ForDo <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Block e ->
    Block <$> rewriteExp e
  Parse.ParenInvoke e1 e2 -> do
    e1 <- rewriteExp e1
    e2 <- rewriteExp e2
    parenInvokeM e1 e2
  Parse.BracketInvoke e1 e2 ->
    BracketInvoke <$> rewriteExp e1 <*> rewriteExp e2
  Parse.Exists x ->
    pure . Exists $ Ident.Name <$> x
  Parse.Forall x ->
    pure . Forall $ Ident.Name <$> x
  Parse.Tuple es ->
    Tuple <$> traverse rewriteExp es
  Parse.True ->
    pure $ Truth (Tuple [] <$ e)
  Parse.False ->
    pure $ Tuple []
  Parse.Int x ->
    pure $ Int x
  Parse.Float x ->
    pure $ Float x
  Parse.Char x ->
    pure $ Char x
  Parse.Char32 x ->
    pure $ Char32 x
  Parse.String txt [] ->
    case loc e of
      Loc p _ ->
        pure $ Tuple $ map (\ (i,x) -> L (Loc p{column = column p + i} p{column = column p + i + 1}) (Char $ w2c x)) $ zip [1..] $ ByteString.unpack $ Text.encodeUtf8 txt
  e@(Parse.String _txt _txts) ->
    notImplemented "rewriteExp on string with {}" e
  Parse.InfixColonEqual (expToPat -> Just p) e ->
    rewriteDef p =<< rewriteExp e
  Parse.Fun e1 e2 ->
    Fun <$> rewriteExp e1 <*> rewriteExp e2
  Pat p ->
    rewritePat p
  Parse.ExpInfixColon (Parse.expToPat -> Just pat) e2 ->  -- Try to fix Parse2 so that we can get rid of this
    rewritePat $ Parse.InfixColon pat e2
  e -> notImplemented "rewriteExp" e

notImplemented :: (MonadAbort Error m, Show a) => String -> a -> m b
notImplemented fun e = abort $ NotImplemented $ fun ++ " on: " ++ show e


isMacroParensBraces :: Name -> L (Parse.Exp Name)  -> Maybe (Maybe (L (Parse.Exp Name)), [L (Parse.Exp Name)])
isMacroParensBraces  macro (extract -> Parse.ParenInvoke (extract -> Parse.Pat (Parse.Name (Parse.IdentName name))) args) | name == macro = Just (Just args, [])
isMacroParensBraces  macro (stripSpecs -> (_inner@(extract -> Parse.Pat _pat@(Parse.Name (Parse.IdentName name))), specs)) | name == macro = Just (Nothing, specs)
isMacroParensBraces _macro  _ = Nothing

isPredefined :: Name -> L (Parse.Exp Name)  -> Bool
isPredefined predefined _exp@(extract -> Parse.Pat _pat@(Parse.Name (Parse.IdentName name))) = name == predefined
isPredefined _predefined _exp = False

stripSpecs :: L (Parse.Exp Name)  -> (L (Parse.Exp Name), [L (Parse.Exp Name)])
stripSpecs (extract -> Parse.ExpSpecs exp specs) = case stripSpecs exp of
                                                  (exp', specs') -> (exp', specs ++ specs')
stripSpecs exp = (exp, [])

rewritePat
  :: (MonadAbort Error m, MonadSupply Label m)
  => Pat Name
  -> m (Exp L Ident)
rewritePat = \ case
  Parse.Name (Parse.IdentName x) -> pure . Name $ Ident.Name x
  InfixColon (extract -> Parse.Var _ p@(extract -> Parse.IdentName x)) e -> do -- ignore attributes
     let x' = Ident.Name <$> (x <$ p)
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
    e1 <- traverse rewritePat p
    e2 <- rewriteExp e
    parenInvokeM e1 e2
  e -> notImplemented "rewritePat" e


rewriteDef
  :: (MonadAbort Error m, MonadSupply Label m)
  => L (Pat Name)
  -> L (Exp L Ident)
  -> m (Exp L Ident)
rewriteDef p e = case extract p of
  Parse.Name (Parse.IdentName x) -> do
    let x' = Ident.Name x <$ p
    pure $
      InfixColonEqual False x' $
      ifArchetypeName x' e e
  InfixColon (extract -> Parse.Var _ p@(extract -> Parse.IdentName x)) e' -> do -- ignore attributes
    let x' = Ident.Name <$> (x <$ p)
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
  e -> notImplemented "rewriteDef" e

rewriteDef'
  :: (MonadAbort Error m, MonadSupply Label m)
  => Bool
  -> L (Pat Name)
  -> L (Exp L Ident)
  -> L (Exp L Ident)
  -> m (Exp L Ident)
rewriteDef' funName p e1 e2 = case extract p of
  Parse.Name (Parse.IdentName x) -> do
    let x' = Ident.Name x <$ p
    pure $
      InfixColonEqual funName x' $
      ifArchetypeName x' e1 e2
  InfixColon (extract -> Parse.Var _ p@(extract -> Parse.IdentName x)) e' -> do -- ignore attributes
   let x' = Ident.Name <$> (x <$ p)
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
  e -> notImplemented "rewriteDef'" e

rewriteOperator1
  :: (MonadAbort Error m, MonadSupply Label m)
  => Name
  -> L (Parse.Exp Name)
  -> m (Exp L Ident)
rewriteOperator1 x e =
  rewriteExp e <&> \ e ->
  BracketInvoke (Name (Ident.Name x) <$ e) e

rewriteOperator2
  :: (MonadAbort Error m, MonadSupply Label m)
  => Name
  -> L (Parse.Exp Name)
  -> L (Parse.Exp Name)
  -> m (Exp L Ident)
rewriteOperator2 x e1 e2 = bracketInvoke2 x <$> rewriteExp e1 <*> rewriteExp e2

parenInvokeM
  :: (MonadSupply Label m, Apply f)
  => f (Exp f Ident)
  -> f (Exp f Ident)
  -> m (Exp f Ident)
parenInvokeM e1 e2 = do
  x1 <- (e1 $>) . Ident.Label <$> supply
  x2 <- (e2 $>) . Ident.Label <$> supply
  pure $ List
    [ infixColonEqual False x1 e1
    , infixColonEqual False x2 e2
    , succeeds $ bracketInvoke (Name <$> x1) (Name <$> x2)
    ]

bracketInvoke2 :: Apply f => Name -> f (Exp f Ident) -> f (Exp f Ident) -> Exp f Ident
bracketInvoke2 x e1 e2 =
  BracketInvoke (Name (Ident.Name x) <$ e1 <. e2) (Tuple [e1, e2] <$ e1 <. e2)

bracketInvoke :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
bracketInvoke = liftL2 BracketInvoke

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

succeeds :: Functor f => f (Exp f a) -> f (Exp f a)
succeeds = liftL1 Succeeds

ifArchetypeName :: Apply f => f a -> f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ifArchetypeName = liftL3 IfArchetypeName

ofType :: Apply f => f (Exp f a) -> f (Exp f a) -> f (Exp f a)
ofType = liftL2 (:|>:)

liftL1 :: Functor f => (f a -> b) -> f a -> f b
liftL1 f x = f x <$ x

liftL2 :: Apply f => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f x y = f x y <$ x <. y

liftL3 :: Apply f => (f a -> f b -> f c -> d) -> f a -> f b -> f c -> f d
liftL3 f x y z = f x y z <$ x <. y <. z
