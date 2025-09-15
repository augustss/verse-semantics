-----------------------------------------------------------------------------
-- |
-- Module    : Parser.Compat
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module defines functions that translate ASTs from
-- Language.Verse.Rewrite.Exp to FrontEnd.Expr. This allows the tester and the
-- rest of the infrastructure to utilize the parser but still operate on the
-- FrontEnd.Expr AST.
--
-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings    #-}

module Parser.Compat
  ( expToSrcExpr
  , lexp
  , desugar
  , toSrcExpr
  , mkSrcIdent
  , locToSrcLoc
  ) where

import qualified Language.Verse.Exp         as P
import qualified Language.Verse.Rewrite     as R
import qualified Language.Verse.Rewrite.Exp as R
import Language.Verse.Ident
import Language.Verse.Loc (L (..), Loc(..), uncurryL)
import Language.Verse.Label
import Language.Verse.Effect.Split
import Language.Verse.SimpleName
import Language.Verse.Error
import Language.Verse.Pos

import qualified FrontEnd.Expr              as Src

import qualified Control.Monad.Supply       as C
import qualified Control.Monad.Wrong        as C

import Data.Scientific (fromFloatDigits)
import Control.Monad
import qualified Data.Text as T

toSrcExpr :: (L (P.Exp SimpleName)) -> Src.SrcExpr
toSrcExpr = expToSrcExpr . desugar

expToSrcExpr :: L (R.Exp L Ident) -> Src.SrcExpr
expToSrcExpr = uncurryL expToSrcExpr'

-- | adapter that translates the Parser.Language.Verse to SrcExpr
expToSrcExpr' :: Loc -> R.Exp L Ident -> Src.SrcExpr
expToSrcExpr' l (e1 R.:=:  e2)    = Src.InfixOp (lexp e1) (inOp l "=")  (lexp e2)
expToSrcExpr' l (e1 R.:|:  e2)    = Src.InfixOp (lexp e1) (inOp l "|")  (lexp e2)
expToSrcExpr' _ (R.List es)       = Src.eSeq (map lexp es)
expToSrcExpr' l (R.Where e1 e2)   = Src.InfixOp (lexp e1) (inOp l "where") (lexp e2)
expToSrcExpr' _ R.Fail            = Src.Fail
expToSrcExpr' l (R.One e)         = Src.Macro1 (macro l "one") [] (lexp e)
expToSrcExpr' l (R.All e)         = Src.Macro1 (macro l "all") [] (lexp e)
expToSrcExpr' l (R.Not e)         = Src.PrefixOp (preOp l "not") (lexp e)
expToSrcExpr' l (R.Verify e)      = Src.Macro1 (macro l "verify") [] (lexp e)
expToSrcExpr' _ (R.Check eff e)   = Src.Check (refImplEffToSrcEff eff) (lexp e)
expToSrcExpr' _ (R.OfType e1 e2)  = Src.OfType (lexp e1) Src.effTop (lexp e2)
expToSrcExpr' l (R.Assume e)      = Src.Macro1 (macro l "assume") [] (lexp e)
-- expToSrcExpr' l (e1 R.:&:  e2)    = Src.InfixOp (lexp e1) (inOp l "&")  (lexp e2)
-- expToSrcExpr' l (e1 R.:.:  e2) = Src.InfixOp (lexp e1) (inOp l ".")  (lexp e2)
-- expToSrcExpr' l (R.Module e)   = XXX
-- expToSrcExpr' l (R.Struct e)   = XXX
-- expToSrcExpr' l (R.Class e)    = XXX
-- expToSrcExpr' l (R.Inst e1 e2) = XXX
-- expToSrcExpr' l (R.Enum e)     = XXX
-- expToSrcExpr' _ (Forall e)     = XXX
-- expToSrcExpr' _ (Alloc2 )      = XXX
-- expToSrcExpr' _ (Alloc3 )      = XXX
-- expToSrcExpr' QualName         = ???
-- expToSrcExpr' Domain           = ???
expToSrcExpr' _ (R.IfThenElse e1 e2 e3) = Src.If3 (lexp e1) (lexp e2) (lexp e3)
expToSrcExpr' _ (R.ForDo e1 e2)         = Src.For2 (lexp e1) (lexp e2)
expToSrcExpr' _ (R.Block e)             = Src.Block (lexp e)
expToSrcExpr' _ (R.BracketInvoke f a)   = Src.ApplyD (lexp f) (lexp a)
expToSrcExpr' _ (R.ParenInvoke f a)     = Src.ApplyS (lexp f) (lexp a)
expToSrcExpr' _ (R.Exists idents (L bl body)) =
  let handle_ident (L l i) = ident l i
  in Src.Exists (handle_ident <$> idents) (expToSrcExpr' bl body)
expToSrcExpr' l (R.Set (L l' x) e)   = Src.Set (Src.Variable (ident l' x)) (ident l "=") (lexp e)
expToSrcExpr' _ (R.Tuple es)         = Src.Tuple (fmap lexp es)
expToSrcExpr' _ (R.Array es)         = Src.Array (fmap lexp es)
expToSrcExpr' _ (R.Truth e)          = Src.Truth (lexp e)
expToSrcExpr' _ (R.Int i)            = Src.Lit (Src.LInt i)
expToSrcExpr' _ (R.Float f)          = Src.Lit (Src.LRat (fromFloatDigits f) (show f))
expToSrcExpr' _ (R.Char c)           = Src.Lit (Src.LChar (toEnum (fromEnum c)))
expToSrcExpr' _ (R.Char32 c)         = Src.Lit (Src.LChar c)
expToSrcExpr' _ (R.Lam e1 oc eff e2) = Src.Function ap_ (lexp e1) rs (lexp e2)
  where
    ap_ = case oc of { R.O -> Src.Open; R.C -> Src.Closed }
    rs = refImplEffToSrcEff eff
expToSrcExpr' l (R.InfixColonEqual _ q (L l' x) e) | ok q = Src.InfixOp (Src.Variable (ident l' x)) (inOp l ":=") (lexp e)
  where ok R.Var = False
        ok _ = True
expToSrcExpr' l (R.PrefixColon e) = Src.PrefixOp (preOp l ":") (lexp e)
expToSrcExpr' l (R.MixfixArrowColonEqual (L lx x) (L ly y) e) =
  Src.InfixOp lhs (ident l ":=") (lexp e)
  where lhs = Src.InfixOp (Src.Variable (ident lx x)) (strIdent l "->") (Src.Variable (ident ly y))
expToSrcExpr' l (R.Name n) = Src.Variable (ident l n)
expToSrcExpr' _ (R.IfArchetypeName _ e1 e2) | x1 == x2 = x1
  where x1 = lexp e1; x2 = lexp e2
expToSrcExpr' _ (R.IfArchetypeName _ _ e2) = lexp e2
-- TODO: Jeff: parser does not export pretty instances
-- expToSrcExpr' _ e = error $ "expToSrcExpr': unimp " ++ show (pretty e) ++ "\n" ++ show e
expToSrcExpr' _ e = error $ "expToSrcExpr': unimp " ++ "\n" ++ show e

newtype M a = M { unM :: Label -> (Label, a) }
instance Functor M where
  fmap f ma = M $ \ l -> case unM ma l of (l', a) -> (l', f a)
instance Applicative M where
  pure a = M $ \ l -> (l, a)
  (<*>) = ap
instance Monad M where
  ma >>= k = M $ \ l -> case unM ma l of (l', a) -> unM (k a) l'
instance C.MonadWrong Error M where
  wrong e = error $ show e
instance C.MonadSupply Label M where
  supply = M $ \ l -> let !l' = l + 1 in (l', l)

runM :: M a -> a
runM (M a) = snd (a 0)

desugar :: L (P.Exp SimpleName) -> L (R.Exp L Ident)
desugar = runM . R.rewrite

lexp :: L (R.Exp L Ident) -> Src.SrcExpr
lexp (L l e) = expToSrcExpr' l e

strIdent :: Loc -> String -> Src.Ident
strIdent (Loc (Pos l c _) _) s = Src.Ident (Src.mkLoc "?" l c) s

inOp :: Loc -> Ident -> Src.Ident
inOp l s = ident l s

preOp :: Loc -> Ident -> Src.Ident
preOp l s = ident l s

ident :: Loc -> Ident -> Src.Ident
ident l i = strIdent l (f i)
  where
    f :: Ident -> String
    f (Name s)   = T.unpack s
    f (Label l') = "_" ++ show l'

mkSrcIdent :: L SimpleName -> Src.Ident
mkSrcIdent (L (Loc (Pos line col _offset) _end_pos) idnt) = Src.Ident loc new
  where
    loc = Src.mkLoc "?" line col
    new = T.unpack idnt


macro :: Loc -> Ident -> Src.Ident
macro l s = ident l s

refImplEffToSrcEff :: Effect -> Src.Eff
refImplEffToSrcEff Fails    = Src.effFails
refImplEffToSrcEff Succeeds = Src.effSucceeds
refImplEffToSrcEff Decides  = Src.effDecides

locToSrcLoc :: Loc -> Src.Loc
locToSrcLoc (Loc (Pos l c _o) _endPos) = Src.mkLoc "?" l c -- ? becomes the file name
