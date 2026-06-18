{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
     {- -Wno-missing-methods -Wno-incomplete-uni-patterns -Wno-unused-matches
        -Wno-missing-pattern-synonym-signatures -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}


module Red( Blk(Blk, BlkX), Exp(..), pattern Val, Term(..), Ident(..)
          , matchTop, topMatchContext
          , ReductionContext(..), ReductionMode(..), setJustMatching
          , VerificationContext(..)
          , MatchContext, mcAssumeVerified
          , thePrelude
          , run, runTraced, isVal, mkBlkE
          , srcToTerm, addInScopeSkols, addInScopeExis
          , freshId, freshIds2, freshId4
          , freeVarsTerm
          , emptyHeap, mkCheck, mkAll
          , validBlk
  ) where

import Prelude hiding ((<>))
import Data.Graph(stronglyConnComp, SCC(..))

import qualified FrontEnd.Expr as F
import FrontEnd.ToCore( toCoreEff )
import FrontEnd.Error

import Core.Traced
import Core.Solver( unsat )
import Core.Expr as C ( Assump, Effect(..), Lit(..), PrimOp(..)
                      , GroundVal(..), Assump(..), FailableAssump(..)
                      , PredAssump(..), AssumpOp(..)
                      , primOpIsCheck, primOpCanFail, substAssump
                      , primOpPreCond, intersectEffect, notPred
                      , isBinOp, isBinRelOp )
import Core.Bind as C

import Epic.Print hiding( mode )
import Epic.List

import Control.Arrow(second, (***))
import qualified Data.List as L
import qualified Data.Monoid as D
import Data.Maybe
import qualified Data.Set as S
import Data.Set( Set )
import Data.Char( isDigit )
import Data.List( isPrefixOf )

import Debug.Trace
import GHC.Stack

-- Show every reduction step
traceReductions :: Bool
traceReductions = False

ruleVerbosity :: RuleName -> Verbosity
-- Higher numbers => less likely to be displayed
ruleVerbosity (RNM ns)
  | null ns   = 0
  | otherwise = lookupVerbosity (last ns)
                -- last ns: Just look at the last component

verbosityTable :: [(String,Verbosity)]
verbosityTable = [ ("Norm1", 3)
                 , ("Norm2", 3)
                 , ("Norm3", 3)
                 , ("Norm4", 3)
                 , ("Norm5", 3)
                 , ("FloatB", 3)  -- Floats the bindings of {b}
                 , ("GC",    2)
                 , ("Seq",   2)  -- Another sort of GC
                 ]

lookupVerbosity :: String -> Verbosity
lookupVerbosity s
  = go verbosityTable
  where
    go [] = 0  -- Default to showing everything
    go ((prefix,v):tbl) | prefix `isPrefixOf` s = v
                        | otherwise             = go tbl

--------------------------------------------------------------------------------
--
-- Sets
---------------------------------------------------------------------------------

sing :: a -> Set a
sing x = S.singleton x

--------------------------------------------------------------------------------
--
--             Data types Term and Exp
--
--------------------------------------------------------------------------------

infixr 0 :>
infix  2 :=
infixr 4 :|:
infixr 5 :=:

data Term
  = TVar Ident                -- x
  | TLit Lit                  -- k
  | TPrm PrimOp               -- op
  | Term :>%  Term            -- t1; t2
  | Term :|:% Term            -- t1 | t2
  | TFail                     -- fail
  | Term :=:% Term            -- t1 = t2
  | Term :@%  Term            -- t1[t2]
  | TArr [Term]               -- array{t1,...,t2}
  | TOfType Term Effect Term  -- t1 |><fx> t2
  | TTru Term                 -- truth{t}
  | TMap [(Term, Term)]        -- map{...}

  | Und                     -- _
  | Term `Where` Term       -- t1 where t2
  | Fun Term Effect Term    -- fun(t1)<fx>{t2}
  | If Term Term Term       -- if (t1){t2}{t3}
  | For Term Term           -- for (t1){t2}
  | Rng Term                -- :t
  | Ident := Term           -- x := t
  | Ident :-> Term          -- x ??? t
  | TBlock Term             -- block{ t }
  | Splice Term             -- ..t
  | Check Effect Term       -- check<fx>{t}
  deriving (Eq, Ord, Show)

data Exp
  -- Values
  = Var Ident             -- x
  | Lit Lit               -- k
  | Prm PrimOp            -- op
  | Exp :>  Exp           -- e1; e2
  | Blk :|: Blk           -- e1 | e2
  | Fail                  -- fail
  | Exp :=: Exp           -- e1 = e2
  | Exp :@  Exp           -- e1[e2]
  | Arr [Val]             -- array{v1,...,vn}
  | Tru Val               -- truth{v}
  | Map [(Gnd, Val)]      -- map{...}

  | Lam Ident Blk          -- \ x . e
  | Iter IterCtx Blk Exp   -- if/for
  | Dly Exp                -- delay{e}
  | Crl Blk                -- {...}
  | Err String             -- Stuck/error expression

  | Match MatchContext Ident Term   -- e ~>mc t
  | OfType Exp Effect Exp           -- e1 |><fx> e2
  | Verify (Set SkolIdent)
           [Assump]
           Blk
  deriving (Eq, Ord, Show)

data Blk = BlkX (Set Ident) Heap Exp
  deriving (Eq, Ord, Show)

-- The equation RHSs have no variables from the LHSs
-- A block (Blk xs eqs e) satisfies these invariants:
--    (A)  dom(eqs)    `subset`   X
--    (B)  occfvs(eqs) `disjoint` dom(eqs)

data MatchContext
  = MC { mc_blob     :: Blob
       , mc_verify   :: Bool  -- True <=> generate verification
                              --          constraints for definitions
       , mc_effect   :: DRContext }
  deriving(Eq,Ord,Show)

data Blob
  = MTop     -- Written 'o' in the rules.  Do not generate f[x] in (MFun)
  | MNested  -- Written 'bullet' in the rules. Do generate f[x] in (MFun)
  deriving (Eq, Ord, Show)

data DRContext  -- "DR" connotes "domain or range"
  = DR_Dom          -- In the Domain of a function(at)<fx>{bt}
  | DR_Rng Effect   -- In the Range  of a function(at)<fx>{bt}, with effects <fx>
  deriving(Eq,Ord,Show)

type Eqn = (Ident, Val)
type Heap = [Eqn]  -- Invariant: all identifiers in the domain are distinct

emptyHeap :: Heap
emptyHeap = []

isEmptyHeap :: Heap -> Bool
isEmptyHeap [] = True
isEmptyHeap _  = False

matchTop :: ReductionContext -> MatchContext -> Term -> Exp

-- Three (optional) short-cuts that avoid ever creating 'u'
matchTop _ _ (TVar v) = Var v
matchTop _ _ (TLit k) = Lit k
matchTop _ _ (TPrm o) = Prm o

-- This is the default
matchTop cxt mc t = Crl $ Blk (sing u) emptyHeap $
                    Match (mc { mc_blob = MTop, mc_effect = DR_Dom }) u t
  where
    u = freshId cxt "u"

mcAssumeVerified :: MatchContext -> MatchContext
mcAssumeVerified mc = mc { mc_verify = False }

mcNested :: MatchContext -> MatchContext
mcNested mc = mc { mc_blob = MNested }

topMatchContext :: MatchContext
topMatchContext = MC { mc_blob   = MTop
                     , mc_verify = True
                     , mc_effect = DR_Dom
                     }

{-# COMPLETE Blk #-}
pattern Blk :: (Set Ident) -> Heap -> Exp -> Blk
--pattern Blk is eqs e = BlkX is eqs e  -- do not check invariant
pattern Blk is eqs e <- BlkX is eqs e   -- check invariant
  where Blk is eqs e = assertP "BadBlock1" invariant1 (pPrint blk) $
                       assertP "BadBlock2" invariant2 (pPrint blk) $
                       blk
           where
             blk = BlkX is eqs e
             invariant1 = dom eqs `S.isSubsetOf` is
             invariant2 = S.unions (map (occfvs . snd) eqs) `S.disjoint` dom eqs

-- exists x y{ x <-3; y<-x }  -- no to inv2

type Val = Exp

data IterCtx = IF | FOR | ALL
  deriving (Eq, Ord, Show)

mkBlkE :: Exp -> Blk
mkBlkE (Crl b) = b
mkBlkE e       = Blk S.empty emptyHeap e

mkCrl :: Blk -> Exp
mkCrl (Blk is hp e) | S.null is, isEmptyHeap hp = e
mkCrl b                                         = Crl b

mkSeq :: Exp -> Exp -> Exp
-- Aggressively elminate values, and re-associate to the right
mkSeq (Val {})   e2 = e2
mkSeq (e1 :> e2) e3 = mkSeq e1 (mkSeq e2 e3)
mkSeq e1         e2 = e1 :> e2

mkSeqs :: [Exp] -> Exp
mkSeqs []     = Arr []
mkSeqs [e]    = e
mkSeqs (e:es) = e `mkSeq` mkSeqs es

mkCheck :: Effect -> Blk -> Exp
mkCheck Fails    blk = Prm ChkFails    :@ Iter ALL blk (Arr [])
mkCheck Succeeds blk = Prm ChkSucceeds :@ Iter ALL blk (Arr [])
mkCheck Decides  blk = Prm ChkDecides  :@ Iter ALL blk (Arr [])
mkCheck Iterates blk = mkCrl blk

mkAll :: Blk -> Exp
mkAll b = Iter ALL b (Arr [])

mkArr :: [Exp] -> Exp
-- Does ANF if any of the argment arrays are non-empty
mkArr es = mkCrl (Blk (S.fromList (map fst aux_binds))
                      []
                      (foldr mk_bind (Arr vs) aux_binds))
  where
    in_scope = S.unions $ map freeVars es
    (aux_binds,vs) = foldr do_one ([], []) es
    mk_bind (s,rhs) e = (Var s :=: rhs) :> e

    do_one :: Exp -> ([(Ident,Exp)], [Val]) -> ([(Ident,Exp)], [Val])
    -- The [(Ident,Exp)] are the auxiliary bindings
    do_one (Val v) (binds, acc_vs) = (      binds, v    :acc_vs)
    do_one e       (binds, acc_vs) = ((s,e):binds, Var s:acc_vs)
      where
        s = findFresh is_taken (mkName "s")
        is_taken i = i `S.member` in_scope || any (\(j,_) -> i==j) binds

--------------------------------------------------------------------------------
--
--             Pretty-printing
--
--------------------------------------------------------------------------------

newtype PExp = P Exp
instance Show PExp where
  show (P e) = prettyShow e

instance PrettyBrief Exp where
  pPrintBrief t = text "size:" <> int (expSize t)
instance PrettyBrief Term where
  pPrintBrief t = text "size:" <> int (termSize t)
instance PrettyBrief Blk where
  pPrintBrief b = text "size:" <> int (blkSize b)

instance Pretty Term where
  pPrintPrec l p (TVar i) = pPrintPrec l p i
  pPrintPrec l p (TLit k) = pPrintPrec l p k
  pPrintPrec l p (TPrm o) = pPrintPrec l p o
  pPrintPrec _ _ Und      = text "_"

  pPrintPrec l p (Rng e)         = maybeParens (p > 10) $ text ":" <> pPrintPrec l 11 e
  pPrintPrec l _ (Fun e1 fx e2)  = text "fun" <> cat [parens (pPrintPrec l 0 e1), angleBrackets (pPrint fx), braces (pPrintPrec l 0 e2)]
  pPrintPrec l p (x := e)        = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":=" <+> pPrintPrec l 2 e
  pPrintPrec l p (e1 :@% e2)     = maybeParens (p > 10) $ cat [pPrintPrec l 10 e1, nest 2 (text "[" <> pPrintPrec l 0 e2 <> text "]")]

  -- Flatten out t1;t2;t3 into a list
  pPrintPrec l p tt@(_ :>% _)
    = maybeParens (p > 0) $ sep $ punctuate (text ";") (map (pPrintL l) $ flat tt)
    where flat (t1 :>% t2) = flat t1 ++ flat t2
          flat t = [t]

  pPrintPrec l p (e1 `Where` e2) = maybeParens (p > 0) $ pPrintPrec l 1 e1 <+> text "where" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 :=:% e2)    = maybeParens (p > 5) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (TArr es)
    | l == prettyNormal     = text "<" <> hsep (punctuate (text ",") (map (pPrintL l) es)) <> text ">"
  pPrintPrec l _ (TArr [e]) = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (TArr es)  = parens $ hsep $ punctuate (text ",") $ map (pPrintL l) es
  pPrintPrec l _ (TTru e)   = text "truth" <> braces (pPrintL l e)
  pPrintPrec l _ (TMap kes) = text "map" <> braces
     (hsep (punctuate (text ",") (map (\ (k, e) -> pPrintPrec l 4 k <+> text "=>" <+> pPrintPrec l 4 e) kes)))

  pPrintPrec l p (b1 :|:% b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec _ _ TFail        = text "fail"

  pPrintPrec l _ (If t1 t2 t3) = sep [text "if" <> parens (pPrintL l t1), nest 2 (braces (pPrintL l t2)), text "else", nest 2 (braces (pPrintL l t3)) ]
  pPrintPrec l _ (For t1 t2)   = hsep [text "for" <> parens (pPrintL l t1), nest 2 (braces (pPrintL l t2))]
  pPrintPrec l p (x :-> e)     = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":->" <+> pPrintPrec l 2 e

  pPrintPrec l _ (TBlock t)    = braces $ (pPrintL l t)
  pPrintPrec l _ (Check fx t)  = text "check" <> angleBrackets (pPrint fx) <> braces (pPrintL l t)
  pPrintPrec l _ (Splice t)    = text ".." <> pPrintPrec l 10 t
  pPrintPrec l _ (TOfType t1 fx t2) = sep [ pPrintPrec l 6 t1
                                          , text "|>" <> angleBrackets (pPrint fx)
                                            <+> pPrintPrec l 6 t2 ]

instance Pretty Exp where
  pPrintPrec l p (Var i)     = pPrintPrec l p i
  pPrintPrec l p (Lit i)     = pPrintPrec l p i
  pPrintPrec l p (Prm o)     = pPrintPrec l p o
  pPrintPrec l p (Lam i b)   = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 i <> text "." <> pPrintPrec l 0 b
  pPrintPrec l p (Match cxt x e) = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>"<> pPrint cxt <+> pPrintPrec l 1 e
  pPrintPrec l p (e1 :@ e2)  = maybeParens (p > 10) $ cat [pPrintPrec l 10 e1, nest 2 (text "[" <> pPrintPrec l 0 e2 <> text "]")]

  -- Flatten out e1;e2;e3 into a list
  pPrintPrec l p ee@(_ :> _)
    = maybeParens (p > 0) $ sep $ punctuate (text ";") (map (pPrintL l) $ flat ee)
    where flat (e1 :> e2) = flat e1 ++ flat e2
          flat e = [e]

  pPrintPrec l p (e1 :=: e2) = maybeParens (p > 0) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (Arr es)
    | l == prettyNormal      = text "<" <> sep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (Arr [e])   = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (Arr es)    = parens $ sep $ punctuate (text ",") $ map (pPrintPrec l 0) es
  pPrintPrec l _ (Tru e)     = text "truth" <> braces (pPrintL l e)

  pPrintPrec l _ (Crl b)     = braces $ pPrintPrec l 0 b
  pPrintPrec l _ (Dly e)     = text "delay" <> braces (pPrintL l e)
  pPrintPrec l p (b1 :|: b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec _ _ Fail        = text "fail"
  pPrintPrec _ _ (Err s)     = text "error" <> braces (text s)
  pPrintPrec l _ (Map kes)   = text "map" <> braces
     (hsep (punctuate (text ",") (map (\ (k, e) -> pPrintPrec l 4 k <+> text "=>" <+> pPrintPrec l 4 e) kes)))


  pPrintPrec l _ (Iter ic b1 b2)
    = text "iter" <> cat [parens (text (show ic)), braces (pPrintL l b1), braces (pPrintL l b2)]
  pPrintPrec l _ (Verify rs as e)
    = sep [ text "verify" <> parens (sep [ fsep (map pPrint (S.toList rs)) <> text ";"
                                         , fsep (punctuate comma $ map pPrint as) ])
          , nest 2 (braces (pPrintL l e)) ]

  pPrintPrec l p (OfType e1 fx e2)
    = maybeParens (p > 7) $
      sep [ pPrintPrec l 6 e1
          , text "|>" <> pPrintEffect fx
             <+> pPrintPrec l 6 e2 ]

instance Pretty Blk where
  pPrintPrec l p (Blk vs eqns e) = ppBlk l p vs eqns e

instance Pretty MatchContext where
  pPrint (MC { mc_blob = b, mc_verify = v, mc_effect = fx })
    = parens (pPrint b <> pp_v v <> pp_fx fx)
    where
      pp_fx DR_Dom = empty
      pp_fx (DR_Rng Succeeds) = char 's'
      pp_fx (DR_Rng Decides)  = char 'd'
      pp_fx (DR_Rng Fails)    = char 'f'
      pp_fx (DR_Rng Iterates) = char 'i'

      pp_v True  = char 'v'
      pp_v False = char 'x'

instance Pretty Blob where
  pPrint MTop    = char 'o'
  pPrint MNested = char '⦁'   -- Nearest thing to \bullet

pPrintEffect :: Effect -> Doc
pPrintEffect Iterates = empty
pPrintEffect fx       = angleBrackets (pPrint fx)

ppBlk :: PrettyLevel -> Rational -> Set Ident -> Heap -> Exp -> Doc
ppBlk l p vs eqns e
  | S.null vs, null eqns
  = pPrintPrec l p e
  | otherwise
  = maybeParens (p > 0) $ sep [ text "∃" <> ppBlkIntro vs eqns <> text "."
                              , pPrintPrec l 0 e ]

ppBlkIntro :: Set Ident -> Heap -> Doc
ppBlkIntro is eqs
  = sep [ fsep (map pPrint $ S.toList is)
        , braces (fsep $ punctuate (text ";") $ map ppr_eqn eqs) ]

ppr_eqn :: (Ident, Exp) -> Doc
ppr_eqn (i, d) = pPrint i <+> text "<-" <+> pPrint d

--------------------------------------------------------------------------------
--
--             Pattern synonyms that carve out Exp subsets
--
--------------------------------------------------------------------------------

type HNF = Exp

-- A HNF, i.e., a value, but not a variable
pattern HNF :: Exp -> HNF
pattern HNF e <- (getHNF -> Just e)

getHNF :: Exp -> Maybe HNF
getHNF e@Lit{} = Just e
getHNF e@Prm{} = Just e
getHNF e@Lam{} = Just e
getHNF e@Arr{} = Just e
getHNF e@Tru{} = Just e
getHNF e@Dly{} = Just e
getHNF e@Map{} = Just e
getHNF _ = Nothing

pattern Val :: Exp -> Val
pattern Val e <- (getVal -> Just e)

-- Values
getVal :: Exp -> Maybe Val
getVal e@Var{} = Just e
getVal e@Lit{} = Just e
getVal e@Prm{} = Just e
getVal e@Lam{} = Just e
getVal e@(Arr es) | Just _ <- mapM getVal es = Just e
getVal e@(Tru e') | Just _ <- getVal e' = Just e
getVal e@Dly{} = Just e
getVal e@(Map kvs) | Just _ <- mapM (getVal . snd) kvs = Just e
getVal _ = Nothing

isVal :: Exp -> Bool
isVal e = isJust (getVal e)

isAtomic :: Exp -> Bool
isAtomic (Var {}) = True
isAtomic (Lit {}) = True
isAtomic (Arr []) = True
isAtomic (Prm {}) = True
isAtomic _ = False

pattern Comp :: Exp -> Exp
pattern Comp v <- (getComp -> Just v)

-- Comparable values
getComp :: Exp -> Maybe Exp
getComp e@Lit{} = Just e
getComp e@(Arr es) | Just _ <- mapM getComp es = Just e
getComp e@(Tru e') | Just _ <- getComp e' = Just e
getComp e@(Map kvs) | Just _ <- mapM (getComp . snd) kvs = Just e
getComp _ = Nothing

type Con = Exp
pattern Con :: Con -> Exp
pattern Con v <- (getCon -> Just v)

-- Constants, no further reductions possible
getCon :: Exp -> Maybe Con
getCon e@Lit{} = Just e
getCon e@Prm{} = Just e
getCon e@Lam{} = Just e
getCon e@(Arr es) | Just _ <- mapM getCon es = Just e
getCon e@(Tru e') | Just _ <- getCon e' = Just e
getCon e@Dly{} = Just e
getCon e@(Map kvs) | Just _ <- mapM (getCon . snd) kvs = Just e
getCon _ = Nothing

type Gnd = Exp
--pattern Gnd :: Exp -> Gnd
--pattern Gnd v <- (getGnd -> Just v)

-- Ground values, Con, but no functions
getGnd :: Exp -> Maybe Gnd
getGnd e@Lit{} = Just e
getGnd e@(Arr es) | Just _ <- mapM getGnd es = Just e
getGnd e@(Tru e') | Just _ <- getGnd e' = Just e
getGnd e@(Map kvs) | Just _ <- mapM (getGnd . snd) kvs = Just e
getGnd _ = Nothing

-- Turn every kind of HNF into a trivial one
root :: HNF -> HNF
root Lit{} = IntE 0
root Prm{} = Prm Add
root Lam{} = Lam (Name "") (mkBlkE $ IntE 0)
root Arr{} = Arr []
root Dly{} = Dly (IntE 0)
root Tru{} = Tru (IntE 0)
root _ = error "root: not an HNF"

pattern IntE :: Integer -> Exp
pattern IntE i = Lit (LInt i)

--pattern StrE :: String -> Exp
--pattern StrE s = Lit (LStr s)

pattern CharE :: Char -> Exp
pattern CharE s = Lit (LChar s)

--------------------------------------------------------------------------------
--
--             Convert SrcEssential to Term
--
--------------------------------------------------------------------------------

srcToTerm :: F.SrcEssential -> Term
srcToTerm (F.Variable i)
  | F.isSrcUnderscore i          = Und
  | otherwise                    = TVar (srcToCoreIdent i)
srcToTerm (F.EPrim o)            = TPrm o
srcToTerm (F.Lit (LStr s))       = TArr $ map (TLit . LChar) s
srcToTerm (F.Lit k)              = TLit k
srcToTerm (F.DefineE i e)        = srcToCoreIdent i := srcToTerm e
srcToTerm (F.Choice e1 e2)       = srcToTerm e1 :|:% srcToTerm e2
srcToTerm (F.Unify e1 e2)        = srcToTerm e1 :=:% srcToTerm e2
srcToTerm (F.Seq e1 e2)          = srcToTerm e1 :>% srcToTerm e2
srcToTerm (F.Where e1 e2)        = srcToTerm e1 `Where` srcToTerm e2
srcToTerm (F.Range e)            = Rng (srcToTerm e)
srcToTerm (F.Splice e)           = Splice (srcToTerm e)
srcToTerm (F.Array es)           = TArr (map srcToTerm es)
srcToTerm (F.Fail)               = TFail
srcToTerm (F.Function _ e1 fx e2)
  | Just eff <- toCoreEff fx     = Fun (srcToTerm e1) eff (srcToTerm e2)
srcToTerm (F.DefineV x)          = srcToCoreIdent x := Rng (TVar (Name "any"))
srcToTerm (F.DefineIE i e)       = srcToCoreIdent i :-> srcToTerm e
srcToTerm (F.If3 e1 e2 e3)       = If (srcToTerm e1) (srcToTerm e2) (srcToTerm e3)
srcToTerm (F.For2 e1 e2)         = For (srcToTerm e1) (srcToTerm e2)
srcToTerm (F.OfType t1 mfx t2)
  | let fx = fromMaybe F.effSucceeds mfx                 -- Missing effects turns into succeeds
  ,  Just eff <- toCoreEff fx     = TOfType (srcToTerm t1) eff (srcToTerm t2)
srcToTerm (F.One e)              = If (x := srcToTerm e) (TVar x) TFail
  where
    x = mkName "oneBinder"  -- Hack; hope this is not free in 'e'!
srcToTerm (F.ApplyD e1 e2)       = srcToTerm e1 :@% srcToTerm e2
srcToTerm (F.Exists is e)        = TBlock (foldr bind_one (srcToTerm e) is)
                                 where
                                   bind_one x t = (srcToCoreIdent x := Und) :>% t

srcToTerm (F.Check fx e)
  | Just eff <- toCoreEff fx     = Check eff (srcToTerm e)
srcToTerm (F.Truth e)            = TTru (srcToTerm e)

-- XXX Hacky translation of maps.  We could instead go via functions
-- enumerating the arguments.
srcToTerm (F.Macro1 m _ kes) | m == F.Ident undefined "map" =
  TPrm MkMap :@% TMap
    (case kes of
       F.Array xs -> map keyValue xs
       x          -> [keyValue x]
    )
  where keyValue :: F.SrcEssential -> (Term, Term)
        keyValue (F.Function F.Closed k suc v) | suc == F.effSucceeds = (srcToTerm k, srcToTerm v)
        keyValue kv = error $ "map: not a key-value pair " ++ prettyShow kv

srcToTerm e = error $ "srcToTerm: unimplemented " ++ show e

srcToCoreIdent :: F.Ident -> Ident
srcToCoreIdent (F.Ident _ s) = Name s

--------------------------------------------------------------------------------
--
--             Reduction context
--
--------------------------------------------------------------------------------

data ReductionContext
  = RC { rc_depth  :: Int         -- Number of enclosing Blks

       , rc_exis   :: Set Ident
       , rc_eqns   :: Heap        -- In-scope equations for the rc_exis

       , rc_skols  :: Set Ident
       , rc_vcxt   :: VerificationContext
       , rc_mode   :: ReductionMode
    }
    deriving (Show)

data ReductionMode
  = RM { rm_just_matching :: Bool   -- True <=> run only the 'match' rules, and
                                    --          recurse under lambda and iter
       }
  deriving( Show )

data VerificationContext
  = NotVerifying          -- Outside verify{}
  | Verifying             -- Inside verify{}
        [Assump]    -- In-scope assumptions for the rc_skols
                    --   including from /outer/ verifys
        (Set Ident) -- Existentials bound inside the innermost verify
  deriving (Show)

addInScopeExis :: ReductionContext -> Set Ident -> ReductionContext
addInScopeExis cxt bndrs = cxt { rc_exis = rc_exis cxt `S.union` bndrs }

addInScopeSkols :: ReductionContext -> Set Ident -> ReductionContext
addInScopeSkols cxt bndrs = cxt { rc_skols = rc_skols cxt `S.union` bndrs }

getInScope :: ReductionContext -> Set Ident
getInScope (RC { rc_exis = exis, rc_skols = skols }) = exis `S.union` skols

insideVerify :: ReductionContext -> Bool
insideVerify (RC { rc_vcxt = Verifying {} }) = True
insideVerify _                               = False

setJustMatching :: ReductionContext -> ReductionContext
setJustMatching rc@(RC { rc_mode = mode })
  = rc { rc_mode = mode { rm_just_matching = True } }

justMatching :: ReductionContext -> Bool
justMatching (RC { rc_mode = RM { rm_just_matching = jm }}) = jm


--------------------------------------------------------------------------------
--
--             Reductions
--
--------------------------------------------------------------------------------

newtype RuleName = RNM [String]   -- The list gives a trace of nested invocations
 deriving( Eq )

instance Show RuleName where
  show (RNM ns) = foldr1 (\s n -> s ++ "/" ++ n) ns

mkRuleName :: String -> RuleName
mkRuleName s = RNM [s]

wrapRuleName :: String -> RuleName -> RuleName
wrapRuleName s (RNM rn) = RNM (s : rn)

data Reduction a
  = RedNone                                -- No redex found
  | RedStep RuleName [Result BlkFloats a]  -- A regular reduction step
  | VerStep RuleName [Result VerFloats a]  -- A verification step
  deriving (Eq, Show, Functor)

data Result floats payload = Res floats payload
   deriving( Eq, Show, Functor )

data BlkFloats   -- Floating out the the innermost enclosing Blk
  = NoFloats
  | FloatB (Set Ident) Heap         -- From (FloatB)
  | Promote (Set Ident) Ident Exp   -- From (Promote)
  deriving (Eq, Show)

data VerFloats   -- Floating out to the innermost enclosing Verify
  = NoVerFloats
  | FloatRigid (Set SkolIdent) [Assump]
  | FloatFlexi SkolIdent Ident Exp
  deriving (Eq, Show)

orTry :: Reduction a -> Reduction a -> Reduction a
orTry RedNone r2 = r2
orTry r1      _  = r1

mapPayload :: (a -> b) -> [Result flt a] -> [Result flt b]
mapPayload f rs = [ Res flt (f payload) | Res flt payload <- rs ]

rednFired :: Reduction a -> Maybe (Reduction a)
rednFired RedNone = Nothing
rednFired r       = Just r

mkExiFloat1 :: Ident -> BlkFloats
-- Single existential only, no heap
mkExiFloat1 x = FloatB (sing x) emptyHeap

mkExiFloat :: [Ident] -> BlkFloats
-- Existentials only, no heap
mkExiFloat xs = FloatB (S.fromList xs) emptyHeap

mkFailure :: String -> Reduction a
mkFailure s = RedStep (mkRuleName s) []

mkDone :: String -> a -> Reduction a
mkDone s e = RedStep (mkRuleName s) [Res NoFloats e]

mkWrapDone :: RuleName -> a -> Reduction a
mkWrapDone rn e = RedStep rn [Res NoFloats e]

mkStep :: String -> BlkFloats -> Exp -> Reduction Exp
mkStep s flts e = RedStep (mkRuleName s) [Res flts e]

instance Pretty a => Pretty (Reduction a) where
  pPrintPrec _ _ RedNone         = text "None"
  pPrintPrec _ _ (RedStep s [])  = text "Failure" <+> text (show s)
  pPrintPrec _ _ (RedStep s [r]) = sep [ text "Step" <+> text (show s), pPrint r ]
  pPrintPrec _ _ (RedStep s rs)  = sep [ text "StepC" <+> text (show s)
                                       , nest 2 (vcat (map pPrint rs)) ]
  pPrintPrec _ _ (VerStep s [])  = text "VFailure" <+> text (show s)
  pPrintPrec _ _ (VerStep s [r]) = sep [ text "VStep" <+> text (show s), pPrint r ]
  pPrintPrec _ _ (VerStep s rs)  = sep [ text "VStepC" <+> text (show s)
                                       , nest 2 (vcat (map pPrint rs)) ]

instance Pretty BlkFloats where
  pPrintPrec _ _ NoFloats         = empty
  pPrintPrec _ _ (FloatB is eqs)  = text "FB" <> parens (ppBlkIntro is eqs)
  pPrintPrec _ _ (Promote is i e) = text "Prom" <> braces (sep [ fsep (map pPrint $ S.toList is) <> text ";"
                                                               , ppr_eqn (i,e) ])

instance Pretty VerFloats where
  pPrintPrec _ _ NoVerFloats     = empty
  pPrintPrec _ _ (FloatRigid s asm)  = text "FR" <> braces (pPrint s <+> pPrint asm)
  pPrintPrec _ _ (FloatFlexi sk i e) = text "FF" <> braces (pPrint sk <> char ';' <+>
                                                            pPrint i <+> equals <+> pPrint e)

instance (Pretty flts, Pretty a) => Pretty (Result flts a) where
  pPrintPrec _ _ (Res flts res)
    = sep [ pPrint flts, nest 2 (pPrint res) ]

--------------------------------------------------------------------------------
--
--             The prelude
--
--------------------------------------------------------------------------------

mkName :: String -> Ident
mkName s = Name s

mkCons :: Exp -> Exp -> Exp  -- Array cons
mkCons a as = Prm ArrCons :@ mkArr [a, as]

mkCons2 :: ReductionContext -> Exp -> Exp -> Exp -> Exp
-- cons2 = \xy. \xys. exists x y <xs,ys> = <cons x xs, cons y ys>
-- Note: x, y, and xys are expressions, so we need to avoid accudental capture in those.
mkCons2 cxt x y xys
  = Crl $ Blk (S.fromList [xs,ys,ar]) emptyHeap $
    -- We need have the two Arr in this order, otherwise choices
    -- in the body of a 'for' will come in the wrong order.
    (Var ar :=: mkArr [ mkCons x (Var xs)
                      , mkCons y (Var ys)]) :>
    (Arr [Var xs, Var ys] :=: xys) :>
    Var ar
  where
    (xs, ys, ar)  = freshId3 cxt ("xs", "ys", "ar")

-- The definitions in thePrelude are not subject to the ~> transformations.
-- So they should be in the correct form.  In particular, they should be ANFed.
thePrelude :: [Eqn]
thePrelude
  = [ (mkName "int",          Lam vp  $ mkBlkE $ Prm IsInt :@ (Var vp) :> Var vp)
    , (mkName "char",         Lam vp  $ mkBlkE $ Prm IsChar :@ (Var vp) :> Var vp)
    , (mkName "nat",          Lam vp  $ mkBlkE $ Prm IsInt :@ (Var vp) :>
                                                 Prm GEq :@ Arr [Var vp, IntE 0])
--  , (mkName "string",       Lam vp  $ mkBlkE $ Prm IsStr :@ (Var vp) :> Var vp)
    , (mkName "comparable",   Lam vp  $ mkBlkE $ Prm IsComp :@ (Var vp) :> Var vp)
    , (mkName "any",          Lam vp  $ mkBlkE $ Var vp)
    , (mkName "void",         Lam vp  $ mkBlkE $ Arr [])
    , (mkName "Length",       Prm ArrLen)
    , (mkName "prefix'+'",    Prm Pls)
    , (mkName "prefix'-'",    Prm Neg)
    , (mkName "operator'+'",  Prm Add)
    , (mkName "operator'-'",  Prm Sub)
    , (mkName "operator'*'",  Prm Mul)
    , (mkName "operator'/'",  Prm Div)
    , (mkName "operator'<'",  Prm Lt)
    , (mkName "operator'<='", Prm LEq)
    , (mkName "operator'>='", Prm GEq)
    , (mkName "operator'>'",  Prm Gt)
    , (mkName "operator'<>'", Prm NEq)
    , (mkName "operator'..'", Prm DotDot)

    -- [] = \t.\p. isArr[p]; map[t,p]
    , (mkName "prefix'[]'"  , Lam vt $ mkBlkE $ Lam vp $ mkBlkE $
                              (Prm IsArr :@ Var vp) :>
                              (Prm ArrMap :@ (Arr [Var vt, Var vp])))

    -- ? = \t.\x. if (truth{y:any} = x) then { z:=t[y]; truth{z}} else x=()
    , (mkName "prefix'?'"   , Lam vt $ mkBlkE $ Lam vx $ mkBlkE $
                              let y = mkName "_y"
                                  z = mkName "_z" in
                              Iter IF (Blk (S.fromList [y,z])
                                           emptyHeap
                                           ((Tru (Var y) :=: Var vx) :>                             -- condition
                                             Dly ((Var z :=: (Var vt :@ Var y)) :> Tru (Var z) )))  -- then branch
                                      (Var vx :=: Arr [])                                           -- else branch
      )

    ]

vp,vt,vx :: Ident
vp  = mkName "p"
vt  = mkName "t"
vx  = mkName "x"

--------------------------------------------------------------------------------
--
--             The evaluator: driver
--

runTraced :: Fuel -> ReductionContext -> Blk -> Traced Blk
runTraced fuel top_cxt top_blk
  = normalize step valid  fuel top_blk
  where
    valid :: Blk -> Validity
    valid = validBlk (getInScope top_cxt)

    step :: Blk -> Maybe (TraceStep Blk)
    step blk
      | Blk _ _ Fail <- blk   -- Fail will return RedStep; check for naked
      = Nothing               -- failure here to avoid an infinite loop
      | otherwise
      = case reduceBlock top_cxt blk of
          RedNone -> Nothing
          RedStep rule_nm []
             -> Just $ mkTraceStep rule_nm (mkBlkE Fail)
          RedStep rule_nm [Res NoFloats blk']
             -> -- ppTrace ("------ runTraced: " ++ rule_nm ++ " ----------------")
                --        (pPrint blk' $$ text "") $
                Just $ mkTraceStep rule_nm blk'
          red -> error ("runTraced" ++ show red)


mkTraceStep :: RuleName -> Blk -> TraceStep Blk
mkTraceStep rule_nm payload
  = TS { ts_str     = show rule_nm
       , ts_payload = payload
       , ts_verb    = ruleVerbosity rule_nm }

run :: F.SrcEssential -> PExp
-- An alternative to runTraced
run src = P $ evalBlk 1000000 top_blk
  where
    (top_cxt, top_blk) = initialBlk src

    evalBlk :: Int -> Blk -> Exp
    evalBlk _ b | traceReductions && trace (prettyShow b ++ "\n") False = undefined
    evalBlk 0 b = error $ "No fuel: " ++ prettyShow b
    evalBlk fuel b@(Blk is eqs expr) =
        case reduceBlock top_cxt b of
          RedNone | S.null is         -> expr
                  | otherwise         -> mkCrl (Blk is eqs expr)
          RedStep _ []                -> Fail
          RedStep _ [Res NoFloats b'] -> evalBlk (fuel-1) b'
          -- Allow top level choices.  This cannot happen in a real program, but is useful for the REPL
          RedStep _ rs | let bs = [ b' | Res NoFloats b' <- rs ], length rs == length bs
                                      -> mkChoices $ filter (/= Fail) $ map (evalBlk (fuel-1)) bs
          RedStep _ _ -> error "impossible: findTopRedex StepC"  -- can only happen with depth>0

mkChoices :: [Exp] -> Exp
mkChoices [] = Fail
mkChoices es = foldr1 (\ l r -> Blk S.empty [] l :|: Blk S.empty [] r) es

initialBlk :: F.SrcEssential -> (ReductionContext, Blk)
initialBlk src
  = (top_cxt, top_blk)
  where
    term = srcToTerm src
    top_skols   = freeVarsTerm term `S.difference` init_locals
    init_locals = termBndrs term `S.union` S.fromList (map fst thePrelude)

    top_blk = Blk init_locals thePrelude
                  (matchTop top_cxt topMatchContext term)

    top_cxt = RC { rc_depth  = 0
                 , rc_eqns   = emptyHeap
                 , rc_exis   = S.empty
                 , rc_skols  = top_skols
                 , rc_vcxt   = NotVerifying
                 , rc_mode   = top_mode }
    top_mode = RM { rm_just_matching = False }

mergeStep :: HasCallStack => Blk -> Result BlkFloats Exp -> Result BlkFloats Blk
mergeStep (Blk is eqs _) (Res NoFloats res_e)
  = Res NoFloats (Blk is eqs res_e)

mergeStep (Blk is1 eqs _) (Res (Promote is2 i v) res_e)
  = -- `i` should be in `is1`, the enclosing binders
    assertP "mergeStepP1" (i `S.member` is1)
          (vcat [ pPrint i,  pPrint (S.toList is1)
                , pPrint (map (==i) (S.toList is1))
                , pPrint (i `S.member` is1) ]) $
    -- `i` should not be in the domain of `eqs`; we should have substitute instead
    assertP "mergeStepP2" (i `S.notMember` dom eqs) (pPrint (i, dom eqs)) $
    -- Likewise the range `v` of the new binding (i<-v) should not have any occurs checks
    assertP "mergeStepP3" (occfvs v `S.disjoint` dom eqs) (pPrint (occfvs v, dom eqs)) $
    -- `is2` should be disjoint to `is1`
    assertP "mergeStepP4" (is1 `S.disjoint` is2) (pPrint is1 $$ pPrint is2) $
    Res NoFloats (Blk (is1 `S.union` is2)
                      ((i,v) : map (second $ substVal [(i,v)]) eqs) res_e)
    -- We must substitute for `i` in `eqs`;
    --   e.g. exists x,y { x<-y }.  ...(y=3)...
    -- When we promote (y=3) into the heap, we must substitute to get x<-3

mergeStep (Blk is1 eqs1 _) (Res (FloatB is2 eqs2) res_e)
  = assertP "mergeStepF" (is1 `S.disjoint` is2) (pPrint is1 $$ pPrint is2) $
    -- The payload of `FloatB` is already freshened
    Res NoFloats (Blk (is1 `S.union` is2) (eqs1 ++ eqs2') res_e)
  where
    eqs2' = map (second $ substVal eqs1) eqs2
            -- Maybe this would be better done when building FloatB

dom :: Heap -> Set Ident
dom eqs = S.fromList (map fst eqs)

--------------------------------------------------------------------------------
--
--             The evaluator: reduction rules
--
--------------------------------------------------------------------------------

reduceBlock :: ReductionContext -> Blk -> Reduction Blk
reduceBlock cxt@(RC { rc_depth = d, rc_eqns = eqns
                    , rc_exis = all_exis, rc_vcxt = vcxt }) blk
  | not (S.null dead_vars)   -- First try garbage collection
  = (if traceReductions
     then trace $ render $ nest (4*rc_depth cxt) $
          text "reduceBlockGC" <+> (pPrint dead_vars $$ text "---" $$ pPrint blk')
     else id)
    mkDone ("GC" ++ showIds dead_vars) (gcVarsBlk dead_vars blk')

  | let res = case reduceExp cxt' blk' of
          RedNone       -> RedNone
          RedStep rn rs -> RedStep (wrapRuleName blk_nm rn) (map (mergeStep blk') rs)
          VerStep rn rs -> VerStep (wrapRuleName blk_nm rn) (mapPayload (Blk locals' leqns') rs)

  = if traceReductions then
    trace (render $ nest (2*rc_depth cxt) $
           text "reduceBlock enter parent =" <+> pPrintL prettyNormal blk') $
    trace ((render $ nest (2*rc_depth cxt) $
            text "reduceBlock exit " <+>
                ((text "parent =" <+> pPrintL prettyNormal blk') $$
                (text "res    =" <+> pPrintL prettyNormal res)))
            ++ "\n") $
    res
    else res

  where
    blk_nm :: String
    blk_nm = "Blk" ++ showIds locals'

    dead_vars :: Set Ident  -- Subset of locals that are unused
    dead_vars =
        let graph = [ (eqn, i, S.toList $ freeVars e) | eqn@(i, e) <- leqns' ]
            sccs = stronglyConnComp graph
            used = foldr f (freeVars expr') sccs
              where f (AcyclicSCC (i, e)) u | i `S.notMember` u = u
                                            | otherwise         = freeVars e `S.union` u
                    f (CyclicSCC ies)     u | S.fromList is `S.disjoint` u = u
                                            | otherwise         = S.unions (u : map freeVars es)
                                            where (is, es) = unzip ies
        in  locals' `S.difference` used

    blk'@(Blk locals' leqns' expr') = freshenBlk cxt blk
    cxt' = cxt { rc_depth  = d + 1
               , rc_eqns  = eqns ++ leqns'
               , rc_exis  = all_exis `S.union` locals'
               , rc_vcxt  = case vcxt of
                              Verifying as vexis -> Verifying as (vexis `S.union` locals')
                              NotVerifying       -> NotVerifying }

-----------------------------
reduceExp :: ReductionContext -> Blk -> Reduction Exp
-- This function reduces the expresion inside a Blk
-- The context has already been extended with locals, leqns etc
-- We pass in the parent Blk just so that we can give it to `promotionOK`
reduceExp cxt parent@(Blk _ _ body)
  = find True body
 where
    find :: Bool -> Exp -> Reduction Exp
    find leftCF expr  -- leftCF means that the context to the left of the find is choice free
      = find_step      leftCF expr `orTry`
        find_verify           expr `orTry`
        find_admin            expr `orTry`
        find_recursive leftCF expr `orTry`
        find_rec_matches      expr

    find_admin :: Exp -> Reduction Exp
    -- These work even when (findMatching cxt)
    find_admin expr
      = case expr of
          -- Unification, structural
          Val v  :=: (e1 :>  e2) -> mkDone "Norm1" $ e1 :> (v :=: e2)
          Val v  :=: (e1 :=: e2) -> mkDone "Norm2" $ (v :=: e1) :> (v :=: e2)
          (e1 :> e2) :=: e3      -> mkDone "Norm3" $ e1 :> (e2 :=: e3)
          (Val v :=: e1) :=: e2  -> mkDone "Norm4" $ (v :=: e1) :> (v :=: e2)
          (e1 :> e2) :> e3       -> mkDone "Norm5" $ e1 :> (e2 :> e3)
          Val{}  :>  e2          -> mkDone "Seq1" e2

          -- Substitution
          Var i | Just v <- lookup i (rc_eqns cxt)  -- Includes leqns
                -> mkDone ("Subst " ++ show i) v

          -- Promotion
          -- We must try both ways round.  Here's a tricky case
          --   exists x. ...if( exists y. ..x=y.. )...
          -- We must fire (Promote2) on x=y
          -- Hence we must fall through from the first to the second
          Var x :=: e | Just redn <- rednFired $ reduceVarEq cxt "1" parent x e -> redn
          e :=: Var x | Just redn <- rednFired $ reduceVarEq cxt "2" parent x e -> redn

          Match mc x tm -> reduceMatch cxt x tm mc

          -- Floating a nested block {b}
          Crl blk -> mkStep "FloatB" (FloatB is' eqs') e'
            where
              Blk is' eqs' e' = freshenBlk cxt blk

          _ -> RedNone

    find_recursive leftCF expr
      =  -- Catch-all cases for context C; just walk downwards
         -- Aka "look at sub-expressions"
         case expr of
           e1 :>  e2       -> find2 leftCF (:>)  e1 e2
           e1 :=: e2       -> find2 leftCF (:=:) e1 e2
           e1 :@  e2       -> find2 leftCF (:@)  e1 e2
           OfType e1 fx e2 -> find2 leftCF (\x y -> OfType x fx y) e1 e2 `orTry`
                              findOfTypeLam e1 fx e2

           -- We need to look inside arrays to for the (Subst) rule to apply
           -- e.g.  exists x{ x<-3 }. add[x,7]
           -- We want to substitute 3 for x so that we can execute add[3,7]
           -- Similarly (I believe) maps and truth-values
           Arr vs          -> findArr leftCF vs
           Map kvs         -> findMap leftCF kvs
           Tru e           -> find1 leftCF Tru e
           _               -> RedNone

    findOfTypeLam e1 fx (Lam x b)
       = fmap (\b'' -> OfType e1 fx (Lam x' b'')) (reduceBlock cxt' b')
       where
         (cxt', x', b') = freshenLam cxt x b
    findOfTypeLam _ _ _ = RedNone


    -- `find_rec_matches` looks under lambda and delay to find
    -- occurrences of `Match`, so it can rewrite them away
    find_rec_matches expr
      | not (justMatching cxt)
      = RedNone
      | otherwise  -- We are just matching; look under lambda
      = case expr of
          Dly e   -> find1 False Dly e
--ToDo
--          Chocice b1 b2 -> fmap (Lam x') (reduceBlock cxt' b')
          Lam x b -> fmap (Lam x') (reduceBlock cxt'' b')
                  where
                    (cxt', x', b') = freshenLam cxt x b
                    cxt'' = cxt' { rc_vcxt = NotVerifying }

          Iter ic b e -> fmap (\b' -> Iter ic b' e) (reduceBlock cxt b) `orTry`
                         find1 False (Iter ic b) e
                               -- False: irrelevant because we never fire choices
          _ -> RedNone

    find_verify expr
      = case expr of
           e1 :@ e2          -> reduceVerifyApp cxt e1 e2
           Verify skols as e -> reduceVerify cxt skols as e
           _                 -> RedNone

    find_step leftCF expr
      | justMatching cxt
      = RedNone
      | otherwise
      = case expr of
        -- Primops
        Prm op :@ v  -> reducePrimOp op v

        -- Unification
        Val v1 :=: Val v2 | v1 == v2 -> mkDone    "EqVal" v1
        Lit k1 :=: Lit k2 | k1 /= k2 -> mkFailure "EqValFail"

        Arr vs :=: Arr ws
          | length vs == length ws -> mkDone    "EqTup" $
                                      mkSeqs (zipWith (:=:) vs ws) :> Arr vs
          | otherwise              -> mkFailure "EqTupFail"

        Tru v       :=: Tru w      -> mkDone    "EqTru" $ v :=: w :> Tru v
        Val (Map kvs1) :=: Map kvs2 | map fst kvs1 == map fst kvs2 ->
          mkDone "EqMap" $ Map (zipWith (\ (k1, v1) (_, e2) -> (k1, v1 :=: e2)) kvs1 kvs2)
        HNF h1       :=: HNF h2 | root h1 /= root h2       -> mkFailure "EqFail"

        -- Beta (\x.blk)[a] -->  exists x. x = a; blk
        -- NOTE: it should be enough to match with eqs=[]
        Lam x blk :@ arg
            -> mkStep ("Beta(" ++ show x ++ ")") (mkExiFloat1 x') $
               (Var x' :=: arg) :> mkCrl blk'
            where
              x' = freshenId cxt x
              blk' | x==x'     = blk
                   | otherwise = renameBlk (mkRenaming cxt x x') blk

        -- This rule isn't strictly necessary, but it allows indexing by
        -- a constant to proceed outside a failure context.
        Arr es :@ IntE i | 0 <= i' && i' < length es -> mkDone "ITup-k" (es !! i')
                                                     where i' = fromInteger i
        Arr es :@ ei -> mkStep "ITup" (mkExiFloat1 x) $
                        Var x :=: ei :>
                        foldr alt Fail (zipWith (\ i e -> (Var x :=: IntE i) :> e) [0..] es)
          where
            x = freshId cxt "x"
            alt e1 e2 = mkBlkE e1 :|: mkBlkE e2

        Tru e1 :@ e2 -> mkDone "ITru" $ e1 :=: e2

        Val (Map kvs) :@ ei -> mkStep "IMap" (mkExiFloat1 x) $
                               Var x :=: ei :>
                               foldr alt Fail (map (\ (k,v)  -> (Var x :=: k) :> v) kvs)
          where
            x = freshId cxt "x"
            alt e1 e2 = mkBlkE e1 :|: mkBlkE e2

        -- Choice and failure
        Fail -> mkFailure "Fail"
        b1 :|: b2 | rc_depth cxt > 0 && leftCF
             -> -- Found a choice, return it if inside a block
                RedStep (mkRuleName "Choice")
                [ Res NoFloats (mkCrl b1), Res NoFloats (mkCrl b2) ]

        Iter ic b e     -> reduceIter cxt ic b e
        OfType e1 fx e2 -> reduceOfType cxt e1 fx e2

        _ -> RedNone


    find1 :: Bool -> (Exp -> Exp) -> Exp -> Reduction Exp
    find1 leftCF wrap e = fmap wrap (find leftCF e)

    find2 :: Bool -> (Exp -> Exp -> Exp) -> Exp -> Exp -> Reduction Exp
    find2 leftCF c e1 e2
      = find1 leftCF (`c` e2) e1 `orTry`
        find1 (leftCF && choiceFree e1) (e1 `c`) e2

    findArr :: Bool -> [Val] -> Reduction Exp
    findArr _ []          = RedNone
    findArr leftCF (v:vs) = find1 leftCF k v `orTry`
                            find1 leftCF ks (Arr vs)
      where
        k v'         = Arr (v':vs)
        ks (Arr vs') = Arr (v:vs')
        ks e'        = error "findArr" (prettyShow e')

    findMap :: Bool -> [(Gnd,Exp)] -> Reduction Exp
    findMap _      []             = RedNone
    findMap leftCF (kv@(k,v):kvs)
      = find1 leftCF mk1 k `orTry`
        find1 (leftCF && choiceFree k) mk2 v `orTry`
        find1 (leftCF && choiceFree k && choiceFree v) mk3 (Map kvs)
      where
        mk1 k'         = Map ((k', v):kvs)
        mk2 v'         = Map ((k, v'):kvs)
        mk3 (Map kvs') = Map (kv:kvs')
        mk3 e'         = error "findMap" (prettyShow e')

------------------------------------
reduceVarEq :: ReductionContext -> String -> Blk -> Ident -> Val -> Reduction Exp

reduceVarEq rc@(RC { rc_vcxt = vcxt, rc_skols = skols }) left_or_right parent x e
  | promotionOK rc parent x e
  =  mkStep rule_name (Promote S.empty x e) e

  | Verifying {} <- vcxt
  , x `S.member` skols
  , Just (new_skols, gv, v') <- groundValue2 rc e
  , let asm = A_GVEq (GVVar x) gv
  = VerStep (mkRuleName ("V" ++ rule_name ))
     [ Res (FloatRigid new_skols [A_Pred $ A_Pos asm]) v'
     , Res (FloatRigid new_skols [A_Pred $ A_Neg asm]) Fail ]

  | otherwise
  = RedNone
  where
    rule_name = "Promote"++left_or_right ++ "(" ++ show x ++ ")"

promotionOK :: ReductionContext -> Blk -> Ident -> Exp -> Bool
-- True if we can promote (var=val) into the heap for the parent block
-- Even when JustMatching we want to promote /atomic/ values which are just clutter
promotionOK rc (Blk locals leqns _) x e
  =  isVal e
  && x `S.member` locals                    -- Must be bound by the /immediately enclosing/ block
                                            --   i.e. is "flexible"
  && x `S.notMember` dom leqns              -- x must not have an eqn
  && occfvs e `S.disjoint` (sing x `S.union` dom leqns)    -- v must not have variables from eqns
  && (not (justMatching rc) || isAtomic e)

groundValue :: Set SkolIdent -> Exp -> Maybe GroundVal
-- Like skolValue, but no lambdas
groundValue _  (Lit l)                   = Just (GVLit l)
groundValue rs (Var v) | v `S.member` rs = Just (GVVar v)
groundValue rs (Arr vs)                  = do { gvs <- mapM (groundValue rs) vs; Just (GVArr gvs) }
groundValue rs (Tru v)                   = do { gv <- groundValue rs v; Just (GVTru gv) }
groundValue _  _                         = Nothing

groundValue2 :: ReductionContext -> Exp -> Maybe (Set SkolIdent, GroundVal, Exp)
groundValue2 rc e
  | Var v <- e
  = if v `S.member` skols
    then Just (S.empty, GVVar v, e)
    else Nothing

  | Just gv <- gv_maybe e
  , let xs = S.toList (freeVars e `S.difference` skols)  -- Free existentials
        rs = freshIds rc [ "r" | _ <- xs ]
        prs = xs `zip` rs
        ren = mkRenamings rc prs
        binds = [ Var x :=: Var r | (x,r) <- prs ]
  = if null xs   -- Very common case, e.g. literals
    then Just (S.empty, gv, e)
    else Just (S.fromList rs
              , renameGV ren gv
              , mkSeqs binds `mkSeq` rename ren e)

  | otherwise
  = Nothing
  where
    skols = rc_skols rc

    gv_maybe :: Val -> Maybe GroundVal
    gv_maybe (Lit l)  = Just (GVLit l)
    gv_maybe (Var x)  = Just (GVVar x)
    gv_maybe (Arr vs) = do { gvs <- mapM gv_maybe vs; Just (GVArr gvs) }
    gv_maybe (Tru v)  = do { gv  <- gv_maybe v; Just (GVTru gv) }
    gv_maybe _ = Nothing

renameGV :: Renaming -> GroundVal -> GroundVal
renameGV ren gv = go gv
  where
    go (GVLit l)     = GVLit l
    go (GVVar x)     = GVVar (lookupRn x ren)
    go (GVArr gvs)   = GVArr (map go gvs)
    go (GVTru gv1)   = GVTru (go gv1)

skolValue :: ReductionContext -> Exp -> Bool
-- Returns True if
--   Inside verify{}
--   Expression is a value
--   No free var bound inside the verify
skolValue cxt e
  | Verifying _ local_exis <- rc_vcxt cxt
  , isVal e
  = S.null (freeVars e `S.intersection` local_exis)
  | otherwise
  = False

------------------------------------
data PrimOpResult = P_None | P_Failure | P_Done Exp

orTryP :: PrimOpResult -> PrimOpResult -> PrimOpResult
orTryP P_None r2 = r2
orTryP r1     _  = r1

reducePrimOp ::PrimOp -> Exp -> Reduction Exp
reducePrimOp op e
  = case try_it of
       P_Failure -> mkFailure rule_nm
       P_Done e' -> mkDone rule_nm e'
       P_None    -> reduceDotDot op e  -- Returns more than one result
  where
    rule_nm :: String
    rule_nm = "Prim(" ++ show op ++ ")"

    try_it = reduceArithOp op e `orTryP`
             reduceRelOp   op e `orTryP`
             reduceCheckOp op e `orTryP`
             reduceArrOp   op e `orTryP`
             reduceTruOp   op e

-----------------
reduceArithOp :: PrimOp -> Exp -> PrimOpResult
-- Arithmetic ops get stuck on values outside the domain
reduceArithOp Add (Arr [IntE i, IntE j]) = P_Done    $ IntE (i + j)
reduceArithOp Add (Arr [Arr as, Arr bs]) = P_Done    $ Arr (as ++ bs)  -- Array append
reduceArithOp Sub (Arr [IntE i, IntE j]) = P_Done    $ IntE (i - j)
reduceArithOp Mul (Arr [IntE i, IntE j]) = P_Done    $ IntE (i * j)
reduceArithOp Div (Arr [IntE i, IntE j])
  | j /= 0                               = P_Done    $ IntE (i `div` j)

reduceArithOp Neg (IntE i)               = P_Done $ IntE (- i)
reduceArithOp Pls (IntE i)               = P_Done $ IntE i

reduceArithOp IsInt v@(IntE {})          = P_Done v
reduceArithOp IsInt HNF{}                = P_Failure

--reduceArithOp IsStr v@(StrE {})          = P_Done v
--reduceArithOp IsStr HNF{}                = P_Failure

reduceArithOp IsChar v@(CharE {})        = P_Done v
reduceArithOp IsChar HNF{}               = P_Failure

reduceArithOp IsComp (Comp v)            = P_Done v
reduceArithOp IsComp (Con _)             = P_Failure

reduceArithOp _ _ = P_None


-----------------
reduceRelOp :: PrimOp -> Exp -> PrimOpResult
reduceRelOp Lt  (Arr [IntE i, IntE j]) | i<j       = P_Done $ IntE i
                                       | otherwise = P_Failure
reduceRelOp LEq (Arr [IntE i, IntE j]) | i<=j      = P_Done $ IntE i
                                       | otherwise = P_Failure
reduceRelOp GEq (Arr [IntE i, IntE j]) | i>=j      = P_Done $ IntE i
                                       | otherwise = P_Failure
reduceRelOp Gt  (Arr [IntE i, IntE j]) | i>j       = P_Done $ IntE i
                                       | otherwise = P_Failure
reduceRelOp NEq (Arr [IntE i, IntE j]) | i/=j      = P_Done $ IntE i
                                       | otherwise = P_Failure
reduceRelOp _ _ = P_None

-----------------
reduceDotDot :: PrimOp -> Exp -> Reduction Exp
-- Return multiple results for the enumeration.  Get stuck outside the domain
reduceDotDot DotDot (Arr [IntE l, IntE h]) = RedStep (mkRuleName "Prim..")
                                             [Res NoFloats (IntE i) | i <- [l .. h]]
reduceDotDot _ _ = RedNone

-----------------
reduceCheckOp :: PrimOp -> Exp -> PrimOpResult
-- Check operations
reduceCheckOp ChkFails    (Arr [])  = P_Failure
reduceCheckOp ChkSucceeds (Arr [e]) = P_Done e
reduceCheckOp ChkDecides  (Arr [])  = P_Failure
reduceCheckOp ChkDecides  (Arr [e]) = P_Done e
reduceCheckOp _ _ = P_None

-----------------
reduceArrOp :: PrimOp -> Exp -> PrimOpResult
-- Array operations
reduceArrOp ArrCons (Arr [x, Arr xs]) = P_Done $ Arr (x:xs)
reduceArrOp ArrLen (Arr xs)           = P_Done $ IntE (toInteger (length xs))
-- Could have some inverse of ArrLen by reducing
--  (ArrLen :@ e) :=: IntE k  -->  e :=: Arr [_,_,...,_] k new existentials

reduceArrOp IsArr v@(Arr _) = P_Done v
reduceArrOp IsArr HNF{}     = P_Failure

reduceArrOp IsMap v@(Map _) = P_Done v
reduceArrOp IsMap HNF{}     = P_Failure

reduceArrOp ArrMap (Arr [fun, Arr xs])
  = P_Done (mkArr (map (fun :@) xs))

reduceArrOp ArrApp (Arr [a1,a2,res])
   | Arr as1 <- a1, Arr as2 <- a2
   =  -- <vs1>++<vs2> = res  -->  <vs1++vs2> = res
     P_Done (Arr (as1++as2) :=: res)

   | Arr [] <- a1
   = -- <>++a2 = res  -->  a2 = res
     P_Done (a2 :=: res)

   | Arr [] <- a2
   = -- a1++<> = res  -->  a1 = res
     P_Done (a1 :=: res)

   | Arr [] <- res
   = -- a1++a2 = <>  -->  a1=<>; a2=<>
     P_Done $ (a1 :=: Arr []) :> (a2 :=: Arr [])

   | Arr (v:vs) <- a1
   , Arr (r:rs) <- res
   = -- <v:vs>++a2 = <r:rs>  -->  v=r; <vs>++a2 = <rs>; res
     P_Done $
     (v :=: r) :> (Prm ArrApp :@ (Arr [Arr vs, a2, Arr rs])) :> res

   | Arr (Snoc vs v) <- a2
   , Arr (Snoc rs r) <- res
   = -- a1++<vs,v> = <rs,r>  -->  a1++<vs> = <rs>; v=r; res
     P_Done $
     (Prm ArrApp :@ (Arr [a1, Arr vs, Arr rs])) :> (v :=: r) :> res

   -- ToDo: worry about duplicating `res`!!

-- Make sure the keys are ground and unique
reduceArrOp MkMap (Map kvs)
  | let kvs' = L.sort kvs
  , Just ks <- mapM (getGnd . fst) kvs'
  , unique ks
  = P_Done $ Map kvs'

reduceArrOp _ _ = P_None

-- Check that a sorted array has no duplicates
unique :: Eq a => [a] -> Bool
unique xs = and $ zipWith (/=) xs (drop 1 xs)

-----------------
reduceTruOp :: PrimOp -> Exp -> PrimOpResult
reduceTruOp IsTru v@(Tru _) = P_Done v
reduceTruOp IsTru HNF{}     = P_Failure
reduceTruOp _     _         = P_None

------------------------------------
reduceMatch ::  ReductionContext -> Ident -> Term -> MatchContext -> Reduction Exp
-- :~> reduction

reduceMatch cxt x tm mc
  = case tm of
        -- Blocks
        TBlock t -> mkStep ("MBlock" ++ render (pPrint tbndrs))
                           (FloatB tbndrs emptyHeap) (Match mc x t')
                 where
                   (tbndrs, t') = freshenTerm cxt t

        -- Matching
        Und                  -> mkDone "MWild" $ Var x
        TVar i               -> mkDone "MVar"  $ equalsCirc cxt mc x (Var i)
        TLit k               -> mkDone "MLit"  $ equalsCirc cxt mc x (Lit k)
        TPrm o               -> mkDone "MPrim" $ equalsCirc cxt mc x (Prm o)
        TFail                -> mkDone "Mfail" $ Fail

        (t1 :@% t2)          -> mkDone "MApp" $
                                equalsCirc cxt mc x (matchTop cxt mc t1 :@ matchTop cxt mc t2)
        (t1 :=:% t2)         -> mkDone "MUnif"    $ (Match mc x t1) :=: (Match mc x t2)
        (t1 :|:% t2)         -> mkDone "MChoice"  $
                                (mkBlkE $ Match mc x (TBlock t1)) :|:
                                (mkBlkE $ Match mc x (TBlock t2))

        (t1 :>% t2)          -> mkDone "MSemi" $
                                matchTop cxt mc t1 :> Match mc x t2
        (t1 `Where` t2)      -> mkStep "MWhere" (mkExiFloat1 w) $
                                (Var w :=: Match mc x t1) :> matchTop cxt mc t2 :> Var w
                             where
                                w = freshId cxt "w"

        Rng t | DR_Rng fx <- mc_effect mc
              -> mkDone "Mcolon1" $
                 OfType (Var x) fx (matchTop cxt mc t)
              | otherwise
              -> mkDone "MColon2" $ matchTop cxt mc t :@ Var x

        (i := t)   -> mkDone ("MDef " ++ show i) $ Var i :=: Match mc x t
        (i :-> t)  -> mkDone ("MArr " ++ show i) $ (equalsCirc cxt mc x (Var i)) :>
                                                   Match (mcNested mc) i t

        -- Tuples and functions
        TMap kvs     -> matchMap cxt x kvs mc
        Fun at fx bt -> matchFun cxt x at fx bt mc
        TArr ts      -> matchTup cxt x ts mc
        TTru t       -> mkStep "MTru" (mkExiFloat [u,v]) $
                        (Var x :=: Tru (Var u)) :>
                        (Var v :=: Match mc u t) :>
                        Tru (Var v)
                       where
                        (u,v) = freshId2 cxt ("u","v")

        If t0 t1 t2  -> mkDone "MIf" $
                        Iter IF (Blk tbs0 emptyHeap $
                                 matchTop cxt' mc t0' :>
                                 Dly (Match mc x (TBlock t1')))
                                (Match mc x (TBlock t2))
                      where
                        (tbs0, t0', t1') = freshenTerm2 cxt t0 t1
                        cxt' = cxt `addInScopeExis` tbs0

        -- Recognize for(i:=e){i}.  It is how 'all' is encoded,
        -- and we know that it is choice free.
        -- ToDo: isn't it enough to have for{e}{i} where is a variable
        --   x ~> for(t0){v}   -->    iter(ALL){exists tbs(t0); D(t0); D(v)}{<>}
{-
        For (i := t0) (TVar i') | i == i' ->
                        mkDone "MAll" $
                        Var x :=: mkAll (Blk tbs0 emptyHeap $ matchTop cxt' mc t0')
                      where
                        (tbs0, t0') = freshenTerm cxt t0
                        cxt' = cxt `addInScopeExis` tbs0
-}
        For t0 t1 | TVar i <- t1'
                  -> mkDone "MAll" $
                     Var x :=: mkAll (Blk tbs0 emptyHeap $ matchTop cxt' mc t0' :> Var i)

                  | otherwise

                  -> mkStep "MFor" (mkExiFloat1 y) $
                        (Arr [Var x, Var y] :=:
                         Iter FOR (Blk tbs0 emptyHeap $
                                   (matchTop cxt' mc t0') :>
                                   (Lam w (mkBlkE $ Match mc w (TBlock t1'))))
                                  (Arr [Arr [], Arr []])) :>
                        Var y
                  where
                    (tbs0, t0', t1') = freshenTerm2 cxt t0 t1
                    cxt' = cxt `addInScopeExis` tbs0
                    (y,w) = freshId2 (cxt `addInScopeExis` tbs0) ("y","w")

        TOfType t1 _fx t2 -> matchOfType cxt mc x t1 Succeeds t2
          -- Hack: Ignoring the fx on |> in terms
          -- The parser seems to always put <decides> there

        Check fx t -> mkDone ("MCheck " ++ show fx) $
                      Var x :=: mkCheck fx (mkBlkE $ matchTop cxt mc (TBlock t))

        Splice t -> reduceMatch cxt x t mc
                    -- See (AMP1) in Note [Desugaring ampersand]

matchOfType :: ReductionContext -> MatchContext -> Ident -> Term -> Effect -> Term -> Reduction Exp
matchOfType cxt mc x t1 fx t2
 = case mc of
     MC { mc_verify = False, mc_effect = DR_Dom }
        -> mkDone ("TOfTypeXD " ++ show fx) $
           Var x :=: (matchTop cxt mc t2 :@ matchTop cxt mc t1)

     MC { mc_verify = False, mc_effect = DR_Rng fx_pushed }
        -> mkDone ("TOfTypeXR " ++ show fx) $
           OfType (Match mc x t1)
                  (fx `intersectEffect` fx_pushed)
                  (matchTop cxt mc t2)

     MC { mc_verify = True, mc_effect = DR_Dom }
        -> mkStep ("TOfTypeVD " ++ show fx) (mkExiFloat [y,t]) $
           (Var y :=: matchTop cxt mc t1) :>
           (Var t :=: matchTop cxt mc t2) :>
           (mkCheck Succeeds (mkBlkE $ Var t :@ Var y)) :>
           (OfType (Var y) fx (Var t))

     MC { mc_verify = True, mc_effect = DR_Rng {} }
        -> mkStep ("TOfTypeVR " ++ show fx) (mkExiFloat [y,t]) $
           (Var y :=: matchTop cxt mc t1) :>
           (Var t :=: matchTop cxt mc t2) :>
           mkCheck Succeeds (mkBlkE $ Var t :@ Var y)
  where
    (y,t) = freshId2 cxt ("y","t")

equalsCirc :: ReductionContext -> MatchContext -> Ident -> Exp -> Exp
equalsCirc _cxt _mc x e = Var x :=: e

matchFun :: ReductionContext -> Ident -> Term -> Effect -> Term -> MatchContext -> Reduction Exp
--  x ~>mc function(at)<fx>{bt}
matchFun cxt f at fx bt mc
  = mkDone "MFun" $ fun_verify `mkSeq` the_lambda
  where
    blob = mc_blob mc
    (tbs_at, at', bt') = freshenTerm2 cxt at bt
    (u,p,q) = freshId3 (cxt `addInScopeExis` tbs_at) ("u", "p", "q")

    -- Match context for the four sub-matches
    mc_lam_dom = MC { mc_blob   = MNested
                    , mc_effect = DR_Dom
                    , mc_verify = mc_verify mc }

    mc_lam_rng = MC { mc_blob   = blob
                    , mc_effect = DR_Rng fx
                    , mc_verify = False }

    mc_ver_dom = MC { mc_blob   = MTop
                    , mc_effect = DR_Dom
                    , mc_verify = False }

    mc_ver_rng = MC { mc_blob   = blob
                    , mc_effect = DR_Rng fx
                    , mc_verify = True } -- Here we know mc_verify mc = True

    the_lambda = Lam u $
                 Blk (S.fromList [p,q] `S.union` tbs_at) emptyHeap $
                 (Var p :=: Match mc_lam_dom u at') :>
                 wrap_code :> Match mc_lam_rng q (TBlock bt')

    wrap_code = case blob of
                  MNested -> Var q :=: (Var f :@ Var p)
                  MTop    -> IntE 99999

    -- Generate a verify{} if `mc` says so
    fun_verify | mc_verify mc = the_verify
               | otherwise    = Arr []

    -- verify(u){ u ~>(flip) at; check<fx>{ wrap; q~>(no-flip) bt } }
    the_verify = Verify (sing u) [] $
                 Blk (S.fromList [p,q] `S.union` tbs_at) emptyHeap $
                 (Var p :=: Match mc_ver_dom u at') :>
                 (mkCheck fx $ Blk (sing q) emptyHeap $
                  wrap_code :> Match mc_ver_rng q (TBlock bt'))

---------------------------------------
reduceIter :: ReductionContext -> IterCtx -> Blk -> Exp -> Reduction Exp
reduceIter _ IF (Blk is eqs (Dly e)) _
  = mkDone "IIf" $ mkCrl (Blk is eqs e)

reduceIter cxt FOR blk@(Blk _ _ Val{}) e2
  = mkStep "IFor" (mkExiFloat1 x)
                  (mkCons2 cxt (Var x) (mkCrl blk :@ Var x) e2)
  where
    x = freshId cxt "x"

reduceIter _ ALL blk@(Blk _ _ Val{}) e2
  = mkDone "IAll" $ mkCons (mkCrl blk) e2

reduceIter cxt ic b1 e2
  = case reduceBlock cxt b1 of  -- Find a redex in B context
      RedNone       -> RedNone

      VerStep rn rs -> -- For a VerStep, just wrap the unchanged Iter around the outside
                       VerStep (wrapRuleName (show ic) rn)
                               (mapPayload (\b -> Iter ic b e2) rs)

      RedStep rn rs -> mkWrapDone (wrapRuleName (show ic) rn) (foldr iter e2 rs)
           -- Note 1: We update the RuleName to give
           --         more info about where the reduction happened
           -- Note 2: `rs` may be an empty list, indicating failure
  where
    iter :: Result BlkFloats Blk -> Exp -> Exp
    iter (Res NoFloats b) e = Iter ic b e
    iter res e = error ("reduceIter " ++ render (pPrint res $$ pPrint e))

---------------------------------------
matchTup :: ReductionContext -> Ident -> [Term] -> MatchContext -> Reduction Exp
matchTup cxt x ts mc
  = mkStep "MTup" (mkExiFloat (fresh_xs ++ fresh_ys)) $
    (Var x :=: appendArrs cxt3 (map mk_in segs)) :>
    appendArrs cxt3 (map mk_out segs)
  where
    mk_in :: Segment (Ident,Ident,Term) -> Exp
    mk_in (STrue (xi,_,_)) = Var xi
    mk_in (SFalse sprs)    = Arr [ Var xi | (xi,_,_) <- sprs ]

    mk_out :: Segment (Ident,Ident,Term) -> Exp
    mk_out (STrue (xi,_,t)) = Match mc xi t
    mk_out (SFalse sprs) = mkSeqs [ Var yi :=: Match mc xi t | (xi,yi,t) <- sprs ]
                           `mkSeq` Arr [ Var yi | (_,yi,_) <- sprs ]

    fresh_xs = freshIds cxt ["x" | _ <- ts]
    cxt2 = cxt `addInScopeExis` S.fromList fresh_xs
    fresh_ys = freshIds cxt2 ["y" | _ <- ts]
    cxt3 = cxt `addInScopeExis` S.fromList fresh_ys

    segs :: [Segment (Ident,Ident,Term)]
    segs = segments is_splice (zip3 fresh_xs fresh_ys ts)

    is_splice (xi, yi, Splice t) = Just (xi,yi,t)
    is_splice _             = Nothing

data Segment a = STrue  a | SFalse [a]

appendArrs :: ReductionContext -> [Exp] -> Exp
-- appendArrs [a1, .., an] = a1 `ArrApp` a2 `ArrApp` ... an
--
-- We want   arrApp$[a1,a2,b2]
--         ; arrApp$[y2,a3,b3]; ...
--         ; arrApp[b(n-1),an,bn]
--         ; bn
appendArrs _   []      = Arr []
appendArrs cxt (a:as) = mkCrl $ Blk (S.fromList (map fst prs)) [] $
                        go a prs
  where
    go a1 ((b2,a2):bas) = (Prm ArrApp :@ mkArr [a1, a2, Var b2]) :> go (Var b2) bas
    go a1 []            = a1

    prs :: [(Ident,Exp)]
    prs = freshIds cxt ["b" | _ <- as] `zip` as

segments :: (a-> Maybe a) -> [a] -> [Segment a]
-- Split the list into chunks
segments _ []              = []
segments p (t : ts)
  = case p t of
      Just t' ->  STrue t' : segments p ts
      Nothing ->  SFalse (t:ts1) : segments p ts2
              where
                (ts1,ts2) = span (isNothing . p) ts

---------------------------------------
matchMap :: ReductionContext -> Ident -> [(Term, Term)] -> MatchContext -> Reduction Exp
matchMap cxt x akvs mc
  = mkDone "MMap" $ Var x :=: Map (map do_one akvs)
  where
    do_one (k, e) = (matchTop cxt mc k, matchTop cxt mc e)

---------------------------------------
choiceFree :: Exp -> Bool
choiceFree Var{} = True
choiceFree Lit{} = True
choiceFree Prm{} = True
choiceFree Lam{} = True
choiceFree (Dly e) = choiceFree e
choiceFree (e1 :> e2) = choiceFree e1 && choiceFree e2
choiceFree (_ :|: _) = False
choiceFree Fail = True
choiceFree (e1 :=: e2) = choiceFree e1 && choiceFree e2
choiceFree (Match _ _ _) = False         -- force ~> to happen
choiceFree (e1 :@ e2) = choiceFree e1 && choiceFree e2
choiceFree (Arr es) = and (map choiceFree es)
choiceFree (Iter IF _ _) = False   -- could do better
choiceFree (Iter ALL _ _) = True
choiceFree (Iter FOR _ e) = choiceFree e
choiceFree (Crl b) = choiceFreeB b
choiceFree Verify{} = True
choiceFree (Tru e) = choiceFree e
choiceFree (Map kvs) = and (map (choiceFree . fst) kvs) && and (map (choiceFree . snd) kvs)

choiceFreeB :: Blk -> Bool
choiceFreeB (Blk _ _ e) = choiceFree e


--------------------------------------------------------------------------------
--
--             Verification rules
--
--------------------------------------------------------------------------------

reduceOfType :: ReductionContext -> Exp -> Effect -> Exp -> Reduction Exp
-- Inside verify:
--    verify(R;A){ P[ e1 |>fx e2 ] }  -->   verify(R,sk;A){ exists z. z = e2[sk]; P[ z ] }
reduceOfType cxt e1 fx e2
  | skolValue cxt e2  -- Only true inside verify{}
  , [sk, z] <- freshIds cxt ["sk", "z"]
  = let succ_res = Res (FloatFlexi sk z (e2 :@ Var sk)) (Var z)
        fail_res = Res NoVerFloats                      Fail
    in
    VerStep (mkRuleName "Skolemise") $
    case fx of
       Succeeds -> [succ_res]
       Fails    -> [fail_res]
       Decides  -> [succ_res, fail_res]
       Iterates -> error "reduceOfType:Iterates"  -- Not sure, probably impossible

   | not (insideVerify cxt)
   = mkDone "OfType" (e2 :@ e1)   -- Ignores <fx>

   | otherwise
   = RedNone

reduceVerify :: ReductionContext -> Set Ident -> [Assump] -> Blk -> Reduction Exp
reduceVerify cxt skols as blk
  | Blk _ _ (Val {}) <- blk
  = mkDone "VVal" (Arr [])
    -- We can finish up with
    --  verify(r){ exists x {x <- r}. \y. x }
    -- and this is a successful verification

  | Just reason <- unsat all_as
  = mkDone ("VUnsat " ++ render (pPrint reason)) (Arr [])

  | otherwise
  = case reduceBlock cxt' blk' of
      RedNone       -> RedNone
      RedStep rn rs -> mkWrapDone (wrapRuleName "VR" rn) (mkSeqs (map wrap_blk rs))
      VerStep rn rs -> mkWrapDone (wrapRuleName "VV" rn) (mkSeqs (map wrap_ver rs))
  where
    wrap_blk (Res NoFloats blk'') = Verify skols' as' blk''
    wrap_blk r = error ("reduceVerify " ++ render (pPrint r))

    wrap_ver (Res NoVerFloats blk'')           = Verify skols' as' blk''
    wrap_ver (Res (FloatRigid sks asms) blk'') = Verify (sks `S.union` skols')
                                                        (asms ++ as')
                                                        blk''
    wrap_ver (Res (FloatFlexi sk z rhs) blk'')
      | Blk exis hp body <- blk''
      = Verify (sk `S.insert` skols') as' $
        Blk (z `S.insert` exis) hp ((Var z :=: rhs) :> body)

    (subst, skols') = freshenBndrs (emptyRenaming cxt) skols
    as'  = renameAssumps subst as
    blk' = renameBlk subst blk

    all_as :: [Assump]  -- Includes outer assumptions
    all_as = case rc_vcxt cxt of
               NotVerifying         -> as'
               Verifying outer_as _ -> outer_as ++ as'

    cxt' = cxt { rc_skols = rc_skols cxt `S.union` skols'
               , rc_vcxt  = Verifying all_as S.empty }

reduceVerifyApp :: ReductionContext -> Exp -> Exp -> Reduction Exp
reduceVerifyApp cxt fun arg
  | NotVerifying <- rc_vcxt cxt
  = RedNone

  | Prm op <- fun
  , Just gv <- groundValue (rc_skols cxt) arg
  , not (isClosedGV gv)
  = reduceVerifyOpGV cxt op arg gv

  | Var f <- fun
  , f `S.member` rc_skols cxt
  , Just gv <- groundValue (rc_skols cxt) arg
  , let ap_op = A_PrimOp r AO_Apply (GVArr [GVVar f, gv])
  = VerStep (mkRuleName "VSkolApply") $
    [ Res (FloatRigid (sing r) [ap_op]) (Var r)  ]

  | otherwise
  = RedNone
  where
    r = freshId cxt "apr"

isClosedGV :: GroundVal -> Bool
isClosedGV (GVVar {})  = False
isClosedGV (GVLit {})  = True
isClosedGV (GVArr gvs) = all isClosedGV gvs
isClosedGV (GVTru gv)  = isClosedGV gv

reduceVerifyOpGV :: ReductionContext -> PrimOp -> Exp -> GroundVal -> Reduction Exp
reduceVerifyOpGV _cxt op _arg _gv
  | primOpIsCheck op --  Do not skolemise ChkSucceeds etc
  =  RedNone

-- Primitive operators
-- verify(R;A){ P[     op[gv] ] } -> verify(R,r; A,r=op[gv],preconds(op,gv)){ P[ r ] }
--                                   verify(R; A,not(precond1(op,gv))){ P[ stuck ] }
--                                   verify(R; A,not(precond2(op,gv))){ P[ stuck ] }
--
-- verify(R;A){ P[ predop[gv] ] } -> verify(R; A,    op[gv], preconds(op,gv) ){ P[ gv ] }
--                                   verify(R; A,not(precond1(op,gv))){ P[ stuck ] }
--                                   verify(R; A,not(precond2(op,gv))){ P[ stuck ] }
--                                   verify(R; A,not(op[gv])){ P[ fail ] }
reduceVerifyOpGV cxt op arg gv
  | Just preds <- primOpPreCond op gv
  , let pos_assumps = map A_Pred preds
  = VerStep (mkRuleName ("VUnaryOp" ++ show op)) $
    [ Res (FloatRigid S.empty [A_Pred (notPred p)]) stuck | p <- preds ] ++
    if primOpCanFail op
    then [ Res (FloatRigid S.empty  [A_Pred (A_Neg rel_op)])              Fail
         , Res (FloatRigid S.empty  (A_Pred (A_Pos rel_op) : pos_assumps)) result   ]
    else [ Res (FloatRigid (sing r) (bin_op                : pos_assumps)) (Var r)  ]
  where
    r = freshId cxt "r"

    rel_op = A_RelOp op gv
    bin_op = A_PrimOp r (AO_Prim op) gv

    stuck :: Exp
    stuck = Err (render (pPrint op <> brackets (pPrint gv)))

    result :: Exp
    result | isBinRelOp op = get_arg1 arg
           | otherwise     = arg

    -- get_arg1 is a bit horrible, but if primOpPreCond fires, and isBinOp,
    -- then we know that gv must be GVArr, and hence arg is Arr.
    get_arg1 (Arr [arg1,_]) = arg1
    get_arg1 _ = error "reduceVerifyOpGV:get_arg1" (pPrint op <+> pPrint arg)

-- Binary operators where the argument is a skolem
reduceVerifyOpGV cxt op _ (GVVar r)
  | isBinOp op
  = VerStep (mkRuleName "VBinOp-Arr")
    [ Res (FloatRigid rs [A_Pred $ A_Pos eq_asm]) (Arr [Var r1,Var r2])
    , Res (FloatRigid rs [A_Pred $ A_Neg eq_asm]) Fail ]
  where
    rs = S.fromList [r1,r2]
    (r1,r2) = freshIds2 cxt "r"
    eq_asm = A_GVEq (GVVar r) (GVArr [GVVar r1, GVVar r2])

reduceVerifyOpGV _cxt op arg _gv = error ("reduceVerifyOp " ++ render (pPrint op $$ pPrint arg))

--------------------------------------------------------------------------------
--
--             Free and bound variables
--
--------------------------------------------------------------------------------

showIds :: Set Ident -> String
showIds xs = show (S.toList xs)

-- Top level binders
termBndrs :: Term -> Set Ident
termBndrs tm = go tm
  where
    go Und{}             = S.empty
    go TLit{}            = S.empty
    go TVar{}            = S.empty
    go TPrm{}            = S.empty
    go (t1 :>% t2)       = go t1 `S.union` go t2
    go (t1 `Where` t2)   = go t1 `S.union` go t2
    go (t1 :=:% t2)      = go t1 `S.union` go t2
    go (t1 :@% t2)       = go t1 `S.union` go t2
    go TFail{}           = S.empty
    go (_ :|:% _)        = S.empty
    go (TArr ts)         = S.unions $ map go ts
    go Fun{}             = S.empty
    go If{}              = S.empty
    go For{}             = S.empty
    go (Rng t)           = go t
    go (x := t)          = sing x `S.union` go t   -- (:=) binds
    go (_ :-> t)         = go t                  -- (:->) does not bind
    go (TBlock {})       = S.empty
    go (TOfType t1 _ t2) = go t1 `S.union` go t2
    go (Splice t)        = go t
    go (Check _ t)       = go t
    go (TTru t)          = go t
    go (TMap kvs)        = S.unions $ map (go . fst) kvs ++ map (go . snd) kvs

-- Variable uses, not under lambda/delay
occfvs :: Exp -> Set Ident
occfvs (Var x)        = sing x
occfvs Err{}          = S.empty
occfvs Lit{}          = S.empty
occfvs Prm{}          = S.empty
occfvs Lam{}          = S.empty
occfvs Dly{}          = S.empty
occfvs (e1 :> e2)     = occfvs e1 `S.union` occfvs e2
occfvs (b1 :|: b2)    = occfvsB b1 `S.union` occfvsB b2
occfvs Fail           = S.empty
occfvs (e1 :=: e2)    = occfvs e1 `S.union` occfvs e2
occfvs (Match _ i _)   = sing i  -- XXX what should we do here
occfvs (e1 :@ e2)     = occfvs e1 `S.union` occfvs e2
occfvs (Arr es)       = S.unions (map occfvs es)
occfvs (Iter _ b1 e2) = occfvsB b1 `S.union` occfvs e2
occfvs (Crl b)        = occfvsB b
occfvs Verify{}       = S.empty
occfvs (OfType e1 _ e2) = occfvs e1 `S.union` occfvs e2
occfvs (Tru e)        = occfvs e
occfvs (Map kvs)      = S.unions $ map (occfvs . fst) kvs ++ map (occfvs . snd) kvs

occfvsB :: Blk -> Set Ident
occfvsB (Blk is eqs e) = (S.unions (occfvs e : map (occfvs . snd) eqs)) `S.difference` is

-- All /free/ /existential/ variables
freeVars :: Exp -> Set Ident
freeVars (Err {})         = S.empty
freeVars (Lit {})         = S.empty
freeVars (Prm {})         = S.empty
freeVars (Var i)          = sing i
freeVars (Lam i e)        = i `S.delete` freeVarsBlk e
freeVars (e1 :>  e2)      = freeVars e1 `S.union` freeVars e2
freeVars (e1 :=: e2)      = freeVars e1 `S.union` freeVars e2
freeVars (Match _ i t)     = sing i `S.union` freeVarsTerm t
freeVars (e1 :@  e2)      = freeVars e1 `S.union` freeVars e2
freeVars (Arr es)         = S.unions $ map freeVars es
freeVars (Tru e)          = freeVars e
freeVars (b1 :|: b2)      = freeVarsBlk b1 `S.union` freeVarsBlk b2
freeVars Fail             = S.empty
freeVars (Dly e)          = freeVars e
freeVars (Crl b)          = freeVarsBlk b
freeVars (Iter _ b1 e2)   = freeVarsBlk b1 `S.union` freeVars e2
freeVars (Verify s _ b)   = freeVarsBlk b `S.difference` s  -- No free exis in assumptions
freeVars (OfType e1 _ e2) = freeVars e1 `S.union` freeVars e2
freeVars (Map kvs)        = S.unions $ map (freeVars . fst) kvs ++ map (freeVars . snd) kvs

freeVarsBlk :: Blk -> Set Ident
freeVarsBlk (Blk is eqs e) = (S.unions (map (freeVars . snd) eqs) `S.union` freeVars e)
                              `S.difference` is

freeVarsTerm :: Term -> Set Ident
-- All variables mentioned, either as occurrences /or binders/,
-- freeVarsTermBlock handles the block scope
freeVarsTerm (TVar i) = sing i
freeVarsTerm Und      = S.empty
freeVarsTerm (TLit _) = S.empty
freeVarsTerm (TPrm _) = S.empty
freeVarsTerm (i := t) = sing i `S.union` freeVarsTerm t
freeVarsTerm (e1 :>%  e2) = freeVarsTerm e1 `S.union` freeVarsTerm e2
freeVarsTerm (e1 `Where`  e2) = freeVarsTerm e1 `S.union` freeVarsTerm e2
freeVarsTerm (e1 :=:% e2)  = freeVarsTerm e1 `S.union` freeVarsTerm e2
freeVarsTerm (e1 :@%  e2)  = freeVarsTerm e1 `S.union` freeVarsTerm e2
freeVarsTerm (Fun t1 _ t2) = (freeVarsTerm t1 `S.union` freeVarsTermBlock t2)
                             `S.difference` termBndrs t1
freeVarsTerm (Rng e)       = freeVarsTerm e
freeVarsTerm (TArr es)     = S.unions $ map freeVarsTerm es
freeVarsTerm (b1 :|:% b2)  = freeVarsTermBlock b1 `S.union` freeVarsTermBlock b2
freeVarsTerm TFail         = S.empty
freeVarsTerm (If t1 t2 t3) = ((freeVarsTerm t1 `S.union` freeVarsTermBlock t2)
                              `S.difference` termBndrs t1)
                             `S.union` freeVarsTermBlock t3
freeVarsTerm (For t1 t2)   = (freeVarsTerm t1 `S.union` freeVarsTermBlock t2)
                             `S.difference` termBndrs t1
freeVarsTerm (i :-> e)     = sing i `S.union` freeVarsTerm e
freeVarsTerm (TBlock t)    = freeVarsTermBlock t
freeVarsTerm (Check _ t)   = freeVarsTerm t
freeVarsTerm (Splice t)    = freeVarsTerm t
freeVarsTerm (TOfType t1 _ t2) = freeVarsTerm t1 `S.union` freeVarsTerm t2
freeVarsTerm (TTru t)      = freeVarsTerm t
freeVarsTerm (TMap kvs)    = S.unions $ map (freeVarsTerm . fst) kvs ++ map (freeVarsTerm . snd) kvs

freeVarsTermBlock :: Term -> Set Ident
freeVarsTermBlock t = freeVarsTerm t `S.difference` termBndrs t

expSize :: Exp -> Int
expSize (Err {})    = 1
expSize (Var {})    = 1
expSize (Lit {})    = 1
expSize (Prm {})    = 1
expSize (Lam _ e)   = 1 + blkSize e
expSize (e1 :>  e2) = 1 + expSize e1 + expSize e2
expSize (e1 :=: e2) = 1 + expSize e1 + expSize e2
expSize (Match _ _  e) = 2 + termSize e
expSize (e1 :@  e2) = 1 + expSize e1 + expSize e2
expSize (Arr es)    = 1 + sum (map expSize es)
expSize (b1 :|: b2) = 1 + blkSize b1 + blkSize b2
expSize Fail        = 1
expSize (Dly e)     = 1 + expSize e
expSize (Crl b)     = 1 + blkSize b
expSize (Iter _ b1 e2) = 1 + blkSize b1 + expSize e2
expSize (Verify _ _ b)   = 1 + blkSize b
expSize (OfType e1 _ e2) = 1 + expSize e1 + expSize e2
expSize (Tru e)          = 1 + expSize e
expSize (Map kvs)    = 1 + sum (map (expSize . fst) kvs) + sum (map (expSize . snd) kvs)

blkSize :: Blk -> Int
blkSize (Blk is eqs e) = S.size is + sum (map eqnSize eqs) + expSize e

eqnSize :: Eqn -> Int
eqnSize (_,e) = 1 + expSize e

termSize :: Term -> Int
-- All variables mentioned, either as occurrences or binders
termSize (TVar _)          = 1
termSize Und               = 1
termSize (TLit _)          = 1
termSize (TPrm _)          = 1
termSize (_ := e)          = 1 + termSize e
termSize (e1 :>%  e2)      = 1 + termSize e1 + termSize e2
termSize (e1 `Where`  e2)  = 1 + termSize e1 + termSize e2
termSize (e1 :=:% e2)      = 1 + termSize e1 + termSize e2
termSize (e1 :@%  e2)      = 1 + termSize e1 + termSize e2
termSize (Fun e1 _ e2)     = 1 + termSize e1 + termSize e2
termSize (Rng e)           = 1 + termSize e
termSize (TArr es)         = 1 + sum (map termSize es)
termSize (b1 :|:% b2)      = 1 + termSize b1 + termSize b2
termSize TFail             = 1
termSize (If t1 t2 t3)     = 1 + termSize t1 + termSize t2 + termSize t3
termSize (For t1 t2)       = 1 + termSize t1 + termSize t2
termSize (_ :-> e)         = 1 + termSize e
termSize (TBlock t)        = 1 + termSize t
termSize (Check _ t)       = 1 + termSize t
termSize (Splice t)        = 1 + termSize t
termSize (TOfType t1 _ t2) = 1 + termSize t1 + termSize t2
termSize (TTru t)          = 1 + termSize t
termSize (TMap kvs)        = 1 + sum (map (termSize . fst) kvs) + sum (map (termSize . snd) kvs)

validBlk :: Set Ident -> Blk -> Validity
validBlk in_scope (BlkX xs hp e)
  = mconcat (map valid_eqn hp) D.<>
    validExp in_scope' e
  where
    in_scope' = in_scope `S.union` xs
    eqn_bndrs = S.fromList (map fst hp)

    valid_eqn (x,rhs)
      = validIdOcc eqn_bndrs x D.<>
        check_rhs D.<>
        validExp in_scope' rhs
      where
        check_rhs | S.null (occfvs rhs `S.intersection` eqn_bndrs)
                  = Valid
                  | otherwise
                  = Invalid (text "Occurs check"
                             <+> pPrint x <+> text "<-" <+> pPrint rhs)

validExp :: Set Ident -> Exp -> Validity
validExp in_scope e = go e
  where
    goB blk = validBlk in_scope blk

    go :: Exp -> Validity
    go (Var x)           = validIdOcc in_scope x
    go Err{}             = mempty
    go Lit{}             = mempty
    go Prm{}             = mempty
    go (Lam x b)         = validBlk (S.insert x in_scope) b
    go (Dly e1)          = go e1
    go (e1 :> e2)        = go e1 D.<> go e2
    go (b1 :|: b2)       = goB b1 D.<> goB b2
    go Fail              = mempty
    go (e1 :=: e2)       = go e1 D.<> go e2
    go (Match _ i _)     = validIdOcc in_scope i
    go (e1 :@ e2)        = go e1 D.<> go e2
    go (Arr vs)          = mconcat (map goV vs)
    go (Iter _ b1 e2)    = goB b1 D.<> go e2
    go (Crl b)           = goB b
    go (Verify rs _as b) = validBlk (in_scope `S.union` rs) b
                           -- ToDo: check 'as
    go (OfType e1 _ e2)  = go e1 D.<> go e2
    go (Tru v)           = goV v
    go (Map kvs)         = mconcat [ go a D.<> goV b | (a,b) <- kvs ]

    goV :: Val -> Validity
    goV (Var x)   = validIdOcc in_scope x
    goV (HNF e1)  = go e1
    goV e1        = Invalid (text "Not a value:" <+> pPrint e1 $$ pPrint (in_scope, e))

validIdOcc :: Set Ident -> Ident -> Validity
validIdOcc in_scope x
  | x `S.member` in_scope = Valid
  | otherwise             = Invalid (quotes (pPrint x) <+> text "is not in scope")

 --------------------------------------------------------------------------------
--
--             Renaming
--
--------------------------------------------------------------------------------

data Renaming = RN { rn_in_scope :: Set Ident
                   , rn_subst    :: Subst Ident }

instance Pretty Renaming where
  pPrintPrec _ _ (RN { rn_in_scope = in_scope, rn_subst = subst })
     = text "RN" <> braces (sep [ text "in_scope=" <> pPrint in_scope
                                , text "subst=" <> pPrint subst ])

nullRenaming :: Renaming -> Bool
nullRenaming rn = nullSubst (rn_subst rn)

emptyRenaming :: ReductionContext -> Renaming
emptyRenaming cxt = RN { rn_in_scope = getInScope cxt, rn_subst = emptySubst }

mkRenaming :: ReductionContext -> Ident -> Ident -> Renaming
mkRenaming cxt x x' = RN { rn_in_scope = S.insert x' (getInScope cxt)
                         , rn_subst    = [(x,x')] }
mkRenamings :: ReductionContext -> [(Ident,Ident)] -> Renaming
mkRenamings cxt prs = RN { rn_in_scope = foldr (S.insert . snd) (getInScope cxt) prs
                         , rn_subst    = prs }


lookupRn :: Ident -> Renaming -> Ident
lookupRn i (RN { rn_subst = sub }) = case lookup i sub of
                                       Just i' -> i'
                                       Nothing -> i

rename :: Renaming -> Exp -> Exp
rename sub expr
  | nullRenaming sub = expr
  | otherwise        = ren expr
  where
    ren :: Exp -> Exp
    ren e@(Err {})    = e
    ren e@(Lit {})    = e
    ren e@(Prm {})    = e
    ren e@Fail        = e
    ren (Var i)       = Var (lookupRn i sub)
    ren (Lam i b)     = let (sub', i') = freshenBndr S.empty sub i
                        in Lam i' (renameBlk sub' b)
    ren (e1 :> e2)    = ren e1 :> ren e2
    ren (e1 :=: e2)   = ren e1 :=: ren e2
    ren (Match b i t) = Match b (lookupRn i sub) (renT t)
    ren (e1 :@ e2)    = ren e1 :@ ren e2
    ren (Arr es)      = Arr (map ren es)
    ren (b1 :|: b2)   = renB b1 :|: renB b2
    ren (Dly e)       = Dly (ren e)
    ren (Crl b)       = Crl (renB b)
    ren (Iter ic b1 e2) = Iter ic (renB b1) (ren e2)
    ren (OfType e1 fx e2) = OfType (ren e1) fx (ren e2)
    ren (Verify is as b) = Verify is' (renameAssumps sub' as) (renameBlk sub' b)
       where
         (sub', is') = freshenBndrs sub is
    ren (Tru e) = Tru (ren e)
    ren (Map kvs) = Map (map (ren *** ren) kvs)

    renB = renameBlk sub
    renT = renameTerm sub

renameTerm :: Renaming -> Term -> Term
renameTerm sub term = go term
  where
    go e@(TLit {})        = e
    go e@(TPrm {})        = e
    go e@TFail            = e
    go e@Und              = e
    go (TVar i)           = TVar (lookupRn i sub)
    go (e1 :>% e2)        = go e1 :>% go e2
    go (e1 :=:% e2)       = go e1 :=:% go e2
    go (e1 :@% e2)        = go e1 :@% go e2
    go (TArr es)          = TArr (map go es)
    go (b1 :|:% b2)       = go b1 :|:% go b2
    go (Where t1 t2)      = Where (go t1) (go t2)
    go (For t1 t2)        = For (go t1) (go t2)
    go (If t1 t2 t3)      = If (go t1) (go t2) (go t3)
    go (Fun t1 fx t2)     = Fun (go t1) fx (go t2)
    go (Rng t)            = Rng (go t)
    go (i := t)           = lookupRn i sub := go t
    go (i :-> t)          = lookupRn i sub :-> go t
    go (TBlock t)         = TBlock (go t)
    go (Check fx t)       = Check fx (go t)
    go (Splice t)         = Splice (go t)
    go (TOfType t1 fx t2) = TOfType (go t1) fx (go t2)
    go (TTru t)           = TTru (go t)
    go (TMap kvs)         = TMap (map (go *** go) kvs)

renameBlk :: Renaming -> Blk -> Blk
renameBlk sub (Blk is eqs e)
  = Blk is' (renameEqns sub' eqs) (rename sub' e)
  where
    (sub', is') = freshenBndrs sub is

renameEqns :: Renaming -> [Eqn] -> [Eqn]
renameEqns sub eqns
  = [ (lookupRn i sub, rename sub e) | (i,e) <- eqns ]

renameAssumps :: Renaming -> [Assump] -> [Assump]
renameAssumps subst as = map (C.substAssump (rn_subst subst)) as

-- substVal is only used to substitute into the RHS of the heap.
-- Lambdas may still contain variables in the heap, so we leave those alone.
substVal :: Subst Exp -> Val -> Val
substVal sub e@(Var i) = fromMaybe e $ lookup i sub
substVal _ e@Lit{} = e
substVal _ e@Prm{} = e
substVal sub (Arr vs) = Arr (map (substVal sub) vs)
substVal sub (Tru v) = Tru (substVal sub v)
substVal _ e@Lam{} = e
substVal sub (Map kvs) = Map (map (substVal sub *** substVal sub) kvs)
substVal _ e = error $ "substVal: not a Val: " ++ show e

gcVarsBlk :: Set Ident -> Blk -> Blk
gcVarsBlk xs (Blk is eqs expr) = Blk (is `S.difference` xs)
                                     (filter ((`S.notMember` xs) . fst) eqs)
                                     expr


--------------------------------------------------------------------------------
--
--             Fresh names
--
--------------------------------------------------------------------------------

freshenTerm :: ReductionContext -> Term -> (Set Ident, Term)
freshenTerm cxt term
  | nullRenaming subst = (tbndrs, term)
  | otherwise          = (tbndrs', renameTerm subst term)
  where
    tbndrs = termBndrs term
    (subst, tbndrs') = freshenBndrs (emptyRenaming cxt) tbndrs

-- Use to freshen if/for/fun, because renamings from term1
-- also need to apply to term2.
freshenTerm2 :: ReductionContext -> Term -> Term -> (Set Ident, Term, Term)
freshenTerm2 cxt term1 term2
  | nullRenaming subst = (tbndrs, term1, term2)
  | otherwise          = (tbndrs', renameTerm subst term1, renameTerm subst term2)
  where
    tbndrs = termBndrs term1
    (subst, tbndrs') = freshenBndrs (emptyRenaming cxt) tbndrs

freshenLam :: ReductionContext -> Ident -> Blk -> (ReductionContext, Ident, Blk)
freshenLam cxt x b = (cxt', x', b')
  where
    (subst, x') = freshenBndr S.empty (emptyRenaming cxt) x
    b' = renameBlk subst b
    cxt' = cxt { rc_skols = S.insert x' (rc_skols cxt) }

freshenBlk :: ReductionContext -> Blk -> Blk
freshenBlk cxt blk@(Blk locals leqns expr)
  | nullRenaming subst = blk
  | otherwise          = Blk locals' (renameEqns subst leqns) (rename subst expr)
  where
    (subst, locals') = freshenBndrs (emptyRenaming cxt) locals

freshId :: ReductionContext -> String -> Ident
freshId cxt s = case freshIds cxt [s] of
                  (n:_) -> n
                  []    -> error "freshId"

freshIds2 :: ReductionContext -> String -> (Ident,Ident)
freshIds2 cxt s = case freshIds cxt (repeat s) of
                   (n1:n2:_) -> (n1,n2)
                   []        -> error "freshIds2"

freshId2 :: ReductionContext -> (String,String) -> (Ident,Ident)
freshId2 cxt (s1,s2)
  = case freshIds cxt [s1,s2] of
      [n1,n2] -> (n1,n2)
      _       -> error "freshId2"

freshId3 :: ReductionContext -> (String,String,String) -> (Ident,Ident,Ident)
freshId3 cxt (s1,s2,s3)
  = case freshIds cxt [s1,s2,s3] of
      [n1,n2,n3] -> (n1,n2,n3)
      _          -> error "freshId3"

freshId4 :: ReductionContext -> (String,String,String,String) -> (Ident,Ident,Ident,Ident)
freshId4 cxt (s1,s2,s3,s4)
  = case freshIds cxt [s1,s2,s3,s4] of
      [n1,n2,n3,n4] -> (n1,n2,n3,n4)
      _             -> error "freshId4"

freshIds :: ReductionContext -> [String] -> [Ident]
-- Return a list same length as input
freshIds (RC { rc_exis = exis, rc_skols = skols }) strings
  = go (exis `S.union` skols) strings
  where
    go :: Set Ident -> [String] -> [Ident]
    go _avoid []     = []
    go avoid  (s:ss) = n : go (n `S.insert` avoid) ss
             where
               n = findFresh (`S.member` avoid) (Name s)

freshenId :: ReductionContext -> Ident -> Ident
freshenId (RC { rc_exis = exis, rc_skols = skols }) x
  = findFresh bad x
  where
    bad n = n `S.member` exis || n `S.member` skols

freshenBndr :: Set Ident -> Renaming -> Ident -> (Renaming, Ident)
freshenBndr avoids (RN { rn_in_scope = in_scope, rn_subst = subst }) x
  | need_fresh = (RN { rn_in_scope = S.insert x' in_scope
                     , rn_subst    = insertSubst x x' subst }, x')
  | otherwise  = (RN { rn_in_scope = S.insert x in_scope
                     , rn_subst    = deleteSubst x subst  }, x)
  where
    need_fresh = x `S.member` in_scope
    x' = findFresh bad x
    bad i = i `S.member` avoids || i `S.member` in_scope

freshenBndrs :: Renaming -> Set Ident -> (Renaming, Set Ident)
freshenBndrs rn xs
  = (rn', S.fromList xs')
  where
    (rn', xs') = L.mapAccumL (freshenBndr xs) rn (S.toList xs)
    -- Treat the current xs as 'avoids'. Consider
    --   in-scope:  {u0}
    --   xs:        {u0, u1}
    -- We must freshen u0; but we should rename it to 'u2',
    -- not 'u1', to avoid gratuitously renaming u1.

findFresh :: (Ident -> Bool) -> Ident -> Ident
findFresh bad orig_id@(Name s)
  | bad orig_id = go 0
  | otherwise   = orig_id
  where
    prefix1 = reverse $ dropWhile isDigit $ reverse s
    prefix | null prefix1 = "u"
           | otherwise    = prefix1

    go :: Int -> Ident
    go n | n > 10000 = error ("findFresh " ++ show s)
         | bad new_id = go (n+1)
         | otherwise  = new_id
         where
           new_id = Name (prefix ++ show n)

