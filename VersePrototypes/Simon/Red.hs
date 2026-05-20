{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
     {- -Wno-missing-methods -Wno-incomplete-uni-patterns -Wno-unused-matches
        -Wno-missing-pattern-synonym-signatures -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}


module Red( Blk(Blk, BlkX), Exp(..), Term(..)
          , run, runTraced, isVal
  ) where

import Prelude hiding ((<>))
import Data.Graph(stronglyConnComp, SCC(..))

import qualified FrontEnd.Expr as F
import FrontEnd.ToCore( toCoreEff )
import FrontEnd.Error

import Core.Traced
import Core.Solver( unsat )
import Core.Expr as C ( Assump, Effect(..), Lit(..), PrimOp(..)
                      , GroundVal(..), Assump(..), FailableAssump(..), AssumpOp(..)
                      , primOpIsCheck, primOpCanFail, substAssump
                      , isUnaryOp, isBinOp )
import Core.Bind as C

import Epic.Print
import Epic.List

import Control.Arrow(second)
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Data.Set( Set )
import Data.Char( isAlpha )

import Debug.Trace
import GHC.Stack

{-
  Potential problems:
  :sim f:=fun(x:int){x+1}; ((f[y]; y:= -2) | 5)

-}

-- Show every reduction step
traceReductions :: Bool
traceReductions = False

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
  deriving (Eq, Show)

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
  | Arr [Exp]             -- array{e1,...,e2}
  | OfType Exp Effect Exp -- e1 |><fx> e2
  | Tru Exp               -- truth{e{

  | Lam Ident Blk          -- \ x . e
  | Iter IterCtx Blk Exp   -- if/for
  | Dly Exp                -- delay{e}
  | Crl Blk                -- {...}
  | SArr Blob Ident Term   -- e ~> t

  | Verify (Set SkolIdent) [Assump] Blk
  | Err String             -- Stuck/error expression
  deriving (Eq, Show)

data Blk = BlkX (Set Ident) Heap Exp
  deriving (Eq, Show)

data Blob = Blob | NoBlob
  deriving (Eq, Show)

type Eqn = (Ident, Val)
type Heap = [Eqn]  -- Invariant: all identifiers in the domain are distinct

emptyHeap :: Heap
emptyHeap = []

-- The equation RHSs have no variables from the LHSs
-- A block (Blk xs eqs e) satisfies these invariants:
--    (A)  dom(eqs)    `subset`   X
--    (B)  occfvs(eqs) `disjoint` dom(eqs)

(~~>) :: Ident -> Term -> Exp
i ~~> t = SArr Blob i t

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
  deriving (Eq, Show)

pattern BlkE :: Exp -> Blk
pattern BlkE e <- BlkX (S.null -> True) (null -> True) e
  where
    BlkE e = Blk S.empty emptyHeap e

mkCrl :: Blk -> Exp
mkCrl (BlkE e) = e
mkCrl b        = Crl b

mkSeq :: Exp -> Exp -> Exp
-- Aggressively elminate values, and re-associate to the right
mkSeq (Val {})   e2 = e2
mkSeq (e1 :> e2) e3 = mkSeq e1 (mkSeq e2 e3)
mkSeq e1         e2 = e1 :> e2

mkSeqs :: [Exp] -> Exp
mkSeqs []     = Arr []
mkSeqs [e]    = e
mkSeqs (e:es) = e `mkSeq` mkSeqs es

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
    | l == prettyNormal     = text "<" <> hsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (TArr [e]) = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (TArr es)  = parens $ hsep $ punctuate (text ",") $ map (pPrintPrec l 0) es
  pPrintPrec l _ (TTru e)   = text "truth" <> braces (pPrintL l e)

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
  pPrintPrec l p (SArr NoBlob x e) = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>" <+> pPrintPrec l 1 e
  pPrintPrec l p (SArr Blob   x e) = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~~>" <+> pPrintPrec l 1 e
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


  pPrintPrec l _ (Iter ic b1 b2)
    = text "iter" <> cat [parens (text (show ic)), braces (pPrintL l b1), braces (pPrintL l b2)]
  pPrintPrec l _ (Verify rs as e)
    = sep [ text "verify" <> parens (sep [ fsep (map pPrint (S.toList rs)) <> text ";"
                                         , fsep (map pPrint as) ])
          , nest 2 (braces (pPrintL l e)) ]

  pPrintPrec l p (OfType e1 fx e2)
    = maybeParens (p > 7) $
      sep [ pPrintPrec l 6 e1
          , text "|>" <> angleBrackets (pPrint fx)
             <+> pPrintPrec l 6 e2 ]

instance Pretty Blk where
  pPrintPrec l p (Blk vs eqns e) = ppBlk l p vs eqns e

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
type HNFV = Exp

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
getHNF _ = Nothing

-- Like HNF, but also variables
pattern HNFV :: Exp -> HNFV
pattern HNFV e <- (getHNFV -> Just e)

getHNFV :: Exp -> Maybe HNFV
getHNFV e@Var{} = Just e
getHNFV e = getHNF e

pattern Val :: Exp -> Val
pattern Val e <- (getVal -> Just e)

-- Values
getVal :: Exp -> Maybe Exp
getVal e@Var{} = Just e
getVal e@Lit{} = Just e
getVal e@Prm{} = Just e
getVal e@Lam{} = Just e
getVal e@(Arr es) | Just _ <- mapM getVal es = Just e
getVal e@(Tru e') | Just _ <- getVal e' = Just e
getVal e@Dly{} = Just e
getVal _ = Nothing

isVal :: Exp -> Bool
isVal e = isJust (getVal e)

pattern Comp :: Exp -> Exp
pattern Comp v <- (getComp -> Just v)

-- Comparable values
getComp :: Exp -> Maybe Exp
getComp e@Lit{} = Just e
getComp e@(Arr es) | Just _ <- mapM getComp es = Just e
getComp e@(Tru e') | Just _ <- getComp e' = Just e
getComp _ = Nothing

pattern Con :: Exp -> Exp
pattern Con v <- (getCon -> Just v)

-- Constants, no further reductions possible
getCon :: Exp -> Maybe Exp
getCon e@Lit{} = Just e
getCon e@Prm{} = Just e
getCon e@Lam{} = Just e
getCon e@(Arr es) | Just _ <- mapM getCon es = Just e
getCon e@(Tru e') | Just _ <- getCon e' = Just e
getCon e@Dly{} = Just e
getCon _ = Nothing

pattern ArrOrTru :: ([Exp] -> Exp) -> [Exp] -> Exp
pattern ArrOrTru con vs <- (getArrOrTru -> Just (con, vs))

-- If the expression is Arr or Tru, return the constructor and the list of subparts.
-- For Tru we fake a list.  This way Arr and Tru can share code in some cases.
getArrOrTru :: Exp -> Maybe ([Exp] -> Exp, [Exp])
getArrOrTru (Arr es) = Just (Arr, es)
getArrOrTru (Tru e)  = Just (Tru . (!!0), [e])
getArrOrTru _ = Nothing

-- Turn every kind of HNF into a trivial one
root :: HNF -> HNF
root Lit{} = IntE 0
root Prm{} = Prm Add
root Lam{} = Lam (Name "") (BlkE $ IntE 0)
root Arr{} = Arr []
root Dly{} = Dly (IntE 0)
root Tru{} = Tru (IntE 0)
root _ = error "root: not an HNF"

pattern IntE :: Integer -> Exp
pattern IntE i = Lit (LInt i)

pattern StrE :: String -> Exp
pattern StrE s = Lit (LStr s)

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
srcToTerm (F.OfType t1 fx t2)
  | Just eff <- toCoreEff fx     = TOfType (srcToTerm t1) eff (srcToTerm t2)
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

srcToTerm e = error $ "srcToTerm: unimplemented " ++ show e

srcToCoreIdent :: F.Ident -> Ident
srcToCoreIdent (F.Ident _ s) = Name s

--------------------------------------------------------------------------------
--
--             Reductions
--
--------------------------------------------------------------------------------

type RuleName = String

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
  | FloatS (Set SkolIdent) [Assump]
  deriving (Eq, Show)


mapPayload :: (a -> b) -> [Result flt a] -> [Result flt b]
mapPayload f rs = [ Res flt (f payload) | Res flt payload <- rs ]

reductionFired :: Reduction a -> Maybe (Reduction a)
reductionFired RedNone = Nothing
reductionFired r       = Just r

orTry :: Reduction a -> Reduction a -> Reduction a
orTry RedNone r2 = r2
orTry r1      _  = r1

addInScopeExis :: ReductionContext -> Set Ident -> ReductionContext
addInScopeExis cxt bndrs = cxt { rc_exis = rc_exis cxt `S.union` bndrs }

mkExiFloat1 :: Ident -> BlkFloats
-- Single existential only, no heap
mkExiFloat1 x = FloatB (sing x) emptyHeap

mkExiFloat :: [Ident] -> BlkFloats
-- Existentials only, no heap
mkExiFloat xs = FloatB (S.fromList xs) emptyHeap

pattern Failure :: String -> Reduction a
pattern Failure s = RedStep s []

pattern Done :: String -> a -> Reduction a
pattern Done s e = RedStep s [Res NoFloats e]

mkStep :: String -> BlkFloats -> Exp -> Reduction Exp
mkStep s flts e = RedStep s [Res flts e]

mkStepC :: RuleName -> Exp -> Exp -> Reduction Exp
mkStepC rn e1 e2 = RedStep rn [Res NoFloats e1, Res NoFloats e2]

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
  pPrintPrec _ _ (FloatS s asm)  = text "FS" <> braces (pPrint s <+> pPrint asm)

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
mkCons a as = Prm ArrCons :@ Arr [a, as]

mkCons2 :: ReductionContext -> Exp -> Exp -> Exp -> Exp
-- cons2 = \xy. \xys. exists x y <xs,ys> = <cons x xs, cons y ys>
-- Note: x, y, and xys are expressions, so we need to avoid accudental capture in those.
mkCons2 cxt x y xys
  = Crl $ Blk (S.fromList [xs,ys,ar]) emptyHeap $
    -- We need have the two Arr in this order, otherwise choices
    -- in the body of a 'for' will come in the wrong order.
    (Var ar :=: Arr [ mkCons x (Var xs)
                    , mkCons y (Var ys)]) :>
    (Arr [Var xs, Var ys] :=: xys) :>
    Var ar
  where
    (xs, ys, ar)  = freshId3 cxt ("xs", "ys", "ar")

thePrelude :: [Eqn]
thePrelude
  = [ (mkName "int",          Lam vp  $ BlkE $ Prm IsInt :@ (Var vp) :> Var vp)
    , (mkName "string",       Lam vp  $ BlkE $ Prm IsStr :@ (Var vp) :> Var vp)
    , (mkName "comparable",   Lam vp  $ BlkE $ Prm IsComp :@ (Var vp) :> Var vp)
    , (mkName "any",          Lam vp  $ BlkE $ Var vp)
    , (mkName "void",         Lam vp  $ BlkE $ Arr [])
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
    , (mkName "prefix'[]'"  , Lam vt $ BlkE $ Lam vp $ BlkE $
                              (Prm IsArr :@ Var vp) :>
                              (Prm ArrMap :@ (Arr [Var vt, Var vp])))

    -- ? = \t.\x. if (truth{y:any} = x) then truth{t[y]} else x=()
    , (mkName "prefix'?'"   , Lam vt $ BlkE $ Lam vx $ BlkE $
                              let y = mkName "_y" in
                              Iter IF (Blk (sing y) emptyHeap $ ((Tru (Var y) :=: Var vx) :> Dly (Tru (Var vt :@ Var y))))
                                      (Var vx :=: Arr [])
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

runTraced :: Fuel -> F.SrcEssential -> (NormResult, Traced Blk)
runTraced fuel src
  = normalize step valid fuel top_blk
  where
    (top_cxt, top_blk) = initialBlk src

    valid :: Blk -> Bool
    valid _ = True

    step :: Blk -> Maybe (TraceStep Blk)
    step blk
      | BlkE Fail <- blk   -- Simon: seems ad-hoc, but otherwise we loop on failure
      = Nothing
      | otherwise
      = case reduceBlock top_cxt blk of
          RedNone -> Nothing
          RedStep rule_nm []
             -> Just $ TS { ts_str = rule_nm, ts_payload = BlkE Fail, ts_verb = 1 }
          RedStep rule_nm [Res NoFloats blk']
             -> -- ppTrace ("------ runTraced: " ++ rule_nm ++ " ----------------")
                --        (pPrint blk' $$ text "") $
                Just $ TS { ts_str = rule_nm, ts_payload = blk', ts_verb = 1 }
          red -> error ("runTraced" ++ show red)

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

    top_blk = Blk (sing u `S.union` init_locals)
                  thePrelude
                  (u ~~> term)

    top_cxt = RC { rc_depth  = 0
                 , rc_eqns   = emptyHeap
                 , rc_exis   = S.empty
                 , rc_skols  = top_skols
                 , rc_vcxt   = NotVerifying }

    u = freshId top_cxt "u"

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

data ReductionContext
  = RC { rc_depth  :: Int         -- Number of enclosing Blks

       , rc_exis   :: Set Ident
       , rc_eqns   :: Heap        -- In-scope equations for the rc_exis

       , rc_skols  :: Set Ident
       , rc_vcxt   :: VerificationContext
    }
    deriving (Show)

data VerificationContext
  = NotVerifying
  | Verifying [Assump]    -- In-scope assumnptions for the rc_skols
    deriving (Show)

--------------------------------------------------------------------------------
--
--             The evaluator: reduction rules
--
--------------------------------------------------------------------------------

reduceBlock :: ReductionContext -> Blk -> Reduction Blk
reduceBlock cxt@(RC { rc_depth = d, rc_eqns = eqns, rc_exis = exis }) blk
  | not (S.null dead_vars)   -- First try garbage collection
  = (if traceReductions
     then trace $ render $ nest (4*rc_depth cxt) $
          text "reduceBlockGC" <+> (pPrint dead_vars $$ pPrint blk')
     else id)
    Done ("GC " ++ show dead_vars) (gcVarsBlk dead_vars blk')

  | let res = case reduceExp cxt' blk' of
                RedNone       -> RedNone
                RedStep rn rs -> RedStep rn (map (mergeStep blk') rs)
                VerStep rn rs -> VerStep rn (mapPayload (Blk locals' leqns') rs)

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
               , rc_exis  = exis `S.union` locals' }

-----------------------------
reduceExp :: ReductionContext -> Blk -> Reduction Exp
-- This function reduces the expresion inside a Blk
-- The context has already been extended with locals, leqns etc
-- We pass in the parent Blk just so that we can give it to `promotionOK`
reduceExp cxt parent@(Blk _ _ body)
  = find True body
 where
    find :: Bool -> Exp -> Reduction Exp
    find leftCF expr =    -- leftCF means that the context to the left of the find is choice free
      case expr of
        -- Floating a nested block {b}
        Crl blk -> mkStep "FloatB" (FloatB is' eqs') e'
          where
            Blk is' eqs' e' = freshenBlk cxt blk
            -- GC rules handled above

        -- Substitution
        Var i | Just v <- lookup i (rc_eqns cxt)  -- Includes leqns
              -> Done ("Subst " ++ show i) v

        -- Promotion
        -- We must try both ways round.  Here's a tricky case
        --   exists x. ...if( exists y. ..x=y.. )...
        -- We must fire (Promote2) on x=y
        Var  x  :=: HNFV v | Just redn <- reductionFired (reduceVarVal cxt "1" parent x v)
                           -> redn
        HNFV v  :=: Var  x | Just redn <- reductionFired (reduceVarVal cxt "2" parent x v)
                           -> redn

        -- Primops
        Prm op :@ v | Just redn <- reductionFired (reducePrimOp cxt op v)
                    -> redn
                    -- The guard is important; if the primop doesn't fire, we want to
                    -- fall through the "look at sub-expressions" code

        -- Unification
        Val v1       :=: Val v2 | v1 == v2                 -> Done    "EqVal" v1
        Lit k1       :=: Lit k2 | k1 /= k2                 -> Failure "EqValFail"

        Val (Arr vs) :=: Arr es | length vs == length es   -> Done    "EqTup" $ Arr (zipWith (:=:) vs es)
        Arr ds       :=: Arr es | length ds /= length es   -> Failure "EqTupFail"
        Tru e1       :=: Tru e2                            -> Done    "EqTru" $ Tru (e1 :=: e2)
        HNF h1       :=: HNF h2 | root h1 /= root h2       -> Failure "EqFail"

        -- Sequencing
        Val{}  :>  e2                                      -> Done "Seq1" e2

        -- Unification, structural
        Val v  :=: (e1 :>  e2) -> Done "Norm1" $ e1 :> (v :=: e2)
        Val v  :=: (e1 :=: e2) -> Done "Norm2" $ (v :=: e1) :> (v :=: e2)
        (e1 :> e2) :=: e3      -> Done "Norm3" $ e1 :> (e2 :=: e3)
        (Val v :=: e1) :=: e2  -> Done "Norm4" $ (v :=: e1) :> (v :=: e2)

        -- Beta (\x.blk)[a] -->  exists x. x = a; blk
        -- NOTE: it should be enough to match with eqs=[]
        Lam x blk :@ arg
            -> mkStep ("Beta " ++ show x) (mkExiFloat1 x') $
               (Var x' :=: arg) :> mkCrl blk'
            where
              x' = freshenId cxt x
              blk' | x==x'     = blk
                   | otherwise = renameBlk [(x,x')] blk

        -- This rule isn't strictly necessary, but it allows indexing by
        -- a constant to proceed outside a failure context.
        Arr es :@ IntE i | 0 <= i' && i' < length es -> Done "ITup-k" (es !! i')
                                                     where i' = fromInteger i
        Val (Arr es) :@ ei -> mkStep "ITup" (mkExiFloat1 x) $
                              Var x :=: ei :>
                              foldr alt Fail (zipWith (\ i e -> (Var x :=: IntE i) :> e) [0..] es)
          where
            x = freshId cxt "x"
            alt e1 e2 = BlkE e1 :|: BlkE e2

--        Tru e :@ Val v -> Done "ITru" $ e :=: v; v
        Tru e1 :@ e2 -> Done "ITru" $ e1 :=: e2

        SArr b x tm  -> reduceMatch cxt x tm b

        -- Choice and failure
        Fail -> Failure "Fail"
        b1 :|: b2 | rc_depth cxt > 0 && leftCF
             -> mkStepC "B" (mkCrl b1) (mkCrl b2)     -- Found a choice, return it if inside a block

        Iter ic b e -> reduceIter cxt ic b e

        OfType e1 _fx e2  -> Done "RunOfType" (e2 :@ e1)

        Verify skols as e -> reduceVerify cxt skols as e

        -- Catch-all cases for context C; just walk downwards
        -- Aka "look at sub-expressions"
        e1 :>  e2  -> find2   leftCF (:>)  e1 e2
        e1 :=: e2  -> find2   leftCF (:=:) e1 e2
        e1 :@  e2  -> find2   leftCF (:@)  e1 e2
        Arr es     -> findArr leftCF es
        Tru e      -> find1 leftCF Tru e

        _ -> RedNone

    find1 :: Bool -> (Exp -> Exp) -> Exp -> Reduction Exp
    find1 leftCF wrap e = fmap wrap (find leftCF e)

    find2 :: Bool -> (Exp -> Exp -> Exp) -> Exp -> Exp -> Reduction Exp
    find2 leftCF c e1 e2
      = case find1 leftCF (`c` e2) e1 of
          RedNone -> find1 (leftCF && choiceFree e1) (e1 `c`) e2
          r       -> r

    findArr :: Bool -> [Exp] -> Reduction Exp
    findArr _ []          = RedNone
    findArr leftCF (e:es) = case find1 leftCF k e of
                              RedNone -> find1 (leftCF && choiceFree e) ks (Arr es)
                              r       -> r
      where
        k e'         = Arr (e':es)
        ks (Arr es') = Arr (e:es')
        ks e'        = error "findArr" (prettyShow e')

------------------------------------
reduceVarVal :: ReductionContext -> String -> Blk -> Ident -> Val -> Reduction Exp
-- Deal with (x=<v1..vn>)   -->   (x=<x1,..xn>)    <x1=v1, ..,vn>
reduceVarVal cxt left_or_right parent x (ArrOrTru con vs)
  | promotionOK parent x xv
  = mkStep ("PromoteT"++left_or_right) (Promote as x xv) v'

  | Just asm <- skolemEquality cxt' x xv
  = VerStep ("VPromoteT"++left_or_right)
    [ Res (FloatS as [A_Pos asm]) v'
    , Res (FloatS as [A_Neg asm]) Fail ]
  where
    prs :: [(Maybe Ident, Val)]
    prs = zipWith mk_elt (freshIds cxt (repeat "a0")) vs
    as = S.fromList [ a | (Just a, _) <- prs ]

    mk_elt a av | do_anf av = (Just a,  av)
                | otherwise = (Nothing, av)

    xv = con (map get_a prs)
    v' = con (map get_e prs)

    get_a :: (Maybe Ident,Val) -> Exp
    get_a (Just a,  _)  = Var a
    get_a (Nothing, av) = av

    get_e :: (Maybe Ident,Val) -> Exp
    get_e (Just a,  av) = Var a :=: av
    get_e (Nothing, av) = av

    RC { rc_skols = skols } = cxt
    cxt' = cxt { rc_skols = skols `S.union` as }

    x_is_skol = x `S.member` skols

    do_anf v | x_is_skol -- `x` is a skolem
             = isNothing (groundValue skols v)
             | otherwise -- `x` is an existential
             = case v of
                 Var {} -> False
                 _      -> not (S.null (freeVars v))

reduceVarVal cxt left_or_right parent x v
-- Deal with non-tuple var/val equalities (x=v) or (v=x)
  | promotionOK parent x v
  = mkStep ("Promote"++left_or_right) (Promote S.empty x v) v

  | Just asm <- skolemEquality cxt x v
  = VerStep ("VPromote"++left_or_right)
     [ Res (FloatS S.empty [A_Pos asm]) v
     , Res (FloatS S.empty [A_Neg asm]) Fail ]

  | otherwise
  = RedNone

promotionOK :: Blk -> Ident -> Val -> Bool
-- True if we can promote (var=val) into the heap for the parent block
promotionOK (Blk locals leqns _) x v
  =  x `S.member` locals                    -- Must be bound by the /immediately enclosing/ block
                                            --   i.e. is "flexible"
  && x `S.notMember` dom leqns              -- x must not have an eqn
  && occfvs v `S.disjoint` (sing x `S.union` dom leqns)    -- v must not have variables from eqns


------------------------------------
reducePrimOp :: ReductionContext -> PrimOp -> Exp -> Reduction Exp
reducePrimOp cxt op e
  = reduceArithOp op e `orTry`
    reduceRelOp   op e `orTry`
    reduceCheckOp op e `orTry`
    reduceArrOp   op e `orTry`
    reduceTruOp   op e `orTry`
    reduceDotDot  op e `orTry`
    reduceVerifyOp cxt op e

-----------------
reduceArithOp :: PrimOp -> Exp -> Reduction Exp
-- Arithmetic ops get stuck on values outside the domain
reduceArithOp Add (Arr [IntE i, IntE j]) = Done    "Prim+" $ IntE (i + j)
reduceArithOp Sub (Arr [IntE i, IntE j]) = Done    "Prim-" $ IntE (i - j)
reduceArithOp Mul (Arr [IntE i, IntE j]) = Done    "Prim*" $ IntE (i * j)
reduceArithOp Div (Arr [IntE i, IntE j])
  | j /= 0                               = Done    "Prim/" $ IntE (i `div` j)
  | otherwise                            = Failure "Prim/"

reduceArithOp Neg (IntE i)               = Done    "Prim-neg" $ IntE (- i)
reduceArithOp Pls (IntE i)               = Done    "Prim-pls" $ IntE i

reduceArithOp IsInt v@(IntE {})          = Done    "Prim-isInt" v
reduceArithOp IsInt HNF{}                = Failure "Prim-isInt"

reduceArithOp IsStr v@(StrE {})          = Done    "Prim-isStr" v
reduceArithOp IsStr HNF{}                = Failure "Prim-isStr"

reduceArithOp IsComp (Comp v)            = Done    "Prim-isComp" v
reduceArithOp IsComp (Con _)             = Failure "Prim-isComp"

reduceArithOp _ _ = RedNone


-----------------
reduceRelOp :: PrimOp -> Exp -> Reduction Exp
reduceRelOp Lt  (Arr [IntE i, IntE j]) | i<j       = Done    "Prim-Lt"  $ IntE i
                                        | otherwise = Failure "Prim-Lt"
reduceRelOp LEq (Arr [IntE i, IntE j]) | i<=j      = Done    "Prim-LEq" $ IntE i
                                        | otherwise = Failure "Prim-LEt"
reduceRelOp GEq (Arr [IntE i, IntE j]) | i>=j      = Done    "Prim-GEq" $ IntE i
                                        | otherwise = Failure "Prim-GEq"
reduceRelOp Gt  (Arr [IntE i, IntE j]) | i>j       = Done    "Prim-Gt"  $ IntE i
                                        | otherwise = Failure "Prim-Gt"
reduceRelOp NEq (Arr [IntE i, IntE j]) | i/=j      = Done    "Prim-NEq" $ IntE i
                                        | otherwise = Failure "Prim-NEq"
reduceRelOp _ _ = RedNone

-----------------
reduceDotDot :: PrimOp -> Exp -> Reduction Exp
-- Return multiple results for the enumeration.  Get stuck outside the domain
reduceDotDot DotDot (Arr [IntE l, IntE h]) = RedStep "Prim.." [Res NoFloats (IntE i) | i <- [l .. h]]
reduceDotDot _ _ = RedNone

-----------------
reduceCheckOp :: PrimOp -> Exp -> Reduction Exp
-- Check operations
reduceCheckOp ChkFails    (Arr [])  = Failure "ChkFail"
reduceCheckOp ChkSucceeds (Arr [e]) = Done "ChkSucc" e
reduceCheckOp ChkDecides  (Arr [])  = Failure "ChkDec0"
reduceCheckOp ChkDecides  (Arr [e]) = Done "ChkDec1" e
reduceCheckOp _ _ = RedNone

-----------------
reduceArrOp :: PrimOp -> Exp -> Reduction Exp
-- Array operations
reduceArrOp ArrCons (Arr [x, Arr xs]) = Done "Prim-cons"   $ Arr (x:xs)
reduceArrOp ArrLen (Arr xs)           = Done "Prim-length" $ IntE (toInteger (length xs))
-- Could have some inverse of ArrLen by reducing
--  (ArrLen :@ e) :=: IntE k  -->  e :=: Arr [_,_,...,_] k new existentials

reduceArrOp IsArr v@(Arr _) = Done    "Prim-isArr" v
reduceArrOp IsArr HNF{}     = Failure "Prim-isArr"

reduceArrOp ArrMap (Arr [fun, Arr xs])
  = Done "Prim-ArrMap" (Arr (map (fun :@) xs))

reduceArrOp ArrApp (Arr [a1,a2,res])
   | Arr as1 <- a1, Arr as2 <- a2
   =  -- <vs1>++<vs2> = res  -->  <vs1++vs2> = res
     Done "Prim-ArrApp1" (Arr (as1++as2) :=: res)

   | Arr [] <- a1
   = -- <>++a2 = res  -->  a2 = res
     Done "Prim-ArrApp2" (a2 :=: res)

   | Arr [] <- a2
   = -- a1++<> = res  -->  a1 = res
     Done "Prim-ArrApp3" (a1 :=: res)

   | Arr [] <- res
   = -- a1++a2 = <>  -->  a1=<>; a2=<>
     Done "Prim-ArrApp4" $
     (a1 :=: Arr []) :> (a2 :=: Arr [])

   | Arr (v:vs) <- a1
   , Arr (r:rs) <- res
   = -- <v:vs>++a2 = <r:rs>  -->  v=r; <vs>++a2 = <rs>; res
     Done "Prim-ArrApp5" $
     (v :=: r) :> (Prm ArrApp :@ (Arr [Arr vs, a2, Arr rs])) :> res

   | Arr (Snoc vs v) <- a2
   , Arr (Snoc rs r) <- res
   = -- a1++<vs,v> = <rs,r>  -->  a1++<vs> = <rs>; v=r; res
     Done "Prim-ArrApp6" $
     (Prm ArrApp :@ (Arr [a1, Arr vs, Arr rs])) :> (v :=: r) :> res

   -- ToDo: worry about duplicating `res`!!

reduceArrOp _ _ = RedNone

-----------------
reduceTruOp :: PrimOp -> Exp -> Reduction Exp
reduceTruOp IsTru v@(Tru _) = Done    "Prim-isTru" v
reduceTruOp IsTru HNF{}     = Failure "Prim-isTru"
reduceTruOp _ _ = RedNone

------------------------------------
reduceMatch ::  ReductionContext -> Ident -> Term -> Blob -> Reduction Exp
-- :~> reduction

reduceMatch cxt x tm blob
  = case tm of
        -- Blocks
        TBlock t -> mkStep  "MBlock" (FloatB tbndrs emptyHeap) (SArr blob x t')
                 where
                   (tbndrs, t') = freshenTerm cxt t

        -- Matching
        Und                  -> Done "MWild" $ Var x
        TVar i               -> Done "MVar"  $ Var x :=: Var i
        TLit k               -> Done "MLit"  $ Var x :=: Lit k
        TPrm o               -> Done "MPrim" $ Var x :=: Prm o
        TFail                -> Done "Mfail" $ Fail

        (t1 :@% t2)          -> mkStep "MApp" (mkExiFloat [u1,u2]) $
                                     Var x :=: ((u1 ~~> t1) :@ (u2 ~~> t2))
                             where (u1,u2) = freshIds2 cxt "u"
        (t1 :=:% t2)         -> Done "MUnif"    $ (SArr blob x t1) :=: (SArr blob x t2)
        (t1 :|:% t2)         -> Done "MChoice"  $
                                (BlkE $ SArr blob x (TBlock t1)) :|:
                                (BlkE $ SArr blob x (TBlock t2))

        (t1 :>% t2)          -> mkStep "MSemi"  (mkExiFloat1 u) $ (u ~~> t1) :> SArr blob x t2
                              where u = freshId cxt "u"
        (t1 `Where` t2)      -> mkStep "MWhere" (mkExiFloat [u,w]) $
                                (Var w :=: SArr blob x t1) :> (u ~~> t2) :> Var w
                             where (u,w) = freshIds2 cxt "u"

        Rng t      -> mkStep "MColon" (mkExiFloat1 u) $ (u ~~> t) :@ Var x
                   where u = freshId cxt "u"

        (i := t)   -> Done ("MDef " ++ show i) $ Var i :=: SArr blob x t
        (i :-> t)  -> Done ("MArr " ++ show i) $ (Var i :=: Var x) :> SArr NoBlob i t
                      -- XXX should this be NoBlob

        -- Tuples and functions
        TArr ts      -> matchTup cxt x ts blob
        Fun at fx bt -> matchFun cxt x at fx bt blob
        TTru t       -> mkStep "MTru" (mkExiFloat [u]) $ (Var x :=: Tru (Var u)) :> Tru (SArr blob u t)
                       where u = freshId cxt "u"

        If t0 t1 t2  -> Done "MIf" $
                        Iter IF (Blk (sing u `S.union` tbs0) emptyHeap $
                                 (u ~~> t0') :>
                                 (Dly (SArr blob x (TBlock t1'))))
                                (SArr blob x (TBlock t2))
                      where
                        (tbs0, t0', t1') = freshenTerm2 cxt t0 t1
                        u = freshId (cxt `addInScopeExis` tbs0) "u"

        -- Recognize for(i:=e){i}, they are encodings of 'all', and we know that they are
        -- choice free.
        For (i := t0) (TVar i') | i == i' ->
                        Done "MAll" $ Var x :=:
                         (Iter ALL (Blk (sing u `S.union` tbs0) emptyHeap $
                                        (u ~~> t0'))
                                   (Arr []))
                      where
                        (tbs0, t0') = freshenTerm cxt t0
                        u = freshId (cxt `addInScopeExis` tbs0) "u"

        For t0 t1    -> mkStep "MFor" (mkExiFloat1 y) $
                        (Arr [Var x, Var y] :=:
                         Iter FOR (Blk (sing u `S.union` tbs0) emptyHeap $
                                   (u ~~> t0') :>
                                   (Lam w (BlkE $ SArr blob w (TBlock t1'))))
                                  (Arr [Arr [], Arr []])) :>
                        Var y
                      where
                        (tbs0, t0', t1') = freshenTerm2 cxt t0 t1
                        (y,u,w) = freshId3 (cxt `addInScopeExis` tbs0) ("y","u","w")

        TOfType t1 fx t2 -> mkStep ("TOfType " ++ show fx) (mkExiFloat [u1,u2]) $
                            (OfType (u1 ~~> t1) fx (u2 ~~> t2))
                      where (u1,u2) = freshIds2 cxt "u"

        Check fx t -> Done ("MCheck " ++ show fx) $
                      Var x :=: mkCheck fx (Blk (sing u) emptyHeap $ u ~~> TBlock t)
                   where
                      u  = freshId cxt "u"

        Splice t -> reduceMatch cxt x t blob
                    -- See (AMP1) in Note [Desugaring ampersand]


matchFun :: ReductionContext -> Ident -> Term -> Effect -> Term -> Blob -> Reduction Exp
matchFun cxt f at fx bt blob
  = Done "MFun" $
    fun_verify :>
    the_lambda
  where
    (tbs_at, at', bt') = freshenTerm2 cxt at bt
    (u,p,q) = freshId3 (cxt `addInScopeExis` tbs_at) ("u", "p", "q")
    the_lambda = Lam u $ Blk (S.fromList [p,q] `S.union` tbs_at) emptyHeap $
                 (Var p :=: SArr NoBlob u at')       :>
                 (if blob == NoBlob then ((Var q :=: (Var f :@ Var p)) :>)
                                    else ( (IntE 99999) :>))
                 (SArr blob q (TBlock bt'))

    fun_verify = Verify (sing u) [] $
                 Blk tbs_at emptyHeap $
                 (SArr blob u at') :>
                 (mkCheck fx (Blk (sing q) emptyHeap (SArr NoBlob q (TBlock bt))))

mkCheck :: Effect -> Blk -> Exp
mkCheck Fails    blk = Prm ChkFails    :@ Iter ALL blk (Arr [])
mkCheck Succeeds blk = Prm ChkSucceeds :@ Iter ALL blk (Arr [])
mkCheck Decides  blk = Prm ChkDecides  :@ Iter ALL blk (Arr [])
mkCheck Iterates blk = mkCrl blk

---------------------------------------
reduceIter :: ReductionContext -> IterCtx -> Blk -> Exp -> Reduction Exp
reduceIter _ IF (Blk is eqs (Dly e)) _
  = Done "IIf" $ mkCrl (Blk is eqs e)

reduceIter cxt FOR blk@(Blk _ _ Val{}) e2
  = mkStep "IFor" (mkExiFloat1 x)
                  (mkCons2 cxt (Var x) (mkCrl blk :@ Var x) e2)
  where
    x = freshId cxt "x"

reduceIter _ ALL blk@(Blk _ _ Val{}) e2
  = Done "IAll" $ mkCons (mkCrl blk) e2

reduceIter cxt ic b1 e2
  = case reduceBlock cxt b1 of  -- Find a redex in B context
      RedNone       -> RedNone
      VerStep rn rs -> VerStep rn (mapPayload (\b -> Iter ic b e2) rs)
           -- For a VerStep, just wrap the unchanged Iter around the outside
      RedStep rn rs -> Done (show ic ++ " " ++ rn) (foldr iter e2 rs)
           -- Note that we update the RuleName to give
           -- more info about where the reduction happened
  where
    iter :: Result BlkFloats Blk -> Exp -> Exp
    iter (Res NoFloats b) e = Iter ic b e
    iter res e = error ("reduceIter " ++ render (pPrint res $$ pPrint e))

---------------------------------------
matchTup :: ReductionContext -> Ident -> [Term] -> Blob -> Reduction Exp
matchTup cxt x ts blob
  = mkStep "MTup" (mkExiFloat fresh_xs) $
    (Var x :=: appendArrs cxt2 (map mk_in segs)) :>
    appendArrs cxt2 (map mk_out segs)
  where
    mk_in :: Segment (Ident,Term) -> Exp
    mk_in (STrue (y,_)) = Var y
    mk_in (SFalse sprs) = Arr [ Var y | (y,_) <- sprs ]

    mk_out :: Segment (Ident,Term) -> Exp
    mk_out (STrue (y,t)) = SArr blob y t
    mk_out (SFalse sprs) = Arr [ SArr blob y t | (y,t) <- sprs ]

    fresh_xs = freshIds cxt ["x" | _ <- ts]
    cxt2 = cxt { rc_exis = rc_exis cxt `S.union` S.fromList fresh_xs }  -- Yuk

    segs :: [Segment (Ident,Term)]
    segs = segments is_splice (fresh_xs `zip` ts)

    is_splice (y, Splice t) = Just (y,t)
    is_splice _             = Nothing

data Segment a = STrue  a | SFalse [a]

appendArrs :: ReductionContext -> [Exp] -> Exp
-- appendArrs [a1, .., an] = a1 `ArrApp` a2 `ArrApp` ... an
appendArrs _   []      = Arr []
appendArrs cxt (a1:as) = mkCrl $ Blk (S.fromList (map fst prs)) [] $
                         foldl do_one a1 prs
  where
    do_one rest (r,a) = (Prm ArrApp :@ Arr [rest, a, Var r]) :> Var r

    fresh_ids = freshIds cxt ["y" | _ <- as]

    prs :: [(Ident,Exp)]
    prs = fresh_ids `zip` as

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
choiceFree (SArr _ _ _) = False         -- force ~> to happen
choiceFree (e1 :@ e2) = choiceFree e1 && choiceFree e2
choiceFree (Arr es) = and (map choiceFree es)
choiceFree (Iter ALL _ _) = True
choiceFree (Iter _ _ _) = False
choiceFree (Crl b) = choiceFreeB b
choiceFree Verify{} = True
choiceFree (Tru e) = choiceFree e

choiceFreeB :: Blk -> Bool
choiceFreeB (Blk _ _ e) = choiceFree e


--------------------------------------------------------------------------------
--
--             Verification rules
--
--------------------------------------------------------------------------------

reduceVerify :: ReductionContext -> Set Ident -> [Assump] -> Blk -> Reduction Exp
reduceVerify cxt skols as blk
  | Blk _ _ (Val {}) <- blk
  = Done "VVal" (Arr [])
    -- We can finish up with
    --  verify(r){ exists x {x <- r}. \y. x }
    -- and this is a successful verification

  | Just reason <- unsat all_as
  = Done ("VUnsat " ++ render (pPrint reason)) (Arr [])

  | otherwise
  = case reduceBlock cxt' blk' of
      RedNone       -> RedNone
      RedStep rn rs -> Done ("VR-" ++ rn) (mkSeqs (map wrap_blk rs))
      VerStep rn rs -> Done ("VV-" ++ rn) (mkSeqs (map wrap_ver rs))
  where
    wrap_blk (Res NoFloats blk'') = Verify skols' as' blk''
    wrap_blk r = error ("reduceVerify " ++ render (pPrint r))

    wrap_ver (Res NoVerFloats blk'')       = Verify skols' as' blk''
    wrap_ver (Res (FloatS sks asms) blk'') = Verify (sks `S.union` skols')
                                                    (asms ++ as')
                                                    blk''

    (subst, skols') = freshenBndrs cxt skols
    as'  = renameAssumps subst as
    blk' = renameBlk subst blk

    all_as :: [Assump]  -- Includes outer assumptions
    all_as = case rc_vcxt cxt of
               NotVerifying       -> as'
               Verifying outer_as -> outer_as ++ as'

    cxt' = cxt { rc_skols = rc_skols cxt `S.union` skols'
               , rc_vcxt  = Verifying all_as }

reduceVerifyOp :: ReductionContext -> PrimOp -> Exp -> Reduction Exp
reduceVerifyOp cxt op arg
  | NotVerifying <- rc_vcxt cxt
  = RedNone

  | otherwise
  = case groundValue (rc_skols cxt) arg of
      Nothing -> RedNone
      Just gv | isClosedGV gv -> RedNone
              | otherwise     -> reduceVerifyOpGV cxt op gv

isClosedGV :: GroundVal -> Bool
isClosedGV (GVVar {})  = False
isClosedGV (GVLit {})  = True
isClosedGV (GVArr gvs) = all isClosedGV gvs
isClosedGV (GVTru gv)  = isClosedGV gv

reduceVerifyOpGV :: ReductionContext -> PrimOp -> GroundVal -> Reduction Exp
reduceVerifyOpGV _cxt op _gv
  | primOpIsCheck op    -- ??
  =  RedNone

reduceVerifyOpGV cxt op gv
  | Just arg_op <- isUnaryOp op
  , let check_args = mkArgCheck arg_op gv
  = VerStep ("VUnaryOp-" ++ show op) $
    [ Res (FloatS S.empty [A_Neg assump]) stuck | assump <- check_args ] ++
    if primOpCanFail op
    then [ Res (FloatS S.empty  [A_Neg rel_op]) Fail
         , Res (FloatS S.empty  (A_Pos rel_op : map A_Pos check_args)) (Arr []) ]
    else [ Res (FloatS (sing r) (bin_op       : map A_Pos check_args)) (Var r)  ]
  where
    r = freshId cxt "r"
    rel_op = A_RelOp op gv
    bin_op = A_PrimOp r (AO_Prim op) gv
    stuck :: Exp
    stuck = Err (render (pPrint op <> brackets (pPrint gv)))

-- Binary operators where the argument is a skolem
reduceVerifyOpGV cxt op (GVVar r)
  | Just {} <- isBinOp op
  = VerStep "VBinOp-Arr" [ Res (FloatS rs [A_Pos eq_asm]) (Arr [Var r1,Var r2])
                         , Res (FloatS rs [A_Neg eq_asm]) Fail ]
  where
    rs = S.fromList [r1,r2]
    (r1,r2) = freshIds2 cxt "r"
    eq_asm = A_GVEq r (GVArr [GVVar r1, GVVar r2])

-- Binary operators where we can see both arguments
reduceVerifyOpGV cxt op gv@(GVArr [gv1,gv2])
  | Just (arg1_op, arg2_op) <- isBinOp op
  , let check_args :: [FailableAssump]
        check_args = mkArgCheck arg1_op gv1 ++ mkArgCheck arg2_op gv2
  = VerStep ("VBinOp-" ++ show op) $
    [ Res (FloatS S.empty [A_Neg assump]) stuck | assump <- check_args ] ++
    if primOpCanFail op
    then [ Res (FloatS S.empty  [A_Neg rel_op]) Fail
         , Res (FloatS S.empty  (A_Pos rel_op : map A_Pos check_args)) (Arr []) ]
    else [ Res (FloatS (sing r) (bin_op       : map A_Pos check_args)) (Var r)  ]
  where
    r = freshId cxt "r"
    rel_op = A_RelOp op gv
    bin_op = A_PrimOp r (AO_Prim op) gv
    stuck :: Exp
    stuck = Err (render (pPrint op <> brackets (pPrint gv)))

mkArgCheck :: PrimOp -> GroundVal -> [FailableAssump]
mkArgCheck IsAny _  = []
mkArgCheck op    gv = [A_RelOp op gv]

skolemEquality :: ReductionContext -> Ident -> Exp -> Maybe FailableAssump
skolemEquality (RC { rc_skols = skols, rc_vcxt = vcxt }) r v
  | Verifying {} <- vcxt
  , Just gv <- groundValue skols v
  , r `S.member` skols
  = Just (A_GVEq r gv)
  | otherwise
  = Nothing

groundValue :: Set SkolIdent -> Exp -> Maybe GroundVal
-- Like skolValue, but no lambdas
groundValue _  (Lit l)                   = Just (GVLit l)
groundValue rs (Var v) | v `S.member` rs = Just (GVVar v)
groundValue rs (Arr vs)                  = do { gvs <- mapM (groundValue rs) vs; Just (GVArr gvs) }
groundValue rs (Tru v)                   = do { gv <- groundValue rs v; Just (GVTru gv) }
groundValue _  _                         = Nothing

--------------------------------------------------------------------------------
--
--             Free and bound variables
--
--------------------------------------------------------------------------------

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
occfvs (SArr _ i _)   = sing i  -- XXX what should we do here
occfvs (e1 :@ e2)     = occfvs e1 `S.union` occfvs e2
occfvs (Arr es)       = S.unions (map occfvs es)
occfvs (Iter _ b1 e2) = occfvsB b1 `S.union` occfvs e2
occfvs (Crl b)        = occfvsB b
occfvs Verify{}       = S.empty
occfvs (OfType e1 _ e2) = occfvs e1 `S.union` occfvs e2
occfvs (Tru e)        = occfvs e

occfvsB :: Blk -> Set Ident
occfvsB (Blk is eqs e) = (S.unions (occfvs e : map (occfvs . snd) eqs)) `S.difference` is

-- All /free/ variables
freeVars :: Exp -> Set Ident
freeVars (Err {})    = S.empty
freeVars (Lit {})    = S.empty
freeVars (Prm {})    = S.empty
freeVars (Var i)     = sing i
freeVars (Lam i e)   = i `S.delete` freeVarsBlk e
freeVars (e1 :>  e2) = freeVars e1 `S.union` freeVars e2
freeVars (e1 :=: e2) = freeVars e1 `S.union` freeVars e2
freeVars (SArr _ i t) = sing i `S.union` freeVarsTerm t
freeVars (e1 :@  e2) = freeVars e1 `S.union` freeVars e2
freeVars (Arr es)    = S.unions $ map freeVars es
freeVars (b1 :|: b2) = freeVarsBlk b1 `S.union` freeVarsBlk b2
freeVars Fail        = S.empty
freeVars (Dly e)     = freeVars e
freeVars (Crl b)     = freeVarsBlk b
freeVars (Iter _ b1 e2) = freeVarsBlk b1 `S.union` freeVars e2
freeVars Verify{}    = S.empty
freeVars (OfType e1 _ e2) = freeVars e1 `S.union` freeVars e2
freeVars (Tru e)     = freeVars e

freeVarsBlk :: Blk -> Set Ident
freeVarsBlk (Blk is eqs e) = (S.unions (map (freeVars . snd) eqs) `S.union` freeVars e)
                              `S.difference` is

freeVarsTerm :: Term -> Set Ident
-- All variables mentioned, either as occurrences or binders,
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

freeVarsTermBlock :: Term -> Set Ident
freeVarsTermBlock t = freeVarsTerm t `S.difference` termBndrs t

-- Do NOT use a Set, we use this to count occurrences.
allVars :: Exp -> [Ident]
-- All variables, including binders
allVars (Var i)     = [i]
allVars (Lit {})    = []
allVars (Prm {})    = []
allVars (Lam i e)   = i : allVarsBlk e
allVars (e1 :>  e2) = allVars e1 ++ allVars e2
allVars (e1 :=: e2) = allVars e1 ++ allVars e2
allVars (SArr _ i e) = i : allVarsTerm e
allVars (e1 :@  e2) = allVars e1 ++ allVars e2
allVars (Arr es)    = concatMap allVars es
allVars (b1 :|: b2) = allVarsBlk b1 ++ allVarsBlk b2
allVars Fail        = []
allVars (Dly e)     = allVars e
allVars (Crl b)     = allVarsBlk b
allVars (Iter _ b1 e2)   = allVarsBlk b1 ++ allVars e2
allVars (Verify is _ b)  = S.toList is ++ allVarsBlk b
allVars (OfType e1 _ e2) = allVars e1 ++ allVars e2
allVars (Tru e)     = allVars e

allVarsBlk :: Blk -> [Ident]
allVarsBlk (Blk is eqs e) = S.toList is ++ concatMap (allVars . snd) eqs ++ allVars e

allVarsTerm :: Term -> [Ident]
-- All variables mentioned, either as occurrences or binders
allVarsTerm (TVar i) = [i]
allVarsTerm Und      = []
allVarsTerm (TLit _) = []
allVarsTerm (TPrm _) = []
allVarsTerm (i := e) = i : allVarsTerm e
allVarsTerm (e1 :>%  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 `Where`  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 :=:% e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 :@%  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (Fun e1 _ e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (Rng e) = allVarsTerm e
allVarsTerm (TArr es) = concatMap allVarsTerm es
allVarsTerm (b1 :|:% b2) = allVarsTerm b1 ++ allVarsTerm b2
allVarsTerm TFail = []
allVarsTerm (If t1 t2 t3) = allVarsTerm t1 ++ allVarsTerm t2 ++ allVarsTerm t3
allVarsTerm (For t1 t2) = allVarsTerm t1 ++ allVarsTerm t2
allVarsTerm (i :-> e) = i : allVarsTerm e
allVarsTerm (TBlock t) = allVarsTerm t
allVarsTerm (Check _ t) = allVarsTerm t
allVarsTerm (Splice t)  = allVarsTerm t
allVarsTerm (TOfType e1 _ e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (TTru t)    = allVarsTerm t

expSize :: Exp -> Int
expSize (Err {})    = 1
expSize (Var {})    = 1
expSize (Lit {})    = 1
expSize (Prm {})    = 1
expSize (Lam _ e)   = 1 + blkSize e
expSize (e1 :>  e2) = 1 + expSize e1 + expSize e2
expSize (e1 :=: e2) = 1 + expSize e1 + expSize e2
expSize (SArr _ _  e) = 2 + termSize e
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

--------------------------------------------------------------------------------
--
--             Renaming
--
--------------------------------------------------------------------------------

type Renaming = Subst Ident

rangeSubst :: Ord a => Subst a -> S.Set a
rangeSubst = S.fromList . map snd

rename :: Renaming -> Exp -> Exp
rename [] = id
rename sub = ren
  where
    ren :: Exp -> Exp
    ren e@(Var i) | Just j <- lookup i sub = Var j
                  | otherwise = e
    ren e@(Err {}) = e
    ren e@(Lit {}) = e
    ren e@(Prm {}) = e
    ren e@(Lam i b) | i `elem` map snd sub =
      -- We need to rename i to something else
      let i' = findFresh (`elem` (allVars e ++ map snd sub)) i   -- find an unused variable
      in  ren (Lam i' (renameBlk [(i, i')] b))
    ren (Lam i b) = let sub' = filter ((/= i) . fst) sub
                    in Lam i (renameBlk sub' b)
    ren (Var i :=: Var j) | Just j' <- lookup i sub, j == j' = Var j
    ren (e1 :> e2) = ren e1 :> ren e2
    ren (e1 :=: e2) = ren e1 :=: ren e2
    ren (SArr b i t) = SArr b (fromMaybe i (lookup i sub)) (renT t)
    ren (e1 :@ e2) = ren e1 :@ ren e2
    ren (Arr es) = Arr (map ren es)
    ren (b1 :|: b2) = renB b1 :|: renB b2
    ren e@Fail = e
    ren (Dly e) = Dly (ren e)
    ren (Iter ic b1 e2) = Iter ic (renB b1) (ren e2)
    ren (Crl b) = Crl (renB b)
    ren (OfType e1 fx e2) = OfType (ren e1) fx (ren e2)
    ren (Verify is as b) = Verify is (renameAssumps sub' as) (renameBlk sub' b)
       where
         sub' = [pr | pr@(x,_) <- sub, not (x `S.member` is)]
    ren (Tru e) = Tru (ren e)

    renB = renameBlk sub
    renT = renameTerm sub

renameTerm :: Renaming -> Term -> Term
renameTerm sub term = go term
  where
    go e@(TLit {}) = e
    go e@(TPrm {}) = e
    go e@TFail     = e
    go e@Und       = e
    go e@(TVar i) | Just j <- lookup i sub = TVar j
                  | otherwise              = e
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
    go (i := t)           = fromMaybe i (lookup i sub) := go t
    go (i :-> t)          = fromMaybe i (lookup i sub) :-> go t
    go (TBlock t)         = TBlock (go t)
    go (Check fx t)       = Check fx (go t)
    go (Splice t)         = Splice (go t)
    go (TOfType t1 fx t2) = TOfType (go t1) fx (go t2)
    go (TTru t)           = TTru (go t)

renameBlk :: Renaming -> Blk -> Blk
renameBlk sub b@(Blk is _ e) | not (S.disjoint (rangeSubst sub) is)
  -- If the range of the substitution clashesm we freshen the block.
  -- Freshening uses a reduction context, so we fake one.  UGLY!
  = renameBlk sub (freshenBlk fakeCxt b)
  where fakeCxt = RC { rc_depth = undefined, rc_exis = freeVars e `S.union` is `S.union` rangeSubst sub
                     , rc_eqns = undefined, rc_skols = S.empty, rc_vcxt = undefined }
renameBlk sub (Blk is eqs e)
  = Blk is (renameEqns sub' eqs) (rename sub' e)
  where
    sub' = [pr | pr@(x,_) <- sub, not (x `S.member` is)]

renameEqns :: Renaming -> [Eqn] -> [Eqn]
renameEqns sub = map (second (rename sub))

renameAssumps :: Renaming -> [Assump] -> [Assump]
renameAssumps subst as = map (C.substAssump subst) as

substVal :: Subst Exp -> Val -> Val
substVal sub e@(Var i) = fromMaybe e $ lookup i sub
substVal _ e@Lit{} = e
substVal _ e@Prm{} = e
substVal sub (Arr vs) = Arr (map (substVal sub) vs)
substVal sub (Tru v) = Tru (substVal sub v)
substVal sub e@Lam{} | S.null $ S.fromList (map fst sub) `S.intersection` S.fromList (allVars e) = e
                     | otherwise = e -- error "substVal: Lam unimplemented"
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
  | null subst = (tbndrs, term)
  | otherwise  = (tbndrs', renameTerm subst term)
  where
    tbndrs = termBndrs term
    (subst, tbndrs') = freshenBndrs cxt tbndrs

-- Use to freshen if/for/fun, because renamings from term1
-- also need to apply to term2.
freshenTerm2 :: ReductionContext -> Term -> Term -> (Set Ident, Term, Term)
freshenTerm2 cxt term1 term2
  | null subst = (tbndrs, term1, term2)
  | otherwise  = (tbndrs', renameTerm subst term1, renameTerm subst term2)
  where
    tbndrs = termBndrs term1
    (subst, tbndrs') = freshenBndrs cxt tbndrs

freshenBlk :: ReductionContext -> Blk -> Blk
freshenBlk cxt blk@(Blk locals leqns expr)
  | null subst = blk
  | otherwise  = Blk locals' (map ren_eqn leqns) (rename subst expr)
  where
    (subst, locals') = freshenBndrs cxt locals
    ren_eqn (i, e) = (fromMaybe i (lookup i subst), rename subst e)

freshenBndrs :: ReductionContext -> Set Ident -> (Renaming, Set Ident)
freshenBndrs (RC { rc_exis = exis, rc_skols = skols }) bndrs
  = (subst, S.fromList bndrs_list')
  where
    (subst, bndrs_list') = L.mapAccumL do_one [] (S.toList bndrs)
    in_scope = skols `S.union` exis

    do_one :: Renaming -> Ident -> (Renaming,Ident)
    do_one subst_acc lcl
      | lcl `S.member` in_scope = (subst_acc', lcl')
      | otherwise               = (subst_acc,  lcl)
       where
         lcl' = findFresh bad lcl
         subst_acc' = (lcl,lcl'):subst_acc
         bad x = x `S.member` in_scope || any (bad_rng x) subst_acc || x `S.member` bndrs
         bad_rng x (_,y') = x==y'

freshId :: ReductionContext -> String -> Ident
freshId cxt s = case freshIds cxt [s] of
                  (n:_) -> n
                  []    -> error "freshId"

freshIds2 :: ReductionContext -> String -> (Ident,Ident)
freshIds2 cxt s = case freshIds cxt (repeat s) of
                   (n1:n2:_) -> (n1,n2)
                   []        -> error "freshId2"

freshId3 :: ReductionContext -> (String,String,String) -> (Ident,Ident,Ident)
freshId3 cxt (s1,s2,s3)
  = case freshIds cxt [s1,s2,s3] of
      [n1,n2,n3] -> (n1,n2,n3)
      _          -> error "freshId3"

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

findFresh :: (Ident -> Bool) -> Ident -> Ident
findFresh bad orig_id@(Name s)
  | bad orig_id = go 0
  | otherwise   = orig_id
  where
    prefix1 = takeWhile isAlpha s
    prefix | null prefix1 = "u"
           | otherwise    = prefix1

    go :: Int -> Ident
    go n | n > 10000 = error ("findFresh " ++ show s)
         | bad new_id = go (n+1)
         | otherwise  = new_id
         where
           new_id = Name (prefix ++ show n)

