{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}
     {- -Wno-missing-methods -Wno-incomplete-uni-patterns -Wno-unused-matches
        -Wno-missing-pattern-synonym-signatures -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Red( Blk(Blk, BlkX), Exp(..), Term(..)
          , run, runTraced, isVal
  ) where

import Prelude hiding ((<>))

import qualified FrontEnd.Expr as F
import FrontEnd.ToCore( toCoreEff )
import FrontEnd.Error

import Core.Traced
import Core.Solver( unsat )
import Core.Expr as C ( Ident(..), Assump, Effect(..), Lit(..), PrimOp(..) )

import Epic.Print
import Epic.List hiding ((\\))

import Control.Arrow(second)
import Data.List(group, sort)
import qualified Data.List as L
import Data.Maybe

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

newtype Set a = Set [a]
  deriving (Show)
instance Eq a => Eq (Set a) where
  a == b  =  subset a b && subset b a
mkSet :: Ord a => [a] -> Set a
mkSet = Set . nub
mkSetUnsafe :: [a] -> Set a        -- Only use this when the list has unique elements
mkSetUnsafe = Set
sing :: a -> Set a
sing x = Set [x]
pattern Empty :: Set a
pattern Empty = Set []
isEmptySet :: Set a -> Bool
isEmptySet (Set []) = True
isEmptySet _ = False
union :: Eq a => Set a -> Set a -> Set a
union (Set a) (Set b) = Set (a `L.union` b)
intersect :: Eq a => Set a -> Set a -> Set a
intersect (Set a) (Set b) = Set (a `L.intersect` b)
(\\) :: Eq a => Set a -> Set a -> Set a
(\\) (Set a) (Set b) = Set (a L.\\ b)
subset :: Eq a => Set a -> Set a -> Bool
subset small big = isEmptySet (small \\ big)
unions :: Eq a => [Set a] -> Set a
unions = foldr union Empty
toList :: Set a -> [a]
toList (Set s) = s
fromList :: Ord a => [a] -> Set a
fromList xs = Set (nub xs)
mapSetUnsafe :: (a -> b) -> Set a -> Set b        -- Only use this when the result list has unique elements
mapSetUnsafe f (Set xs) = Set (map f xs)
member :: Eq a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs
notMember :: Eq a => a -> Set a -> Bool
notMember x (Set xs) = x `notElem` xs
filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (Set xs) = Set (filter p xs)
size :: Set a -> Int
size (Set xs) = length xs
disjoint :: Eq a => Set a -> Set a -> Bool
disjoint xs ys = isEmptySet $ xs `intersect` ys

instance Pretty a => Pretty (Set a) where
  pPrintPrec l _ (Set xs) = braces $ fsep $ punctuate (text ",") $ map (pPrintL l) xs

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
  = TVar Ident              -- x
  | TLit Lit                -- k
  | TPrm PrimOp             -- op
  | Term :>%  Term          -- t1; t2
  | Term :|:% Term          -- t1 | t2
  | TFail                   -- fail
  | Term :=:% Term          -- t1 = t2
  | Term :@%  Term          -- t1[t2]
  | Term :..% Term          -- t1 .. t2
  | TArr [Term]             -- array{t1,...,t2}

  | Und                     -- _
  | Term `Where` Term       -- t1 where t2
  | Fun Term Effect Term    -- fun(t1)<fx>{t2}
  | If Term Term Term       -- if (t1){t2}{t3}
  | For Term Term           -- for (t1){t2}
  | Rng Term                -- :t
  | Ident := Term           -- x := t
  | Ident :-> Term          -- x ??? t
  | TBlock Term             -- block{ t }
  | Check Effect Term       -- check<fx>{t}
  deriving (Eq, Show)

data Exp
  -- Values
  = Var Ident             -- x
  | Lit Lit             -- k
  | Prm PrimOp            -- op
  | Exp :>  Exp           -- e1; e2
  | Blk :|: Blk           -- e1 | e2
  | Fail                  -- fail
  | Exp :=: Exp           -- e1 = e2
  | Exp :@  Exp           -- e1[e2]
  | Exp :.. Exp           -- e1 .. e2
  | Arr [Exp]             -- array{e1,...,e2}

  | Lam Ident Blk          -- \ x . e
  | Iter IterCtx Blk Exp   -- if/for
  | Dly Blk                -- delay{b}
  | Crl Blk                -- {...}
  | Ident :~> Term         -- e ~> t

  | Verify [Ident] [Assump] Blk
  deriving (Eq, Show)

type Eqn = (Ident, Val)

-- The equation RHSs have no variables from the LHSs
-- A block (Blk xs eqs e) satisfies these invariants:
--    (A)  dom(eqs)    `subset`   X
--    (B)  occfvs(eqs) `disjoint` dom(eqs)

data Blk = BlkX (Set Ident) (Set Eqn) Exp
  deriving (Eq, Show)

{-# COMPLETE Blk #-}
pattern Blk :: (Set Ident) -> (Set Eqn) -> Exp -> Blk
--pattern Blk is eqs e = BlkX is eqs e  -- do not check invariant
pattern Blk is eqs e <- BlkX is eqs e   -- check invariant
  where Blk is eqs e = assertP "BadBlock1" invariant1 (pPrint blk) $
                       assertP "BadBlock2" invariant2 (pPrint blk) $
                       blk
           where
             blk = BlkX is eqs e
             invariant1 = dom eqs `subset` is
             invariant2 = unions (map (occfvs . snd) $ toList eqs) `disjoint` dom eqs

-- exists x y{ x <-3; y<-x }  -- no to inv2

type Val = Exp

data IterCtx = IF | FOR | ALL
  deriving (Eq, Show)

pattern BlkE :: Exp -> Blk
pattern BlkE e = BlkX Empty Empty e

mkCrl :: Blk -> Exp
mkCrl (BlkE e) = e
mkCrl b        = Crl b

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
  pPrintPrec l p (e1 :>% e2)     = maybeParens (p > 0) $ pPrintPrec l 1 e1 <> text ";" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 `Where` e2) = maybeParens (p > 0) $ pPrintPrec l 1 e1 <+> text "where" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 :=:% e2)    = maybeParens (p > 5) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (TArr es)
    | l == prettyNormal     = text "<" <> hsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (TArr [e]) = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (TArr es)  = parens $ hsep $ punctuate (text ",") $ map (pPrintPrec l 0) es

  pPrintPrec l p (b1 :|:% b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec l p (e1 :..% e2) = maybeParens (p > 7) $ pPrintPrec l 8 e1 <> text ".." <> pPrintPrec l 8 e2
  pPrintPrec _ _ TFail        = text "fail"

  pPrintPrec l _ (If t1 t2 t3) = sep [text "if" <> parens (pPrintL l t1), nest 2 (braces (pPrintL l t2)), text "else", nest 2 (braces (pPrintL l t3)) ]
  pPrintPrec l _ (For t1 t2)   = sep [text "for" <> parens (pPrintL l t1), nest 2 (braces (pPrintL l t2))]
  pPrintPrec l p (x :-> e)     = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":->" <+> pPrintPrec l 2 e

  pPrintPrec l _ (TBlock t)    = braces $ (pPrintL l t)
  pPrintPrec l _ (Check fx t)  = text "check" <> angleBrackets (pPrint fx) <> braces (pPrintL l t)

instance Pretty Exp where
  pPrintPrec l p (Var i)     = pPrintPrec l p i
  pPrintPrec l p (Lit i)     = pPrintPrec l p i
  pPrintPrec l p (Prm o)     = pPrintPrec l p o
  pPrintPrec l p (Lam i b)   = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 i <> text "." <> pPrintPrec l 0 b
  pPrintPrec l p (x :~> e)   = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>" <+> pPrintPrec l 1 e
  pPrintPrec l p (e1 :@ e2)  = maybeParens (p > 10) $ cat [pPrintPrec l 10 e1, nest 2 (text "[" <> pPrintPrec l 0 e2 <> text "]")]
  pPrintPrec l p ee@(_ :> _) = maybeParens (p > 0) $ sep $ punctuate (text ";") (map (pPrintL l) $ flat ee)
                               where flat (e1 :> e2) = flat e1 ++ flat e2
                                     flat e = [e]
  pPrintPrec l p (e1 :=: e2) = maybeParens (p > 0) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (Arr es)
    | l == prettyNormal      = text "<" <> sep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (Arr [e])   = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (Arr es)    = parens $ sep $ punctuate (text ",") $ map (pPrintPrec l 0) es

  pPrintPrec l _ (Crl b)     = braces $ pPrintPrec l 0 b
  pPrintPrec l _ (Dly b)     = text "delay" <> braces (pPrintL l b)
  pPrintPrec l p (b1 :|: b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec l p (e1 :.. e2) = maybeParens (p > 7) $ pPrintPrec l 8 e1 <> text ".." <> pPrintPrec l 8 e2
  pPrintPrec _ _ Fail        = text "fail"


  pPrintPrec l _ (Iter ic b1 b2)
    = text "iter" <> cat [parens (text (show ic)), braces (pPrintL l b1), braces (pPrintL l b2)]
  pPrintPrec l _ (Verify rs as e)
    = sep [ text "verify" <> parens (sep [ fsep (map pPrint rs) <> text ";"
                                         , fsep (map pPrint as) ])
          , nest 2 (braces (pPrintL l e)) ]

instance Pretty Blk where
  pPrintPrec l p (Blk vs eqns e) = ppBlk l p vs eqns e

ppBlk :: PrettyLevel -> Rational -> Set Ident -> Set Eqn -> Exp -> Doc
ppBlk l p Empty Empty e
  = pPrintPrec l p e
ppBlk l p vs eqns e
  = maybeParens (p > 0) $ text "∃" <+> sep [pp_bndrs, pp_eqns, pp_body]
  where
    pp_bndrs = hsep (map (pPrintPrec l 10) $ toList vs)
    pp_eqns = braces (fsep $ punctuate (text ";") $ map (ppr_eqn l) $ toList eqns) <> text "."
    pp_body = pPrintPrec l 0 e

ppr_eqn :: PrettyLevel -> (Ident, Exp) -> Doc
ppr_eqn l (i, d) = pPrintPrec l 0 i <+> text "<-" <+> pPrintPrec l 0 d

instance Pretty SBlk where
  pPrintPrec l p (FloatB b) = pPrintPrec l p b
  pPrintPrec l p (Promote eq e) = maybeParens (p > 10) $ braces (ppr_eqn l eq) <+> pPrintPrec l 11 e

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
-- A value with a constructor at the top, not a variable
--   E.g.  7, (\x.e),  <x,3>
getHNF Var{} = Nothing
getHNF e = getVal e

pattern Val :: Exp -> Val
pattern Val e <- (getVal -> Just e)

-- Values
getVal :: Exp -> Maybe Exp
getVal e@Var{} = Just e
getVal e@Lit{} = Just e
getVal e@Prm{} = Just e
getVal e@Lam{} = Just e
getVal e@(Arr es) | Just _ <- mapM getVal es = Just e
getVal e@Dly{} = Just e
getVal _ = Nothing

isVal :: Exp -> Bool
isVal e = isJust (getVal e)

-- Turn every kind of HNF into a trivial one
root :: HNF -> HNF
root Lit{} = IntE 0
root Prm{} = Prm Add
root Lam{} = Lam (Name "") (BlkE $ IntE 0)
root Arr{} = Arr []
root Dly{} = Dly (BlkE $ IntE 0)
root _ = error "root: not an HNF"

pattern IntE :: Integer -> Exp
pattern IntE i = Lit (LInt i)

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
srcToTerm (F.Array es)           = TArr (map srcToTerm es)
srcToTerm (F.Fail)               = TFail
srcToTerm (F.Function _ e1 fx e2)
  | Just eff <- toCoreEff fx     = Fun (srcToTerm e1) eff (srcToTerm e2)
srcToTerm (F.DefineV x)          = srcToCoreIdent x := Rng (TVar (Name "any"))
srcToTerm (F.DefineIE i e)       = srcToCoreIdent i :-> srcToTerm e
srcToTerm (F.If3 e1 e2 e3)       = If (srcToTerm e1) (srcToTerm e2) (srcToTerm e3)
srcToTerm (F.For2 e1 e2)         = For (srcToTerm e1) (srcToTerm e2)

-- Special case for operator'..'[ t1, t2 ]
-- We want to turn that into the syntactic form (t1 @.. t2),
-- because ".." plays a special role in pattern matching
srcToTerm (F.ApplyD e1 e2)
  | F.Variable (F.Ident _ "operator'..'") <- e1
  , F.Array [e2a, e2b] <- e2     = srcToTerm e2a :..% srcToTerm e2b

srcToTerm (F.ApplyD e1 e2)       = srcToTerm e1 :@% srcToTerm e2
srcToTerm (F.Exists is e)        = TBlock (foldr bind_one (srcToTerm e) is)
                                 where
                                   bind_one x t = (srcToCoreIdent x := Und) :>% t

srcToTerm (F.Check fx e)
  | Just eff <- toCoreEff fx     = Check eff (srcToTerm e)

srcToTerm e = error $ "srcToTerm: unimplemented " ++ show e

srcToCoreIdent :: F.Ident -> Ident
srcToCoreIdent (F.Ident _ s) = Name s

--------------------------------------------------------------------------------
--
--             Reductions
--
--------------------------------------------------------------------------------

type RuleName = String

data Reduction
  = None               -- No redex found
  | Failure RuleName   -- Evaluation failed
  | Delete (Set Ident)  -- Delete the identifiers from the block existential and equations
  | Step RuleName SBlk -- The named rule fired, returning this Blk
  | StepC RuleName Blk Blk  -- The named rule fired, returning a choice.  Used for the B context
  deriving (Eq, Show)

-- A Step returns a SBlk
data SBlk = FloatB Blk                   -- From (FloatB)
          | Promote (Ident, Exp) Exp     -- From (Promote1) or (Promote2)
  deriving (Eq, Show)


pattern Done :: String -> Exp -> Reduction
pattern Done s e = Step s (FloatB (BlkE e))

instance Pretty Reduction where
  pPrintPrec l p (Delete is) = text "GC" <+> pPrintPrec l p is
  pPrintPrec _ _ None               = text "None"
  pPrintPrec _ _ (Failure s)        = text "Failure" <+> text (show s)
  pPrintPrec l _ (Done s e)         = text "Done" <+> text (show s) <+> pPrintPrec l 0 e
  pPrintPrec l _ (Step s b)         = text "Step" <+> text (show s)
                                      <+> pPrintPrec l 0 b
  pPrintPrec l _ (StepC s b1 b2)    = text "StepC" <+> text (show s)
                                      <+> pPrintPrec l 11 b1 <+> pPrintPrec l 11 b2

--------------------------------------------------------------------------------
--
--             The prelude
--
--------------------------------------------------------------------------------

mkName :: String -> Ident
mkName s = Name s

mkCons :: Exp -> Exp -> Exp  -- Array cons
mkCons a as = Prm ArrCons :@ Arr [a, as]

mkCons2 :: Exp -> Exp -> Exp -> Exp
-- cons2 x y <xs,ys> = <cons x xs, cons y ys>
mkCons2 x y xys
  = Crl $ Blk (mkSetUnsafe [xs,ys,ar]) Empty $
    -- We need have the two Arr in this order, otherwise choices
    -- in the body of a 'for' will come in the wrong order.
    (Var ar :=: Arr [ mkCons x (Var xs)
                    , mkCons y (Var ys)]) :>
    (Arr [Var xs, Var ys] :=: xys) :>
    (Var ar)
  where
    xs  = mkName "xs"
    ys  = mkName "ys"
    ar  = mkName "ar"

thePrelude :: [Eqn]
thePrelude
  = [ (mkName "int",          Lam vp  $ BlkE $ Prm IsInt :@ (Var vp) :> Var vp)
    , (mkName "any",          Lam vp  $ BlkE $ Var vp)
    , (mkName "length",       Lam vp  $ BlkE $ Prm ArrLen :@ (Var vp))
    , (mkName "prefix'+'",    Lam vp  $ BlkE $ Var vp)
    , (mkName "prefix'-'",    Lam vp  $ BlkE $ Prm Neg    :@ (Var vp))
    , (mkName "operator'+'",  Lam vpq $ BlkE $ Prm Add    :@ (Var vpq))
    , (mkName "operator'-'",  Lam vpq $ BlkE $ Prm Sub    :@ (Var vpq))
    , (mkName "operator'*'",  Lam vpq $ BlkE $ Prm Mul    :@ (Var vpq))
    , (mkName "operator'/'",  Lam vpq $ BlkE $ Prm Div    :@ (Var vpq))
    , (mkName "operator'<'",  Lam vpq $ BlkE $ Prm Lt     :@ (Var vpq))
    , (mkName "operator'<='", Lam vpq $ BlkE $ Prm LEq    :@ (Var vpq))
    , (mkName "operator'>='", Lam vpq $ BlkE $ Prm GEq    :@ (Var vpq))
    , (mkName "operator'>'",  Lam vpq $ BlkE $ Prm Gt     :@ (Var vpq))
    , (mkName "operator'<>'", Lam vpq $ BlkE $ Prm NEq    :@ (Var vpq))
    , (mkName "operator'..'", Lam vpq $ BlkE $ Prm DotDot :@ (Var vpq))
    ]

vp,vpq :: Ident
vpq = mkName "pq"
vp  = mkName "p"

--------------------------------------------------------------------------------
--
--             The evaluator: driver
--
--------------------------------------------------------------------------------

runTraced :: Fuel -> F.SrcEssential -> (NormResult, Traced Blk)
runTraced fuel src
  = normalize step valid fuel (initialBlk src)
  where
    valid :: Blk -> Bool
    valid _ = True

    step :: Blk -> Maybe (TraceStep Blk)
    step blk
      | BlkE Fail <- blk   -- Simon: seems ad-hoc, but otherwise we loop on failure
      = Nothing
      | otherwise
      = case findTopRedex (freshVarsBlk blk) blk of
          Step rule_nm flts -> Just $ TS { ts_str = rule_nm
                                         , ts_payload = mergeStep blk flts
                                         , ts_verb = 1 }
          Delete xs         -> Just $ TS { ts_str = "GC " ++ show xs
                                         , ts_payload = gcVarsBlk xs blk
                                         , ts_verb = 1 }
          Failure rule_nm   -> Just $ TS { ts_str = rule_nm
                                         , ts_payload = BlkE Fail
                                         , ts_verb = 1 }
          None       -> Nothing

run :: F.SrcEssential -> PExp
-- An alternative to runTraced
run src = P $ evalBlk 1000000 (initialBlk src)
  where
    evalBlk :: Int -> Blk -> Exp
    evalBlk _ b | traceReductions && trace (prettyShow b ++ "\n") False = undefined
    evalBlk 0 b = error $ "No fuel: " ++ prettyShow b
    evalBlk fuel b@(Blk is eqs expr) =
        case findTopRedex (freshVarsBlk b) b of
          Step _ b'         -> evalBlk (fuel-1) $ mergeStep b b'
          None | isEmptySet is -> expr
               | otherwise  -> mkCrl (Blk is eqs expr)
          Failure _         -> Fail
          Delete xs         -> evalBlk (fuel-1) (gcVarsBlk xs b)
          StepC _ _ _       -> error "impossible: findTopRedex StepC"  -- can only happen with depth>0

initialBlk :: F.SrcEssential -> Blk
initialBlk src
  = Blk (sing u `union` tbs term `union` fromList (map fst thePrelude))
        (mkSetUnsafe thePrelude)
        (u :~> term)
  where
    term = srcToTerm src
    u    = freshVarsTerm term !! 0

mergeStep :: HasCallStack => Blk -> SBlk -> Blk
mergeStep (Blk is eqs _) (Promote eq@(i, v) e) =
  assertP "mergeStep1" (i `member` is) (pPrint (i, is)) $
  assertP "mergeStep2" (i `notMember` dom eqs) (pPrint (i, dom eqs)) $
  assertP "mergeStep2" (occfvs v `disjoint` dom eqs) (pPrint (occfvs v, dom eqs)) $
  let v' = substVal (toList eqs) v
  in  Blk is (sing eq `union` mapSetUnsafe (second $ substVal [(i, v)]) eqs) e
mergeStep b1@(Blk is1 _ _) (FloatB b2@(Blk is2 _ _))
  | not (is1 `disjoint` is2)
  =  -- must freshen b2 to avoid clash
    merge b1 (freshen (freshVarsBlk b1) b2)
  | otherwise
  = merge b1 b2
  where
    merge (Blk is outer_eqs _) (Blk xs inner_eqs e)
      = Blk (is `union` xs)
            (        outer_eqs
             `union` mapSetUnsafe (second $ substVal $ toList outer_eqs) inner_eqs)
            e

dom :: Set Eqn -> Set Ident
dom = mapSetUnsafe fst

findTopRedex :: [Ident] -> Blk -> Reduction
findTopRedex fresh blk@(Blk locals eqns ex)
  = reduceBlock top_cxt blk
  where
    top_cxt = RC { rc_depth  = 0
                 , rc_fresh  = fresh
                 , rc_single = singleOcc
                 , rc_eqns   = Empty
                 , rc_skols  = [] }

    -- Subset of `locals` that occur exactly once, and have no Eqn
    -- To support EXI-APP
    -- XXX This is wrong.  Can't handle multiple uses of a function
    -- ToDo: what about occurrences in `eqns` under a lambda?
    singleOcc :: Set Ident
    singleOcc = mkSet [ x | [x] <- group (sort (allVars ex))
                      , x `member` locals
                      , isNothing (lookup x $ toList eqns) ]

data ReductionContext
  = RC { rc_depth  :: Int         -- Number of enclosing Blks
       , rc_fresh  :: NameSupply  -- Supply of fresh names
       , rc_single :: Set Ident    -- Supports ExiApp
       , rc_eqns   :: Set Eqn     -- In-scope equations
       , rc_skols  :: [Ident]
    }

lookupEqn :: ReductionContext -> Ident -> Maybe Val
lookupEqn (RC { rc_eqns = eqns }) x = lookup x $ toList eqns

--------------------------------------------------------------------------------
--
--             The evaluator: reduction rules
--
--------------------------------------------------------------------------------

reduceBlock :: ReductionContext -> Blk -> Reduction
reduceBlock cxt parent@(Blk locals leqns ex) =
  if traceReductions then
    trace (render (nest (4*rc_depth cxt) (text "reduceBlock enter parent =" <+> pPrintL prettyNormal parent))) $
    trace (render (nest (4*rc_depth cxt) (text "reduceBlock exit " <+> ((text "parent =" <+> pPrintL prettyNormal parent) $$
                                                                     (text "res    =" <+> pPrintL prettyNormal res))))
           ++ "\n")
    res
  else
    res
  where
    res | not (isEmptySet dead_vars) = Delete dead_vars  -- GC rules
        | otherwise                  = find ex

    -- XXX This needs to construct SCCs from uses inside lambda
    dead_vars :: Set Ident  -- Subset of locals that are unused
    dead_vars = locals \\ (freeVars ex `union` (unions (map (freeVars . snd) $ toList leqns)))

    -- inner_cxt: update the ReductionContext for when we walk inside the block
    inner_cxt = cxt { rc_depth = rc_depth cxt + 1
                    , rc_eqns = filterSet ((`notMember` locals) . fst) (rc_eqns cxt)
                                -- The filter implements the side condition for subst
                                `union` leqns }

    find :: Exp -> Reduction
    find expr =
      case expr of
        -- Scope and substitution
        Var x  :=: Val v  | promotionOK parent x v
                          -> Step "Promote1" $ Promote (x, v) v
        Val v  :=: Var x  | promotionOK parent x v
                          -> Step "Promote2" $ Promote (x, v) v

        Var i             | Just v <- lookupEqn inner_cxt i -> Done ("Subst " ++ show i) v
        Crl b@Blk{}       -> Step "FloatB" $ FloatB $ freshen (rc_fresh cxt) b
        -- GC rules handled above

        -- Primops
        Prm op :@ v | Just redn <- reducePrimOp op v -> redn

        -- Unification
        Val v1       :=: Val v2 | v1 == v2                 -> Done    "EqVal" v1
        Lit k1       :=: Lit k2 | k1 /= k2                 -> Failure "EqValFail"

        Val (Arr vs) :=: Arr es | length vs == length es   -> Done    "EqTup" $ Arr (zipWith (:=:) vs es)
        Arr ds       :=: Arr es | length ds /= length es   -> Failure "EqTupFail"
        HNF h1       :=: HNF h2 | root h1 /= root h2       -> Failure "EqFail"

        -- Sequencing
        Val{}  :>  e2                                      -> Done "Seq1" e2


        -- Unification, structural
        Val v  :=: (e1 :>  e2) -> Done "Norm1" $ e1 :> (v :=: e2)
        Val v  :=: (e1 :=: e2) -> Done "Norm2" $ (v :=: e1) :> (v :=: e2)
        (e1 :> e2) :=: e3      -> Done "Norm3" $ e1 :> (e2 :=: e3)
        (Val v :=: e1) :=: e2  -> Done "Norm4" $ (v :=: e1) :> (v :=: e2)

        IntE l :.. IntE h     -> Done "Enum"  $ if h < l then Fail else foldr alt Fail [l .. h]
          where alt i e = BlkE (IntE i) :|: BlkE e

        -- Beta (\x.blk)[a] -->  exists x. x = a; blk
        -- NOTE: it should be enough to match with eqs=[]
        Lam x blk :@ arg
            -> Step ("Beta " ++ show x) $ FloatB $ Blk (sing x' `union` is') eqs' ((Var x' :=: arg) :> body')
            where
              x' = rc_fresh cxt !! 0
              Blk is' eqs' body' = renameBlk [(x,x')] blk

        -- This rule isn't strictly necessary, but it allows indexing by
        -- a constant to proceed outside a failure context.
        Arr es :@ IntE i | 0 <= i' && i' < length es -> Done "ITup-k" (es !! i')
                                                     where i' = fromInteger i

        Val (Arr es) :@ e -> Step "ITup" $
                             FloatB $ Blk (sing x) Empty $
                             Var x :=: e :>
                            foldr alt Fail (zipWith (\ i e -> (Var x :=: IntE i) :> e) [0..] es)
          where
            x = rc_fresh cxt !! 0
            alt e1 e2 = BlkE e1 :|: BlkE e2

        -- (ExiApp)
        Var f   :@ Val _ | f `member` rc_single cxt
                         -> Step "ExiApp" $ FloatB $ Blk (sing u) Empty (Var u)
                         where u = rc_fresh cxt !! 0

        x :~> tm  -> reduceMatch (rc_fresh cxt) x tm

        -- Choice and failure
        Fail -> Failure "Fail"
        b1 :|: b2 | rc_depth cxt > 0
             -> StepC "B" b1 b2     -- Found a choice, return it if inside a block

        Iter ic b e -> reduceIter inner_cxt ic b e

        Verify skols as e -> reduceVerify cxt skols as e

        -- Catch-all cases for context C; just walk downwards
        e1 :>  e2  -> find2  (:>)  e1 e2
        e1 :=: e2  -> find2  (:=:) e1 e2
        e1 :@  e2  -> find2  (:@)  e1 e2
        e1 :.. e2  -> find2  (:..) e1 e2
        Arr es     -> findArr es

        _ -> None

    find1 :: (Exp -> Exp) -> Exp -> Reduction
    find1 c e =
      case find e of
        Step s (FloatB (Blk is eqs e')) -> Step s $ FloatB $ Blk is eqs (c e')
        Step s (Promote eq e') -> Step s $ Promote eq (c e')
        StepC s (Blk is1 eqs1 e1') (Blk is2 eqs2 e2') -> StepC s (Blk is1 eqs1 (c e1')) (Blk is2 eqs2 (c e2'))
        r                      -> r

    find2 :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Reduction
    find2 c e1 e2 =
      case find e1 of
        Step s (FloatB (Blk is eqs e1')) -> Step s $ FloatB $ Blk is eqs (e1' `c` e2)
        Step s (Promote eq e1') -> Step s $ Promote eq (e1' `c` e2)
        StepC s (Blk is1 eqs1 e1') (Blk is2 eqs2 e2') -> StepC s (Blk is1 eqs1 (e1' `c` e2)) (Blk is2 eqs2 (e2' `c` e2))
        None                    -> find1 (c e1) e2
        r                       -> r

    findArr :: [Exp] -> Reduction
    findArr [] = None
    findArr (e:es) =
      case find e of
        Step s (FloatB (Blk is eqs e')) -> Step s $ FloatB $ Blk is eqs (Arr (e':es))
        Step s (Promote eq e') -> Step s $ Promote eq (Arr (e':es))
        StepC s (Blk is1 eqs1 e1') (Blk is2 eqs2 e2') -> StepC s (Blk is1 eqs1 (Arr (e1':es))) (Blk is2 eqs2 (Arr (e2':es)))
        None                   ->
          case findArr es of
            Step s (FloatB (Blk is eqs (Arr es'))) -> Step s $ FloatB $ Blk is eqs (Arr (e:es'))
            Step s (Promote eq (Arr es')) -> Step s $ Promote eq (Arr (e:es'))
            StepC s (Blk is1 eqs1 (Arr es1')) (Blk is2 eqs2 (Arr es2')) -> StepC s  (Blk is1 eqs1 (Arr (e:es1')))  (Blk is2 eqs2 (Arr (e:es2')))
            r                             -> r
        r                      -> r


promotionOK :: Blk -> Ident -> Val -> Bool
-- True if we can promote (var=val) into the heap for the parent block
promotionOK (Blk locals leqns _) x v
  =  x `member` locals                        -- Must be bound by the /immediately enclosing/ block
                                            --   i.e. is "flexible"
  && x `notMember` dom leqns                  -- x must not have an eqn
  && occfvs v `disjoint` (sing x `union` dom leqns)    -- v must not have variables from eqns


------------------------------------
reducePrimOp :: PrimOp -> Exp -> Maybe Reduction
-- Primops get stuck on values outside the domain
reducePrimOp Add (Arr [IntE i, IntE j])             = Just $ Done    "Prim+" $ IntE (i + j)
reducePrimOp Sub (Arr [IntE i, IntE j])             = Just $ Done    "Prim-" $ IntE (i - j)
reducePrimOp Mul (Arr [IntE i, IntE j])             = Just $ Done    "Prim*" $ IntE (i * j)
reducePrimOp Div (Arr [IntE i, IntE j])
  | j /= 0                                          = Just $ Done    "Prim/" $ IntE (i `div` j)
  | otherwise                                       = Just $ Failure "Prim/"

reducePrimOp Neg (IntE i)                           = Just $ Done    "Prim-neg" $ IntE (- i)
reducePrimOp Pls (IntE i)                           = Just $ Done    "Prim-pls" $ IntE i

reducePrimOp IsInt v@(IntE {})                      = Just $ Done    "Prim-isInt" v
reducePrimOp IsInt HNF{}                            = Just $ Failure "Prim-isInt"

reducePrimOp Lt  (Arr [IntE i, IntE j]) | i<j       = Just $ Done    "Prim-Lt"  $ IntE i
                                        | otherwise = Just $ Failure "Prim-Lt"
reducePrimOp LEq (Arr [IntE i, IntE j]) | i<=j      = Just $ Done    "Prim-LEq" $ IntE i
                                        | otherwise = Just $ Failure "Prim-LEt"
reducePrimOp GEq (Arr [IntE i, IntE j]) | i>=j      = Just $ Done    "Prim-GEq" $ IntE i
                                        | otherwise = Just $ Failure "Prim-GEq"
reducePrimOp Gt  (Arr [IntE i, IntE j]) | i>j       = Just $ Done    "Prim-Gt"  $ IntE i
                                        | otherwise = Just $ Failure "Prim-Gt"
reducePrimOp NEq (Arr [IntE i, IntE j]) | i/=j      = Just $ Done    "Prim-NEq" $ IntE i
                                        | otherwise = Just $ Failure "Prim-NEq"

reducePrimOp ArrCons (Arr [x, Arr xs])              = Just $ Done    "Prim-cons" $ Arr (x:xs)
reducePrimOp ArrLen (Arr xs)                        = Just $ Done    "prim-length" $ IntE (toInteger (length xs))
-- Could have some inverse of ArrLen by reducing
--  (ArrLen :@ e) :=: IntE k  -->  e :=: Arr [_,_,...,_] k new existentials

reducePrimOp ChkFails    (Arr [])  = Just $ Failure "ChkFail"
reducePrimOp ChkSucceeds (Arr [e]) = Just $ Done "ChkSucc" e
reducePrimOp ChkDecides  (Arr [])  = Just $ Failure "ChkDec0"
reducePrimOp ChkDecides  (Arr [e]) = Just $ Done "ChkDec1" e

reducePrimOp _ _ = Nothing

------------------------------------
reduceMatch ::  NameSupply -> Ident -> Term -> Reduction
-- :~> reduction

reduceMatch _fresh x tm
-- We always push down a variable that is not mentioned or bound in t
-- Thus    x ~> (x := 7)  is not allowed
-- Reason: when pushing (x~>) inside, we don't want to capture.
-- Alternative: alpha-rename when pushing inside
  | x `elem` allVarsTerm tm
  = error "unimplemented: reduceMatch, possible name clash"

reduceMatch fresh x tm
  = case tm of
        -- Blocks
        TBlock t             -> Step  "MBlock" $ FloatB $ Blk (tbs t) Empty (x :~> t)

        -- Matching
        Und                  -> Done "MWild" $ Var x
        TVar i               -> Done "MVar"  $ Var x :=: Var i
        TLit k               -> Done "MLit"  $ Var x :=: Lit k
        TPrm o               -> Done "MPrim" $ Var x :=: Prm o
        TFail                -> Done "Mfail"    $ Fail

        (t1 :@% t2)          -> Step "MApp" $ FloatB $ Blk (mkSet [u1,u2]) Empty $
                                     Var x :=: ((u1 :~> t1) :@ (u2 :~> t2))
                             where u1:u2:_ = fresh
        (t1 :=:% t2)         -> Done "MUnif"    $ (x :~> t1) :=: (x :~> t2)
        (t1 :|:% t2)         -> Done "MChoice"  $
                                (Blk (tbs t1) Empty $ x :~> t1) :|:
                                (Blk (tbs t2) Empty $ x :~> t2)
        (t1 :>% t2)          -> Step "MSemi"    $ FloatB $ Blk (sing u) Empty $ (u :~> t1) :> (x :~> t2)
                              where u = fresh!!0
        (t1 `Where` t2)      -> Step "MWhere"   $ FloatB $ Blk (mkSet [u,w]) Empty $
                                (Var w :=: (x :~> t1)) :> (u :~> t2) :> Var w
                             where u:w:_ = fresh

        TArr ts              -> Step "MTup"     $ FloatB $ Blk (mkSet xs) Empty $
                                (Var x :=: Arr (map Var xs)) :> Arr (zipWith (:~>) xs ts)
                              where xs = take (length ts) fresh

        Rng t      -> Step "MColon" $ FloatB $ Blk (sing u) Empty $ (u :~> t) :@ Var x
                   where u = fresh!!0

        (i := t)   -> Done ("MDef " ++ show i) $ Var i :=: (x :~> t)
        (i :-> t)  -> Done ("MArr " ++ show i) $ (Var i :=: Var x) :> (x :~> t)

        (t1 :..% t2) -> Step "MEnum" $ FloatB $ Blk (mkSet [u1,u2]) Empty $
                        Var x :=: ((u1 :~> t1) :.. (u2 :~> t2))
                      where u1:u2:_ = fresh

        -- Functions
        Fun at fx bt -> matchFun fresh x at fx bt

        If t0 t1 t2 -> Done "MIf" $
                       Iter IF (Blk (sing u `union` tbs t0) Empty ((u :~> t0) :> Dly (Blk (tbs t1) Empty (x :~> t1))))
                               (mkCrl (Blk (tbs t2) Empty (x :~> t2)))
                     where u = fresh!!0

        For t0 t1    -> Step "MFor" $ FloatB $ Blk (sing y) Empty $
                        (Arr [Var x, Var y] :=:
                         Iter FOR (Blk (sing u `union` tbs t0) Empty ((u :~> t0) :> Lam w (BlkE $ w :~> t1)))
                                  (Arr [Arr [], Arr []])) :>
                        Var y
                      where y:u:w:_ = fresh

        Check fx t -> Done ("MCheck " ++ show fx) $
                      matchCheck fresh x fx t

matchFun :: NameSupply -> Ident -> Term -> Effect -> Term -> Reduction
matchFun fresh f at fx bt
  = Done "MFun" $
--    fun_verify :>
    the_lambda
  where
    u:p:q:fresh2 = fresh
    the_lambda = Lam u $ Blk (mkSet [p,q] `union` tbs at) Empty $
                 (Var p :=: (u :~> at))       :>
                 (Var q :=: (Var f :@ Var p)) :>
                 (q :~> TBlock bt)

    fun_verify = Verify [u] [] $
                 Blk (sing q `union` tbs at) Empty $
                 (u :~> at) :>
                 matchCheck fresh2 q fx bt

matchCheck :: NameSupply -> Ident -> Effect -> Term -> Exp
matchCheck fresh x fx t
  | Iterates <- fx = x :~> t   -- check<iterates> is a no-op
  | otherwise        = (Prm chk_op :@) $
                       Var x :=: Iter ALL (Blk (sing u `union` tbs t) Empty $ u :~> t) (Arr [])
  where
    u  = fresh !! 0
    chk_op = case fx of
               Fails    -> ChkFails
               Succeeds -> ChkSucceeds
               Decides  -> ChkDecides
               Iterates -> error "reduceMatch:check"  -- handled in earlier rule

---------------------------------------
reduceIter :: ReductionContext -> IterCtx -> Blk -> Exp -> Reduction
reduceIter _ IF (BlkE (Dly b)) _
  = Done "IIf" $ mkCrl b

reduceIter cxt FOR (Blk xs eqs v@Val{}) e2
  = Step "IFor" $ FloatB $ freshen (drop 1 fresh) $
                  Blk (sing x `union` xs) eqs $
                  mkCons2 (Var x) (v :@ Var x) e2
  where
    fresh = rc_fresh cxt
    x = fresh !! 0

reduceIter _ ALL (BlkE v@Val{}) e2
  = Done "IAll" $ mkCons v e2

reduceIter cxt ic b1 e2
  = case reduceBlock cxt b1 of  -- Find a redex in B context
      Failure s     -> Done ("IFail-" ++ s) e2
      None          -> None
      Delete xs     -> Done (show ic ++ "-GC") $ Iter ic (gcVarsBlk xs b1) e2
      Step s b      -> Done (show ic ++ "-" ++ s) $ Iter ic (mergeStep b1 b) e2
      StepC s bl br -> Done ("IChoice-" ++ s) $ Iter ic (mergeStep b1 (FloatB bl)) $
                                                Iter ic (mergeStep b1 (FloatB br)) e2
      -- Note that we update the RuleName to give
      -- more info about where the reduction happened

--------------------------------------------------------------------------------
--
--             Verification rules
--
--------------------------------------------------------------------------------

reduceVerify :: ReductionContext -> [Ident] -> [Assump] -> Blk -> Reduction
reduceVerify cxt skols as blk
  | BlkE v <- blk
  = Done "VVal" v

  | Just reason <- unsat as
  = Done ("VUnsat " ++ render (pPrint reason)) (Arr [])

  | otherwise
  = case reduceBlock cxt blk of
      None          -> None
      Failure s     -> Done ("VFail-" ++ s) (Arr [])
      Delete xs     -> Done "VGC" (Verify skols as (gcVarsBlk xs blk))
      Step s sb     -> Done ("V-" ++ s) (Verify skols as (mergeStep blk sb))
      StepC s bl br -> Done ("VChoice-" ++ s) $
                       (BlkE (Verify skols as (mergeStep blk (FloatB bl))) :|:
                        BlkE (Verify skols as (mergeStep blk (FloatB br))))

--------------------------------------------------------------------------------
--
--             Free and bound variables
--
--------------------------------------------------------------------------------

-- Top level binders
tbs :: Term -> Set Ident
tbs Und{}           = Empty
tbs TLit{}          = Empty
tbs TVar{}          = Empty
tbs TPrm{}          = Empty
tbs (t1 :>% t2)     = tbs t1 `union` tbs t2
tbs (t1 `Where` t2) = tbs t1 `union` tbs t2
tbs (t1 :=:% t2)    = tbs t1 `union` tbs t2
tbs (t1 :@% t2)     = tbs t1 `union` tbs t2
tbs (t1 :..% t2)    = tbs t1 `union` tbs t2
tbs TFail{}         = Empty
tbs (_ :|:% _)      = Empty
tbs (TArr ts)       = unions $ map tbs ts
tbs Fun{}           = Empty
tbs If{}            = Empty
tbs For{}           = Empty
tbs (Rng t)         = tbs t
tbs (x := t)        = sing x `union` tbs t   -- (:=) binds
tbs (_ :-> t)       = tbs t                  -- (:->) does not bind
tbs (TBlock {})     = Empty
tbs (Check _ t)     = tbs t

-- Variable uses, not under lambda/delay
occfvs :: Exp -> Set Ident
occfvs (Var x) = sing x
occfvs Lit{} = Empty
occfvs Prm{} = Empty
occfvs Lam{} = Empty
occfvs Dly{} = Empty
occfvs (e1 :> e2) = occfvs e1 `union` occfvs e2
occfvs (b1 :|: b2) = occfvsB b1 `union` occfvsB b2
occfvs Fail = Empty
occfvs (e1 :=: e2) = occfvs e1 `union` occfvs e2
occfvs (i :~> _) = sing i  -- XXX what should we do here
occfvs (e1 :@ e2) = occfvs e1 `union` occfvs e2
occfvs (e1 :.. e2) = occfvs e1 `union` occfvs e2
occfvs (Arr es) = unions (map occfvs es)
occfvs (Iter _ b1 e2) = occfvsB b1 `union` occfvs e2
occfvs (Crl b) = occfvsB b
occfvs Verify{} = Empty

occfvsB :: Blk -> Set Ident
occfvsB (Blk is eqs e) = (unions (occfvs e : map (occfvs . snd) (toList eqs))) \\ is

-- All /free/ variables
freeVars :: Exp -> Set Ident
freeVars (Var i)     = sing i
freeVars (Lit {})    = Empty
freeVars (Prm {})    = Empty
freeVars (Lam i e)   = freeVarsBlk e \\ sing i
freeVars (e1 :>  e2) = freeVars e1 `union` freeVars e2
freeVars (e1 :=: e2) = freeVars e1 `union` freeVars e2
freeVars (i  :~> t ) = sing i `union` freeVarsTerm t
freeVars (e1 :@  e2) = freeVars e1 `union` freeVars e2
freeVars (Arr es)    = unions $ map freeVars es
freeVars (b1 :|: b2) = freeVarsBlk b1 `union` freeVarsBlk b2
freeVars (e1 :.. e2) = freeVars e1 `union` freeVars e2
freeVars Fail        = Empty
freeVars (Dly b)     = freeVarsBlk b
freeVars (Crl b)     = freeVarsBlk b
freeVars (Iter _ b1 e2) = freeVarsBlk b1 `union` freeVars e2
freeVars Verify{}    = Empty

freeVarsBlk :: Blk -> Set Ident
freeVarsBlk (Blk is eqs e) = (unions (map (freeVars . snd) (toList eqs)) `union` freeVars e) \\ is

freeVarsTerm :: Term -> Set Ident
-- All variables mentioned, either as occurrences or binders,
-- freeVarsTermBlock handles the block scope
freeVarsTerm (TVar i) = sing i
freeVarsTerm Und      = Empty
freeVarsTerm (TLit _) = Empty
freeVarsTerm (TPrm _) = Empty
freeVarsTerm (i := t) = sing i `union` freeVarsTerm t
freeVarsTerm (e1 :>%  e2) = freeVarsTerm e1 `union` freeVarsTerm e2
freeVarsTerm (e1 `Where`  e2) = freeVarsTerm e1 `union` freeVarsTerm e2
freeVarsTerm (e1 :=:% e2) = freeVarsTerm e1 `union` freeVarsTerm e2
freeVarsTerm (e1 :@%  e2) = freeVarsTerm e1 `union` freeVarsTerm e2
freeVarsTerm (Fun t1 _ t2) = (freeVarsTerm t1 `union` freeVarsTermBlock t2) \\ tbs t1
freeVarsTerm (Rng e) = freeVarsTerm e
freeVarsTerm (TArr es) = unions $ map freeVarsTerm es
freeVarsTerm (b1 :|:% b2) = freeVarsTermBlock b1 `union` freeVarsTermBlock b2
freeVarsTerm (e1 :..% e2) = freeVarsTerm e1 `union` freeVarsTerm e2
freeVarsTerm TFail = Empty
freeVarsTerm (If t1 t2 t3) = ((freeVarsTerm t1 `union` freeVarsTermBlock t2) \\ tbs t1)
                             `union` freeVarsTermBlock t3
freeVarsTerm (For t1 t2) = (freeVarsTerm t1 `union` freeVarsTermBlock t2) \\ tbs t1
freeVarsTerm (i :-> e) = sing i `union` freeVarsTerm e
freeVarsTerm (TBlock t) = freeVarsTermBlock t
freeVarsTerm (Check _ t) = freeVarsTerm t

freeVarsTermBlock :: Term -> Set Ident
freeVarsTermBlock t = freeVarsTerm t \\ tbs t

-- Do NOT use a Set, we use this to count occurrences.
allVars :: Exp -> [Ident]
-- All variables, including binders
allVars (Var i)     = [i]
allVars (Lit {})    = []
allVars (Prm {})    = []
allVars (Lam i e)   = i : allVarsBlk e
allVars (e1 :>  e2) = allVars e1 ++ allVars e2
allVars (e1 :=: e2) = allVars e1 ++ allVars e2
allVars (i  :~> e ) = i : allVarsTerm e
allVars (e1 :@  e2) = allVars e1 ++ allVars e2
allVars (Arr es)    = concatMap allVars es
allVars (b1 :|: b2) = allVarsBlk b1 ++ allVarsBlk b2
allVars (e1 :.. e2) = allVars e1 ++ allVars e2
allVars Fail        = []
allVars (Dly b)     = allVarsBlk b
allVars (Crl b)     = allVarsBlk b
allVars (Iter _ b1 e2) = allVarsBlk b1 ++ allVars e2
allVars Verify{}    = []

allVarsBlk :: Blk -> [Ident]
allVarsBlk (Blk is eqs e) = toList is ++ concatMap (allVars . snd) (toList eqs) ++ allVars e

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
allVarsTerm (e1 :..% e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm TFail = []
allVarsTerm (If t1 t2 t3) = allVarsTerm t1 ++ allVarsTerm t2 ++ allVarsTerm t3
allVarsTerm (For t1 t2) = allVarsTerm t1 ++ allVarsTerm t2
allVarsTerm (i :-> e) = i : allVarsTerm e
allVarsTerm (TBlock t) = allVarsTerm t
allVarsTerm (Check _ t) = allVarsTerm t

expSize :: Exp -> Int
expSize (Var {})    = 1
expSize (Lit {})    = 1
expSize (Prm {})    = 1
expSize (Lam _ e)   = 1 + blkSize e
expSize (e1 :>  e2) = 1 + expSize e1 + expSize e2
expSize (e1 :=: e2) = 1 + expSize e1 + expSize e2
expSize (_  :~> e ) = 2 + termSize e
expSize (e1 :@  e2) = 1 + expSize e1 + expSize e2
expSize (Arr es)    = 1 + sum (map expSize es)
expSize (b1 :|: b2) = 1 + blkSize b1 + blkSize b2
expSize (e1 :.. e2) = 1 + expSize e1 + expSize e2
expSize Fail        = 1
expSize (Dly b)     = 1 + blkSize b
expSize (Crl b)     = 1 + blkSize b
expSize (Iter _ b1 e2) = 1 + blkSize b1 + expSize e2
expSize (Verify {})    = 1  -- For now

blkSize :: Blk -> Int
blkSize (Blk is eqs e) = size is + sum (map eqnSize $ toList eqs) + expSize e

eqnSize :: Eqn -> Int
eqnSize (_,e) = 1 + expSize e

termSize :: Term -> Int
-- All variables mentioned, either as occurrences or binders
termSize (TVar _)         = 1
termSize Und              = 1
termSize (TLit _)         = 1
termSize (TPrm _)         = 1
termSize (_ := e)         = 1 + termSize e
termSize (e1 :>%  e2)     = 1 + termSize e1 + termSize e2
termSize (e1 `Where`  e2) = 1 + termSize e1 + termSize e2
termSize (e1 :=:% e2)     = 1 + termSize e1 + termSize e2
termSize (e1 :@%  e2)     = 1 + termSize e1 + termSize e2
termSize (Fun e1 _ e2)    = 1 + termSize e1 + termSize e2
termSize (Rng e)          = 1 + termSize e
termSize (TArr es)        = 1 + sum (map termSize es)
termSize (b1 :|:% b2)     = 1 + termSize b1 + termSize b2
termSize (e1 :..% e2)     = 1 + termSize e1 + termSize e2
termSize TFail            = 1
termSize (If t1 t2 t3)    = 1 + termSize t1 + termSize t2 + termSize t3
termSize (For t1 t2)      = 1 + termSize t1 + termSize t2
termSize (_ :-> e)        = 1 + termSize e
termSize (TBlock t)       = 1 + termSize t
termSize (Check _ t)      = 1 + termSize t

rename :: [(Ident, Ident)] -> Exp -> Exp
rename sub = ren
  where
    ren :: Exp -> Exp
    ren e@(Var i) | Just j <- lookup i sub = Var j
                  | otherwise = e
    ren e@(Lit {}) = e
    ren e@(Prm {}) = e
    ren (Lam i b) | isJust (lookup i sub) = let Crl b' = rename (filter ((/= i) . fst) sub) (Crl b) in Lam i b'
                  | otherwise = Lam i (renB b)
    ren (Var i :=: Var j) | Just j' <- lookup i sub, j == j' = Var j
    ren (e1 :> e2) = ren e1 :> ren e2
    ren (e1 :=: e2) = ren e1 :=: ren e2
    ren (i :~> t) = fromMaybe i (lookup i sub) :~> renT t
    ren (e1 :@ e2) = ren e1 :@ ren e2
    ren (Arr es) = Arr (map ren es)
    ren (b1 :|: b2) = renB b1 :|: renB b2
    ren (e1 :.. e2) = ren e1 :.. ren e2
    ren e@Fail = e
    ren (Dly b) = Dly (renB b)
    ren (Iter ic b1 e2) = Iter ic (renB b1) (ren e2)
    ren (Crl b) = Crl (renB b)
    ren e@Verify{} = e

    renB = renameBlk sub

    renT e@(TVar i) | Just j <- lookup i sub = TVar j
                    | otherwise = e
    renT e@(TLit {}) = e
    renT e@(TPrm {}) = e
    renT (TVar i :=:% TVar j) | Just j' <- lookup i sub, j == j' = TVar j
    renT (e1 :>% e2) = renT e1 :>% renT e2
    renT (e1 :=:% e2) = renT e1 :=:% renT e2
    renT (e1 :@% e2) = renT e1 :@% renT e2
    renT (TArr es) = TArr (map renT es)
    renT (b1 :|:% b2) = renT b1 :|:% renT b2
    renT (e1 :..% e2) = renT e1 :..% renT e2
    renT e@TFail = e
    renT (Where t1 t2) = Where (renT t1) (renT t2)
    renT (For t1 t2) = For (renT t1) (renT t2)
    renT (If t1 t2 t3) = If (renT t1) (renT t2) (renT t3)
    renT e@Und = e
    renT (Fun t1 fx t2) = Fun (renT t1) fx (renT t2)
    renT (Rng t) = Rng (renT t)
    renT (i := t) = fromMaybe i (lookup i sub) := renT t
    renT (i :-> t) = fromMaybe i (lookup i sub) :-> renT t
    renT (TBlock t) = TBlock (renT t)
    renT (Check fx t) = Check fx (renT t)

renameBlk :: [(Ident,Ident)] -> Blk -> Blk
renameBlk sub b@(Blk is eqs e)
  | any (isJust . (`lookup` sub)) (toList is)
  = let Crl b' = rename (filter ((`notMember` is) . fst) sub) (Crl b)
    in b'
  | otherwise = Blk is (mapSetUnsafe (second (rename sub)) eqs)
                       (rename sub e)

substVal :: [(Ident, Val)] -> Val -> Val
substVal sub e@(Var i) = fromMaybe e $ lookup i sub
substVal _ e@Lit{} = e
substVal _ e@Prm{} = e
substVal sub (Arr vs) = Arr (map (substVal sub) vs)
substVal sub e@Lam{} | isEmptySet $ mkSetUnsafe (map fst sub) `intersect` mkSet (allVars e) = e
                     | otherwise = e -- error "substVal: Lam unimplemented"
substVal _ e = error $ "substVal: not a Val: " ++ show e

gcVarsBlk :: Set Ident -> Blk -> Blk
gcVarsBlk xs (Blk is eqs expr) = Blk (is \\ xs) (filterSet ((`notMember` xs) . fst) eqs) expr


--------------------------------------------------------------------------------
--
--             Fresh names
--
--------------------------------------------------------------------------------

type NameSupply = [Ident]  -- An infinite list of fresh names

freshVars :: Exp -> [Ident]
freshVars e = idenSupply L.\\ allVars e

idenSupply :: [Ident]
idenSupply = [Name $ "u" ++ show i | i <- [1::Int ..]]

freshVarsBlk :: Blk -> [Ident]
freshVarsBlk b = idenSupply L.\\ allVarsBlk b

freshVarsTerm :: Term -> [Ident]
freshVarsTerm t = freshVars (Name "" :~> t)

--freshVar :: Exp -> Ident
--freshVar = (!!0) . freshVars

freshen :: [Ident] -> Blk -> Blk
freshen fresh _b@(Blk is eqs expr) =
--  trace ("freshen " ++ show sub ++ "\n" ++ show _b ++ "\n" ++ show res)
  res
  where res = Blk (mkSetUnsafe vs) (mapSetUnsafe renEqn eqs) (rename sub expr)
        sub = zip (toList is) fresh
        vs = map snd sub
        renEqn (i, e) = (fromMaybe i (lookup i sub), rename sub e)

