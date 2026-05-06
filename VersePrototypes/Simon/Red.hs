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
import Core.Traced
import qualified Core.Expr as Core  -- For literals, primops, identifiers

import Epic.Print

import Control.Arrow(second)
import Data.List(intersect, nub, (\\), union, group, sort)
import Data.Maybe

import Debug.Trace
import GHC.IO.Exception(assertError)
import GHC.Stack

{-
  Potential problems:
  :sim f:=fun(x:int){x+1}; ((f[y]; y:= -2) | 5)

-}

-- Add a verify block
addVerify :: Bool
addVerify = True

-- Show every reduction step
traceReductions :: Bool
traceReductions = False

--------------------------------------------------------------------------------
--
-- Utils
--
--------------------------------------------------------------------------------

subset :: Eq a => [a] -> [a] -> Bool
subset small big = null (small \\ big)

unions :: Eq a => [[a]] -> [a]
unions = foldr union []

--------------------------------------------------------------------------------
--
--             Data types Term and Exp
--
--------------------------------------------------------------------------------

infixr 0 :>
infix  2 :=
infixr 4 :|:
infixr 5 :=:

-- Synonyms to line up with the rewrite rules doca
type Iden = Core.Ident
type Op   = Core.PrimOp

data Term
  = TVar Iden         -- x
  | TLit Core.Lit     -- k
  | TPrm Op           -- op
  | Term :>%  Term    -- t1; t2
  | Term :|:% Term    -- t1 | t2
  | TFail             -- fail
  | Term :=:% Term    -- t1 = t2
  | Term :@%  Term    -- t1[t2]
  | Term :..% Term    -- t1 .. t2
  | TArr [Term]       -- array{t1,...,t2}

  | Und               -- _
  | Term `Where` Term -- t1 where t2
  | Fun Term Term     -- fun(t1){t2}
  | If Term Term Term -- if (t1){t2}{t3}
  | For Term Term     -- for (t1){t2}
  | Rng Term          -- :t
  | Iden := Term      -- x := t
  | Iden :-> Term     -- x ??? t
  | TBlock Term       -- block{ t }
  | Succ Term         -- check<succeeds>{t}
  deriving (Eq, Show)

data Exp
  -- Values
  = Var Iden              -- x
  | Lit Core.Lit          -- k
  | Prm Op                -- op
  | Exp :>  Exp           -- e1; e2
  | Blk :|: Blk           -- e1 | e2
  | Fail                  -- fail
  | Exp :=: Exp           -- e1 = e2
  | Exp :@  Exp           -- e1[e2]
  | Exp :.. Exp           -- e1 .. e2
  | Arr [Exp]             -- array{e1,...,e2}

  | Lam Iden Blk          -- \ x . e
  | Iter IterCtx Blk Exp  -- if/for
  | Dly Blk               -- delay{b}
  | Crl Blk               -- {...}
  | Iden :~> Term         -- e ~> t

  | Verify String         -- the string should be replaced by something sensible
  deriving (Eq, Show)

type Eqn = (Iden, Val)

type Set a = [a]   -- Represented as a list, but order is immaterial

-- The equation RHSs have no variables from the LHSs
-- A block (Blk xs eqs e) satisfies these invariants:
--    (A)  dom(eqs)    `subset`   X
--    (B)  occfvs(eqs) `disjoint` dom(eqs)

newtype Blk = BlkX SBlk
  deriving (Eq, Show, Pretty)

{-# COMPLETE Blk #-}
pattern Blk :: (Set Iden) -> (Set Eqn) -> Exp -> Blk
--pattern Blk is eqs e = BlkX (SBlk is eqs e)  -- do not check invariant
pattern Blk is eqs e <- BlkX (SBlk is eqs e)   -- check invariant
  where Blk is eqs e =
--          trace ("Blk " ++ show (is, eqs, e)) $
          assertError (dom eqs `subset` is) $
          assertError (unions (map (occfvs . snd) eqs) `disjoint` dom eqs) $
          BlkX (SBlk is eqs e)

-- A block with no invariants.
-- This is what Step returns.
data SBlk = SBlk (Set Iden) (Set Eqn) Exp
  deriving (Eq, Show)

type Val = Exp

data IterCtx = IF | FOR | SUCC
  deriving (Eq, Show)

mkCrl :: Blk -> Exp
mkCrl (Blk [] [] e) = e
mkCrl b             = Crl b

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
  pPrintPrec l _ (Fun e1 e2)     = text "fun" <> parens (pPrintPrec l 0 e1) <> braces (pPrintPrec l 0 e2)
  pPrintPrec l p (x := e)        = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":=" <+> pPrintPrec l 2 e
  pPrintPrec l p (e1 :@% e2)     = maybeParens (p > 10) $ pPrintPrec l 10 e1 <> text "[" <> pPrintPrec l 0 e2 <> text "]"
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

  pPrintPrec l _ (If t1 t2 t3) = text "if" <> parens (pPrintL l t1) <> braces (pPrintL l t2) <> text "else" <> braces (pPrintL l t3)
  pPrintPrec l _ (For t1 t2)   = text "for" <> parens (pPrintL l t1) <> braces (pPrintL l t2)
  pPrintPrec l p (x :-> e)     = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":->" <+> pPrintPrec l 2 e

  pPrintPrec l _ (TBlock t)    = braces $ (pPrintL l t)
  pPrintPrec l _ (Succ t)      = text "check<succeeds>" <> braces (pPrintL l t)

instance Pretty Exp where
  pPrintPrec l p (Var i)     = pPrintPrec l p i
  pPrintPrec l p (Lit i)     = pPrintPrec l p i
  pPrintPrec l p (Prm o)     = pPrintPrec l p o
  pPrintPrec l p (Lam i b)   = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 i <> text "." <> pPrintPrec l 0 b
  pPrintPrec l p (x :~> e)   = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>" <+> pPrintPrec l 1 e
  pPrintPrec l p (e1 :@ e2)  = maybeParens (p > 10) $ pPrintPrec l 10 e1 <> text "[" <> pPrintPrec l 0 e2 <> text "]"
  pPrintPrec l p (e1 :> e2)  = maybeParens (p > 0) $ pPrintPrec l 1 e1 <> text ";" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 :=: e2) = maybeParens (p > 0) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (Arr es)
    | l == prettyNormal    = text "<" <> sep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (Arr [e]) = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (Arr es)  = parens $ sep $ punctuate (text ",") $ map (pPrintPrec l 0) es

  pPrintPrec l _ (Crl b)     = braces $ pPrintPrec l 0 b
  pPrintPrec l _ (Dly b)     = text "delay" <> braces (pPrintL l b)
  pPrintPrec l p (b1 :|: b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec l p (e1 :.. e2) = maybeParens (p > 7) $ pPrintPrec l 8 e1 <> text ".." <> pPrintPrec l 8 e2
  pPrintPrec _ _ Fail        = text "fail"


  pPrintPrec l _ (Iter ic b1 b2)
    = text "iter" <> parens (text (show ic)) <> braces (pPrintL l b1) <> braces (pPrintL l b2)
  pPrintPrec _ _ (Verify s) = text "verify" <> parens (text (show s))

instance Pretty SBlk where
  pPrintPrec l p (SBlk vs eqns e) = ppBlk l p vs eqns e

ppBlk :: PrettyLevel -> Rational -> [Iden] -> [Eqn] -> Exp -> Doc
ppBlk l p [] [] e
  = pPrintPrec l p e
ppBlk l p vs eqns e
  = maybeParens (p > 0) $ sep [pp_bndrs, pp_body]
  where
    pp_bndrs = text "∃" <> hsep (map (pPrintPrec l 10) vs) <> braces (fsep pp_eqns) <> text "."
    pp_eqns = punctuate (text ";") $ map ppr_eqn eqns
    pp_body = pPrintPrec l 0 e
    ppr_eqn (i, d) = pPrintPrec l 0 i <+> text "<-" <+> pPrintPrec l 0 d

--------------------------------------------------------------------------------
--
--             Pattern synonyms that carve out Exp subsets
--
--------------------------------------------------------------------------------

-- A HNF, i.e., a value, but not a variable
pattern HNF :: Exp -> Val
pattern HNF e <- (getHNF -> Just e)

getHNF :: Exp -> Maybe Exp
-- A value with a constructor at the top, not a variable
--   E.g.  7, (\x.e),  <x,3>
getHNF Var{} = Nothing
getHNF e = getVal e

-- Either an Arr with all HNF, or a non-arr HNF
-- Simon: why?
pattern AHNF :: Exp -> Val
pattern AHNF e <- (getAHNF -> Just e)

getAHNF :: Exp -> Maybe Exp
getAHNF (Arr es) | Nothing <- mapM getHNF es = Nothing
getAHNF e = getHNF e

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

pattern Con :: Exp -> Val
pattern Con e <- (getCon -> Just e)

-- Constants
getCon :: Exp -> Maybe Exp
getCon e@Lit{} = Just e
getCon e@(Arr es) | Just _ <- mapM getCon es = Just e
getCon _ = Nothing

pattern LInt :: Integer -> Exp
pattern LInt i = Lit (Core.LInt i)

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
srcToTerm (F.Function _ e1 _ e2) = Fun (srcToTerm e1) (srcToTerm e2)
srcToTerm (F.DefineV x)          = srcToCoreIdent x := Rng (TPrm F.IsAny)
srcToTerm (F.DefineIE i e)       = srcToCoreIdent i :-> srcToTerm e
srcToTerm (F.If3 e1 e2 e3)       = If (srcToTerm e1) (srcToTerm e2) (srcToTerm e3)
srcToTerm (F.For2 e1 e2)         = For (srcToTerm e1) (srcToTerm e2)
srcToTerm (F.ApplyD e1 e2)
  | F.EPrim F.DotDot <- e1
  , F.Array [e2a, e2b] <- e2     = srcToTerm e2a :..% srcToTerm e2b
  | otherwise                    = srcToTerm e1 :@% srcToTerm e2
srcToTerm (F.Exists is e)        = TBlock (foldr bind_one (srcToTerm e) is)
                                 where
                                   bind_one x t = (srcToCoreIdent x := Und) :>% t

srcToTerm (F.Check suc e) | suc == F.effSucceeds = Succ (srcToTerm e)
srcToTerm e = error $ "srcToTerm: unimplemented " ++ show e

srcToCoreIdent :: F.Ident -> Core.Ident
srcToCoreIdent (F.Ident _ s) = Core.Name s

--------------------------------------------------------------------------------
--
--             Reductions
--
--------------------------------------------------------------------------------

type RuleName = String

data Reduction
  = None               -- No redex found
  | Failure RuleName   -- Evaluation failed
  | Delete (Set Iden)  -- Delete the identifiers from the block existential and equations
  | Step RuleName SBlk -- The named rule fired, returning this Blk
  | StepC RuleName SBlk SBlk  -- The named rule fired, returning a choice.  Used for the B context
  deriving (Eq, Show)

pattern Done :: String -> Exp -> Reduction
pattern Done s e = Step s (SBlk [] [] e)

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
--             The evaluator
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
      | Blk [] [] Fail <- blk   -- Simon: seems ad-hoc, but otherwise we loop on failure
      = Nothing
      | otherwise
      = case findTopRedex (freshVarsBlk blk) blk of
          Step rule_nm flts -> Just $ TS { ts_str = rule_nm
                                         , ts_payload = mergeStep blk flts
                                         , ts_verb = 1 }
          Delete xs         -> Just $ TS { ts_str = "Delete " ++ show xs
                                         , ts_payload = gcVarsBlk xs blk
                                         , ts_verb = 1 }
          Failure rule_nm   -> Just $ TS { ts_str = rule_nm
                                         , ts_payload = Blk [] [] Fail
                                         , ts_verb = 1 }
          None       -> Nothing

run :: F.SrcEssential -> PExp
run src = P $ evalBlk 1000000 (initialBlk src)

initialBlk :: F.SrcEssential -> Blk
initialBlk src
  = Blk (u:tbs term) [] (u :~> term)
  where
    term = srcToTerm src
    u = freshVarsTerm term !! 0

evalBlk :: Int -> Blk -> Exp
evalBlk _ b | traceReductions && trace (prettyShow b ++ "\n") False = undefined
evalBlk 0 b = error $ "No fuel: " ++ prettyShow b
evalBlk fuel b@(Blk is eqs expr) =
    case findTopRedex (freshVarsBlk b) b of
      Step _ b'         -> evalBlk (fuel-1) $ mergeStep b b'
      None | null is    -> expr
           | otherwise  -> Crl (Blk is eqs expr)
      Failure _         -> Fail
      Delete xs         -> evalBlk (fuel-1) (gcVarsBlk xs b)
      StepC _ _ _       -> error "impossible: findTopRedex StepC"  -- can only happen with depth>0

mergeStep :: HasCallStack => Blk -> SBlk -> Blk
mergeStep b1@(Blk is1 _ _) b2@(SBlk is2 _ _)
  | not (is1 `disjoint` is2)
  =  -- must freshen b2 to avoid clash
    merge b1 (freshen (freshVarsBlk b1) (BlkX b2))
  | otherwise
  = merge b1 b2
  where
    merge (Blk is eqs _) (SBlk xs eqns e)
      = Blk (is `union` xs)
            (map (second $ substVal eqs) eqns ++
             map (second $ substVal eqns) eqs)
            e

dom :: Set Eqn -> Set Iden
dom = map fst

disjoint :: Eq a => Set a -> Set a -> Bool
disjoint xs ys = null $ xs `intersect` ys

findTopRedex :: [Iden] -> Blk -> Reduction
findTopRedex fresh blk@(Blk locals eqns ex)
  = reduceBlock top_cxt blk
  where
    top_cxt = RC { rc_depth  = 0
                 , rc_fresh  = fresh
                 , rc_single = singleOcc
                 , rc_eqns   = [] }

    -- Subset of `locals` that occur exactly once, and have no Eqn
    -- To support EXI-APP
    -- XXX This is wrong.  Can't handle multiple uses of a function
    -- ToDo: what about occurrences in `eqns` under a lambda?
    singleOcc :: Set Iden
    singleOcc = [ x | [x] <- group (sort (allVars ex))
                    , x `elem` locals
                    , isNothing (lookup x eqns) ]

data ReductionContext
  = RC { rc_depth  :: Int         -- Number of enclosing Blks
       , rc_fresh  :: NameSupply  -- Supply of fresh names
       , rc_single :: Set Iden    -- Supports ExiApp
       , rc_eqns   :: Set Eqn     -- In-scope equations
    }

lookupEqn :: ReductionContext -> Iden -> Maybe Val
lookupEqn (RC { rc_eqns = eqns }) x = lookup x eqns

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
    res | not (null dead_vars) = Delete dead_vars  -- GC rules
        | otherwise            = find ex

    -- XXX This needs to construct SCCs from uses inside lambda
    dead_vars :: [Iden]  -- Subset of locals that are unused
    dead_vars = locals \\ (allVars' ex ++ concatMap (allVars' . snd) leqns)

    -- inner_cxt: update the ReductionContext for when we walk inside the block
    inner_cxt = cxt { rc_depth = rc_depth cxt + 1
                    , rc_eqns = filter ((`notElem` locals) . fst) (rc_eqns cxt)
                                -- The filter implements the side condition for subst
                                ++ leqns }

    find :: Exp -> Reduction
    find expr =
      case expr of
        -- Scope and substitution
        Var x  :=: Val v  | promotionOK parent x v
                          -> Step "Promote1" $ SBlk [] [(x, v)] v
        Val v  :=: Var x  | promotionOK parent x v
                          -> Step "Promote2" $ SBlk [] [(x, v)] v

        Var i             | Just v <- lookupEqn inner_cxt i -> Done ("Subst " ++ show i) v
        Crl b@Blk{}       -> Step "FloatB" $ freshen (rc_fresh cxt) b
        -- GC rules handled above

        -- Primops
        Prm op :@ v | Just redn <- reducePrimOp op v -> redn

        -- Unification
        Val v1 :=: Val v2 | v1 == v2 -> Done "EqVal" v1
        Con v1 :=: Con v2 | v1 /= v2 -> Failure "EqFail"

-- Let this be stuck instead        Var i  :=: HNF v  | i `elem` allFreeVars' v -> Failure "OCCUR"  -- occurs check
        Val (Arr vs) :=: Arr es | length vs /= length es   -> Failure "arr /="
                                | otherwise                -> Done "EqTup" $ Arr (zipWith (:=:) vs es)

        -- Sequencing
        Val{}  :>  e2                                      -> Done "Seq1" e2


        -- Unification, structural
        Val v  :=: (e1 :>  e2) -> Done "Norm1" $ e1 :> (v :=: e2)
        Val v  :=: (e1 :=: e2) -> Done "Norm2" $ (v :=: e1) :> (v :=: e2)
        (e1 :> e2) :=: e3      -> Done "Norm3" $ e1 :> (e2 :=: e3)
        (Val v :=: e1) :=: e2  -> Done "Norm4" $ (v :=: e1) :> (v :=: e2)

        LInt l :.. LInt h     -> Done "Enum"  $ if h < l then Fail else foldr alt Fail [l .. h]
          where alt i e = Blk [] [] (LInt i) :|: Blk [] [] e

        -- Beta
        -- NOTE: it should be enough to match with eqs=[]
        Lam x b :@ e -> Step "Beta" $ SBlk [x'] [] ((Var x' :=: e) :> mkCrl b')
            where
              x' = rc_fresh cxt !! 0
              b' = renameBlk [(x,x')] b

        -- This rule isn't strictly necessary, but it allows indexing by
        -- a constant to proceed outside a failure context.
        Arr es :@ LInt i | 0 <= i' && i' < length es -> Done "ITup-k" (es !! i')
                                                     where i' = fromInteger i

        Arr es :@ Val v -> Done "ITup" $
                           foldr alt Fail (zipWith (\ i e -> (v :=: LInt i) :> e) [0..] es)
          where alt e1 e2 = Blk [] [] e1 :|: Blk [] [] e2

        -- (ExiApp)
        Var f   :@ Val _ | f `elem` rc_single cxt
                         -> Step "ExiApp" $ SBlk [u] [] (Var u)
                         where u = rc_fresh cxt !! 0

        Fail             -> Failure "Fail"

        x :~> tm  -> reduceMatch (rc_fresh cxt) x tm

        -- Catch-all cases for context C; just walk downwards
        e1 :>  e2  -> find2  (:>)  e1 e2
        e1 :=: e2  -> find2  (:=:) e1 e2
        e1 :@  e2  -> find2  (:@)  e1 e2
        e1 :.. e2  -> find2  (:..) e1 e2
        Arr es     -> findArr es

        BlkX b1 :|: BlkX b2 | rc_depth cxt > 0
             -> StepC "B" b1 b2     -- Found a choice, return it if inside a block

        Iter ic b e -> reduceIter inner_cxt ic b e

        Verify s -> trace ("*** discard verify " ++ show s) $ Done "Verify" (Arr [])

        _ -> None

    find1 :: (Exp -> Exp) -> Exp -> Reduction
    find1 c e =
      case find e of
        Step s (SBlk is eqs e') -> Step s $ SBlk is eqs (c e')
        StepC s (SBlk is1 eqs1 e1') (SBlk is2 eqs2 e2') -> StepC s (SBlk is1 eqs1 (c e1')) (SBlk is2 eqs2 (c e2'))
        r                      -> r

    find2 :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Reduction
    find2 c e1 e2 =
      case find e1 of
        Step s (SBlk is eqs e1') -> Step s $ SBlk is eqs (e1' `c` e2)
        StepC s (SBlk is1 eqs1 e1') (SBlk is2 eqs2 e2') -> StepC s (SBlk is1 eqs1 (e1' `c` e2)) (SBlk is2 eqs2 (e2' `c` e2))
        None                    -> find1 (c e1) e2
        r                       -> r

    findArr :: [Exp] -> Reduction
    findArr [] = None
    findArr (e:es) =
      case find e of
        Step s (SBlk is eqs e') -> Step s $ SBlk is eqs (Arr (e':es))
        StepC s (SBlk is1 eqs1 e1') (SBlk is2 eqs2 e2') -> StepC s (SBlk is1 eqs1 (Arr (e1':es))) (SBlk is2 eqs2 (Arr (e2':es)))
        None                   ->
          case findArr es of
            Step s (SBlk is eqs (Arr es')) -> Step s $ SBlk is eqs (Arr (e:es'))
            StepC s (SBlk is1 eqs1 (Arr es1')) (SBlk is2 eqs2 (Arr es2')) -> StepC s  (SBlk is1 eqs1 (Arr (e:es1')))  (SBlk is2 eqs2 (Arr (e:es2')))
            r                             -> r
        r                      -> r


promotionOK :: Blk -> Iden -> Val -> Bool
-- True if we can promote (var=val) into the heap for the parent block
promotionOK (Blk locals leqns _) x v
  =  x `elem` locals                        -- Must be bound by the /immediately enclosing/ block
                                            --   i.e. is "flexible"
  && x `notElem` dom leqns                  -- x must not have an eqn
  && occfvs v `disjoint` (x : dom leqns)    -- v must not have variables from eqns


------------------------------------
reducePrimOp :: Core.PrimOp -> Exp -> Maybe Reduction

reducePrimOp F.Add (Arr [LInt i, LInt j]) = Just $ Done "Prim+" $ LInt (i + j)
reducePrimOp F.Sub (Arr [LInt i, LInt j]) = Just $ Done "Prim-" $ LInt (i - j)
reducePrimOp F.Mul (Arr [LInt i, LInt j]) = Just $ Done "Prim*" $ LInt (i * j)
reducePrimOp F.Div (Arr [LInt i, LInt j])
  | j /= 0                                = Just $ Done "Prim/" $ LInt (i `div` j)

reducePrimOp F.Add AHNF{} = Just $ Failure "Prim+"
reducePrimOp F.Sub AHNF{} = Just $ Failure "Prim-"
reducePrimOp F.Mul AHNF{} = Just $ Failure "Prim*"
reducePrimOp F.Div AHNF{} = Just $ Failure "Prim/"

reducePrimOp F.Neg (LInt i) = Just $ Done    "Prim-neg" $ LInt (- i)
reducePrimOp F.Neg HNF{}    = Just $ Failure "Prim-neg"
reducePrimOp F.Pls (LInt i) = Just $ Done    "Prim-pls" $ LInt i
reducePrimOp F.Pls HNF{}    = Just $ Failure "Prim-pls"

reducePrimOp F.IsInt v@(LInt {}) = Just $ Done "Prim-isInt" v
reducePrimOp F.IsInt HNF{}       = Just $ Failure "Prim-isInt"

reducePrimOp F.Lt  (Arr [LInt i, LInt j]) | i<j  = Just $ Done "Prim-Lt"  $ LInt i
reducePrimOp F.LEq (Arr [LInt i, LInt j]) | i<=j = Just $ Done "Prim-LEq" $ LInt i
reducePrimOp F.GEq (Arr [LInt i, LInt j]) | i>=j = Just $ Done "Prim-GEq" $ LInt i
reducePrimOp F.Gt  (Arr [LInt i, LInt j]) | i>j  = Just $ Done "Prim-Gt"  $ LInt i
reducePrimOp F.NEq (Arr [LInt i, LInt j]) | i/=j = Just $ Done "Prim-NEq"  $ LInt i

reducePrimOp F.Lt  AHNF{} = Just $ Failure "Prim-Lt"
reducePrimOp F.LEq AHNF{} = Just $ Failure "Prim-LEq"
reducePrimOp F.GEq AHNF{} = Just $ Failure "Prim-GEq"
reducePrimOp F.Gt  AHNF{} = Just $ Failure "Prim-Gt"
reducePrimOp F.NEq AHNF{} = Just $ Failure "Prim-NEq"

reducePrimOp F.ArrCons (Arr [x, Arr xs]) = Just $ Done    "Prim-cons" $ Arr (x:xs)
reducePrimOp F.ArrCons AHNF{}            = Just $ Failure "Prim-cons"

reducePrimOp _ _ = Nothing

------------------------------------
reduceMatch ::  NameSupply -> Iden -> Term -> Reduction
-- :~> reduction
reduceMatch _fresh x tm
  | x `elem` allVarsTerm tm
  = error "unimplemented: reduceMatch, possible name clash"

reduceMatch fresh x tm
  = case tm of
        -- Hackily turn IsInt, IsAny back to a lambda:
        -- We can't do this earlier because we don't have lambda in Trm.
        TPrm F.IsInt         -> Done "int-hack" $ Var x :=: (Lam u $ Blk [] [] $
                                                             (Prm F.IsInt :@ Var u) :> Var u)
                                where u = fresh !! 0
        TPrm F.IsAny         -> Done "any-hack" $ Var x :=: (Lam u $ Blk [] [] $ Var u)
                                      where u = fresh!!0

        -- Blocks
        TBlock t             -> Step  "MBlock" $ SBlk (tbs t) [] (x :~> t)

        -- Matching
        Und                  -> Done "MWild"    $ Var x
        TVar i               -> Done "MVar"     $ Var x :=: Var i
        TLit k               -> Done "MLit"     $ Var x :=: Lit k
        TPrm o               -> Done "MPrim"    $ Var x :=: Prm o

        (t1 :@% t2)          -> Step "MApp"     $ SBlk [u1,u2] [] $ Var x :=: ((u1 :~> t1) :@ (u2 :~> t2)) where u1:u2:_ = fresh
        (t1 :=:% t2)         -> Done "MUnif"    $ (x :~> t1) :=: (x :~> t2)
        (t1 :|:% t2)         -> Done "MChoice"  $ (Blk (tbs t1) [] $ x :~> t1) :|: (Blk (tbs t2) [] $ x :~> t2)

        TFail                -> Done "Mfail"    $ Fail
        (t1 :>% t2)          -> Step "MSemi"    $ SBlk [u]   [] $ (u :~> t1) :> (x :~> t2)         where u = fresh!!0
        (t1 `Where` t2)      -> Step "MWhere"   $ SBlk [u,w] [] $ (Var w :=: (x :~> t1)) :> (u :~> t2) :> Var w  where u:w:_ = fresh
        TArr ts              -> Step "MTup"     $ SBlk xs    [] $ (Var x :=: Arr (map Var xs)) :> Arr (zipWith (:~>) xs ts)
                                where xs = take (length ts) fresh
        Rng t                -> Step "MColon"   $ SBlk [u]   [] $ (u :~> t) :@ Var x            where u = fresh!!0
        (i := t)             -> Step "MDef"     $ SBlk [i]   [] $ Var i :=: (x :~> t)
        (i :-> t)            -> Step "MArr"     $ SBlk [i]   [] $ (Var i :=: Var x) :> (x :~> t)
        (t1 :..% t2)         -> Step "MEnum"    $ SBlk [u1,u2] [] $ Var x :=: ((u1 :~> t1) :.. (u2 :~> t2)) where u1:u2:_ = fresh
        Fun at bt            -> Done "MFun"     $ (if addVerify then (Verify (prettyShow (Fun at bt)) :>) else id) $
                                                  Lam u $ Blk (p:q:tbs at) [] $ (Var p :=: (u :~> at)) :>
                                                                                 (Var q :=: (Var x :@ Var p)) :>
                                                                                 (q :~> bt)
                                  where u:p:q:_ = fresh
        If t0 t1 t2          -> Done "MIf"      $ Iter IF (Blk (u:tbs t0) [] ((u :~> t0) :> Dly (Blk (tbs t1) [] (x :~> t1))))
                                                                (Crl (Blk (tbs t2) [] (x :~> t2)))
                                        where u = fresh!!0

        For t0 t1            -> Step "MFor"     $ SBlk [y] [] $ (Arr [Var x, Var y] :=:
                                                                  Iter FOR (Blk (u:tbs t0) [] ((u :~> t0) :> Lam w (Blk [] [] $ w :~> t1)))
                                                                            (Arr [Arr [], Arr []])
                                                                ) :> Var y
                                        where y:u:w:_ = fresh
        Succ t               -> Done "MSucc"   $ Var x :=: Iter SUCC (Blk (u:tbs t) [] $ u :~> t) (Var (Core.Name "STUCK-application-failed"))
                                        where u = fresh!!0

---------------------------------------
reduceIter :: ReductionContext -> IterCtx -> Blk -> Exp -> Reduction
reduceIter _ IF (Blk [] [] (Dly b)) _
  = Done "IIf" $ Crl b

reduceIter cxt FOR (Blk xs eqs v@Val{}) e2
  = Step "IFor" $ freshen (drop 1 fresh) $ Blk (x:xs) eqs $ cons2 (Var x) (v :@ Var x) e2
  where
    fresh = rc_fresh cxt
    x = fresh !! 0

reduceIter _ SUCC (Blk xs eqs v@Val{}) _
  = Step "ISucc" $ SBlk xs eqs v

reduceIter cxt ic b1 e2
  = case reduceBlock cxt b1 of  -- find a redex in B context
      Failure s     -> Done ("IFail-" ++ s) e2
      None          -> None
      Delete xs     -> Done (show ic ++ "-GC") $ Iter ic (gcVarsBlk xs b1) e2
      Step s b      -> Done (show ic ++ "-" ++ s) $ Iter ic (mergeStep b1 b) e2
      StepC s bl br -> Done ("IChoice-" ++ s) $ Iter ic (mergeStep b1 bl) $
                                                Iter ic (mergeStep b1 br) e2

cons2 :: Exp -> Exp -> Exp -> Exp
cons2 x y xsys = Crl $ Blk [xs,ys] [] $ (Arr [Var xs, Var ys] :=: xsys) :> Arr [cons x (Var xs), cons y (Var ys)]
  where xs = Core.Name "_xs"
        ys = Core.Name "_ys"
        cons a as = Prm F.ArrCons :@ Arr [a, as]


--------------------------------------------------------------------------------
--
--             Free and bound variables
--
--------------------------------------------------------------------------------

-- Top level binders
tbs :: Term -> Set Iden
tbs Und{}  = []
tbs TLit{} = []
tbs TVar{} = []
tbs TPrm{} = []
tbs (t1 :>% t2) = tbs t1 `union` tbs t2
tbs (t1 `Where` t2) = tbs t1 `union` tbs t2
tbs (t1 :=:% t2) = tbs t1 `union` tbs t2
tbs (t1 :@% t2) = tbs t1 `union` tbs t2
tbs (t1 :..% t2) = tbs t1 `union` tbs t2
tbs TFail{} = []
tbs (_ :|:% _) = []
tbs (TArr ts) = unions $ map tbs ts
tbs Fun{} = []
tbs If{} = []
tbs For{} = []
tbs (Rng t) = tbs t
tbs (x := t) = [x] `union` tbs t
tbs (x :-> t) = [x] `union` tbs t
tbs (TBlock {}) = []
tbs (Succ _) = []

-- Variable uses, not under lambda/delay
occfvs :: Exp -> Set Iden
occfvs (Var x) = [x]
occfvs Lit{} = []
occfvs Prm{} = []
occfvs Lam{} = []
occfvs Dly{} = []
occfvs (e1 :> e2) = occfvs e1 `union` occfvs e2
occfvs (b1 :|: b2) = occfvsB b1 `union` occfvsB b2
occfvs Fail = []
occfvs (e1 :=: e2) = occfvs e1 `union` occfvs e2
occfvs (i :~> _) = [i]  -- XXX what should we do here
occfvs (e1 :@ e2) = occfvs e1 `union` occfvs e2
occfvs (e1 :.. e2) = occfvs e1 `union` occfvs e2
occfvs (Arr es) = unions (map occfvs es)
occfvs (Iter _ b1 e2) = occfvsB b1 `union` occfvs e2
occfvs (Crl b) = occfvsB b
occfvs Verify{} = []

occfvsB :: Blk -> Set Iden
occfvsB (Blk is eqs e) = foldr union (occfvs e) (map (occfvs . snd) eqs) \\ is

allVars :: Exp -> [Iden]
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
allVars Verify{} = []

allVarsBlk :: Blk -> [Iden]
allVarsBlk (Blk is eqs e) = is ++ concatMap (allVars . snd) eqs ++ allVars e

allVars' :: Exp -> Set Iden
allVars' = nub . allVars

allVarsTerm :: Term -> [Iden]
-- All variables mentioned, either as occurrences or binders
allVarsTerm (TVar i) = [i]
allVarsTerm Und = []
allVarsTerm (TLit _) = []
allVarsTerm (TPrm _) = []
allVarsTerm (i := e) = i : allVarsTerm e
allVarsTerm (e1 :>%  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 `Where`  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 :=:% e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (e1 :@%  e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (Fun e1 e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm (Rng e) = allVarsTerm e
allVarsTerm (TArr es) = concatMap allVarsTerm es
allVarsTerm (b1 :|:% b2) = allVarsTerm b1 ++ allVarsTerm b2
allVarsTerm (e1 :..% e2) = allVarsTerm e1 ++ allVarsTerm e2
allVarsTerm TFail = []
allVarsTerm (If t1 t2 t3) = allVarsTerm t1 ++ allVarsTerm t2 ++ allVarsTerm t3
allVarsTerm (For t1 t2) = allVarsTerm t1 ++ allVarsTerm t2
allVarsTerm (i :-> e) = i : allVarsTerm e
allVarsTerm (TBlock t) = allVarsTerm t
allVarsTerm (Succ t) = allVarsTerm t

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

blkSize :: Blk -> Int
blkSize (Blk is eqs e) = length is + sum (map eqnSize eqs) + expSize e

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
termSize (Fun e1 e2)      = 1 + termSize e1 + termSize e2
termSize (Rng e)          = 1 + termSize e
termSize (TArr es)        = 1 + sum (map termSize es)
termSize (b1 :|:% b2)     = 1 + termSize b1 + termSize b2
termSize (e1 :..% e2)     = 1 + termSize e1 + termSize e2
termSize TFail            = 1
termSize (If t1 t2 t3)    = 1 + termSize t1 + termSize t2 + termSize t3
termSize (For t1 t2)      = 1 + termSize t1 + termSize t2
termSize (_ :-> e)        = 1 + termSize e
termSize (TBlock t)       = 1 + termSize t
termSize (Succ t)         = 1 + termSize t

rename :: [(Iden, Iden)] -> Exp -> Exp
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
    renT (Fun t1 t2) = Fun (renT t1) (renT t2)
    renT (Rng t) = Rng (renT t)
    renT (i := t) = fromMaybe i (lookup i sub) := renT t
    renT (i :-> t) = fromMaybe i (lookup i sub) :-> renT t
    renT (TBlock t) = TBlock (renT t)
    renT (Succ t) = Succ (renT t)

renameBlk :: [(Iden,Iden)] -> Blk -> Blk
renameBlk sub b@(Blk is eqs e)
  | any (isJust . (`lookup` sub)) is
  = let Crl b' = rename (filter ((`notElem` is) . fst) sub) (Crl b)
    in b'
  | otherwise = Blk is (map (second (rename sub)) eqs)
                       (rename sub e)

substVal :: [(Iden, Val)] -> Val -> Val
substVal sub e@(Var i) = fromMaybe e $ lookup i sub
substVal _ e@Lit{} = e
substVal _ e@Prm{} = e
substVal sub (Arr vs) = Arr (map (substVal sub) vs)
substVal sub e@Lam{} | null $ map fst sub `intersect` allVars' e = e
                     | otherwise = e -- error "substVal: Lam unimplemented"
substVal _ e = error $ "substVal: not a Val: " ++ show e

-- XXX This is ugly.  Should GC locally instead
gcVars :: Set Iden -> Exp -> Exp
gcVars  _ e@Var{}   = e
gcVars  _ e@Lit{}   = e
gcVars  _ e@Prm{}   = e
gcVars xs (Lam i b) = Lam i (gcVarsBlk xs b)
gcVars  _ e@(:~>){} = e
gcVars xs (e1 :@ e2) = gcVars xs e1 :@ gcVars xs e2
gcVars xs (e1 :> e2) = gcVars xs e1 :> gcVars xs e2
gcVars xs (e1 :=: e2) = gcVars xs e1 :=: gcVars xs e2
gcVars xs (Arr es) = Arr (map (gcVars xs) es)
gcVars xs (Crl b) = Crl (gcVarsBlk xs b)
gcVars  _ e@Dly{} = e
gcVars xs (b1 :|: b2) = gcVarsBlk xs b1 :|: gcVarsBlk xs b2
gcVars xs (e1 :.. e2) = gcVars xs e1 :.. gcVars xs e2
gcVars  _ e@Fail   = e
gcVars xs (Iter ic b1 e2) = Iter ic (gcVarsBlk xs b1) (gcVars xs e2)
gcVars _ e@Verify{} = e

gcVarsBlk :: Set Iden -> Blk -> Blk
gcVarsBlk xs (Blk is eqs expr) = Blk (is \\ xs) (filter ((`notElem` xs) . fst) eqs) $ gcVars xs expr


--------------------------------------------------------------------------------
--
--             Fresh names
--
--------------------------------------------------------------------------------

type NameSupply = [Iden]  -- An infinite list of fresh names

freshVars :: Exp -> [Iden]
freshVars e = idenSupply \\ allVars e

idenSupply :: [Iden]
idenSupply = [Core.Name $ "u" ++ show i | i <- [1::Int ..]]

freshVarsBlk :: Blk -> [Iden]
freshVarsBlk b = idenSupply \\ allVarsBlk b

freshVarsTerm :: Term -> [Iden]
freshVarsTerm t = freshVars (Core.Name "" :~> t)

--freshVar :: Exp -> Iden
--freshVar = (!!0) . freshVars

freshen :: [Iden] -> Blk -> SBlk
freshen fresh _b@(Blk is eqs expr) =
--  trace ("freshen " ++ show sub ++ "\n" ++ show _b ++ "\n" ++ show res)
  res
  where res = SBlk vs (map renEqn eqs) (rename sub expr)
        sub = zip is fresh
        vs = map snd sub
        renEqn (i, e) = (fromMaybe i (lookup i sub), rename sub e)

