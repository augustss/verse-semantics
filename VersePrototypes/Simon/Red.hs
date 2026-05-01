{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns #-}
     {- -Wno-missing-methods -Wno-incomplete-uni-patterns -Wno-unused-matches
        -Wno-missing-pattern-synonym-signatures -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Red(run) where

import Prelude hiding ((<>))
import Control.Arrow(second)
import Data.List(intersect, nub, (\\), union, group, sort)
import Data.Maybe
import qualified FrontEnd.Expr as F
import Epic.Print
import Debug.Trace

{-
  Potential problems:
  :sim f:=fun(x:int){x+1}; ((f[y]; y:= -2) | 5)

-}


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
type Iden = F.Ident
type Op   = F.PrimOp

data Term
  = Und               -- _
  | TVar Iden         -- x
  | TInt Integer      -- k
  | TPrm Op           -- op
  | Term :>%  Term    -- t1; t2
  | Term `Where` Term -- t1 where t2
  | Term :=:% Term    -- t1 = t2
  | Term :@%  Term    -- t1[t2]
  | Term :..% Term    -- t1 .. t2
  | TFail             -- fail
  | Term :|:% Term    -- t1 | t2
  | TArr [Term]       -- array{t1,...,t2}
  | Fun Term Term     -- fun(t1){t2}
  | If Term Term Term -- if (t1){t2}{t3}
  | For Term Term     -- for (t1){t2}
  | Rng Term          -- :t
  | Iden := Term      -- x := t
  | Iden :-> Term     -- x ??? t
  deriving (Eq, Show)

data Exp
  -- Values
  = Var Iden              -- x
  -- HNF
  | Int Integer           -- k
  | Prm Op                -- op
  -- Arr below
  | Lam Iden Blk          -- \ x . e
  | Dly Blk               -- delay{b}
  -- Non-HNF
  | Exp :>  Exp           -- e1; e2
  | Blk :|: Blk           -- e1 | e2
  | Fail                  -- fail
  | Exp :=: Exp           -- e1 = e2
  | Iden :~> Term         -- e ~> t
  | Exp :@  Exp           -- e1[e2]
  | Exp :.. Exp           -- e1 .. e2
  | Arr [Exp]             -- array{e1,...,e2}
  | Iter IterCtx Blk Exp  -- if/for
  | Crl Blk               -- {...}
    deriving (Eq, Show)

type Eqn = (Iden, Val)

type Set a = [a]   -- Represented as a list, but order is immaterial

-- The equation RHSs have no variables from the LHSs
-- A block (Blk xs eqs e) satisfies these invariants:
--    (A)  dom(eqs)    `subset`   X
--    (B)  occfvs(eqs) `disjoint` X

data Blk = Blk (Set Iden) (Set Eqn) Exp
  deriving (Eq, Show)

type Val = Exp

data IterCtx = IF | FOR
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--
--             Pretty-printing
--
--------------------------------------------------------------------------------

newtype PExp = P Exp
instance Show PExp where
  show (P e) = prettyShow e

instance Pretty Term where
  pPrintPrec l p (TVar i) = pPrintPrec l p i
  pPrintPrec l p (TInt i) = pPrintPrec l p i
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
  pPrintPrec l _ (For t1 t2)   = text "if" <> parens (pPrintL l t1) <> braces (pPrintL l t2)
  pPrintPrec l p (x :-> e)     = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":->" <+> pPrintPrec l 2 e

instance Pretty Exp where
  pPrintPrec l p (Var i)     = pPrintPrec l p i
  pPrintPrec l p (Int i)     = pPrintPrec l p i
  pPrintPrec l p (Prm o)     = pPrintPrec l p o
  pPrintPrec l p (Lam i b)   = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 i <> text "." <> pPrintPrec l 0 b
  pPrintPrec l p (x :~> e)   = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>" <+> pPrintPrec l 1 e
  pPrintPrec l p (e1 :@ e2)  = maybeParens (p > 10) $ pPrintPrec l 10 e1 <> text "[" <> pPrintPrec l 0 e2 <> text "]"
  pPrintPrec l p (e1 :> e2)  = maybeParens (p > 0) $ pPrintPrec l 1 e1 <> text ";" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 :=: e2) = maybeParens (p > 5) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2

  pPrintPrec l _ (Arr es)
    | l == prettyNormal    = text "<" <> hsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (Arr [e]) = text "array" <> braces (pPrintL l e)
  pPrintPrec l _ (Arr es)  = parens $ hsep $ punctuate (text ",") $ map (pPrintPrec l 0) es

  pPrintPrec l _ (Crl b)     = braces $ pPrintPrec l 0 b
  pPrintPrec l _ (Dly b)     = text "delay" <> braces (pPrintL l b)
  pPrintPrec l p (b1 :|: b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec l p (e1 :.. e2) = maybeParens (p > 7) $ pPrintPrec l 8 e1 <> text ".." <> pPrintPrec l 8 e2
  pPrintPrec _ _ Fail        = text "fail"

  pPrintPrec l _ (Iter ic b1 b2)
    = text "iter" <> parens (text (show ic)) <> braces (pPrintL l b1) <> braces (pPrintL l b2)

instance Pretty Blk where
  pPrintPrec l p (Blk [] [] e) = pPrintPrec l p e
  pPrintPrec l p (Blk vs eqns e)
    = maybeParens (p > 0) $ text "∃" <+>
      vcat ([hsep (map (pPrintPrec l 10) vs) <> text "."]
            ++ (punctuate (text "") (map (\ (i, d) -> pPrintPrec l 0 i <+> text "<-" <+> pPrintPrec l 0 d) eqns))
            ++ [pPrintPrec l 0 e])

--------------------------------------------------------------------------------
--
--             Pattern synonyms that carve out Exp subsets
--
--------------------------------------------------------------------------------

-- A HNF, i.e., a value, but not a variable
pattern HNF :: Exp -> Val
pattern HNF e <- (getHNF -> Just e)

getHNF :: Exp -> Maybe Exp
getHNF Var{} = Nothing
getHNF e = getVal e

-- Either an Arr with all HNF, or a non-arr HNF
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
getVal e@Int{} = Just e
getVal e@Prm{} = Just e
getVal e@Lam{} = Just e
getVal e@(Arr es) | Just _ <- mapM getVal es = Just e
getVal e@Dly{} = Just e
getVal _ = Nothing

pattern Con :: Exp -> Val
pattern Con e <- (getCon -> Just e)

-- Constants
getCon :: Exp -> Maybe Exp
getCon e@Int{} = Just e
getCon e@(Arr es) | Just _ <- mapM getCon es = Just e
getCon _ = Nothing

--------------------------------------------------------------------------------
--
--             Convert SrcEssential to Term
--
--------------------------------------------------------------------------------

srcToTerm :: F.SrcEssential -> Term
srcToTerm (F.Variable i) | F.isSrcUnderscore i = Und
                         | otherwise = TVar i
srcToTerm (F.EPrim o) = TPrm o
srcToTerm (F.Lit (F.LInt k)) = TInt k
srcToTerm (F.DefineE i e) = i := srcToTerm e
srcToTerm (F.Choice e1 e2) = srcToTerm e1 :|:% srcToTerm e2
srcToTerm (F.Unify e1 e2) = srcToTerm e1 :=:% srcToTerm e2
srcToTerm (F.Seq e1 e2) = srcToTerm e1 :>% srcToTerm e2
srcToTerm (F.Where e1 e2) = srcToTerm e1 `Where` srcToTerm e2
srcToTerm (F.ApplyD (F.EPrim F.DotDot) (F.Array [e1, e2])) = srcToTerm e1 :..% srcToTerm e2
srcToTerm (F.ApplyD e1 e2) = srcToTerm e1 :@% srcToTerm e2
srcToTerm (F.Range e) = Rng (srcToTerm e)
srcToTerm (F.Array es) = TArr (map srcToTerm es)
srcToTerm (F.Fail) = TFail
srcToTerm (F.Function _ e1 _ e2) = Fun (srcToTerm e1) (srcToTerm e2)
srcToTerm (F.DefineV x) = x := Rng (TPrm F.IsAny)
srcToTerm (F.DefineIE i e) = i :-> srcToTerm e
srcToTerm (F.If3 e1 e2 e3) = If (srcToTerm e1) (srcToTerm e2) (srcToTerm e3)
srcToTerm (F.For2 e1 e2) = For (srcToTerm e1) (srcToTerm e2)
srcToTerm e = error $ "srcToTerm: unimplemented " ++ show e

--------------------------------------------------------------------------------
--
--             Reductions
--
--------------------------------------------------------------------------------

type RuleName = String

data Reduction
  = None               -- No redex found
  | Failure String     -- Evaluation failed
  | Delete (Set Iden)  -- Delete the identifiers from the block existential and equations
  | Step RuleName [Iden] [Eqn] Exp  -- The named rule fired, returning this Blk
  deriving (Eq, Show)

pattern Done :: String -> Exp -> Reduction
pattern Done s e = Step s [] [] e

instance Pretty Reduction where
  pPrintPrec l p (Delete is) = text "GC" <+> pPrintPrec l p is
  pPrintPrec _ _ None               = text "None"
  pPrintPrec _ _ (Failure s)        = text "Failure" <+> text (show s)
  pPrintPrec l _ (Done s e)         = text "Done" <+> text (show s) <+> pPrintPrec l 0 e
  pPrintPrec l _ (Step s xs eqns e) = text "Step" <+> text (show s)
                                      <+> pPrintPrec l 0 (Blk xs eqns e)

--------------------------------------------------------------------------------
--
--             The evaluator
--
--------------------------------------------------------------------------------

run :: F.SrcEssential -> PExp
run src = P $ evalBlk 1000000 (Blk (u:tbs t) [] (u :~> t))
  where u = freshVarsTerm t !! 0
        t = srcToTerm src

evalBlk :: Int -> Blk -> Exp
evalBlk _ b | trace (prettyShow b ++ "\n") False = undefined
evalBlk 0 b = error $ "No fuel: " ++ prettyShow b
evalBlk fuel b@(Blk is eqs expr) =
    case findTopRedex (freshVarsBlk b) b of
      -- XXX iterate substVal?
      Step _ xs eqns e -> evalBlk (fuel-1) $
                          (Blk (is `union` xs) (map (second $ substVal eqs) eqns ++
                                                map (second $ substVal eqns) eqs) e)
      None | null is    -> expr
           | otherwise  -> Crl (Blk is eqs expr)
      Failure _         -> Fail
      Delete xs         -> evalBlk (fuel-1) (gcVarsBlk xs b)



dom :: Set Eqn -> Set Iden
dom = map fst

--fv :: Exp -> Set Iden
--fv = allFreeVars'

noIntersect :: Eq a => Set a -> Set a -> Bool
noIntersect xs ys = null $ xs `intersect` ys

findTopRedex :: [Iden] -> Blk -> Reduction
findTopRedex fresh blk@(Blk locals eqns ex) = findRedex fresh singleOcc [] blk
  where
    -- Subset of `locals` that occur exactly once, and have no Eqn
    -- To support EXI-APP
    -- ToDo: what about occurrences in `eqns` under a lambda?
    singleOcc :: Set Iden
    singleOcc = [ x | [x] <- group (sort (allVars ex))
                    , x `elem` locals
                    , isNothing (lookup x eqns) ]

findRedex :: [Iden] -> Set Iden -> Set Eqn -> Blk -> Reduction
findRedex fresh singleOcc geqns parent@(Blk locals leqns ex) =
  trace (render (text "findRedex" <+> (pPrintL prettyNormal ex $$ pPrintL prettyNormal res)) ++ "\n")
          res
  where
    res | not (null dead_vars) = Delete dead_vars  -- GC rules
        | otherwise            = find ex

    dead_vars :: [Iden]  -- Subset of locals that are unused
    dead_vars = locals \\ (allVars' ex ++ concatMap (allVars' . snd) leqns)

    eqns = leqns ++ geqns

    findB :: Set Eqn -> Blk -> Reduction
    findB = findRedex fresh singleOcc

    find :: Exp -> Reduction
    find expr =
      case expr of
        -- Scope and substitution
        Var x  :=: Val v  | promotionOK parent x v
                          -> Step "Promote1" [] [(x, v)] v
        Val v  :=: Var x  | promotionOK parent x v
                          -> Step "Promote2" [] [(x, v)] v

        Var i             | Just v <- lookup i eqns -> Done ("Subst " ++ show i) v
        Crl b@Blk{}       -> Step "FloatB" is' eqs' e' where Blk is' eqs' e' = freshen fresh b
        -- GC rules handled above

        -- Primops
        Prm F.Add :@ v   | Arr [Int i, Int j] <- v         -> Done    "Prim+"    $ Int (i + j)
                         | AHNF{}             <- v         -> Failure "Prim+"
        Prm F.Sub :@ v   | Arr [Int i, Int j] <- v         -> Done    "Prim-"    $ Int (i - j)
                         | AHNF{}             <- v         -> Failure "Prim-"
        Prm F.Mul :@ v   | Arr [Int i, Int j] <- v         -> Done    "Prim*"    $ Int (i * j)
                         | AHNF{}             <- v         -> Failure "Prim*"
        Prm F.Div :@ v   | Arr [Int i, Int j] <- v, j /= 0 -> Done    "Prim/"    $ Int (i `div` j)
                         | AHNF{}             <- v         -> Failure "Prim/"

        Prm F.Neg :@ v   | Int i <- v                      -> Done    "Prim-neg" $ Int (- i)
                         | HNF{} <- v                      -> Failure "Prim-neg"
        Prm F.IsInt :@ v | Int _ <- v                      -> Done    "Prim-isInt" v
                         | HNF{} <- v                      -> Failure "Prim-isInt"

        Prm F.Lt :@ v    | Arr [Int i, Int j] <- v, i < j  -> Done    "Prim-Lt"  $ Int i
                         | AHNF{}             <- v         -> Failure "Prim-Lt"
        Prm F.Gt :@ v    | Arr [Int i, Int j] <- v, i > j  -> Done    "Prim-Gt"  $ Int i
                         | AHNF{}             <- v         -> Failure "Prim-Gt"

        -- Unification
        Val v1 :=: Val v2 | v1 == v2 -> Done "EqVal" v1
        Con v1 :=: Con v2 | v1 /= v2 -> Failure "EqFail"
--        HNF v  :=: Var i             -> Done "SWAP" $ Var i :=: v
-- Let this be stuck instead        Var i  :=: HNF v  | i `elem` allFreeVars' v -> Failure "OCCUR"  -- occurs check
        Val (Arr vs) :=: Arr es | length vs /= length es -> Failure "arr /="
                                | otherwise ->
--                                  Done "EqTup" $ foldr (:>) (Arr vs) (zipWith (:=:) vs es)
                                  Done "EqTup" $ Arr (zipWith (:=:) vs es)

        -- Sequencing
        Val{}  :>  e2    -> Done "Seq1" e2


        -- Unification, structural
        Val v  :=: (e1 :>  e2) -> Done "Norm1" $ e1 :> (v :=: e2)
        Val v  :=: (e1 :=: e2) -> Done "Norm2" $ (v :=: e1) :> (v :=: e2)
        (e1 :> e2) :=: e3      -> Done "Norm3" $ e1 :> (e2 :=: e3)
        (Val v :=: e1) :=: e2  -> Done "Norm4" $ (v :=: e1) :> (v :=: e2)

        -- Beta
        Lam x (Blk vs eqs e) :@ Val v
          -> Step "Beta" is' eqs' e'
          where Blk is' eqs' e' = freshen fresh (Blk (x:vs) ((x, v):eqs) e)

        Arr es :@ Val v -> Done "ITUP" $
                           foldr alt Fail (zipWith (\ i e -> (v :=: Int i) :> e) [0..] es)
          where alt e1 e2 = Blk [] [] e1 :|: Blk [] [] e2

        Var f   :@ Val _ | f `elem` singleOcc -> Step "ExiApp" [u] [] (Var u) where u = fresh!!0

        Fail             -> Failure "FAIL"

        Arr es           -> findArr es

        -- Catch-all cases for context C; just walk downwards
        e1 :>  e2  -> find2  (:>)  e1 e2
        e1 :=: e2  -> find2  (:=:) e1 e2
        e1  :@  e2 -> find2  (:@)  e1 e2
        b1 :|: b2  -> find2B (:|:) b1 b2
        Lam x b    -> find1B (Lam x) b



        Iter ic b1 e2    ->
          case findRedex fresh singleOcc eqns b1 of
            Failure _s -> Done ("IFail-" ++ _s) e2
            None       -> None
            Delete xs  -> Done "GC" $ Iter ic (gcVarsBlk xs b1) e2

            Step _s is eqs (c1 :|: c2) -> Done ("IChoice-" ++ _s) $
                                          Iter ic (Blk is eqs $ Crl c1)
                                                  (Iter ic (Blk is eqs $ Crl c2) e2)
            Step _s [] _ (Dly b) | ic == IF -> Done ("IIf-"++ _s) $ Crl b
            Step s  is eqs e -> Done s $ Iter ic (Blk is eqs e) e2

        -- Reduction under lambda
        -- XXX WRONG, needs a Blk boundary inside the Lam
        -- Lam v e          -> find1 (Lam v) e

        -- :~> reduction
        -- Hackily turn IsInt, IsAny back to a lambda:
        -- We can't do this earlier because we don't have lambda in Trm.
        x :~> TPrm F.IsInt         -> Done "int-hack" $ Var x :=: (Lam u $ Blk [] [] $ (Prm F.IsInt :@ Var u) :> Var u)
                                      where u = fresh!!0
        x :~> TPrm F.IsAny         -> Done "any-hack" $ Var x :=: (Lam u $ Blk [] [] $ Var u)
                                      where u = fresh!!0
        -- Matching
        x :~> Und                  -> Done "MWild"    $ Var x
        x :~> TVar i               -> Done "MVar"     $ Var x :=: Var i
        x :~> TInt k               -> Done "MInt"     $ Var x :=: Int k
        x :~> TPrm o               -> Done "MPrim"    $ Var x :=: Prm o
{-
        x :~> ea@(Val _ :@ Val _)  -> Done "MApp-v-v" $ Var x :=: ea
        x :~> (Val e1 :@ e2)       -> Done "MApp-v-e" $ x :~> ((u := e2) :> (e1 :@ Var u))    where u = fresh!!0
        x :~> (e1 :@ Val e2)       -> Done "MApp-e-v" $ x :~> ((u := e1) :> (Var u :@ e2))    where u = fresh!!0
-}
        x :~> (t1 :@% t2)          -> Step "MApp" [u1,u2] [] $ Var x :=: ((u1 :~> t1) :@ (u2 :~> t2)) where u1:u2:_ = fresh
        x :~> (t1 :=:% t2)         -> Done "MUnif"    $ (x :~> t1) :=: (x :~> t2)
        x :~> (t1 :|:% t2)         -> Done "MChoice"  $ (Blk (tbs t1) [] $ x :~> t1) :|: (Blk (tbs t2) [] $ x :~> t2)
        -- SLPJ what if x is one of the binders in t1/t2?

        _ :~> TFail                -> Done "Mfail"    $ Fail
        x :~> (t1 :>% t2)          -> Step "MSemi"  [u]   [] $ (u :~> t1) :> (x :~> t2)         where u = fresh!!0
        x :~> (t1 `Where` t2)      -> Step "MWhere" [u,w] [] $ (Var w :=: (x :~> t1)) :> (u :~> t2) :> Var w  where u:w:_ = fresh
        x :~> TArr ts              -> Step "MTup"   xs    [] $ (Var x :=: Arr (map Var xs)) :> Arr (zipWith (:~>) xs ts)
                                      where xs = take (length ts) fresh
        x :~> Rng t                -> Step "MColon" [u]   [] $ (u :~> t) :@ Var x            where u = fresh!!0
        x :~> (i := t)             -> Step "MDef"   [i]   [] $ Var i :=: (x :~> t)
        x :~> (i :-> t)            -> Step "MArr"   [i]   [] $ (Var i :=: Var x) :> (x :~> t)
{-
        x :~> ea@(Val{} :.. Val{}) -> Done "MEnum-v-v" $ Var x :=: ea
        x :~> (Val e1 :.. e2)      -> Done "MEnum-v-e" $ x :~> ((u := e1) :> (Var u :.. e2))    where u = fresh!!0
        x :~> (e1 :.. Val e2)      -> Done "MEnum-e-v" $ x :~> ((u := e2) :> (e1 :.. Var u))    where u = fresh!!0
-}
        x :~> (t1 :..% t2)         -> Done "MEnum"    $ Var x :=: ((u1 :~> t1) :.. (u2 :~> t2)) where u1:u2:_ = fresh
        f :~> Fun at bt            -> Done "MFun"     $ Lam u $ Blk (x:y:tbs at) [] $ (Var x :=: (u :~> at)) :>
                                                                                      (Var y :=: (Var f :@ Var x)) :>
                                                                                      (y :~> bt)
                                        where u:x:y:_ = fresh
        x :~> If t0 t1 t2          -> Done "MIf"      $ Iter IF (Blk (u:tbs t0) [] ((u :~> t0) :> Dly (Blk (tbs t1) [] (x :~> t1))))
                                                                (Crl (Blk (tbs t2) [] (x :~> t2)))
                                        where u = fresh!!0
        _ -> None

    find1 :: (Exp -> Exp) -> Exp -> Reduction
    find1 c e =
      case find e of
        Step s is eqs e'  -> Step s is eqs (c e')
        r                 -> r

    find2 :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Reduction
    find2 c e1 e2 =
      case find e1 of
        Step s is eqs e1' -> Step s is eqs (e1' `c` e2)
        None              -> find1 (c e1) e2
        r                 -> r

    findArr :: [Exp] -> Reduction
    findArr [] = None
    findArr (e:es) =
      case find e of
        Step s is eqs e' -> Step s is eqs (Arr (e':es))
        None              ->
          case findArr es of
            Step s is eqs (Arr es') -> Step s is eqs (Arr (e:es'))
            r                       -> r
        r                 -> r

    find1B :: (Blk -> Exp) -> Blk -> Reduction
    find1B c b@(Blk is eqs _) =
      case findB eqns b of
        Step s is' eqs' e' -> Done s (c (Blk (is' ++ is) (eqs' ++ eqs) e'))
        r                  -> r

    find2B :: (Blk -> Blk -> Exp) -> Blk -> Blk -> Reduction
    find2B c b1@(Blk is eqs _) b2 =
      case findB eqns b1 of
        Step s is' eqs' e' -> Done s (Blk (is' ++ is) (eqs' ++ eqs) e' `c` b2)
        None               -> find1B (c b1) b2
        r                  -> r


promotionOK :: Blk -> Iden -> Val -> Bool
-- True if we can promote (var=val) into the heap for the parent block
promotionOK (Blk locals leqns _) x v
  =  x `elem` locals                        -- must be a local variable
  && x `notElem` dom leqns                  -- x must not have an eqn
  && occfvs v `noIntersect` (x : dom leqns) -- v must not have variables from eqns

-- Top level binders
tbs :: Term -> Set Iden
tbs Und{}  = []
tbs TInt{} = []
tbs TVar{} = []
tbs TPrm{} = []
tbs (t1 :>% t2) = tbs t1 `union` tbs t2
tbs (t1 `Where` t2) = tbs t1 `union` tbs t2
tbs (t1 :=:% t2) = tbs t1 `union` tbs t2
tbs (t1 :@% t2) = tbs t1 `union` tbs t2
tbs (t1 :..% t2) = tbs t1 `union` tbs t2
tbs TFail{} = []
tbs (_ :|:% _) = []
tbs (TArr ts) = foldr union [] $ map tbs ts
tbs Fun{} = []
tbs If{} = []
tbs For{} = []
tbs (Rng t) = tbs t
tbs (x := t) = [x] `union` tbs t
tbs (x :-> t) = [x] `union` tbs t

-- Variable uses, not under lambda/delay
occfvs :: Exp -> Set Iden
occfvs (Var x) = [x]
occfvs Int{} = []
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
occfvs (Arr es) = foldr union [] (map occfvs es)
occfvs (Iter _ b1 e2) = occfvsB b1 `union` occfvs e2
occfvs (Crl b) = occfvsB b

occfvsB :: Blk -> Set Iden
occfvsB (Blk is eqs e) = foldr union (occfvs e) (map (occfvs . snd) eqs) \\ is

allVars :: Exp -> [Iden]
allVars (Var i) = [i]
allVars (Int _) = []
allVars (Prm _) = []
allVars (Lam i e) = i : allVarsBlk e
allVars (e1 :>  e2) = allVars e1 ++ allVars e2
allVars (e1 :=: e2) = allVars e1 ++ allVars e2
allVars (i  :~> e ) = i : allVarsTerm e
allVars (e1 :@  e2) = allVars e1 ++ allVars e2
allVars (Arr es) = concatMap allVars es
allVars (b1 :|: b2) = allVarsBlk b1 ++ allVarsBlk b2
allVars (e1 :.. e2) = allVars e1 ++ allVars e2
allVars Fail = []
allVars (Dly b) = allVarsBlk b
allVars (Crl b) = allVarsBlk b
allVars (Iter _ b1 e2) = allVarsBlk b1 ++ allVars e2

allVarsBlk :: Blk -> [Iden]
allVarsBlk (Blk is eqs e) = is ++ concatMap (allVars . snd) eqs ++ allVars e

allVars' :: Exp -> Set Iden
allVars' = nub . allVars

allVarsTerm :: Term -> [Iden]
allVarsTerm (TVar i) = [i]
allVarsTerm Und = []
allVarsTerm (TInt _) = []
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

{-
allUsedVars :: Exp -> [Iden]
allUsedVars (Var i) = [i]
allUsedVars Und = []
allUsedVars (Int _) = []
allUsedVars (Prm _) = []
allUsedVars (_ := e) = allUsedVars e
allUsedVars (e1 :>  e2) = allUsedVars e1 ++ allUsedVars e2
allUsedVars (e1 `Where`  e2) = allUsedVars e1 ++ allUsedVars e2
allUsedVars (e1 :=: e2) = allUsedVars e1 ++ allUsedVars e2
allUsedVars (i  :~> e ) = i : allUsedVars e
allUsedVars (e1 :@  e2) = allUsedVars e1 ++ allUsedVars e2
allUsedVars (Fun e1 e2) = allUsedVarsBlk e1 ++ allUsedVarsBlk e2 -- e1 is a "pattern"
allUsedVars (Rng e) = allUsedVars e
allUsedVars (Arr es) = concatMap allUsedVars es
allUsedVars (Crl b) = allUsedVarsBlk b
allUsedVars (b1 :|: b2) = allUsedVarsBlk b1 ++ allUsedVarsBlk b2
allUsedVars (e1 :.. e2) = allUsedVars e1 ++ allUsedVars e2
allUsedVars Fail = []

allUsedVarsBlk :: Blk -> [Iden]
allUsedVarsBlk (Blk is eqs e) = filter (`notElem` is) (concatMap (allUsedVars . snd) eqs ++ allUsedVars e)

allUsedVars' :: Exp -> Set Iden
allUsedVars' = nub . allUsedVars

allBoundVars :: Exp -> [Iden]
allBoundVars (Var _) = []
allBoundVars Und = []
allBoundVars (Int _) = []
allBoundVars (Prm _) = []
allBoundVars (i := e) = i : allBoundVars e
allBoundVars (e1 :>  e2) = allBoundVars e1 ++ allBoundVars e2
allBoundVars (e1 `Where`  e2) = allBoundVars e1 ++ allBoundVars e2
allBoundVars (e1 :=: e2) = allBoundVars e1 ++ allBoundVars e2
allBoundVars (i  :~> e ) = allBoundVars e
allBoundVars (e1 :@  e2) = allBoundVars e1 ++ allBoundVars e2
allBoundVars (Fun e1 e2) = allBoundVarsBlk e1 ++ allBoundVarsBlk e2 -- e1 is a "pattern"
allBoundVars (Rng e) = allBoundVars e
allBoundVars (Arr es) = concatMap allBoundVars es
allBoundVars (Crl b) = allBoundVarsBlk b
allBoundVars (b1 :|: b2) = allBoundVarsBlk b1 ++ allBoundVarsBlk b2
allBoundVars (e1 :.. e2) = allBoundVars e1 ++ allBoundVars e2
allBoundVars Fail = []

allBoundVarsBlk :: Blk -> [Iden]
allBoundVarsBlk (Blk is _ e) = is ++ allBoundVars e

allBoundVars' :: Exp -> Set Iden
allBoundVars' = nub . allUsedVars
-}
{-
subst :: Iden -> Val -> Exp -> Exp
subst x v = sub
  where
    sub e@(Var i) | i == x = v
                  | otherwise = e
    sub e@Und = e
    sub e@(Int _) = e
    sub e@(Prm _) = e
    sub e@(Var i :=: r) | x /= i || v /= r = Var i :=: sub r
                        | otherwise = e
    sub (i := e) | i == x = error "subst clash 1"
                 | otherwise = i := sub e
    sub (e1 :> e2) = sub e1 :> sub e2
    sub (e1 `Where` e2) = sub e1 `Where` sub e2
    sub (e1 :=: e2) = sub e1 :=: sub e2
    sub (i :~> e2) | i == x = error "subst"
                   | otherwise = i :~> e2
    sub (e1 :@ e2) = sub e1 :@ sub e2
    sub (Fun e1 e2) = Fun e1 (subB e2)
    sub (Rng e) = Rng (sub e)
    sub (Arr es) = Arr (map sub es)
    sub (Crl b) = Crl (subB b)
    sub (b1 :|: b2) = subB b1 :|: subB b2
    sub (e1 :.. e2) = sub e1 :.. sub e2
    sub e@Fail = e
    subB (Blk is eqs e) | x `elem` is = error "subst clash 2"
                        | otherwise = Blk is (map (second sub) eqs) (sub e)   -- XXX requires unique vars
-}

rename :: [(Iden, Iden)] -> Exp -> Exp
rename sub = ren
  where
    ren :: Exp -> Exp
    ren e@(Var i) | Just j <- lookup i sub = Var j
                  | otherwise = e
    ren e@(Int _) = e
    ren e@(Prm _) = e
    ren (Lam i e) | isJust (lookup i sub) = error "rename: clash 3"
                  | otherwise = Lam i (renB e)
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
    renB :: Blk -> Blk
    renB (Blk is eqs e) | any (isJust . (`lookup` sub)) is = error "rename clash 2"
                        | otherwise = Blk is (map (second ren) eqs) (ren e)

    renT e@(TVar i) | Just j <- lookup i sub = TVar j
                    | otherwise = e
    renT e@(TInt _) = e
    renT e@(TPrm _) = e
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

{-
delete :: [Iden] -> Exp -> Exp
delete [] = id
delete xs = del
  where
    del e@(Var _) = e
    del e@Und = e
    del e@(Int _) = e
    del e@(Prm _) = e
    del (i := e) | i `elem` xs = error "delete: clash 1"
                 | otherwise = i := del e
    del (Var i :=: e) | i `elem` xs = e
    del (e1 :> e2) = del e1 :> del e2
    del (e1 `Where` e2) = del e1 `Where` del e2
    del (e1 :=: e2) = del e1 :=: del e2
    del (i :~> e) = i :~> del e
    del (e1 :@ e2) = del e1 :@ del e2
    del (Fun e1 e2) = Fun (delB e1) (delB e2)
    del (Rng e) = Rng (del e)
    del (Arr es) = Arr (map del es)
    del (Crl b) = Crl (delB b)
    del (b1 :|: b2) = delB b1 :|: delB b2
    del (e1 :.. e2) = del e1 :.. del e2
    del e@Fail = e
    delB (Blk is eqs e) | not (null (is `intersect` xs)) = error "delete clash 2"
                        | otherwise = Blk is (map (second del) eqs) (del e)
-}

freshVars :: Exp -> [Iden]
freshVars e = idenSupply \\ allVars e

idenSupply :: [Iden]
idenSupply = [F.Ident F.noLoc $ "u" ++ show i | i <- [1::Int ..]]

freshVarsBlk :: Blk -> [Iden]
freshVarsBlk b = idenSupply \\ allVarsBlk b

freshVarsTerm :: Term -> [Iden]
freshVarsTerm t = freshVars (F.Ident F.noLoc "" :~> t)

--freshVar :: Exp -> Iden
--freshVar = (!!0) . freshVars

freshen :: [Iden] -> Blk -> Blk
freshen fresh _b@(Blk is eqs expr) =
--  trace ("freshen " ++ show sub ++ "\n" ++ show _b ++ "\n" ++ show res)
  res
  where res = Blk vs (map renEqn eqs) (rename sub expr)
        sub = zip is fresh
        vs = map snd sub
        renEqn (i, e) = (fromMaybe i (lookup i sub), rename sub e)

substVal :: [(Iden, Val)] -> Val -> Val
substVal sub e@(Var i) = fromMaybe e $ lookup i sub
substVal _ e@Int{} = e
substVal _ e@Prm{} = e
substVal sub (Arr vs) = Arr (map (substVal sub) vs)
substVal sub e@Lam{} | null $ map fst sub `intersect` allVars' e = e
                     | otherwise = e -- error "substVal: Lam unimplemented"
substVal _ e = error $ "substVal: not a Val: " ++ show e

-- XXX This is ugly.  Should GC locally instead
gcVars :: Set Iden -> Exp -> Exp
gcVars  _ e@Var{}   = e
gcVars  _ e@Int{}   = e
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

gcVarsBlk :: Set Iden -> Blk -> Blk
gcVarsBlk xs (Blk is eqs expr) = Blk (is \\ xs) (filter ((`notElem` xs) . fst) eqs) $ gcVars xs expr
