{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns #-} {- -Wno-missing-methods -Wno-incomplete-uni-patterns -Wno-unused-matches -Wno-missing-pattern-synonym-signatures -}
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

type Iden = F.Ident
type Op = F.PrimOp

infixr 0 :>
infix  2 :=
infixr 4 :|:
infixr 5 :=:

data Exp
  -- variables
  = Var Iden         -- x
  -- irreducible
  | Int Integer      -- k
  | Prm Op           -- op
  | Lam Iden Blk     -- \ x . e

  -- special
  | Iden :~> Exp     -- e1 ~> e2

  -- removed by ~>
  | Iden := Exp      -- x := e
  | Und              -- _  
  | Exp `Where`  Exp -- e1 where e2

  -- reducible in evaluation context
  | Exp :@  Exp      -- e1[e2]
  | Rng Exp          -- :e
  | Fun Exp Exp      -- fun(e1){e2}

  -- need special reductions
  | Exp :>  Exp      -- e1; e2
  | Exp :=: Exp      -- e1 = e2

  | Arr [Exp]        -- array{e1,...,e2}
  | Crl Blk          -- block{...}
  | Dly Blk          -- delay{b}

  | Blk :|: Blk      -- e1 | e2
  | Exp :.. Exp      -- e1 .. e2
  | Fail             -- fail
 -- Iter             -- if/for
  deriving (Eq, Show)

type Eqn = (Iden, Val)

-- The equation RHSs have no variables from the LHSs
data Blk = Blk (Set Iden) (Set Eqn) Exp
  deriving (Eq, Show)

type Val = Exp

instance Pretty Exp where
  pPrintPrec l p (Var i) = pPrintPrec l p i
  pPrintPrec l p (Int i) = pPrintPrec l p i
  pPrintPrec l p (Prm o) = pPrintPrec l p o
  pPrintPrec _ _ Und = text "_"
  pPrintPrec l p (Rng e) = maybeParens (p > 10) $ text ":" <> pPrintPrec l 11 e
  pPrintPrec l _ (Fun e1 e2) = text "fun" <> parens (pPrintPrec l 0 e1) <> braces (pPrintPrec l 0 e2)
  pPrintPrec l p (Lam i b) = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 i <> text "." <> pPrintPrec l 0 b
  pPrintPrec l p (x :~> e) = maybeParens (p > 1) $ pPrintPrec l 1 x <+> text "~>" <+> pPrintPrec l 1 e
  pPrintPrec l p (x := e) = maybeParens (p > 2) $ pPrintPrec l 2 x <+> text ":=" <+> pPrintPrec l 2 e
  pPrintPrec l p (e1 :@ e2) = maybeParens (p > 10) $ pPrintPrec l 10 e1 <> text "[" <> pPrintPrec l 0 e2 <> text "]"
  pPrintPrec l p (e1 :> e2) = maybeParens (p > 0) $ pPrintPrec l 1 e1 <> text ";" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 `Where` e2) = maybeParens (p > 0) $ pPrintPrec l 1 e1 <+> text "where" <+> pPrintPrec l 0 e2
  pPrintPrec l p (e1 :=: e2) = maybeParens (p > 5) $ pPrintPrec l 6 e1 <+> text "=" <+> pPrintPrec l 6 e2
  pPrintPrec l _ (Arr es) | l == prettyNormal = text "<" <> hsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ">"
  pPrintPrec l _ (Arr [e]) = text "array" <> braces (pPrintPrec l 0 e)
  pPrintPrec l _ (Arr es) = parens $ hsep $ punctuate (text ",") $ map (pPrintPrec l 0) es
  pPrintPrec l _ (Crl b) = braces $ pPrintPrec l 0 b
  pPrintPrec l _ (Dly b) = text "delay" <> braces (pPrintPrec l 0 b)
  pPrintPrec l p (b1 :|: b2) = maybeParens (p > 4) $ pPrintPrec l 5 b1 <+> text "|" <+> pPrintPrec l 4 b2
  pPrintPrec l p (e1 :.. e2) = maybeParens (p > 7) $ pPrintPrec l 8 e1 <> text ".." <> pPrintPrec l 8 e2
  pPrintPrec _ _ Fail = text "fail"

instance Pretty Blk where
  pPrintPrec l p (Blk [] [] e) = pPrintPrec l p e
  pPrintPrec l p (Blk vs eqns e) = maybeParens (p > 0) $ text "∃" <> hsep (map (pPrintPrec l 10) vs) <> text "." <+>
                                   vcat (punctuate (text "") (map (\ (i, d) -> pPrintPrec l 0 i <> text "<-" <> pPrintPrec l 0 d) eqns ++ [pPrintPrec l 0 e]))

pattern HNF :: Exp -> Val
pattern HNF e <- (getHNF -> Just e)

getHNF :: Exp -> Maybe Exp
getHNF Var{} = Nothing
getHNF e = getVal e

pattern Val :: Exp -> Val
pattern Val e <- (getVal -> Just e)

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

type Set a = [a]

srcToExp :: F.SrcEssential -> Exp
srcToExp (F.Variable i) | F.isSrcUnderscore i = Und
                        | otherwise = Var i
srcToExp (F.EPrim o) = Prm o
srcToExp (F.Lit (F.LInt k)) = Int k
srcToExp (F.DefineE i e) = i := srcToExp e
srcToExp (F.Choice e1 e2) = srcToBlk e1 :|: srcToBlk e2
srcToExp (F.Unify e1 e2) = srcToExp e1 :=: srcToExp e2
srcToExp (F.Seq e1 e2) = srcToExp e1 :> srcToExp e2
srcToExp (F.Where e1 e2) = srcToExp e1 `Where` srcToExp e2
srcToExp (F.ApplyD (F.EPrim F.DotDot) (F.Array [e1, e2])) = srcToExp e1 :.. srcToExp e2
srcToExp (F.ApplyD e1 e2) = srcToExp e1 :@ srcToExp e2
srcToExp (F.Range e) = Rng (srcToExp e)
srcToExp (F.Block e) = Crl (srcToBlk e)
srcToExp (F.Array es) = Arr (map srcToExp es)
srcToExp (F.Fail) = Fail
srcToExp (F.Function _ e1 _ e2) = Fun (srcToExp e1) (srcToExp e2)
srcToExp e = error $ "srcToBlk: unimplemented " ++ show e

srcToBlk :: F.SrcEssential -> Blk
srcToBlk e = Blk [] [] (srcToExp e)

newtype PExp = P Exp
instance Show PExp where
  show (P e) = prettyShow e

run :: F.SrcEssential -> PExp
run e = P $ evalBlk (Blk [u] [] (u :~> e'))
  where u = freshVars e' !! 0
        e' = srcToExp e

evalBlk :: Blk -> Exp
evalBlk b | trace (prettyShow b ++ "\n") False = undefined
evalBlk b@(Blk is eqs expr) =
    case findRedex (freshVarsBlk b) is eqs expr of
      -- XXX iterate substVal?
      Block _ xs eqns e -> evalBlk (Blk (is `union` xs) (map (second $ substVal eqs) eqns ++
                                                         map (second $ substVal eqns) eqs) e)
      None | null is    -> expr
           | otherwise  -> Crl (Blk is eqs expr)
      Failure _         -> Fail
      Delete xs         -> evalBlk (Blk (is \\ xs) (filter ((`notElem` xs) . fst) eqs) expr)

data Reduction
  = Delete [Iden]                          -- delete the identifiers from the block existential and equations
  | Block String [Iden] [Eqn] Exp          -- the named rule fired.  Add new existential, add new equations, replace expression
  | None                                   -- no redex found
  | Failure String                         -- evaluation failed
  deriving (Eq, Show)

instance Pretty Reduction where
  pPrintPrec l p (Delete is) = text "GC" <+> pPrintPrec l p is
  pPrintPrec _ _ None = text "None"
  pPrintPrec _ _ (Failure s) = text "Failure" <+> text (show s)
  pPrintPrec l _ (Done s e) = text "Done" <+> text (show s) <+> pPrintPrec l 0 e
  pPrintPrec l _ (Block s xs eqns e) = text "Block" <+> text (show s) <+> pPrintPrec l 0 (Blk xs eqns e)

pattern Done :: String -> Exp -> Reduction
pattern Done s e = Block s [] [] e

{-
notVar :: Exp -> Bool
notVar Var{} = False
notVar _ = True
-}

needVar :: Exp -> Bool
needVar (_ :> _) = True
needVar (_ :=: _) = True
needVar _ = False

findRedex :: [Iden] -> Set Iden -> Set Eqn -> Exp -> Reduction
findRedex fresh locals eqns ex =
  trace (render (text "findRedex" <+> (pPrintL prettyNormal ex $$ pPrintL prettyNormal res)) ++ "\n") res
  where
    res | xs@(_ : _) <- locals \\ (allVars' ex ++ concatMap (allVars' . snd) eqns) = Delete xs  -- GC rules
        | otherwise = find ex
    singleOcc = [ x | [x] <- group (sort (allVars ex)), x `elem` locals, isNothing (lookup x eqns) ]
    find :: Exp -> Reduction
    find expr =
      case expr of
        -- These should never occur
        _ := _             -> error "Found :="
        Rng _              -> error "Found Rng"
        Und                -> error "Found Und"
        Fun _ _            -> error "Found Fun"

        -- Scope and ubstitution
        Var i  :=: Val v  | i `elem` locals                                -- must be a local variable
                          , null (allFreeVars' v `intersect` map fst eqns) -- v must not have variables from eqns
                          , isNothing (lookup i eqns)                      -- i must not already have an eqn
                          -> Block "PROMOTE" [] [(i, v)] v
        Var i             | Just v <- lookup i eqns -> Done ("SUBST " ++ show i) v
        Crl b@Blk{}        -> Block "FLOAT" is' eqs' e' where Blk is' eqs' e' = freshen fresh b
        -- GC rules handled above

        -- Primops
        Prm F.Add :@ Arr [Int i, Int j] -> Done "Prim+" $ Int (i + j)
        Prm F.Sub :@ Arr [Int i, Int j] -> Done "Prim-" $ Int (i - j)
        Prm F.Mul :@ Arr [Int i, Int j] -> Done "Prim*" $ Int (i * j)
        Prm F.Div :@ Arr [Int i, Int j] | j == 0 -> Failure "Prim/"
                                      | otherwise-> Done "Prim/" $ Int (i `div` j)
        Prm F.Neg :@ Int i -> Done "Prim-neg" $ Int (- i)
        Prm F.IsInt :@ Con v | Int _ <- v -> Done "Prim-isInt" v
                             | otherwise -> Failure "Prim-isInt"
        Prm F.Lt :@ Arr [Int i, Int j] | i < j -> Done "Prim-Lt" $ Int i
                                       | otherwise -> Failure "Prim-Lt"
        Prm F.Gt :@ Arr [Int i, Int j] | i > j -> Done "Prim-Gt" $ Int i
                                       | otherwise -> Failure "Prim-Gt"

        -- Unification
        Val v1 :=: Val v2 | v1 == v2 -> Done "EQ" v1
        Con v1 :=: Con v2 | v1 /= v2 -> Failure "/="
        HNF v  :=: Var i             -> Done "SWAP" $ Var i :=: v
        Var i  :=: HNF v  | i `elem` allFreeVars' v -> Failure "OCCUR"  -- occurs check
        Val (Arr vs) :=: Arr es | length vs /= length es -> Failure "arr /="
                                | otherwise ->
                                  Done "EQARR" $ foldr (:>) (Arr vs) (zipWith (:=:) vs es)

        -- Unification, structural
        Val v  :=: (e1 :>  e2) -> Done "= ;" $ e1 :> (v :=: e2)
        Val v  :=: (e1 :=: e2) -> Done "= =" $ (v :=: e1) :> (v :=: e2)
        (e1 :> e2) :=: e3      -> Done "; =" $ e1 :> (e2 :=: e3)
        e1     :=: e2 | needVar e1 -> Block "name-=-rhs" [u] [] $ (Var u :=: e1) :> (Var u :=: e2) where u = fresh!!0
        e1     :=: e2    -> find2 (:=:) e1 e2

        -- Sequencing
        Val{}  :>  e2    -> Done "DROP" e2
        e1     :>  e2    -> find2 (:>)  e1 e2

        -- Beta
        Lam x (Blk vs [] e)
                :@ Val v -> Block "BETA" is eqs e' where Blk is eqs e' = freshen fresh (Blk (x:vs) [(x, v)] e)
        Arr es  :@ Val v -> Done "ITUP" $ foldr alt Fail (zipWith (\ i e -> (v :=: Int i) :> e) [0..] es)
          where alt e1 e2 = Blk [] [] e1 :|: Blk [] [] e2

        Var f   :@ Var _ | f `elem` singleOcc -> Block "EXI-APP" [u] [] (Var u) where u = fresh!!0
        e1      :@  e2   -> find2 (:@) e1 e2

        e1     :|: e2    -> find2B (:|:) e1 e2

        Fail             -> Failure "FAIL"

        Arr es           -> findArr es

        -- Reduction under lambda
        -- XXX WRONG, needs a Blk boundary inside the Lam
        -- Lam v e          -> find1 (Lam v) e

        -- :~> reduction
        -- Hackily turn IsInt back to a lambda:
        x :~> Prm F.IsInt          -> Done "int-hack" $ Var x :=: Lam u (Blk [] [] $ (Prm F.IsInt :@ Var u)) :> Var u
                                      where u = fresh!!0
        -- Matching
        x :~> Und                  -> Done "MWild"    $ Var x
        x :~> Val v                -> Done "MVal"     $ Var x :=: v
        x :~> ea@(Val _ :@ Val _)  -> Done "MApp-v-v" $ Var x :=: ea
        x :~> (Val e1 :@ e2)       -> Done "MApp-v-e" $ x :~> ((u := e2) :> (e1 :@ Var u))    where u = fresh!!0
        x :~> (e1 :@ Val e2)       -> Done "MApp-e-v" $ x :~> ((u := e1) :> (Var u :@ e2))    where u = fresh!!0
        x :~> (e1 :=: e2)          -> Done "MUnif"    $ (x :~> e1) :=: (x :~> e2)
        x :~> (b1 :|: b2)          -> Done "MChoice"  $ (x :~> Crl b1) :=: (x :~> Crl b2)
        _ :~> Fail                 -> Done "Mfail"    $ Fail
        x :~> (e1 :> e2)           -> Block "MSemi"  [u]   [] $ (u :~> e1) :> (x :~> e2)         where u = fresh!!0
        x :~> (e1 `Where` e2)      -> Block "MWhere" [u,v] [] $ (Var v :=: (x :~> e1)) :> (u :~> e2) :> Var v  where u:v:_ = fresh
        x :~> Arr es               -> Block "MTup"   xs    [] $ (Var x :=: Arr (map Var xs)) :> Arr (zipWith (:~>) xs es)
                                      where xs = take (length es) fresh
        x :~> Rng e                -> Block "MColon" [u]   [] $ (u :~> e) :@ Var x            where u = fresh!!0
        x :~> (i := e2) | x == i   -> error "name clash 1"
                        | otherwise-> Block "MDef"   [i]   [] $ Var i :=: (x :~> e2)
        x :~> ea@(Val{} :.. Val{}) -> Done "MEnum-v-v" $ Var x :=: ea
        x :~> (Val e1 :.. e2)      -> Done "MEnum-v-e" $ x :~> ((u := e1) :> (Var u :.. e2))    where u = fresh!!0
        x :~> (e1 :.. Val e2)      -> Done "MEnum-e-v" $ x :~> ((u := e2) :> (e1 :.. Var u))    where u = fresh!!0
        f :~> Fun e1 e2            -> Done "MFun" $ Lam v $ Blk [x,fx] [] $ (Var x :=: (v :~> e1)) :>
                                                                            (Var fx :=: (Var f :@ Var x)) :>
                                                                            (fx :~> e2)
                                        where v:x:fx:_ = fresh


        x :~> Crl (Blk is eqs e) | x `elem` is -> error "name clash 2"
                                 | otherwise
                                   -> Block "~> block" is eqs (x :~> e)

        _ :~> (_ :~> _)            -> error "~> with ~>"
        _ :~> Lam{}                -> error "~> with Lam"

        _ -> None

    find2 c e1 e2 =
      case find e1 of
        Block s is eqs e1' -> Block s is eqs (e1' `c` e2)
        None               -> find1 (c e1) e2
        r                  -> r
    find1 c e =
      case find e of
        Block s is eqs e'  -> Block s is eqs (c e')
        r                  -> r
    find2B :: (Blk -> Blk -> Exp) -> Blk -> Blk -> Reduction
    find2B c b1@(Blk is1 eqs1 e1) b2 =
      case find e1 of
        Block s is eqs e1' -> Block s is eqs (Blk is1 eqs1 e1' `c` b2)
        None               -> find1B (c b1) b2
        r                  -> r
    find1B c (Blk is eqs e) =
      case find e of
        Block s is' eqs' e' -> Block s is' eqs' (c (Blk is eqs e'))
        r                   -> r
    findArr :: [Exp] -> Reduction
    findArr [] = None
    findArr (e:es) =
      case find e of
        Block s is eqs e' -> Block s is eqs (Arr (e':es))
        None              ->
          case findArr es of
            Block s is eqs (Arr es') -> Block s is eqs (Arr (e:es'))
            r                        -> r
        r                 -> r

{-
-- Variables in a substitutable position, i.e., not in abstraction context
substVars :: Exp -> [Iden]
substVars (Var i) = [i]
substVars Und = []
substVars (Int _) = []
substVars (Prm _) = []
substVars (_ := e) = substVars e
substVars (e1 :>  e2) = substVars e1 ++ substVars e2
substVars (e1 `Where`  e2) = substVars e1 ++ substVars e2
substVars (e1 :=: e2) = substVars e1 ++ substVars e2
substVars (e1 :~> _) = substVars e1 -- e2 is a "pattern"
substVars (e1 :@  e2) = substVars e1 ++ substVars e2
substVars (Fun _ e2) = substVarsBlk e2 -- e1 is a "pattern"
substVars (Rng e) = substVars e
substVars (Arr es) = concatMap substVars es
substVars (Crl b) = substVarsBlk b
substVars (b1 :|: b2) = substVarsBlk b1 ++ substVarsBlk b2
substVars (e1 :.. e2) = substVars e1 ++ substVars e2
substVars Fail = []

substVars' :: Exp -> Set Iden
substVars' = nub . substVars

substVarsBlk :: Blk -> [Iden]
substVarsBlk (Blk is e) = filter (`notElem` is) (substVars e)
-}

allFreeVars :: Exp -> [Iden]
allFreeVars = allVars -- XXX

allFreeVars' :: Exp -> Set Iden
allFreeVars' = nub . allFreeVars

allVars :: Exp -> [Iden]
allVars (Var i) = [i]
allVars Und = []
allVars (Int _) = []
allVars (Prm _) = []
allVars (Lam i e) = i : allVarsBlk e
allVars (i := e) = i : allVars e
allVars (e1 :>  e2) = allVars e1 ++ allVars e2
allVars (e1 `Where`  e2) = allVars e1 ++ allVars e2
allVars (e1 :=: e2) = allVars e1 ++ allVars e2
allVars (i  :~> e ) = i : allVars e
allVars (e1 :@  e2) = allVars e1 ++ allVars e2
allVars (Fun e1 e2) = allVars e1 ++ allVars e2
allVars (Rng e) = allVars e
allVars (Arr es) = concatMap allVars es
allVars (Crl b) = allVarsBlk b
allVars (b1 :|: b2) = allVarsBlk b1 ++ allVarsBlk b2
allVars (e1 :.. e2) = allVars e1 ++ allVars e2
allVars Fail = []
allVars (Dly b) = allVarsBlk b

allVarsBlk :: Blk -> [Iden]
allVarsBlk (Blk is eqs e) = is ++ concatMap (allVars . snd) eqs ++ allVars e

allVars' :: Exp -> Set Iden
allVars' = nub . allVars

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
    ren e@Und = e
    ren e@(Int _) = e
    ren e@(Prm _) = e
    ren (Lam i e) | isJust (lookup i sub) = error "rename: clash 3"
                  | otherwise = Lam i (renB e)
    ren (i := e) | isJust (lookup i sub) = error "rename: clash 1"
                 | otherwise = i := ren e
    ren (Var i :=: Var j) | Just j' <- lookup i sub, j == j' = Var j
    ren (e1 :> e2) = ren e1 :> ren e2
    ren (e1 `Where` e2) = ren e1 `Where` ren e2
    ren (e1 :=: e2) = ren e1 :=: ren e2
    ren (i :~> e) = fromMaybe i (lookup i sub) :~> ren e
    ren (e1 :@ e2) = ren e1 :@ ren e2
    ren (Fun e1 e2) = Fun (ren e1) (ren e2)
    ren (Rng e) = Rng (ren e)
    ren (Arr es) = Arr (map ren es)
    ren (Crl b) = Crl (renB b)
    ren (b1 :|: b2) = renB b1 :|: renB b2
    ren (e1 :.. e2) = ren e1 :.. ren e2
    ren e@Fail = e
    ren (Dly b) = Dly (renB b)
    renB :: Blk -> Blk
    renB (Blk is eqs e) | any (isJust . (`lookup` sub)) is = error "rename clash 2"
                        | otherwise = Blk is (map (second ren) eqs) (ren e)

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
                     | otherwise = error "substVal: Lam unimplemented"
substVal _ e = error $ "substVal: not a Val: " ++ show e
