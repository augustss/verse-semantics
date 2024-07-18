{-# LANGUAGE LambdaCase #-}
module Rules.Solver (unsat) where
import Rules.Core
import Epic.Print
import qualified Epic.UnionFind as UF
import Data.List ( nub )
import Data.Maybe (mapMaybe)
import Epic.List (groupKey, firstJust)

-- | `unsat` is a simple unsatisfiablity checker, which implements the SOLVER rule
-----------------------------------------------------------------------------------
unsat :: RuleEnv -> Maybe UnsatReason
-----------------------------------------------------------------------------------
unsat (RE { assumps = asms }) = solve s
  where
    pos, neg :: [FailableAssump]
    pos = [asm | A_Pos asm <- asms] ++ [ A_RelOp op gv | A_PrimOp _ (AO_Prim op) gv <- asms ]
    neg = [asm | A_Neg asm <- asms]
    s   = mkSolver pos neg

-- `solve s` repeatedly loops, by generating new equalities, propagating them, and checking for unsatisfiability
solve :: Solver -> Maybe UnsatReason
solve s
  | Just r <- res = Just r
  | null eqs      = Nothing
  | otherwise     = solve s'
  where
    res = check s
    eqs = generate s
    s'  = propagate s eqs

-----------------------------------------------------------------------------------
-- `check s` uses the `neg` assumptions and `lits` to check for unsatisfiability
-----------------------------------------------------------------------------------
check :: Solver -> Maybe UnsatReason
check s = firstJust (checkLits s : (checkNeg s <$> s_neg s))

-- looks for lits k, k' such that s k == s k'
checkLits :: Solver -> Maybe UnsatReason
checkLits cc = firstJust (\case { l1:l2:_ -> Just (DiseqLit l1 l2); _ -> Nothing } <$> litGroups)
  where
   litGroups  = groupKey litRep lits
   litRep l   = UF.find (s_uf cc) (GVLit l)
   lits       = nub (s_lits cc)

-- looks for assumptions `not p` such that `p` is provable in s
checkNeg :: Solver -> FailableAssump -> Maybe UnsatReason
checkNeg s neg@(A_GVEq x (GVVar y))
  | x == y && isPrim s x   = Just (Contra neg)
  | otherwise              = Nothing
checkNeg s neg@(A_GVEq x gv)
  | isEqual s (GVVar x) gv = Just (Contra neg)
  | otherwise              = Nothing
checkNeg _ neg@(A_RelOp op (GVLit l))
  | isRelOpLit op l        = Just (Contra neg)
  | otherwise              = Nothing
checkNeg s neg@(A_RelOp op gv)
  | isRel s op gv          = Just (Contra neg)
  | otherwise              = Nothing

isRelOpLit :: PrimOp -> Lit -> Bool
isRelOpLit IsInt  (LInt _)  = True
isRelOpLit IsChar (LChar _) = True
isRelOpLit IsStr  (LStr _)  = True
isRelOpLit _      _         = False

isRel :: Solver -> PrimOp -> GroundVal -> Bool
isRel s op gv = not $ null [ () | A_RelOp op' gv' <- s_pos s, op' == op, isEqual s gv gv' ]

-----------------------------------------------------------------------------------
type Equality = (GroundVal, GroundVal)

-----------------------------------------------------------------------------------
-- | `generate s` returns a list of equalities that can be derived from the solver,
--    but which are not yet known in the UF graph
-----------------------------------------------------------------------------------
generate :: Solver -> [Equality]
generate s = eqs
  where


-----------------------------------------------------------------------------------
-- | `propagate s eqs` updates the solver state `s` with the new equalities `eqs`
-----------------------------------------------------------------------------------
propagate :: Solver -> [Equality] -> Solver
propagate s eqs = s { s_uf = uf' }
  where
    uf'   = foldr (\(x, y) uf -> UF.union uf x y) (s_uf s) eqs
    -- uf'         = foldl' (\uf (v1, v2) -> UF.union uf v1 v2) (s_uf s) eqs
------------------------------------------------------------------------------------
-- `isEqual s v1 v2` returns true if v1 and v2 are provably equal in solver s
------------------------------------------------------------------------------------
isEqual :: Solver -> GroundVal -> GroundVal -> Bool
isEqual s = UF.equal (s_uf s)

------------------------------------------------------------------------------------
-- `isPrim s x` returns true if `x` has a provably primitive type
------------------------------------------------------------------------------------
isPrim :: Solver -> Ident -> Bool
isPrim s x = not $ null [() | A_RelOp op (GVVar y) <- s_pos s, isTyOp op, isEqual s (GVVar x) (GVVar y)]

isTyOp :: PrimOp -> Bool
isTyOp IsInt  = True
isTyOp IsChar = True
isTyOp IsStr  = True
isTyOp _      = False

------------------------------------------------------------------------------------
-- | Solver State
------------------------------------------------------------------------------------
data Solver = MkSolver
  { s_lits  :: [Lit]
  , s_uf    :: UF.UF GroundVal
  , s_pos   :: [FailableAssump]
  , s_neg   :: [FailableAssump]
  }

mkSolver :: [FailableAssump] -> [FailableAssump] -> Solver
mkSolver pos neg = MkSolver { s_lits = lits, s_uf = ufg, s_pos = pos, s_neg = neg }
  where
    lits = mapMaybe assumpGroundVal (pos ++ neg)
    ufg   = foldr (\(x, y) uf -> UF.union uf x y) UF.new eqs
    eqs  = [(GVVar x, y) | A_GVEq x y <- pos]

assumpGroundVal :: FailableAssump -> Maybe Lit
assumpGroundVal (A_GVEq  _ (GVLit l)) = Just l
assumpGroundVal (A_RelOp _ (GVLit l)) = Just l
assumpGroundVal _                     = Nothing

------------------------------------------------------------------------------------
-- | Why is the solver returning UNSAT
------------------------------------------------------------------------------------
data UnsatReason
   = Contra    FailableAssump
   | DiseqLit  Lit   Lit
   deriving (Show)

instance Pretty UnsatReason where
  pPrint (Contra a)     = text "CONTRA"    <+> pPrint a
  pPrint (DiseqLit x y) = text "DISEQ-LIT" <+> pPrint x <+> pPrint y
