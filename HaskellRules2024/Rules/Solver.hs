{-# LANGUAGE LambdaCase #-}
module Rules.Solver (unsat) where
import Rules.Core
import Epic.Print
import qualified Epic.UnionFind as UF
import Data.List ( nub )
import Data.Maybe (mapMaybe, listToMaybe, maybeToList)
import Epic.List (groupKey, firstJust)
import Data.Containers.ListUtils (nubOrd)

-- | `unsat` is a simple unsatisfiablity checker, which implements the SOLVER rule
-----------------------------------------------------------------------------------
unsat :: RuleEnv -> Maybe UnsatReason
-----------------------------------------------------------------------------------
unsat env = {- ppTrace "TRACE: unsat" msg -} res
  where
    _msg  = pPrint (asms, res)
    asms = assumps env
    res  = solve (mkSolver asms)

-- `solve s` repeatedly loops, by generating new equalities, propagating them, and checking for unsatisfiability
solve :: Solver -> Maybe UnsatReason
solve s
  | Just r <- res = Just r
  | null eqs      = ppTrace "TRACE: solve (SAT)" msg Nothing
  | otherwise     = solve s'
  where
    msg           = pPrint (s_pos s')
    res           = check s
    eqs           = generate s
    s'            = propagate s eqs
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
checkNeg s neg@(A_GVEq x vy@(GVVar _))
  | isEqual s (GVVar x) vy && isPrim s x
  = Just (Contra neg)
  | otherwise
  = Nothing
checkNeg s neg@(A_GVEq x gv)
  | isEqual s (GVVar x) gv
  = Just (Contra neg)
  | otherwise
  = Nothing
checkNeg _ neg@(A_RelOp op (GVLit l))
  | isRelOpLit1 op l
  = Just (Contra neg)
  | otherwise
  = Nothing
checkNeg s neg@(A_RelOp op gv)
  | isRel s op gv
  = Just (Contra neg)
  -- | GVArr[gv1, gv2] <- gv
  -- , Just l1 <- evalsToLit s gv1
  -- , Just l2 <- evalsToLit s gv2
  -- , isRel s op (GVArr [GVLit l1, GVLit l2])
  -- = Just (Contra neg)
  | otherwise
  = Nothing

isRelOpLit1 :: PrimOp -> Lit -> Bool
isRelOpLit1 IsInt  (LInt _)  = True
isRelOpLit1 IsChar (LChar _) = True
isRelOpLit1 IsStr  (LStr _)  = True
isRelOpLit1 _      _         = False

intRel2 :: PrimOp -> Maybe (Integer -> Integer -> Bool)
intRel2 Gt  = Just (>)
intRel2 Lt  = Just (<)
intRel2 NEq = Just (/=)
intRel2 GEq = Just (>=)
intRel2 LEq = Just (<=)
intRel2 _  = Nothing

isRelOpLit2 :: PrimOp -> Lit -> Lit -> Bool
isRelOpLit2 op l1 l2
  | LInt i1 <- l1
  , LInt i2 <- l2
  , Just r  <- intRel2 op
  = i1 `r` i2
  | otherwise
  = False


-----------------------------------------------------------------------------------
-- | `generate s` returns a list of equalities that can be derived from the solver,
--    but which are not yet known in the UF graph
-----------------------------------------------------------------------------------
generate :: Solver -> [Equality]
generate s         = filter (not . known) (evalDefs s ++ pos)
  where
    pos            = [(GVVar x, y) | A_GVEq x y <- s_pos s]
    known (v1, v2) = isEqual s v1 v2

evalDefs :: Solver -> [Equality]
evalDefs s = {- ppTrace "TRACE: evalDefs" msg -} res
  where
    _msg    = pPrint (defs, res)
    res    = mapMaybe (evalDef s) defs
    defs   = s_def s

primOpArith :: PrimOp -> Maybe (Integer -> Integer -> Integer)
primOpArith Add = Just (+)
primOpArith Sub = Just (-)
primOpArith Mul = Just (*)
primOpArith _   = Nothing

evalDef :: Solver -> Definition -> Maybe Equality
evalDef s (x, (op, GVArr [v1, v2])) = do
  o       <- primOpArith op
  LInt l1 <- evalsToLit s v1
  LInt l2 <- evalsToLit s v2
  Just (GVVar x, GVLit (LInt (l1 `o` l2)))
evalDef _ _ = Nothing
-----------------------------------------------------------------------------------
-- | `propagate s eqs` updates the solver state `s` with the new equalities `eqs`
-----------------------------------------------------------------------------------
propagate :: Solver -> [Equality] -> Solver
propagate s eqs = s { s_uf = uf', s_lits = lits' }
  where
    uf'   = foldr (\(x, y) uf -> UF.union uf x y) (s_uf s) eqs
    lits' = nubOrd (s_lits s ++ eq_lits)
    -- new lits generated from eqs
    eq_lits = concatMap (mapMaybe groundLit . (\(x, y) -> [x, y])) eqs

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
-- `isRel s op v` returns true if
--  * there is a relation `op v'` in the solver and `v == v'`
--  * if there is a literal `l` s.t. `s gv == s l` and `isRelOpLit l` holds
------------------------------------------------------------------------------------
isRel :: Solver -> PrimOp -> GroundVal -> Bool
isRel s op gv
  | not (null [ () | A_RelOp op' gv' <- s_pos s, op' == op, isEqual s gv gv' ])
  = True
  | not (null [ () | l <- maybeToList (evalsToLit s gv), isRelOpLit1 op l ])
  = True
  | GVArr [gv1, gv2] <- gv
  , Just l1 <- evalsToLit s gv1
  , Just l2 <- evalsToLit s gv2
  , isRelOpLit2 op l1 l2
  = True
  | otherwise
  = False


------------------------------------------------------------------------------------
-- `eval s v` returns `Just l` if `v` is provably equal to a literal `l` in solver `s`
------------------------------------------------------------------------------------
evalsToLit :: Solver -> GroundVal -> Maybe Lit
evalsToLit s v = listToMaybe (tryLit v ++ tryEqLits s v)

tryLit :: GroundVal -> [Lit]
tryLit (GVLit l) = [l]
tryLit _         = []

tryEqLits :: Solver -> GroundVal -> [Lit]
tryEqLits s v = [l | l <- s_lits s, isEqual s v (GVLit l)]




------------------------------------------------------------------------------------
-- | Solver State
------------------------------------------------------------------------------------
data Solver = MkSolver
  { s_lits  :: [Lit]
  , s_uf    :: UF.UF GroundVal
  , s_pos   :: [FailableAssump]
  , s_neg   :: [FailableAssump]
  , s_def   :: [Definition]
  }

type Equality   = (GroundVal, GroundVal)
type Definition = (Ident, (PrimOp, GroundVal))

mkSolver :: [Assump] -> Solver
mkSolver asms = MkSolver { s_lits = lits, s_uf = UF.new, s_pos = pos, s_neg = neg, s_def = defs }
  where
    defs = [(x, (op, gv)) | A_PrimOp x (AO_Prim op) gv <- asms ]
    pos  = [asm | A_Pos asm <- asms] ++ [ A_RelOp op gv | (_, (op, gv)) <- defs]
    neg  = [asm | A_Neg asm <- asms]
    lits = mapMaybe assumpGroundVal (pos ++ neg)

assumpGroundVal :: FailableAssump -> Maybe Lit
assumpGroundVal (A_GVEq  _ gv) = groundLit gv
assumpGroundVal (A_RelOp _ gv) = groundLit gv

groundLit :: GroundVal -> Maybe Lit
groundLit (GVLit l) = Just l
groundLit _         = Nothing
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
