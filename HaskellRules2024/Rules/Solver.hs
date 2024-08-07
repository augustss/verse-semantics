{-# LANGUAGE LambdaCase #-}
module Rules.Solver (unsat) where
import Rules.Core
import Epic.Print
import qualified Epic.UnionFind as UF
import Data.List ( nub )
import Data.Maybe (mapMaybe, listToMaybe, maybeToList)
import Epic.List (groupKey, firstJust)
import Data.Containers.ListUtils (nubOrd)

-- `unsat` is an unsatisfiablity checker, which implements the SOLVER rule.
-----------------------------------------------------------------------------------
unsat :: RuleEnv -> Maybe UnsatReason
-- Nothing <=> not unsatisfible
-- Just r  <=> unsatisfiable for reason r
-----------------------------------------------------------------------------------
unsat env = {- ppTrace "TRACE: unsat" msg -} res
  where
    _msg  = pPrint (asms, res)
    asms = assumps env
    res  = solver (mkSolver asms)

-- `solve s` repeatedly loops, by generating new equalities, propagating them, and checking for unsatisfiability
solver :: Solver -> Maybe UnsatReason
solver s
  | Just r <- res = Just r
  | null facts    = {- ppTrace "TRACE: solve (SAT)" msg -} Nothing
  | otherwise     = solver s'
  where
    _msg          = pPrint (s_pos s')
    -- 1. check if the current solver state is unsatisfiable
    --    e.g. from [x = 1, x = 3] we get a contradication as 1 = 3
    res           = check s
    -- 2. generate new equalities from the definitions
    --    e.g. from [x = add[a, 2], a = 1] generates a new equality `x=3` or `add[a, 2] = 3` (TODO: check)
    facts         = generate s
    -- 3. update the solver state with the new equalities
    --    e.g. from the above we update the solver state with the new equality `x=3`
    s'            = propagate s facts


{- Note [Solver-Rules]

Equality [s |- gv1 ~ gv2]

-------------[eq-refl]
s |- gv ~ gv

s |- gv1 ~ gv2     s |- gv2 ~ gv3
---------------------------------[eq-trans]
s |- gv1 ~ gv3

s |- gv1 ~ gv2
---------------[eq-symm]
s |- gv2 ~ gv1

-}





{- Note [Solver]
~~~~~~~~~~~~~~~~
The solver maintains a "union-find" (UF) data structure
`s_uf` that tracks all the equivalences between ground
values that can be proved from the *positive* assumptions
of the form `x = gv`.

Given a set of "equalities" e.g. [x = y, x = z, a = b],
the UF data structure will build a directed acyclic graph
with

    * vertices corresponding to ground values, e.g. [x, y, z, a, b]
    * directed edges linking equal nodes, e.g. [x -> y, y -> z, a -> b]

such that the "roots" reachable from any two nodes are equal
iff the two nodes are provably equal under the reflexive,
symmetric, and transitive closure of the "equalities" in the graph.
For example, in the above, x, y are provably equal as their roots
are the same -- z. But x and a are not provably equal as their roots
are different -- z and b.

We write `s |- x ~ y` to denote that x and y are provably equal in solver s.

The solver proceeds as follows:

0. Use the starting assumptions to build the initial solver state `s` comprising
  - `s_pos` the positive equalities e.g. `x = y`, predicates e.g. `int[x]`
  - `s_neg` the negative equalities `not (x = y)`, `not int[x]`
  - `s_def` the definitions e.g. `x = Add[y, z]`
  - `s_lits` all the literals e.g. `1,2,3, 'c'` etc. appearing in assumptions
  - an initial UF structure from the positive equalities in `s_pos`

1. Next, the solver invokes `check`s to see if the solver state is inconsistent,
   i.e. if `!s` can be derived, and if so, exits with the corresponding negative
    fact as the `UnsatReason`

2. Next, (if the solver state is *not* inconsistent), the solver `generate`s
   _new_ equalities from the definitions `s_def` and the UF structure.
   To do so it iterates over `s_defs` and invokes `evalDef` on each term
   of the form `x = op[v1, v2]` to see if there are literals l1 and l2
   such that s |- v1 ~ l1 and s |- v2 ~ l2 and if so, we generate the
   new equality `x = op[l1, l2]`

   If this set is empty, the solver exits with SAT.

3. Next, (if the set of new equalities is non-empty), the solver extends the UF
   structure with the new equalities, and then goes back to step 1.

For example, suppose we are given the set of assumptions:

    x = y
    y = z
    z = Add[1, a]
    a = 2
    not (x = 3)

In step 0, the solver will build the UF structure:

  { s_lits = [1, 2, 3]
  , s_pos  = [(x = y), (y = z), (z = Add[1, a])]
  , s_defs = [z = Add[1, 2]]
  }

  and the UF graph that looks like

    x -> y -> z -> Add[1, 2]
    a -> 2

In step 1, the solver will `check` if the UF graph is inconsistent -- there is NO inconsistency

In step 2, the solver will `generate` new equalities from the definitions.
From the definition `z = Add[1, a]` and `s |- a ~ 2` we generate the new
equality `z = 3`.

In step 3, we update the UF graph with this new equality to get

    x -> y -> z -> Add[1, a] -> 3
    a -> 2

and go back to step 1.

Now in step 1, when we invoke `checkNeg` on the negative assumption `not (x = 3)`
we find that in fact `s |- x ~ 3` which is a contradiction.

 -}

-----------------------------------------------------------------------------------
-- `check s` uses the `neg` assumptions and `lits` to check if !s holds
-----------------------------------------------------------------------------------
check :: Solver -> Maybe UnsatReason
check s = firstJust (checkLits s : (checkNeg s <$> s_neg s))

-- [c-lit], i.e. k, k' yield a contradiction if s |- k ~ k'
checkLits :: Solver -> Maybe UnsatReason
checkLits cc = firstJust (\case { l1:l2:_ -> Just (DiseqLit l1 l2); _ -> Nothing } <$> litGroups)
  where
   litGroups  = groupKey litRep lits
   litRep l   = UF.find (s_uf cc) (GVLit l)
   lits       = nub (s_lits cc)

-- Can we derive !s, see Note [Rules:Contradiction]
checkNeg :: Solver -> FailableAssump -> Maybe UnsatReason

-- [c-eq-*] not (x = y) yields a contradiction if s |- x ~ gv and x OR gv are primitive
checkNeg s neg@(A_GVEq x gv)
  | isEqual s (GVVar x) gv
  , (isPrim s (GVVar x) || isPrim s gv)  -- See Note [Checking negated equalities]
  = Just (Contra neg)
  | otherwise
  = Nothing

-- [c-op] not (op[gv]) yields a contradiction if s |- op[gv]
checkNeg s neg@(A_RelOp op gv)
  | isRel s op gv
  = Just (Contra neg)
  | op == IsComp && isComparableGV s gv
  = Just (Contra neg)
  | otherwise
  = Nothing

isComparableGV :: Solver -> GroundVal -> Bool
isComparableGV s gv = isRel s IsInt gv || isRel s IsChar gv || isRel s IsStr gv

isRelOpLit1 :: PrimOp -> Lit -> Bool
isRelOpLit1 IsInt  (LInt _)  = True
isRelOpLit1 IsChar (LChar _) = True
isRelOpLit1 IsStr  (LStr _)  = True
isRelOpLit1 IsComp l         = isCompLit l
isRelOpLit1 _      _         = False

isCompLit :: Lit -> Bool
isCompLit (LInt _)  = True
isCompLit (LChar _) = True
isCompLit (LStr _)  = True
isCompLit _         = False

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

{- Note [Rules:Contradiction]

Contradiction [!s]

k1, k2 in s   s |- k1 ~ k2
--------------------------- [c-lit]
!s

not (x = gv) in s    s |- x ~ gv     s |- x:prim
-----------------------------------------------[c-eq-l]
!s

not (x = gv) in s    s |- x ~ gv     s |- gv:prim
-------------------------------------------------[c-eq-r]
!s


not (op[gv]) in s   s |- op[gv]
-------------------------------- [c-op]
!s

IsPrimitive [s |- gv : prim]

s |- gv ~ l
--------------[p-lit]
s |- gv: prim

s |- isPrim[gv1]  s |- gv1 ~ gv2
---------------------------------[p-pred]
s |- gv2:prim

Evaluates-To [s |- op[gv]

s |- gv ~ l  and op1[l]
------------------------[ev-op1]
s |- op1[gv]

s |- gv ~ l1    s |- gv2 ~ l2   op2[l1, l2]
----------------------------------------------[ev-op2]
s |- op1[gv]

-}

{- Note [Checking negated equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consdider the assumption not(r=r).  Is that enough to garantee un-satisfiablity.
Currently we say no. Consider T29Jul24-2:

    f( x:any ) := x=x

This gets stuck (we do not have a rewrite rule for x=x), so should be rejected
by the verifier.  In the verifier we will skolemise `x` to say `r`, and use
SPLIT-V to get two verification terms with assumptions (r=r); and not(r=r) resp.
We don't want to regard the latter as unsatisfiable, else we'll erroneously
succeed.

Instead, in `checkNeg` we test (isPrim s x), to check that the skolem `x` is
known to have some primitive type (int, string, etc)
-}
-----------------------------------------------------------------------------------
-- | `generate s` returns a list of equalities that can be derived from the solver,
--    but which are not yet known in the UF graph
-----------------------------------------------------------------------------------
generate :: Solver -> [Fact]
generate s = (FProp  <$> generateProps s)
          ++ (FEqual <$> generateEqs   s)


generateProps :: Solver -> [Prop]
generateProps s = filter (not . knownProp s) (evalProps s)  -- TODO: why concat pos? they get filtered immediately?

knownProp :: Solver -> Prop -> Bool
knownProp s (MkProp r v) = isRel s r v

evalProps :: Solver -> [Prop]
evalProps s = concatMap (evalProp s) (s_pos s)

evalProp :: Solver -> FailableAssump -> [Prop]
evalProp s (A_RelOp IsComp gv)
  = [ MkProp IsComp v' | gvs <- s_tups s, isEqual s gv (GVArr gvs), v' <- gvs ]
evalProp _ _
  = []

generateEqs :: Solver -> [Equality]
generateEqs s = filter (not . knownEq s) (evalDefs s ++ pos)  -- TODO: why concat pos? they get filtered immediately?
  where
    pos       = [MkEqual (GVVar x) y | A_GVEq x y <- s_pos s]

knownEq :: Solver -> Equality -> Bool
knownEq s (MkEqual v1 v2) = isEqual s v1 v2

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

-- | Given a definition `x = op[gv1, gv2]` we see if s |- gv1 ~ l1, s |- gv2 ~ l2
--   and if so, generate the equality `x = op[l1, l2]`
evalDef :: Solver -> Definition -> Maybe Equality
evalDef s (x, (op, GVArr [v1, v2])) = do
  o       <- primOpArith op
  LInt l1 <- evalsToLit s v1
  LInt l2 <- evalsToLit s v2
  Just (MkEqual (GVVar x) (GVLit (LInt (l1 `o` l2))))
evalDef _ _ = Nothing

-----------------------------------------------------------------------------------
-- | `propagate s eqs` updates the solver state `s` with the new equalities `eqs`
-----------------------------------------------------------------------------------
propagate :: Solver -> [Fact] -> Solver
propagate s fs = s''
  where
    s'    = propagateEqs   s  [ e | FEqual e <- fs ]
    s''   = propagateProps s' [ p | FProp  p <- fs ]

propagateProps :: Solver -> [Prop] -> Solver
propagateProps s ps = s { s_pos = s_pos' }
  where
    s_pos' = nubOrd (s_pos s ++ [A_RelOp r v | MkProp r v <- ps])

propagateEqs :: Solver -> [Equality] -> Solver
propagateEqs s eqs = s { s_uf = uf', s_lits = lits' }
  where
    uf'   = foldr (\(MkEqual x y) uf -> UF.union uf x y) (s_uf s) eqs
    lits' = nubOrd (s_lits s ++ eq_lits)
    -- new lits generated from eqs
    eq_lits = concatMap (\(MkEqual gv1 gv2) -> groundLit gv1 ++ groundLit gv2) eqs

------------------------------------------------------------------------------------
-- `isEqual s v1 v2` returns true if v1 and v2 are provably equal in solver s
------------------------------------------------------------------------------------
isEqual :: Solver -> GroundVal -> GroundVal -> Bool
isEqual s = UF.equal (s_uf s)

------------------------------------------------------------------------------------
-- `isPrim s x` returns true if `x` has a provably outermost-primitive type
-- See Note [Checking negated equalities]
------------------------------------------------------------------------------------
isPrim :: Solver -> GroundVal -> Bool
isPrim s (GVVar x)  = isPrimV s x
isPrim _ (GVLit _)  = True
isPrim s (GVArr vs) = all (isPrim s) vs
   -- Recursion in GVArr: a common case is: r=<>, not(r=<>)
   -- and that is definitely contradictory.  Test is T26Jul24-13.
isPrim s (GVTru v) = isPrim s v

isPrimV :: Solver -> Ident -> Bool
isPrimV s x = not $ null [() | A_RelOp op (GVVar y) <- s_pos s, isTyOp op, isEqual s (GVVar x) (GVVar y)]

isTyOp :: PrimOp -> Bool
isTyOp IsInt  = True
isTyOp IsChar = True
isTyOp IsStr  = True
isTyOp IsComp = True
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


-- "things we know"
data Solver = MkSolver
  { s_lits  :: [Lit]                -- ^ all the literals in s_uf
  , s_tups  :: [[GroundVal]]        -- ^ all the tuples   in s_uf
  , s_uf    :: UF.UF GroundVal      -- ^ the union-find data structure used to track equivalence classes (under equalities)
  , s_pos   :: [FailableAssump]     -- ^ all the `A_Pos` from the `Assump`   positive equalities and predicates
  , s_neg   :: [FailableAssump]     -- ^ all the `A_Neg` from the `Assump`   negative equalities and predicates (i.e. under "not")
  , s_def   :: [Definition]         -- ^ all the `A_PrimOp` from the `Assump` terms of the form `x = op[v]`
  }

data Fact
  = FProp Prop
  | FEqual Equality
  deriving (Eq, Ord, Show)

data Prop = MkProp PrimOp GroundVal
  deriving (Eq, Ord, Show)

data Equality = MkEqual GroundVal GroundVal
  deriving (Eq, Ord, Show)

type Definition = (Ident, (PrimOp, GroundVal))

{-
  x = Add[a, b]
  a = 1
  b = 2
-}

mkSolver :: [Assump] -> Solver
mkSolver asms = MkSolver { s_lits = lits, s_tups = tups, s_uf = UF.new, s_pos = pos, s_neg = neg, s_def = defs }
  where
    defs   = [(x, (op, gv)) | A_PrimOp x (AO_Prim op) gv <- asms ]
    pos    = [asm | A_Pos asm <- asms] ++ [ A_RelOp op gv | (_, (op, gv)) <- defs] -- TODO: delete defs from pos and see what happens; add a note to explain
    neg    = [asm | A_Neg asm <- asms]
    lits   = concatMap groundLit    groundVals
    tups   = concatMap assumpTuples groundVals
    groundVals = nubOrd (assumpGroundVal <$> (pos ++ neg))

assumpTuples :: GroundVal -> [[GroundVal]]
assumpTuples (GVArr gvs) = gvs : concatMap assumpTuples gvs
assumpTuples _           = []

assumpGroundVal :: FailableAssump -> GroundVal
assumpGroundVal (A_GVEq  _ gv) = gv
assumpGroundVal (A_RelOp _ gv) = gv

groundLit :: GroundVal -> [Lit]
groundLit (GVLit l)   = [l]
groundLit (GVArr gvs) = concatMap groundLit gvs
groundLit _           = []

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

instance Pretty Equality where
  pPrint (MkEqual x y) = pPrint x <+> text "=" <+> pPrint y
