{-# LANGUAGE LambdaCase #-}
module Core.Solver (unsat, unsatAsms) where
import Core.Expr
import Epic.Print
import qualified Epic.UnionFind as UF
import Data.List ( nub )
import Data.Maybe (mapMaybe, listToMaybe, maybeToList)
import Epic.List (groupKey, firstJust)
import Data.Containers.ListUtils (nubOrd)
import Epic.BellmanFord (negativeCycle)
-- import qualified Debug.Trace as Debug

import qualified Core.Bind as Bind

-- traceShow :: Show a => String -> a -> a
-- traceShow msg x = Debug.trace ("TRACE: " ++ msg ++ ": " ++ show x) x

-- `unsat` is an unsatisfiablity checker, which implements the SOLVER rule.
-----------------------------------------------------------------------------------
unsat :: [Assump] -> Maybe UnsatReason
-- Nothing <=> not unsatisfible
-- Just r  <=> unsatisfiable for reason r
-----------------------------------------------------------------------------------
unsat asms = {- ppTrace "TRACE: unsat" msg -} res
  where
    _msg  = pPrint (asms, res)
    res  = unsatAsms asms

unsatAsms :: [Assump] -> Maybe UnsatReason
unsatAsms = solver . mkSolver

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

We write `s |- x ~ y` to denote that x and y are provably equal in solver s,
using
  * Transitivity of equality
  * Congruences

The solver proceeds as follows (equality saturation):

0. Use the starting assumptions to build the initial solver state `s` comprising
  - `s_pos` the positive equalities e.g. `x = y`, predicates e.g. `int[x]`
  - `s_neg` the negative equalities `not (x = y)`, `not int[x]`
  - `s_def` the definitions e.g. `x = Add[y, z]`
  - `s_lits` all the literals e.g. `1,2,3, 'c'` etc. appearing in assumptions
  - an initial UF structure from the positive equalities in `s_pos`

1. Next, the solver invokes `check`s to see if the solver state is inconsistent,
   i.e. if `!s` can be derived, and if so, exits with the corresponding negative
    fact as the `UnsatReason`

2. Next, (if the solver state is *not* inconsistent -- this point is
   efficiency-only), the solver `generate`s _new_ equalities from the
   definitions `s_def` and the UF structure.  To do so it iterates over `s_defs`
   and invokes `evalDef` on each term of the form `x = op[v1, v2]` to see if
   there are literals l1 and l2 such that s |- v1 ~ l1 and s |- v2 ~ l2 and if
   so, we generate the new equality `x = op[l1, l2]`

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
check s = firstJust
  $  checkLits s
  :  checkTypes s
  :  checkArith s
  : (checkNeg s <$> s_neg s)

-- [c-lit], i.e. k, k' yield a contradiction if s |- k ~ k'
checkLits :: Solver -> Maybe UnsatReason
checkLits cc = firstJust (\case { l1:l2:_ -> Just (DiseqLit l1 l2); _ -> Nothing } <$> litGroups)
  where
   -- litGroups: group literals: each group are known to be equal to each other
   -- If any of these groups is not a singleton, contradiction!
   litGroups :: [[Lit]]
   litGroups = groupKey litRep lits

   litRep :: Lit -> GroundVal   -- Get the representative of this literal
   litRep l = UF.find (s_uf cc) (GVLit l)

   -- lits is all the literals mentioned anywhere
   lits :: [Lit]
   lits = nub (s_lits cc)

checkTypes :: Solver -> Maybe UnsatReason
-- Return (Just reason) if the solver has contradictory type tests
--    e.g. IsInt[r], IsStr[s]  where  r=s
checkTypes s
  | (pos1,pos2) : _ <- [ (pos1, pos2)
                       | pos1@(A_RelOp op1 gv1) <- s_pos s
                       , primOpIsTypeTest op1
                       , pos2@(A_RelOp op2 gv2) <- s_pos s
                       , primOpIsTypeTest op2
                       , op1 /= op2
                       , isEqual s gv1 gv2 ]
  = Just (Contra2 pos1 pos2)
  | otherwise
  = Nothing

-- Can we derive !s, see Note [Rules:Contradiction]
checkNeg :: Solver -> FailableAssump -> Maybe UnsatReason

-- [c-eq-*] not (x = ) yields a contradiction if s |- x ~ gv and x OR gv are primitive
checkNeg s neg@(A_GVEq x gv)
  | isEqual s (GVVar x) gv
  , isPrim s (GVVar x) || isPrim s gv  -- See Note [Checking negated equalities]
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
  where !s means "solver state s is a contradiction"

k1, k2 distinct literals in s   s |- k1 ~ k2
-------------------------------------------- [c-lit]
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
generateEqs s = filter (not . knownEq s) $
                (pos ++         -- TODO: why concat pos? don't they get filtered out immediately?
                 evalDefs s ++
                 cseDefs s)
  where
    pos = [MkEqual (GVVar x) y | A_GVEq x y <- s_pos s]

knownEq :: Solver -> Equality -> Bool
knownEq s (MkEqual v1 v2) = isEqual s v1 v2


evalDefs :: Solver -> [Equality]
evalDefs s = {- ppTrace "TRACE: evalDefs" _msg -} res
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

cseDefs :: Solver -> [Equality]
-- Congruence closure:
--  if we have defs  x = Op[gv1], y = Op[gv2], and s |- gv1 ~ gv2, then generate x=y
cseDefs s
  = [ MkEqual (GVVar x) (GVVar y)
    | (x, (opx,gvx)) <- s_def s
    , (y, (opy,gvy)) <- s_def s
    , x /= y
    , opx == opy
    , isEqual s gvx gvy ]

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

mkSolver :: [Assump] -> Solver
-- Initialise the solver, given a bunch of assumptions
mkSolver asms = MkSolver { s_lits = lits, s_tups = tups, s_uf = UF.new, s_pos = pos, s_neg = neg, s_def = defs }
  where
    defs = [(x, (op, gv)) | A_PrimOp x (AO_Prim op) gv <- asms ]
    pos  = [asm           | A_Pos asm                  <- asms ] ++ defs_result_asms
    neg  = [asm           | A_Neg asm                  <- asms ]
    lits = concatMap groundLit    groundVals
    tups = concatMap assumpTuples groundVals
    groundVals = nubOrd (assumpGroundVal <$> (pos ++ neg))

    defs_result_asms :: [FailableAssump]
    -- Given x = intAdd$[p,q], we know that isInt$[x] holds
    -- See Note [Add arithmetic type assumptions]
    defs_result_asms = mapMaybe type_asm defs
        where
          type_asm (x, (op,_))
            = case primOpResultPred_maybe op of
                Just pred_op -> Just (A_RelOp pred_op (GVVar x))
                Nothing      -> Nothing

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
checkArith :: Solver -> Maybe UnsatReason
------------------------------------------------------------------------------------
checkArith = fmap reason . negativeCycle V0 . arithGraph
  where
    reason vs = Arith [ gv | GV gv <- vs ]

data Vertex = GV GroundVal | V0
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------------

{- Note [Add arithmetic type assumptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we add x = intAdd$[p,q], then we know that isInt$[x] holds. This is obviously
important to verify functions like
   f(x:int):int := x+1

Instead of adding this knowledge directly in the solver we could
(and previously did) define (+) thus:
   (+) := \p. ∃ x y. (x,y) = p; isInt$[x]; isInt$[y];
                     (x,y) >> some(lam z. z = intAdd$[x,y]; isInt$[z]; z)

That works ok, but it takes a lot more verifier steps (skolemisation of the
`some` etc), and it's much faster to define (+) thus
   (+) := \p. ∃ x y. (x,y) = p; isInt$[x]; isInt$[y]; intAdd$[x,y]
and add the isInt$ assumption in the solver. That is what `defs_result_asms`
does in `mkSolver`.

Examples: in verify.versetest,
         Using `some`    Using defs_result_asms
  T11       102 steps       84 steps
  T13       177 steps      141 steps
  Rec1      279 steps      239 steps

Note [Arithmetic Graph]
~~~~~~~~~~~~~~~~~~~~~~~
   We use `Epic.BellmanFord` to solve a restricted form of arithmetic "difference constraints"

     https://www.cs.upc.edu/~oliveras/TDV/dl.pdf

   which are of the form

      gv - gv' <= k

  (note that gv - gv' < k is the same as `gv - gv' <= k - 1`)

  To do this we build a graph where

    * vertices are ground values `gv`
    * each constraint `gv - gv' <= k` yields a directed edge from `gv` to `gv'` with weight `k`

  and then call `BellmanFord.negativeCycle` to search for a negative-weight cycle in the graph which
  indicates the constraints are unsatisfiable.

  We additionally account for constraints of the form k <= x  and x <= k

    1. by adding vertices for the literal `k` e.g. `vk`
    2. adding edges (vk, x, 0) and (x, vk, 0) and
    3. adding edges (v0, vk, -k) and (vk, v0, k)

  which then ends up creating a negative cycle e.g. if you have 10 <= x <= y <= z <= 5

  that goes from

      v10 -----> x -----> y -----> z -----> v5 ----[5]--> v0 ---[-10]--> v10

  (where edges without weights have weight 0)

 -}


arithGraph :: Solver -> [(Vertex, Vertex, Int)]
arithGraph s = nubOrd [ (GV u, GV v, w) | (u, v, w) <- edges ]
  where
    edges    = -- traceShow "ARITHGRAPH" $
               concatMap (arithEdges True)  (s_pos  s)
            ++ concatMap (arithEdges False) (s_neg  s)
            ++ concatMap litEdges           (s_lits s)
            ++ concatMap (defEdges s)       (s_def  s)

arithEdges :: Bool -> FailableAssump -> [(GroundVal, GroundVal, Int)]
arithEdges = go
  where
    go True  (A_RelOp LEq (GVArr [gv1, gv2])) = arithLEq gv1 gv2
    go True  (A_RelOp Lt  (GVArr [gv1, gv2])) = arithLt  gv1 gv2
    go True  (A_RelOp Gt  (GVArr [gv1, gv2])) = arithLt  gv2 gv1
    go True  (A_RelOp GEq (GVArr [gv1, gv2])) = arithLEq gv2 gv1
    go False (A_RelOp LEq (GVArr [gv1, gv2])) = arithLt  gv2 gv1
    go False (A_RelOp Lt  (GVArr [gv1, gv2])) = arithLEq gv2 gv1
    go False (A_RelOp Gt  (GVArr [gv1, gv2])) = arithLEq gv1 gv2
    go False (A_RelOp GEq (GVArr [gv1, gv2])) = arithLt  gv1 gv2
    go True  (A_GVEq  x   gv)                 = arithEq  (GVVar x) gv
    go _ _                                    = []

primOpResultPred_maybe :: PrimOp -> Maybe PrimOp
-- This function specifies what predicate is true of the result of a primop
primOpResultPred_maybe Add = Just IsInt
primOpResultPred_maybe Sub = Just IsInt
primOpResultPred_maybe Mul = Just IsInt
primOpResultPred_maybe Div = Just IsInt
primOpResultPred_maybe Neg = Just IsInt
primOpResultPred_maybe _   = Nothing


-- NOTE: Technically, defEdges is a "hack" that takes us outside the "difference constraint" logic...

defEdges :: Solver -> (Ident, (PrimOp, GroundVal)) -> [(GroundVal, GroundVal, Int)]
defEdges s (x, (Add, GVArr [gv1, gv2]))

  | Just (LInt k) <- evalsToLit s gv1
  = -- x = k + gv2 ====> x - gv2 = k ====>  k <= x - gv2 <= k ====> [gv2 - x <= -k, x - gv2 <= k],
  [ (gv2, GVVar x, fromIntegral (0 - k))
  , (GVVar x, gv2, fromIntegral k)  ]

  | Just (LInt k) <- evalsToLit s gv2
  = -- x = y + k ====> ... ====> [y - x <= -k, x - y <= k],
  [ (gv1, GVVar x, fromIntegral (0 - k))
  , (GVVar x, gv1, fromIntegral k)
  ]

defEdges s (x, (Sub, GVArr [gv1, gv2]))
  | Just (LInt k) <- evalsToLit s gv2
  = -- x = gv1 - k  ====> x - gv1 = -k  ====> -k <= x - gv1 <= -k ====> [gv1 - x <= k, x - gv1 <= -k]
  [ (gv1, GVVar x, fromIntegral k)
  , (GVVar x, gv1, fromIntegral (0-k))
  ]

defEdges _ _ = []

arithEq ::GroundVal -> GroundVal -> [(GroundVal, GroundVal, Int)]
arithEq gv1 gv2 = arithLEq gv1 gv2 ++ arithLEq gv2 gv1

arithLt :: GroundVal -> GroundVal -> [(GroundVal, GroundVal, Int)]
arithLt gv1 gv2 = [(gv1, gv2, -1)]

arithLEq :: GroundVal -> GroundVal -> [(GroundVal, GroundVal, Int)]
arithLEq gv1 gv2 = [(gv1, gv2, 0)]

litEdges :: Lit -> [(GroundVal, GroundVal, Int)]
litEdges l =
  case getLit l of
   Just i ->  [ (zero, vi, negate i), (vi, zero, i) ] where vi = GVLit l
   Nothing -> []

getLit :: Lit -> Maybe Int
getLit (LInt i) = Just (fromIntegral i)
getLit _        = Nothing

zero :: GroundVal
zero = GVLit (LInt 0)

------------------------------------------------------------------------------------
-- | Why is the solver returning UNSAT
------------------------------------------------------------------------------------
data UnsatReason
   = Contra    FailableAssump
   | Contra2   FailableAssump FailableAssump
   | DiseqLit  Lit   Lit
   | Arith    [GroundVal]
   deriving (Show)

instance Pretty UnsatReason where
  pPrint (Contra a)      = text "CONTRA"    <+> pPrint a
  pPrint (Contra2 a1 a2) = text "CONTRA2"   <+> pPrint a1 <+> pPrint a2
  pPrint (DiseqLit x y)  = text "DISEQ-LIT" <+> pPrint x <+> pPrint y
  pPrint (Arith xs)      = text "ARITH"     <+> pPrint xs

instance Pretty Equality where
  pPrint (MkEqual x y) = pPrint x <+> text "=" <+> pPrint y

--------------------------------------------------------------------------------
-- | Some tests below this
--------------------------------------------------------------------------------

-- >>> unsatAsms test4
-- Just (Arith [GVVar $I1,GVLit 0,GVVar $R10])

_test4 :: [Assump]
_test4 = [i1_gt_0, r10_eq_0, r5_eq_0, r10_eq_add_r5_1]
  where
    i1_gt_0         = A_Pos (A_RelOp Gt (GVArr [GVVar i1, zero]) )
    r10_eq_0        = A_Pos (A_GVEq r10 zero)
    r5_eq_0         = A_Pos (A_GVEq r5 zero)
    r10_eq_add_r5_1 = A_PrimOp r10 (AO_Prim Add) (GVArr [GVVar r5, GVVar i1])
    i1              = Bind.ident "$I1"
    r10             = Bind.ident "$R10"
    r5              = Bind.ident "$R5"

-- >>> unsatAsms test3
-- Just (Arith [GVVar $I1,GVLit 0,GVVar $R10])

_test3 :: [Assump]
_test3 = [i1_gt_0, r10_eq_0, r10_eq_add_0_1]
  where
    i1_gt_0         = A_Pos (A_RelOp Gt (GVArr [GVVar i1, zero]) )
    r10_eq_0        = A_Pos (A_GVEq r10 zero)
    r10_eq_add_0_1  = A_PrimOp r10 (AO_Prim Add) (GVArr [zero, GVVar i1])
    i1              = Bind.ident "$I1"
    r10             = Bind.ident "$R10"

-- >>> unsatAsms test2
-- Just (Arith [GVVar $I1,GVLit 0,GVVar $R10])

_test2 :: [Assump]
_test2 = [i1_gt_0, r10_eq_0, r10_eq_i1]
  where
    i1_gt_0   = A_Pos (A_RelOp Gt (GVArr [GVVar i1, zero]) )
    r10_eq_0  = A_Pos (A_GVEq r10 zero)
    r10_eq_i1 = A_Pos (A_GVEq r10 (GVVar i1))
    i1        = Bind.ident "$I1"
    r10       = Bind.ident "$R10"

-- >>> unsatAsms test1
-- Just (Arith [GVVar $I1,GVLit 0])

_test1 :: [Assump]
_test1 = [i1_gt_0, i1_eq_0]
  where
    i1_gt_0 = A_Pos (A_RelOp Gt (GVArr [GVVar i1, zero]) )
    i1_eq_0 = A_Pos (A_GVEq i1 zero)
    i1      = Bind.ident "$I1"


-- >>> unsatAsms test0
-- Just (Arith [GVVar $I1,GVLit 0])

_test0 :: [Assump]
_test0 = [i1_gt_zero, zero_gt_i1]
  where
    i1_gt_zero  = A_Pos (A_RelOp Gt (GVArr [GVVar i1, zero]) )
    zero_gt_i1  = A_Pos (A_RelOp Gt (GVArr [zero, GVVar i1]) )
    i1          = Bind.ident "$I1"

-- >>> unsatAsms test00
-- Just (DiseqLit 20 10)Nothing

_test00 :: [Assump]
_test00 = [i1_eq_10, i1_eq_20]
  where
    i1_eq_10 = A_Pos (A_GVEq i1 (GVLit (LInt 10)))
    i1_eq_20 = A_Pos (A_GVEq i1 (GVLit (LInt 20)))
    i1       = Bind.ident "$I1"
