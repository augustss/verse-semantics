{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}

module Rules.Verifier(
    verificationRules
  ) where

import TRS.Bind
import Rules.Core
import Rules.TRS2024 as TRS2024
import Epic.Print hiding ( (<>) )
import qualified Epic.UnionFind as UF

import Control.Monad (guard)
import Data.List ( (\\), nub )


--------------------------------------------------------
--
--         The verifier's rules
--
--------------------------------------------------------

verificationRules ::  Rule
verificationRules
  =  TRS2024.evalRules
  <> guardRules
  <> checkRules
  <> verifyRules
  <> splitRules

--------------------------------------------------------------------------------
guardRules :: Rule
guardRules env lhs =
   "GUARD-ELIM" `nameWith`
   do v :>>: e <- [lhs]
      guard (skolValue (skolVars env) v)
      pure (pPrintSmallExpr v, e)
   ++
   "GUARD-FAIL" `name`
   do Fail :>>: _ <- [lhs]
      pure Fail

--------------------------------------------------------------------------------
checkRules :: Rule
checkRules env lhs =
   "CHECK-SUC" `name`
   do Check eff v <- [lhs]
      guard (skolValue (skolVars env) v)
      guard (canSucceed eff)
      pure v
   ++
   "CHECK-FAIL" `name`
   do Check eff Fail <- [lhs]
      guard (canFail eff)
      pure Fail

skolValue :: [SkolIdent] -> Expr -> Bool
-- A value whose only free vars are skolems
skolValue rs e = isVal e && null (free e \\ rs)

groundValue :: [SkolIdent] -> Expr -> Maybe GroundVal
-- Like skolValue, but no lambdas
groundValue _  (Lit l)               = Just (GVLit l)
groundValue rs (Var v) | v `elem` rs = Just (GVVar v)
groundValue rs (Arr vs)              = do { gvs <- mapM (groundValue rs) vs; Just (GVArr gvs) }
groundValue _  _                     = Nothing


--------------------------------------------------------------------------------
verifyRules :: Rule
verifyRules env lhs =
   "VERIFY-VAL" `name`
   do (_env', _rs, _as, v) <- matchVerify env lhs
      guard (isVal v)
      pure (Arr [])
   ++
   "VERIFY-FAIL" `name`
   do (_env', _rs, _as, e) <- matchVerify env lhs
      guard (e == Fail)
      pure (Arr [])
   ++
   "SOLVER" `name`
   do (env', _rs, _as, _e) <- matchVerify env lhs
      guard (unsat env')
      pure (Arr [])
   ++
   "SKOLEMIZE" `nameWith`
   do (env', rs, as, e) <- matchVerify env lhs
      (ctx, Some v) <- proofX [] e
      let all_rs = skolVars env'
      guard (skolValue all_rs v)
      guard (blocked ctx)
      let x  = identNotIn (occurs ctx)
          r  = skolNotIn all_rs
      pure ( pPrint (r,x,rs, all_rs)
           , Verify $ bindList (r:rs)
                 (as, Exi $ bind x $
                    Var x :=: (v :@: Var r) :>: (ctx <@ Var x) ))

{-   -- SLPJ: what is this rule?
   ++
   "SUBST-ASM" `name`
   -- VERIFY(rs, A[r = hnf]) { e } ---> VERIFY(rs, A{hnf/r}[r = hnf]) { e{hnf/r} }
   do Verify rs as e <- [lhs]
      (as1, a@(A_GVEq r h), as2) <- asmX as
      guard (not (isArr h))
      guard (r `elem` rs)
      pure $ Verify rs ( subst [(r, h)] as1 ++ [a] ++ subst [(r, h)] as2) ( subst [(r, h)] e)

isArr :: Expr -> Bool
isArr (Arr _) = True
isArr _       = False

asmX :: [a] -> [([a], a, [a])]
asmX = go []
  where
    go _  []      = []
    go as (a:as') = (reverse as, a, as') : go (a:as) as'
-}

unsat :: RuleEnv -> Bool
-- `unsat` is a simple unsatisfiablity checker,
-- which implements the SOLVER rule
unsat (RE { assumps = asms }) = contra pos neg || refl pos neg || unsatCC cc pos neg
  where
    pos, neg :: [FailableAssump]
    pos = [asm | A_Pos asm <- asms] ++ [ A_RelOp op gv | A_PrimOp _ (AO_Prim op) gv <- asms ]
    neg = [asm | A_Neg asm <- asms]
    cc  = mkCC pos neg

-- Looks for (a; not a)
contra :: [FailableAssump] -> [FailableAssump] -> Bool
contra pos neg = any (`elem` neg) pos

-- Looks for x=x; isInt[x] etc.
refl :: [FailableAssump] -> [FailableAssump] -> Bool
refl pos neg = not . null $ [ x | (A_GVEq x (GVVar y)) <- neg, x == y, any (isPrim x) pos ]

isPrim :: Ident -> FailableAssump -> Bool
isPrim x (A_RelOp op (GVVar y)) = x == y && isTyOp op
isPrim _ _                      = False

isTyOp :: PrimOp -> Bool
isTyOp IsInt  = True
isTyOp IsChar = True
isTyOp IsStr  = True
isTyOp _      = False

unsatCC :: CC -> [FailableAssump] -> [FailableAssump] -> Bool
unsatCC cc _pos neg = contraVars || contraLits
  where
   -- [contraVars] exi x, x'. x == x'     in neg, s.t. repr x == repr x'
   contraVars = any (uncurry (UF.equal (eq_uf cc))) diseqVars
   diseqVars  = [(GVVar x, y) | A_GVEq x y <- neg]

   -- [unsatLits] exi k, k'. k != k'           , s.t. repr k == repr k'
   contraLits = length lits /= length (nub litReps)
   litReps    = UF.find (eq_uf cc) <$> lits
   lits       = nub (eq_lits cc)

{-
unsatCC cc pos neg ==> true  if
   - [contraVars] exi x, x'. x == x'     in neg, s.t. repr x == repr x'
   - [unsatLits] exi k, k'. k != k'           , s.t. repr k == repr k'

   - exi k, x . isPrimTy[x] in pos, s.t. repr k == repr x, not isPrimTy[k]
   - exi k, x . isPrimTy[x] in neg, s.t. repr k == repr x,     isPrimTy[k]
   - exi k    . isPrimTy[k] in pos, s.t.                   not isPrimTy[k]
   - exi k    . isPrimTy[k] in neg, s.t.                 ,     isPrimTy[k]
-}

data CC = MkCC
  { eq_lits  :: [GroundVal]
  , eq_uf    :: UF.UF GroundVal
  }

mkCC :: [FailableAssump] -> [FailableAssump] -> CC
mkCC pos neg = MkCC lits ufg
  where
    lits = assumpGroundVal <$> (pos ++ neg)
    ufg   = foldr (\(x, y) uf -> UF.union uf x y) UF.new eqs
    eqs  = [(GVVar x, y) | A_GVEq x y <- pos]

assumpGroundVal :: FailableAssump -> GroundVal
assumpGroundVal (A_GVEq _ gv) = gv
assumpGroundVal (A_RelOp _ gv) = gv

--------------------------------------------------------------------------------


splitRules :: Rule
splitRules env lhs =
   "SPLIT-K" `name`
   do (env', rs, as, e) <- matchVerify env lhs
      (ctx, (Var r :=: v) :>: rest) <- proofX [] e
      guard (r `elem` rs)
      Just gv <- [groundValue (skolVars env') v]
      pure (caseSplit rs (A_GVEq r gv) as ctx rest)

   ++
   "SPLIT-OP" `nameWith`
   do (env', rs, as, e) <- matchVerify env lhs
      let skol_rs = skolVars env'
      (ctx, Op op :@: arg) <- proofX [] e
      Just gv <- [groundValue skol_rs arg]
      guard (free gv `intersects` skol_rs)
          -- At least one skolem in gv
          -- Don't do SPLIT-OP on (3+4)
      let r    = skolNotIn skol_rs
          asm  = A_PrimOp r (AO_Prim op) gv
          asmF = A_RelOp op gv
      if primOpCanFail op
        then pure (pPrint asm, caseSplit (r:rs) asmF as ctx (Var r))
        else pure (pPrint asm, Verify (bindList (r:rs) (asm : as, ctx <@ Var r)))
        -- Generate one or two 'verify' blocks, depending on
        -- whether or not the PrimOp can fail

   ++
   "SPLIT-TUP" `nameWith`
   do (env', rs, as, e) <- matchVerify env lhs
      (ctx, Var r :=: Arr vs :>: rest) <- proofX [] e
      guard (r `elem` rs)
      let rs'  = take (length vs) (skolsNotIn (skolVars env'))
          rvs' = foldr (:>:) rest [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
          asm    = A_GVEq r (GVArr (map GVVar rs'))
      pure (pPrint asm, caseSplit (rs ++ rs') asm as ctx rvs')

   ++
   -- Verify(rs ; as){ P[r[s]] }
   -- Verify(rs ; as){ P[r[s]] }
   -- Verify(rs ; as){ P[r[s]] }
   -- Verify(rs ; as){ P[r[s]] }
   -- Verify(rs ; as){ P[r[s]] }
   -- Verify(rs ; as){ P[r[s]] }
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh

   -- Verify(rs ; as){ P[r[s]] }
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   "SPLIT-APP" `nameWith`
   do (env', rs, as, e) <- matchVerify env lhs
      (ctx, Var r :@: s) <- proofX [] e
      let skol_rs = skolVars env'
      guard (r `elem` rs)
      Just gv <- [groundValue skol_rs s]
      let r' = skolNotIn skol_rs
          asm = A_PrimOp r' AO_Apply (GVArr [GVVar r, gv])
      pure (pPrint asm, Verify (bindList (r':rs) (asm : as, ctx <@ Var r')))

matchVerify :: RuleEnv -> Expr -> [(RuleEnv, [SkolIdent], [Assump], Expr)]
matchVerify env (Verify bnd) = [(extendRuleEnv env rs as, rs, as, e)]
                             where
                               (rs, (as, e)) = alphaRenameVerify (skolVars env) bnd
matchVerify _ _ = []

{-
   ++
   "SPLIT-C" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: Char _)) <- proofX e
      guard (isUni rs bs (Var r))
      pure (a, caseSplit rs a as ctx (Arr []))
   ++
   "SPLIT-VAR" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: Var r')) <- proofX e
      guard (isUni rs bs (Var r))
      guard (isUni rs bs (Var r'))
      pure (a, caseSplit rs a as ctx (Arr []))
   ++
   "SPLIT-PPRED" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(op :@: arg)) <- proofX e
      guard (isUni rs bs arg)
      guard (isPrim1 op)
      pure (a, caseSplit rs a as ctx (Arr []))
   ++
   -- Verify(rs ; as){ P[r[s]] } ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   "SPLIT-APP" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :@: s)) <- proofX e
      let r' = uvIdentNotIn (rs ++ free e ++ bndIds bs ++ boundVars env)
      guard (isUni rs bs (Var r))
      guard (isUni rs bs s)
      pure (a, Verify (r':rs) (Assume (Var r' :=: a) : as) (ctx (Var r')))

isUni :: [Ident] -> [BndVar] -> Expr -> Bool
isUni rs bs (Var r)  = r `elem` rs && r `notElem` bndIds bs
isUni _  _  (Int _)  = True
isUni _  _  (Char _) = True
isUni rs bs (Arr es) = all (isUni rs bs) es
isUni _  _  _        = False

isPrim1 :: Expr -> Bool
isPrim1 (Op IsInt)  = True
isPrim1 (Op IsStr)  = True
isPrim1 (Op Gt)     = True
isPrim1 (Op Ge)     = True
isPrim1 (Op Lt)     = True
isPrim1 (Op Le)     = True
isPrim1 (Op Ne)     = True
isPrim1 _           = False

isPrimOp1 :: Expr -> Bool
isPrimOp1 (Op Add) = True
isPrimOp1 (Op Sub) = True
isPrimOp1 (Op Mul) = True
isPrimOp1 (Op Div) = True
isPrimOp1 (Op Neg) = True
isPrimOp1 _        = False

-}

caseSplit :: [Ident] -> FailableAssump -> [Assump] -> Context -> Expr -> Expr
caseSplit rs a as ctx e
  = (Var underscore :=: Verify (bindList rs (A_Neg a : as, ctx <@ Fail)))
    :>:
    Verify (bindList rs (A_Pos a : as, ctx <@ e))

--------------------------------------------------------------------------------
-- | Contexts ------------------------------------------------------------------
--------------------------------------------------------------------------------

proofX :: [Ident] -> Expr -> [(Context, Expr)]
-- P context
proofX bs lhs =
   pure (HOLE,lhs)
 ++
   do x :>: e <- [lhs]
      (ctx, hole) <- proofX bs x
      pure (ctx :>: e, hole)
 ++
   do cf :>: x <- [lhs]
      guard (TRS2024.choiceFree cf)
      (ctx, hole) <- proofX bs x
      pure (cf :>: ctx, hole)
 ++
   do v :=: x <- [lhs]
      (ctx, hole) <- proofX bs x
      pure (v :=: ctx, hole)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename bs bnd
     (ctx, hole) <- proofX (x : bs) e
     pure (Exi (bind x ctx), hole)
 ++
  do One x <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (One ctx, hole)
 ++
  do All x <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (All ctx, hole)
 ++
  do x :|: e  <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (ctx :|: e, hole)
 ++
  do e :|: x  <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (e :|: ctx, hole)
 ++
  do x :>>: e  <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (ctx :>>: e, hole)
 ++
  do Check fx x <- [lhs]
     (ctx, hole) <- proofX bs x
     pure (Check fx ctx, hole)
