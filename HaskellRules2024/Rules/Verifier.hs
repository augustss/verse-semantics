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

import Control.Monad (guard)
import Data.List ((\\))
import Rules.Solver (unsat)

--------------------------------------------------------
--
--         The verifier's rules
--
--------------------------------------------------------

verificationRules ::  Rule
verificationRules = everywhere verificationStep <> everywhere recStep

verificationStep :: Rule
verificationStep =  TRS2024.evalStep
                 <> guardStep
                 <> checkStep
                 <> verifyStep
                 <> splitStep

--------------------------------------------------------------------------------
guardStep :: Rule
guardStep env lhs =
   "GUARD-ELIM" `nameWith`
   do v :>>: e <- [lhs]
      guard (skolValue (skolVars env) v)
      pure (pPrintSmallExpr v, e)
   ++
   "GUARD-FAIL" `name`
   do Fail :>>: _ <- [lhs]
      pure Fail

--------------------------------------------------------------------------------
checkStep :: Rule
checkStep env lhs =
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
groundValue rs (Lam bnd)
  | rs `includes` free bnd           = Just (GVLam bnd)
groundValue _  _                     = Nothing


--------------------------------------------------------------------------------
verifyStep :: Rule
verifyStep env lhs =
   "VERIFY-VAL" `name`
   do (_skols, _rs, _as, v) <- matchVerify env lhs
      guard (isVal v)
      pure (Arr [])
   ++
   "VERIFY-FAIL" `name`
   do (_skols, _rs, _as, e) <- matchVerify env lhs
      guard (e == Fail)
      pure (Arr [])
   ++
   "SOLVER" `nameWith`
   do (_skols, rs, as, _e) <- matchVerify env lhs
      let env' = extendRuleEnv env rs as
      case unsat env' of
        Just reason -> pure (pPrint reason, Arr [])
        Nothing     -> []
   ++
   "SKOLEMIZE" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, Some v) <- proofX all_rs e
      guard (skolValue all_rs v)
      guard (blocked ctx)
      let x  = identNotIn (occurs ctx)
          r  = skolNotIn all_rs
      pure ( sep [ text "r=" <> pPrint r, text "x=" <> pPrint x
                 , text "rs=" <> pPrint rs ]
           , Verify $ bindList (r:rs)
                 (as, Exi $ bind x $
                    Var x :=: (v :@: Var r) :>: (ctx <@ Var x) ))


--------------------------------------------------------------------------------


splitStep :: Rule
splitStep env lhs =
   "SPLIT-K" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (Var r :=: v) :>: rest) <- proofX all_rs e
      guard (r `elem` all_rs)
      Just gv <- [groundValue all_rs v]
      pure ( pPrint r <+> text "=" <+> pPrint v
           , caseSplit rs (A_GVEq r gv) as ctx rest )

   ++
   "SPLIT-OP" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, Op op :@: arg) <- proofX all_rs e
      Just gv <- [groundValue all_rs arg]
      guard (free gv `intersects` all_rs)
          -- At least one skolem in gv
          -- Don't do SPLIT-OP on (3+4)
      let r    = skolNotIn all_rs
          asm  = A_PrimOp r (AO_Prim op) gv
          asmF = A_RelOp op gv
      if primOpCanFail op
        then pure (pPrint asm, caseSplit (r:rs) asmF as ctx (Var r))
        else pure (pPrint asm, Verify (bindList (r:rs) (asm : as, ctx <@ Var r)))
        -- Generate one or two 'verify' blocks, depending on
        -- whether or not the PrimOp can fail

   ++
   "SPLIT-TUP" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, Var r :=: Arr vs :>: rest) <- proofX all_rs e
      guard (r `elem` rs)
      let rs'  = take (length vs) (skolsNotIn all_rs)
          rvs' = foldr (:>:) rest [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
          asm    = A_GVEq r (GVArr (map GVVar rs'))
      pure (pPrint asm, caseSplit (rs ++ rs') asm as ctx rvs')

   ++
   -- Verify(rs ; as){ P[r[s]] }
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   "SPLIT-APP" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, Var r :@: s) <- proofX all_rs e
      guard (r `elem` rs)
      Just gv <- [groundValue all_rs s]
      let r' = skolNotIn all_rs
          asm = A_PrimOp r' AO_Apply (GVArr [GVVar r, gv])
      pure (pPrint asm, Verify (bindList (r':rs) (asm : as, ctx <@ Var r')))

matchVerify :: RuleEnv -> Expr -> [([SkolIdent], [SkolIdent], [Assump], Expr)]
matchVerify env (Verify bnd)
  = [(all_rs, new_rs, as, e)]
  where
    env_rs = skolVars env
    all_rs = new_rs ++ env_rs
    (new_rs, (as, e)) = alphaRenameVerify env_rs bnd
matchVerify _ _ = []

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
