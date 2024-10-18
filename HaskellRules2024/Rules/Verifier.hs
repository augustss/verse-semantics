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

import Data.Maybe ( isJust )
import Control.Monad (guard)
import Rules.Solver (unsat)

--------------------------------------------------------
--
--         The verifier's rules
--
--------------------------------------------------------

verificationRules ::  Rule
verificationRules = everywhere verificationStep <> everywhere recStep

verificationStep :: Rule
verificationStep =  TRS2024.runtimeAndVerificationStep
                 <> guardStep
                 <> verifyStep
                 <> splitStep
                 <> arrStep

--------------------------------------------------------------------------------
guardStep :: Rule
guardStep env lhs =
   "GUARD-ELIM" `nameWith`
   do v :>>: e <- [lhs]
      guard (TRS2024.skolValue (skolVars env) v)
      pure (pPrintSmallExpr v, e)

{- Guards only have values to the left
   ToDo: check in 'valid'
   ++
   "GUARD-FAIL" `name`
   do Fail :>>: _ <- [lhs]
      pure Fail
-}

--------------------------------------------------------------------------------

groundValue :: [SkolIdent] -> Expr -> Maybe GroundVal
-- Like skolValue, but no lambdas
groundValue _  (Lit l)               = Just (GVLit l)
groundValue rs (Var v) | v `elem` rs = Just (GVVar v)
groundValue rs (Tup vs)              = do { gvs <- mapM (groundValue rs) vs; Just (GVArr gvs) }
groundValue rs (Tru v)               = do gv <- groundValue rs v; Just (GVTru gv)
groundValue _  _                     = Nothing

--------------------------------------------------------------------------------

arrStep :: Rule
--   C[ P[ DotDot$[x,n] ]
--     ---> if x is in flexis(P)
--   verify(R,n;A){ P[ choose(n){x=some(inrange[n]);()} ] }
arrStep env lhs =
   "DD-NARROW" `nameWith`
  do (exis, ctx, e1@(Op DotDot :@: Tup [Var x, v])) <- evalCtxLift (free lhs) lhs
     -- Use this rule when v is not a literal.
     guard (x `elem` exis)
     let i = identNotIn (free v)
     pure (pPrint e1, wrapExis exis $
                      ctx <@ (Choose (SizeIs v)
                                ((Var x :=: Some(Lam $ bind i (inRange (Var i) v)))
                                 :>: Tup [])))

  ++
  "DD-INRANGE" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, e1@(Op DotDot :@: Tup [i, sz]))) <- proofX all_rs e
      guard (isJust (groundValue all_rs i))
      pure (pPrint e1, Verify $ bindList rs
                         (as, ctx <@ inRange i sz))

 ++
  "ARR-MAP" `nameWith`   -- ArrMap$[f, Arr(n){e}]
                         --   --> x:=some(\_.e); f[x]; Arr(n){f[e]}
  do Op ArrMap :@: arg@(Tup [f, arr@(Arr v e)]) <- [lhs]
     let x:y:_ = identsNotIn $ free arg
     pure (pPrint arr, Exi $ bind x $
                       coreSeq [ Var x :=: Some (Lam (bind underscore e))
                               , Var underscore :=: (f :@: Var x)
                               , Arr v (Exi $ bind y $ (Var y :=: e) :>: (f :@: Var y))] )
 ++
  "APP-ARR" `nameWith`  -- (Arr n e)[v] --> Dotdot$[v,n]; some(\_.e)
  do arr@(Arr (SizeIs sz) e) :@: v <- [lhs]
     pure (pPrint arr, (Var underscore :=: (Op DotDot :@: Tup [v,sz])) :>:
                       (Some $ Lam $ bind underscore e) )
  ++
  "CHOOSE-EXPAND" `name`
  do (ctx, Choose sz e) <- evalCtx [] lhs
     guard (ctx /= HOLE)
     let new_sz | choiceAndFailureFree ctx = sz
                | otherwise                = Dunno
     pure (Choose new_sz (ctx <@ e))
  ++
  "ALL-CHOOSE" `name`
  do All (Choose sz e) <- [lhs]
     pure (Arr sz e)
 ++
  "U-ARR" `name`
  do (Arr sz1 e1 :=: Arr sz2 e2) :>: e <- [lhs]
     let x = identNotIn $ free lhs
         add_unif_size body
          = case (sz1,sz2) of
              (SizeIs n1, SizeIs n2) -> (n1 :=: n2) :>: body
              _                      -> body
     pure (add_unif_size $
           (Exi $ bind x $
           ((Var x :=: (Some $ Lam $ bind underscore e1)) :>:
            (Var x :=: (Some $ Lam $ bind underscore e2)) :>:
            e)))
  ++
  "SKOL-ARR-SIZE" `nameWith`  -- Make Arr(Dunno){e} behave like Arr(some(nat)){e}
                              -- by skolemising the some(nat)
  do (all_rs, rs, as, e) <- matchVerify env lhs
     (ctx, (_, Arr Dunno eb)) <- proofX all_rs e
     guard (blocked ctx)
     let x  = identNotIn (occurs ctx)
         r  = skolNotIn all_rs
     pure ( sep [ text "r=" <> pPrint r, text "x=" <> pPrint x
                , text "rs=" <> pPrint rs ]
          , Verify $ bindList (r:rs)
                (as, Exi $ bind x $
                     Var x :=: (nat :@: Var r) :>: (ctx <@ Arr (SizeIs (Var x)) eb)))

--------------------------------------------------------------------------------
verifyStep :: Rule
verifyStep env lhs =
   "VERIFY-VAL" `name`
   do (_skols, _rs, _as, v) <- matchVerify env lhs
      guard (isVal v)
      pure (Tup [])
   ++
   "VERIFY-FAIL" `name`
   do (_skols, _rs, _as, e) <- matchVerify env lhs
      guard (e == Fail)
      pure (Tup [])
   ++
   "VERIFY-CHOICE" `name`
   do (_skols, rs, as, e1 :|: e2) <- matchVerify env lhs
      pure (  (Var underscore :=: (Verify $ bindList rs (as,e1)))
          :>: (Verify $ bindList rs (as,e2)) )
   ++
   "SOLVER" `nameWith`
   do (_skols, rs, as, _e) <- matchVerify env lhs
      let env' = extendRuleEnv env rs as
      case unsat env' of
        Just reason -> pure (pPrint reason, Tup [])
        Nothing     -> []
   ++
   "SKOLEMIZE" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Some v)) <- proofX all_rs e
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
   "SPLIT-V" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, (Var r :=: v) :>: rest)) <- proofX all_rs e
      guard (r `elem` all_rs)
      Just gv <- [groundValue all_rs v]
      pure ( pPrint r <+> text "=" <+> pPrint v
           , caseSplit rs (A_GVEq r gv) as ctx rest )

   ++
   "SPLIT-OP" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Op op :@: arg)) <- proofX all_rs e
      guard (op /= IsArr && op /= DotDot)   -- ToDo: this is a bit awkward
           -- Can't split on DotDot because it produces many results
      Just gv <- [groundValue all_rs arg]
      guard (free gv `intersects` all_rs)
          -- At least one skolem in gv
          -- Don't do SPLIT-OP on (3+4)
      let r    = skolNotIn all_rs
          asm  = A_PrimOp r (AO_Prim op) gv
          asmF = A_RelOp op gv
      if primOpCanFail op
        then pure (pPrint asmF, caseSplit (r:rs) asmF as ctx (Var r))
        else pure (pPrint asm, Verify (bindList (r:rs) (asm : as, ctx <@ Var r)))
        -- Generate one or two 'verify' blocks, depending on
        -- whether or not the PrimOp can fail

   ++
   "SPLIT-ISARR" `nameWith`
       -- verify(R,r;A){ P[ isArr$[r] ] }
       --  --> verify(R,r;A,isArr$[r]){ P[ Arr(.){some(any)} ] }
       --      ..and the fail case..
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Op IsArr :@: Var r)) <- proofX all_rs e
      guard (r `elem` all_rs)   -- r is a skolem
      let asmF = A_RelOp IsArr (GVVar r)
      pure (pPrint asmF, caseSplit rs asmF as ctx (Arr Dunno someAny))
   ++
   "SPLIT-TUP" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Var r :=: Tup vs :>: rest)) <- proofX all_rs e
      guard (r `elem` rs)
      let rs'  = take (length vs) (skolsNotIn all_rs)
          rvs' = foldr (:>:) rest [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
          asm    = A_GVEq r (GVArr (map GVVar rs'))
      pure (pPrint asm, caseSplit (rs ++ rs') asm as ctx rvs')

   ++
   "SPLIT-TRU" `nameWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Var r :=: Tru v :>: rest)) <- proofX all_rs e
      guard (r `elem` rs)
      let r'  = skolsNotIn all_rs !! 0
          rv' = (Var r' :=: v) :>: rest
          asm    = A_GVEq r (GVTru (GVVar r'))
      pure (pPrint asm, caseSplit (rs ++ [r']) asm as ctx rv')

{- SPJ: I am not sure if we need SPLIT-APP at all.
        So I am commenting it out for now.
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
-}

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

proofX :: [Ident] -> Expr -> [( Context    -- The context
                              , ([Ident]   -- Flexible existentials bound by context
                              ,  Expr ))]  -- The expression in the hole
-- P context
proofX bs lhs = go_px (LX { exi_flexi = [], exi_rigid = bs }) lhs

go_px :: LocalExis -> Expr -> [(Context, ([Ident], Expr))]
go_px lx lhs =
   pure (HOLE, (exi_flexi lx, lhs))
 ++
   do x :>: e <- [lhs]
      (ctx, hole) <- go_px lx x
      pure (ctx :>: e, hole)
 ++
   do cf :>: x <- [lhs]
      guard (TRS2024.choiceFreeLH cf)
      (ctx, hole) <- go_px lx x
      pure (cf :>: ctx, hole)
 ++
   do v :=: x <- [lhs]
      (ctx, hole) <- go_px lx x
      pure (v :=: ctx, hole)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename (allExis lx) bnd
     (ctx, hole) <- go_px (addFlexi lx x) e
     pure (Exi (bind x ctx), hole)
 ++
  do x :|: e  <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) x
     pure (ctx :|: e, hole)
 ++
  do e :|: x  <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) x
     pure (e :|: ctx, hole)
 ++
  do x :>>: e  <- [lhs]
     (ctx, hole) <- go_px lx x
     pure (ctx :>>: e, hole)
 ++
  do Check fx x <- [lhs]
     (ctx, hole) <- go_px lx x
     pure (Check fx ctx, hole)
 ++
  do All x <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) x
     pure (All ctx, hole)
 ++
  do Iter x y z w <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) x
     pure (Iter ctx y z w, hole)
