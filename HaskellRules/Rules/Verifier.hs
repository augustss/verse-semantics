{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}

module Rules.Verifier(
  allSystemsVerify,
  verifier,
  verify,
  verifyM,
  wrapAssert,
  ) where
import TRS.Bind
import TRS.TRS hiding (step)
import TRS.Traced
import TRS.Tarjan
import Rules.Core hiding (isHNF)
import qualified Epic.SIntMap as IM
import Epic.Print (prettyShow, Pretty)
import qualified Debug.Trace as Debug
-- import qualified Rules.OldVerifier as Old
-- import Rules.ICFP ({- systemICFPE, -} isChoiceFree)
import Control.Monad (guard)
import Data.List ((\\))
import Rules.TRS2024 (isEffectFree)
import qualified Rules.TRS2024 as TRS2024

-- | Run verification rules.
_traceShow :: (Pretty a) => String -> a -> a
_traceShow msg x = Debug.trace ("TRACE: " ++ msg ++ prettyShow x) x

verifyM :: TRSystem Expr -> Expr -> Result (Bool, Traced Expr)
verifyM sys e = res
 where
   res =
     case tarjan1 (tfNormSteps (ruleEnv sys)) arrow (start e) of
       Finish (tr@(x :<-- _):_) -> Finish (isDone x, tr)
       Timeout (tr:_) -> Timeout (False, tr)
       _ -> undefined -- should never happen as tarjan1 returns a single trace?

   arrow (a :<-- t)       = [ b :<-- ((r,a):t) | (r,b) <- next a ]
   next a = {- _traceShow ("STEPS: " ++ prettyShow a) $ -} stepS sys a

verify :: TRSystem Expr -> Expr -> (Bool, Traced Expr)
verify sys e =
  let sys' = sys{ ruleEnv = (ruleEnv sys){ tfNormSteps = 20000 } }
  in  case verifyM sys' e of
         Finish (b, tr) -> (b, tr)
         Timeout (_, tr) -> (False, tr)

wrapAssert :: Expr -> Expr
wrapAssert = Assert

-- | `isDone e` ignores "assert/decide" that occur under `verify` which are themselves
--   under TOP-LEVEL lambdas, as those are obligations for higher-order args that are checked at
--   *callsites* of those lambdas. There is probably some clever way to do
--   the below _just_ using `collect` but I thought I'd write this out first.
isDone :: Expr -> Bool
isDone = go False False
 where
  go :: Bool -> Bool -> Expr -> Bool
  go _   _   (Assert _)       = False
  go _   _   (Decide _)       = False
  go _   lam (Verify _ _ e)   = lam || go True False e
  go ver _   (Lam (Bind _ e)) = go ver (not ver) e
  go ver _   (Uni (Bind _ e)) = go ver (not ver) e

  go ver lam (Arr es)         = all (go ver lam) es
  go _ _ (Map _) = error "isDone: Map not implemented"
  go ver lam (Exi (Bind _ e)) = go ver lam e
  -- go ver lam (Uni (Bind _ e)) = go ver lam e
  go ver lam (e1 :=: e2)      = go ver lam e1 && go ver lam e2
  go ver lam (e1 :|: e2)      = go ver lam e1 && go ver lam e2
  go ver lam (e1 :>: e2)      = go ver lam e1 && go ver lam e2
  go ver lam (e1 :>>: e2)      = go ver lam e1 && go ver lam e2
  go ver lam (e1 :@: e2)      = go ver lam e1 && go ver lam e2
  go ver lam (One e)          = go ver lam e
  go ver lam (All e)          = go ver lam e
  go ver lam (Fails  e)       = go ver lam e
  go ver lam (Split x y z)    = all (go ver lam) [x,y,z]
  go ver lam (Store h e)      = all (go ver lam) (IM.elems h) && go ver lam e
  go ver lam (If e1 e2 e3)    = all (go ver lam) [e1, e2, e3]
  go ver lam (BlockC e)       = go ver lam e
  go ver lam (IfB (Bind _ e)) = go ver lam e
  go _   _   (Var _)          = True
  go _   _   (Int _)          = True
  go _   _   (Char _ )        = True
  go _   _   (Path _ )        = True
  go _   _   (Op _)           = True
  go _   _   (_ :~:  _)       = True
  go _   _   Fail             = True
  go _   _   (Assume _)       = True
  go _   _   (Some _)         = True
  go _   _   (Ref _)          = True
  go _  _    (Wrong _)        = False -- should this be True?
  go _  _    OLam{}           = error "isDone: OLam not implemented"

verifier :: TRSystem Expr
verifier = splitVerifier

allSystemsVerify :: [TRSystem Expr]
allSystemsVerify = [{- Old.icfpeVerifier, -} splitVerifier]

splitVerifier :: TRSystem Expr
splitVerifier = TRS2024.systemTRS2024 -- systemICFPE
  { sname = "SPLITverify"
  , description = "ICFPE + split verifier rules"
  , rules =     -- (rules systemICFPE -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL"  -= "FAIL-ELIM" )
                --  <> Old.generalizedIcfpRules
                 rules TRS2024.systemTRS2024
              <> ifRules
              <> substRules
              <> guardRules
              <> checkRules
              <> verifyRules
              <> splitRules
   -- TODO:CHECK-FAIL
   -- TODO:CHECK-CHOICE
   -- TODO:SPLIT-PFUN
  , displayRules = const True
  }

------------------------------------------------------------------
isHNF :: TRSFlags -> Expr -> Bool
isHNF _   (HNF _) = True
isHNF env (Var x) = x `elem` rigidVars env
isHNF _   _       = False

ifRules :: Rule Expr
ifRules env lhs =
   "IF-TRUE" `name`
   do If b e _ <- [lhs]
      guard (isHNF env b)
      pure e
   ++
   "IF-FALSE" `name`
   do If Fail _ e <- [lhs]
      pure e

------------------------------------------------------------------
substRules :: Rule Expr
substRules _ lhs =
   "SUBST1" `name`
   do EXI x e <- [lhs]
      (ctx, Var x' :=: Val v) <- substX e
      guard (x == x')
      guard (x `notElem` (free v))
      pure (subst [(x, v)] (ctx (Arr [])))

--------------------------------------------------------------------------------
guardRules :: Rule Expr
guardRules _ lhs =
   "GUARD-ELIM" `name`
   do Val _ :>>: e <- [lhs]
      pure e
   ++
   "GUARD-FAIL" `name`
   do Fail :>>: _ <- [lhs]
      pure Fail

--------------------------------------------------------------------------------
checkRules :: Rule Expr
checkRules env lhs =
   "CHECK-SUC" `name`
   do Assert (Val v) <- [lhs]
      guard (skol (rigidVars env) v)
      pure (Val v)
   ++
   "CHECK-DEC" `name`
   do Decide (Val v) <- [lhs]
      guard (skol (rigidVars env) v)
      pure (Val v)
   ++
   "CHECK-FAIL-DEC" `name`
   do Decide (Fail) <- [lhs]
      pure Fail
   -- ++
   -- we use Fails{e} as (~e) so don't want `fails` to escape that
   -- "CHECK-FAIL-FAILS" `name`
   -- do Fails (Fail) <- [lhs]
   --    pure Fail

skol :: [Ident] -> Expr -> Bool
skol rs e = null (free e \\ rs)

--------------------------------------------------------------------------------
verifyRules :: Rule Expr
verifyRules env lhs =
   "VERIFY-VAL" `name`
   do Verify _ _ (Val _) <- [lhs]
      pure (Arr [])
   ++
   "VERIFY-FAIL" `name`
   do Verify _ _ Fail <- [lhs]
      pure (Arr[])
   ++
   "SMT" `name`
   do Verify rs as _ <- [lhs]
      guard (unsat rs as)
      pure (Arr [])
   ++
   "SKOLEMIZE" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, Some (Val v)) <- proofX [] e
      guard (skol rs v)
      let xs = rs ++ free e ++ bndIds bs ++ boundVars env
      let x  = identNotIn xs
          r  = uvIdentNotIn (x : xs)
      pure $ Verify (r:rs) as (EXI x (Var x :=: (v :@: Var r) :>: ctx (Var x)))
   ++
   "SUBST-ASM" `name`
   -- VERIFY(rs, A[r = hnf]) { e } ---> VERIFY(rs, A{hnf/r}[r = hnf]) { e{hnf/r} }
   do Verify rs as e <- [lhs]
      (a@(Assume (Var r :=: HNF h)), as') <- asmX as
      guard (r `elem` rs)
      pure $ Verify rs (a: subst [(r, h)] as') ( subst [(r, h)] e)



asmX :: [a] -> [(a, [a])]
asmX = go []
  where
    go _  []      = []
    go as (a:as') = (a, as++as') : go (a:as) as'

unsat :: [Ident] -> [Expr] -> Bool
unsat _ es = asmFail || contra || _refl
  where
    asmFail = not . null $ [ e | Assume e@Fail <- es ] ++ [ e | Fails e@(HNF _) <- es ]
    _refl    = not . null $ [ e | Fails e@(Var x :=: Var y) <- es, x == y, isInt asms x ]
    asms    = [ e | Assume e <- es ]
    contra  = any (`elem` pos) neg
    pos     = [ e | Assume e <- es ]
    neg     = [ e | Fails  e <- es ]

isInt :: [Expr] -> Ident -> Bool
isInt asms x = INT (Var x) `elem` asms

--------------------------------------------------------------------------------

isUni :: [Ident] -> [BndVar] -> Expr -> Bool
isUni rs bs (Var r)  = r `elem` rs && r `notElem` (bndIds bs)
isUni _  _  (Int _)  = True
isUni _  _  (Char _) = True
isUni rs bs (Arr es) = all (isUni rs bs) es
isUni _  _  _        = False

splitRules :: Rule Expr
splitRules env lhs =
   "SPLIT-K" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: Int _)) <- proofX [] e
      guard (isUni rs bs (Var r))
      pure $ caseSplit rs a as ctx (Arr [])
   ++
   "SPLIT-C" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: Char _)) <- proofX [] e
      guard (isUni rs bs (Var r))
      pure $ caseSplit rs a as ctx (Arr [])
   ++
   "SPLIT-VAR" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: Var r')) <- proofX [] e
      guard (isUni rs bs (Var r))
      guard (isUni rs bs (Var r'))
      pure $ caseSplit rs a as ctx (Arr [])
   ++
   "SPLIT-TUP" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, Var r :=: Arr vs) <- proofX [] e
      guard (not (null vs))
      guard (isUni rs bs (Var r))
      let xs   = rs ++ free e ++ bndIds bs ++ boundVars env
      let rs'  = take (length vs) (uvIdentsNotIn xs)
      let rvs' = foldr1 (:>:) [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
      let a    = Var r :=: Arr (Var <$> rs')
      pure     $ caseSplit (rs ++ rs') a as ctx rvs'
   ++
   "SPLIT-PPRED" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(op :@: arg)) <- proofX [] e
      guard (isUni rs bs arg)
      guard (isPrim1 op)
      pure $ caseSplit rs a as ctx (Arr [])
   ++
   "SPLIT-OP" `name`
   do Verify rs as e <- [lhs]
      (ctx, bs, a@(Var r :=: (op :@: arg))) <- proofX [] e
      guard (isUni rs bs (Var r))
      guard (isUni rs bs arg)
      guard (isPrimOp1 op)
      pure $ caseSplit (r:rs) a as ctx (Arr [])



caseSplit :: [Ident] -> Expr -> [Expr] -> (Expr -> Expr) -> Expr -> Expr
caseSplit rs a as ctx e =
   Verify rs (Assume a : as) (ctx e)
   :>:
   Verify rs (Fails  a : as) (ctx Fail)

isPrim1 :: Expr -> Bool
isPrim1 (Op IsInt)  = True
isPrim1 (Op IsChar) = True
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

--------------------------------------------------------------------------------
-- | Contexts ------------------------------------------------------------------
--------------------------------------------------------------------------------
type Context = Expr -> Expr

--------------------------------------------------------------------------------
substX, substX1 :: Expr -> [(Context, Expr)]
-- S context
substX lhs = substX1 lhs ++ [(id,lhs)]
-- S context, X /= hole
substX1 lhs =
   do x :>: e <- [lhs]
      (ctx, hole) <- substX x
      pure ((:>: e) . ctx, hole)
   ++
   -- TODO: this `e` should be `ef` means "can fail or have choice but not loop or do I/O"
   do e :>: x <- [lhs]
      guard (isEffectFree e)
      (ctx, hole) <- substX x
      pure ((e :>:) . ctx, hole)
   ++
   do x :>>: e <- [lhs]
      (ctx, hole) <- substX x
      pure ((:>>: e) . ctx, hole)
   ++
   do (v :=: x)   <- [lhs]
      (ctx, hole) <- substX x
      pure ((v :=:) . ctx, hole)

--------------------------------------------------------------------------------
proofX, proofX1 :: [BndVar] -> Expr -> [(Context, [BndVar], Expr)]
-- P context
proofX bs lhs = proofX1 bs lhs ++ [(id, bs, lhs)]
-- P context, X /= hole
proofX1 bs lhs =
   do cf :>: x <- [lhs]
      guard (TRS2024.isChoiceFree cf)
      (ctx, bs', hole) <- proofX bs x
      pure ((cf :>:) . ctx, bs', hole)
 ++
   do x :>: e <- [lhs]
      (ctx, bs', hole) <- proofX bs x
      pure ((:>: e) . ctx, bs', hole)
 ++
  do EXI y x <- [lhs]
     (ctx, bs', hole) <- proofX (BExi y : bs) x
     pure (EXI y . ctx, bs', hole)
 ++
  do One x <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure (One . ctx, bs', hole)
 ++
  do All x <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure (All . ctx, bs', hole)
 ++
  do Val v :=: x <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure ((Val v :=:) . ctx, bs', hole)
 ++
  do x :|: e  <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure ((:|: e) . ctx, bs', hole)
 ++
  do e :|: x  <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure ((e :|:) . ctx, bs', hole)
 ++
  do x :>>: e  <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure ((:>>: e) . ctx, bs', hole)
 ++
  do If x e1 e2 <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure ((\e1' -> If e1' e1 e2) . ctx, bs', hole)
 ++
  do Assert x <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure (Assert . ctx, bs', hole)
 ++
  do Decide x <- [lhs]
     (ctx, bs', hole) <- proofX bs x
     pure (Decide . ctx, bs', hole)
