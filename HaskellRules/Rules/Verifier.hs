{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}

module Rules.Verifier(
  allSystemsVerify,
  icfpVerifier,
  icfpeVerifier,
  l2rVerifier,
  verify,
  verifyM,
  wrapAssert,
  ) where
import TRS.Bind
import TRS.TRS hiding (step)
import TRS.Traced
import TRS.Tarjan
import Rules.Core hiding (Wrong)
import Rules.ICFP (systemICFP, systemICFPE, execX, choiceX, ltExpr, isChoiceFreeOp)
import Rules.LeftToRight hiding (effectFree)
import Control.Monad (guard)
import Data.List( intersect )
import qualified Epic.SIntMap as IM
import Epic.Print (prettyShow, Pretty)
import qualified Debug.Trace as Debug

-- | Run verification rules.
_traceShow :: (Pretty a) => String -> a -> a
_traceShow msg x = Debug.trace ("TRACE: " ++ msg ++ prettyShow x) x


verifyM :: TRSystem Expr -> Expr -> Maybe (Bool, Traced Expr)
verifyM sys e = res
 where
   res =
     case tarjan1 (tfNormSteps (ruleEnv sys)) arrow (start e) of -- (preProcess sys (ruleEnv sys) e :<-- [])
       Just (tr@(x :<-- _):_) -> Just (isDone x, tr)
       _ -> Nothing
   arrow (a :<-- t)       = [ b :<-- ((r,a):t) | (r,b) <- next a ]
   next a = {- traceShow ("STEPS: " ++ prettyShow a) $ -} stepS sys a

  --norms           = normalFormsFuelTracePlain sys (-1) e
  --tr@(x :<-- _):_ = nrDone norms ++ nrLeft norms

verify :: TRSystem Expr -> Expr -> (Bool, Traced Expr)
verify sys e =
  let sys' = sys{ ruleEnv = (ruleEnv sys){ tfNormSteps = 20000 } }
  in  case verifyM sys' e of
        Just  x -> x
        Nothing -> undefined

wrapAssert :: Expr -> Expr
wrapAssert = Assert
--  --  | noChecks e = Assert e
--  --  | otherwise  = e
--  where
--   -- done (Verify _) = False
--   noChecks        = collect done (&&)
--   done (Assert _) = False
--   done (Decide _) = False
--   done _          = True

-- | `isDone e` ignores "assert/decide" that occur under `verify` which are themselves
--   under lambdas, as those are obligations for higher-order args that are checked at
--   *callsites* of those lambdas. There is probably some clever way to do
--   the below _just_ using `collect` but I thought I'd write this out first.
isDone :: Expr -> Bool
isDone = go False
 where
  go :: Bool -> Expr -> Bool
  go _   (Lam (Bind _ e)) = go True e
  go lam (Verify e)       = lam || go lam e
  go lam (Arr es)         = and (go lam <$> es)
  go lam (Exi (Bind _ e)) = go lam e
  go lam (e1 :=: e2)      = go lam e1 && go lam e2
  go lam (e1 :|: e2)      = go lam e1 && go lam e2
  go lam (e1 :>: e2)      = go lam e1 && go lam e2
  go lam (e1 :@: e2)      = go lam e1 && go lam e2
  go lam (One e)          = go lam e
  go lam (All e)          = go lam e
  go lam (Fails  e)       = go lam e
  go _   (Assert _)       = False
  go _   (Decide _)       = False
  go lam (Split x y z)    = go lam x && go lam y && go lam z
  go lam (Store h e)      = and (go lam <$> IM.elems h) && go lam e
  -- go _lam (Assume e)       = True -- go lam e
  go _   _                = True




-- | Top-level "Verifier" rewrite system based on ICFP rules -------------------------

icfpVerifier :: TRSystem Expr
icfpVerifier = icfp
  { sname = "ICFPverify"
  , description = "ICFP + extra verifier rules"
  , rules = (rules icfp -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL" -= "U-TUP")
              <> generalizedIcfpRules
              <> assumeAssertRules
              <> verifierRules
  }
  where icfp = systemICFP

icfpeVerifier :: TRSystem Expr
icfpeVerifier = icfp
  { sname = "ICFPEverify"
  , description = "ICFPE + extra verifier rules"
  , rules = (rules icfp -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL"  -= "FAIL-ELIM" )
              <> (generalizedIcfpRules -= "SUBST-GEN")
              <> l2rSubstRules
              <> uniRules
              <> assumeAssertRules
              <> verifierRules
              <> directRules
  }
  where icfp = systemICFPE

l2rVerifier :: TRSystem Expr
l2rVerifier = l2r
  { sname = "L2Rverify"
  , description = "L2R + extra verifier rules"
  , rules = (rules l2r -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL" -= "VAL-SWAP")
              <> (generalizedIcfpRules -= "SUBST-GEN")
              <> generalizedL2RRules
              <> assumeAssertRules
              <> verifierRules
  }
  where [l2r] = allSystemsLeftToRight

allSystemsVerify :: [TRSystem Expr]
allSystemsVerify = [icfpVerifier, icfpeVerifier, l2rVerifier]

--------------------------------------------------------------------------------------
-- | The "Context" in which a subsumption must hold; Tim's "G" -- set of "known facts"
--------------------------------------------------------------------------------------

type QContext = Expr

--------------------------------------------------------------------------------
-- | Abstract Rules
--------------------------------------------------------------------------------
type VRule = Rule Expr

l2rSubstRules :: VRule
l2rSubstRules _env lhs =
  "SUBST-SIMP" `name`
  do -- guard (not recursiveSubstitution)
     (Var x :=: Val v) :>: e <- [lhs]
     guard (x `notElem` free v)
     pure ((Var x :=: v) :>: subst [(x,v)] e)

uniRules :: VRule
uniRules _env lhs =
  -- Assume { uni x . e } ----> uni x . Assume {e}
  "asm-uni" `name`
  do Assume (Uni (Bind x e)) <- [lhs]
     pure (Uni (Bind x (Assume e)))
  ++
  -- X[uni x. e] ---> uni x. X[e]
  "uni-float" `name`
  do (ctx, UNI x e) <- execX lhs  -- Note: Store not allowed in ctx
     -- guard (hasStore (ctx Fail) <= isChoiceFree e)  -- <= is implication for booleans
     let freeX = free ctx
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (UNI x' (ctx (subst [(x,Var x')] e)))
       else pure (UNI x (ctx e))
  ++
  -- exi x. uni y. e ---> uni y. exi x. e
  "uni-hop" `name`
  do  EXI x (UNI y e) <- [lhs]
      guard (x /= y)
      pure (UNI y (EXI x e))

-- | ICFP rules generalized to remove the trailing `e :>: ...` pattern
generalizedIcfpRules :: VRule
generalizedIcfpRules env lhs =
  "EQN-FLOAT-GEN" `name`
  do Val v :=: (eq :>: e1) <- [lhs]
     pure (eq :>: (Val v :=: e1))
  ++
  "U-LIT-GEN" `name`
  do (Int k1 :=: Int k2) <- [lhs]
     guard (k1 == k2)
     pure (Int k1)
  ++
  "U-TUP-GEN" `name`
  do (Arr vs :=: Arr vs') <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) (Arr[]) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
  ++
  "U-FAIL-GEN" `name`
  do HNF e1 :=: HNF e2 <- [lhs]
     -- Avoid the cases handled above
     guard (case (e1,e2) of (Int k1,Int k2) -> k1 /= k2
                            (Ref k1,Ref k2) -> k1 /= k2
                            (Arr a1,Arr a2) -> length a1 /= length a2
                            _               -> True)
     pure Fail
  ++
  "SUBST-GEN" `name`
  do (ctx, Var x :=: Val v) <- execX lhs
     let freeX = free ctx
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (subst sub (ctx (Var x0 :=: Val v)))
   -- copied from ICFP (but the variant in L2R make `TRSVerify.ex0` fail...?)
   -- restricted/effect-compatible variants of FAIL-ELIM
   ++
   "FAIL-ELIM-R" `name`
   do Fail :>: _ <- [lhs]
      pure Fail
   ++
   "FAIL-ELIM-L" `name`
   do e :>: Fail <- [lhs]
      guard (effectFree e)
      pure Fail
   ++
   -- Generalize `CHOOSE` to use lambda as an SX
   -- \z.CX[e1|e2] --> \z.CX[e1]|CX[e2]
   "CHOOSE-GEN" `name`
   do LAM z e <- [lhs]
      (cx, e1 :|: e2) <- choiceX e
      pure (LAM z (cx e1 :|: cx e2))

effectFree :: Expr -> Bool
effectFree (Val _)       = True
effectFree (One _)       = True
effectFree (All _)       = True
effectFree (Op op :@: _) = isChoiceFreeOp op
effectFree (e1 :=: e2)   = effectFree e1 && effectFree e2
effectFree _             = False



generalizedL2RRules :: VRule
generalizedL2RRules env lhs =
  "HNF-SWAP" `name`
-- Old version, only swap with variables.
-- This is non-confluent with lambda unification.
--   do (hnf@HNF{} :=: v@Var{}) :>: e <- [lhs]
   do (hnf@HNF{} :=: v@Val{}) :>: e <- [lhs]
      pure ((v :=: hnf) :>: e)
   ++
   "VAR-SWAP" `name`
   do y@Var{} :=: x@Var{} <- [lhs]
      guard (ltExpr env x y)
      pure (x :=: y)
   ++
   "SEQ-ASSOC-GEN" `name`
   do (e1 :>: e2) :>: e3 <- [lhs]
      pure (e1 :>: (e2 :>: e3))


-- | Rules for `Assume` and `Assert` -------------------------------------------

assumeAssertRules :: VRule
assumeAssertRules env lhs =
  -- ASSUME --
  "asm-hnf" `name`
  do -- Assume (HNF v) <- [lhs]
     Assume (Val v) <- [lhs]
     pure v
  ++
  -- Assume {e1; e2} ---> Assume e1; Assume e2
  "asm-seq" `name`
  do Assume (e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  -- Assume { exi x . e } ----> exi x . Assume {e}
  "asm-exi" `name`
  do Assume (Exi (Bind x e)) <- [lhs]
     pure (Exi (Bind x (Assume e)))
  ++
  -- Assume {Assume{e}} ----> Assume{e}
  "asm-id" `name`
  do Assume (Assume e) <- [lhs]
     pure (Assume e)
  ++
  -- Assume { Assert {e} } ----> Assume {e}
  "asm-suc" `name`
  do Assume (Assert e) <- [lhs]
     pure (Assume e)
  ++
  -- Assume { Verify {e} } ----> ()
  "asm-ver" `name`
  do Assume (Verify _) <- [lhs]
     pure (Val (Arr []))
  ++
  -- We *used* to get this from plain `HNF-SWAP` when it was of the form `hnf = x -> x = hnf`
  -- Assume e = x ----> x = Assume e
  "asm-swap" `name`
  do Assume e :=: x@Var{} <- [lhs]
     pure (x :=: Assume e)
  ++
  "asm-asm-swap" `name`
  do Assume e1@(_ :@: _) :>: (Assume e2@(Var _ :=: _) :>: e) <- [lhs]
     pure (Assume e2 :>: (Assume e1 :>: e))
  ++
  "EXI-FLOAT-GEN" `name`
  do Assume (Val v :=: EXI x e) <- [lhs]
     let freeX = free v
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (Assume (EXI x' (Val v :=: subst [(x, Var x')] e)))
       else pure (Assume (EXI x  (Val v :=: e)))
     -- pure (Assume (EXI x (Val v :=: e)))
  ++
--   -- ASSERT --
--   -- Assert { e } ----> e   if   e mustSucceed
--   "suc-elim" `name`
--   do Assert e <- [lhs]
--      guard (mustSucceed e)
--      pure e
--   ++
--   -- DECIDE --
--   -- Decide { e } ----> e   if   e mustDecide
--   "dec-elim" `name`
--   do Decide e <- [lhs]
--      guard (mustDecide (bndVars env) e)
--      pure e
--   ++
  -- VERIFY --
  -- Verify { e } ----> ()  if   e no Assert/Decide in e
  "prove-elim" `name`
  do Verify e <- [lhs]
     let verified (Assert _) = False
         verified (Decide _) = False
         verified _          = True
     guard (collect verified (&&) e)
     -- (old-style) pure (Val (Arr []))
     pure e
  ++
  -- Verify{ E [ Assume(e1 | e2) ]  ----> Verify{ E [Assume e1] } ; Verify{ E [Assume e2] }
  "asm-cas" `name`
  do Verify e                 <- [lhs]
     (cx, _, _, Assume (e1 :|: e2)) <-  eX e
     pure (Verify (cx (Assume e1)) :>: Verify (cx (Assume e2)))
  ++
  "subst-asm" `name`
  -- asm{X[x=v]}; e --> asm{X[x=v]} ; (subst x v e)         if x in FV(e), x not in FV(v)
  do (Assume e_asm) :>: e <- [lhs]
     (_ctx, Var x :=: Val v) <- execX e_asm
     let freeE = free e
         freeV = free v
     let -- x0    = identNotIn (freeE ++ freeV) -- replacing x temporarily
         sub   = [(x, v)] -- ,(x0, Var x)]
     guard (x `elem` freeE)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (Assume e_asm :>: subst sub e)


mustSucceed :: QContext -> [BndVar] -> Expr -> Bool
mustSucceed _ bvars = go [x | BLam x <- bvars]
  where
   go _  (Int _)          = True
   go bs (Arr as)         = all (go bs) as
   go _  (Lam _)          = True
   go bs (Var x)          = x `elem` bs
   go _  (Assume _)       = True
   go _  (Fails _)        = True
   go _  (Assume Fail :>: _) = True       -- HACK
   go bs (e1 :>: e2)      = go bs e1 && go bs e2
   go bs (One e)          = go bs e
   go bs (All e)          = go bs e
   go bs (e1 :|: e2)      = go bs e1 || go bs e2
   go bs (Exi (Bind _ e)) = go bs e
   go bs (Uni (Bind x e)) = go (x:bs) e
   -- go _  (Verify _)       = True          -- HACK
   go _  _                = False


mustDecide :: QContext -> [BndVar] -> Expr -> Bool
mustDecide _ bs = go
  where
    lamBinds       = [x | BLam x <- bs]
    go (Int _)     = True
    go (Arr as)    = all go as
    go (Lam _)     = True
    go (Var x)     = x `elem` lamBinds
    go (Assume _)  = True
    -- go (One e)     = go e
    go (e1 :|: e2) = go e1 && go e2
    go (e1 :>: e2) = go e1 && go e2
    go (e1 :=: e2) = go e1 && go e2    -- TODO:COMPARE-ANY!
    go (e1 :@: e2) = go e1 && go e2 && isDecideOp e1
    go (Op _)      = True
    go Fail        = True
    go _           = False

isDecideOp :: Expr -> Bool
isDecideOp (Op Le)     = True
isDecideOp (Op Lt)     = True
isDecideOp (Op Ge)     = True
isDecideOp (Op Gt)     = True
isDecideOp (Op Ne)     = True
isDecideOp (Op Div)    = True
isDecideOp (Op IsInt)  = True
isDecideOp (Op IsChar) = True
isDecideOp (Op DotDot) = True
isDecideOp (Op Append) = True
isDecideOp _           = False

-- | Rules that are like `verifier` but don't require explicit ASSUME but work under left-to-right evaluation order
--   have to be careful as they can be too STRONG, lets us prove stuff like below, regardless of effect, as they are desugared to
--          ... succ{ exi x. x = f(3); x = f(3); ... }
--   and the second x = f(3) is "implied" and hence, gobbled up by the first which is unsound...
--     test(D00){f(:int):int=>{f(3)=f(3)}} 					#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)            :any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)<converges >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)<reads     >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)<writes    >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)<varies    >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ
--     test(U00){f(x:any)<transacts >:any => f(3)=f(3)}	#TODO:FUN-OUT-EQ

--     e; E1[succ{E2[e1;e2]}] --> e; E1[succ{E2[e2]}]    if e `implies` e1
directRules :: VRule
directRules _env lhs =
   "implies-direct" `name`
   do e :>: rhs <- [lhs]
      (ctx1, _, bs1, Assert e') <- eX rhs
      (ctx2, _, bs2, e1 :>: e2) <- eX e'
      guard (null (free e1 `intersect` bndIds (bs1 ++ bs2)))
      guard (implies e e1)
      guard (e /= Fail)
      pure (e :>: ctx1 (Assert (ctx2 e2)))


   --     e; E[e1;e2] --> e; E[e2]     if e `implies` e1
   -- "implies-direct" `name`
   -- do e :>: rhs <- [lhs]
   --    (ctx, _, bs, e1 :>: e2) <- eX rhs
   --    guard (null (free e1 `intersect` bndIds bs))
   --    guard (implies e e1)
   --    guard (e /= Fail)
   --    pure (e :>: ctx e2)

-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
verifierRules :: VRule
verifierRules env lhs =
   -- PROVE --
   -- -- P[e; e'] ----> P[e']   if   P |- e
   -- "prove" `name`
   -- do (ctx, g, bs, e :>: e') <- eX lhs
   --    guard (proves g bs e)
   --    pure (ctx e')
   -- ++
   -- asm{e}; P[e; e'] ----> P[e']   if   fv(e) disjoint from bvars(P)
   "implies" `name`
   do (Assume e) :>: rhs <- [lhs]
      (ctx, _, bs, e1 :>: e2) <- eX rhs
      guard (null (free e1 `intersect` bndIds bs))
      guard (implies e e1)
      guard (e /= Fail)
      pure (Assume e :>: ctx e2)
   ++
   -- ASSERT --
   -- P[Assert { e }] ----> e   if   mustSucceed(P, e)
   "suc-elim" `name`
   do (ctx, g,_, Assert e) <- eX lhs
      guard (mustSucceed g (bndVars env) e)
      -- (old-style)
      pure (ctx e)      -- # old-style
      -- pure (ctx (Arr []))  -- # spj-style
   ++
   -- DECIDE --
   -- Decide { e } ----> e   if   e mustDecide
   "dec-elim" `name`
   do (ctx, g, _, Decide e) <- eX lhs
      guard (mustDecide g (bndVars env) e)
      pure (ctx e)
   ++
   -- P[suc{e}] -> P[e] if mustSucceed(P, e)
   -- "suc-intro" `name`
   -- CTX[e] ---> CTX[assume{e}]    if    CTX |- e
   -- "asm-intro" `name`
   -- do (ctx, g, _, e) <- eX lhs
   --    guard (case e of Assume _ -> False; _ -> True)
   --    guard (g `proves` e)
   --    pure (ctx (Assume e))
   -- ++
   -- -- if e1 e2 e3 ---> (assume{e1} ; e2) | (assume-fail{e1}; e3) IF mustDecides e1
   -- -- unsoundly verifies if foo(n:any):int := if int[n] then 0 else n
   -- "Unfold-If" `name`
   -- do If e1 e2 e3 <- [lhs]
   --    let bs = bndVars env
   --    guard (mustDecide bs e1)
   --    let (eThen, eElse) = unfoldIte e1 e2 e3
   --    pure (eThen :|: eElse)
   -- ++
--   -- Verify{CTX[if e1 e2 e3]} ---> Verify{CTX[(assume{e1} ; e2)}; Verify{CTX(e3)} IF CTX `mustDecide` e1
--   "asm-if" `name`
--   do Verify e <- [lhs]
--      (ctx, g, bs, If e1 e2 e3) <- eX e
--      let bs0 = bndVars env
--      guard (mustDecide g (bs0 ++ bs) e1)
--      pure (Verify (ctx (Assume e1 :>: e2)) :>: Verify (ctx (Fails e1 :>: e3)))
--   ++
   -- Verify{CTX[exi xs. if e1 e2 e3]} ---> Verify{CTX[exi xs. assume{e1} ; e2]}; Verify{CTX(Fails (exis xs e1); e3)} IF CTX + xs `mustDecide` e1
   "asm-if" `name`
   do Verify e <- [lhs]
      (ctx, g, bs, e') <- eX e
      (xs, If e1 e2 e3) <- splitIf e'
      let bs0 = bndVars env
      guard (mustDecide g (bs0 ++ bs ++ (BLam <$> xs)) e1)  -- TODO: new binder type for if-definitions
      pure (Verify (ctx (exis xs (Assume e1 :>: e2))) :>: Verify (ctx (Fails (exis xs e1) :>: e3)))
   ++
   -- Fails {hnf} ---> Assume {fail}
   "fails-hnf" `name`
   do Fails (HNF _) <- [lhs]
      pure (Assume Fail)

splitIf :: Expr -> [([Ident], Expr)]
splitIf e@If{}    = pure ([], e )
splitIf (EXI x e) = do (xs, e') <- splitIf e; pure (x:xs, e')
splitIf _         = []

--------------------------------------------------------------------------------
-- | A simple "decision procedure"
--------------------------------------------------------------------------------
unAssume :: Expr -> Expr
unAssume (e1 :>: e2) = unAssume e1 :>: unAssume e2
unAssume (e1 :|: e2) = unAssume e1 :|: unAssume e2
unAssume (f :@: x)   = unAssume f :@: unAssume x
unAssume (Assume a)  = unAssume a
unAssume (e1 :=: e2) = unAssume e1 :=: unAssume e2
unAssume a           = a

implies :: Expr -> Expr -> Bool
implies e1' e2'
  | e1 == e2                       = True
  | INT a <- e1, (b1 :=: b2) <- e2 = a == b1 && a == b2
  | otherwise                      = False
  where
   e1 = unAssume e1'
   e2 = unAssume e2'

_proves :: QContext -> [BndVar] -> Expr -> Bool
_proves g bs e = unAssume e `elem` facts g && null (vs `intersect` bndIds bs)
 where
  vs = free e

  facts (g1 :>: g2) = facts g1 ++ facts g2
  facts (g1 :|: g2) = facts g1 `intersect` facts g2
  facts (Exi bnd)   = facts g' where Bind _ g' = alphaRename vs bnd
  facts (Assume a)  = assumes (unAssume a)
  facts _           = []

  assumes a = a : derives a

  -- special rules
  -- derives (Op IsInt :@: a) = ( a :=: a ) : assumes a
  -- derives (Op IsChar :@: a) = ( a :=: a ) : assumes a
  derives (INT a) = ( a :=: a ) : assumes a
  derives _                = []

----------------------------------------------------------------------
-- | Expression Contexts
-----------------------------------------------------------------------


eX :: Expr -> [(EContext, QContext, [BndVar], Expr)]
eX = execEX []

execEX :: [BndVar] -> Expr -> [(EContext, QContext, [BndVar], Expr)]
execEX bs lhs = execEX1 bs lhs ++ [(id, Arr [], bs, lhs)]

execEX1 :: [BndVar] -> Expr -> [(EContext, QContext, [BndVar], Expr)]
execEX1 bs lhs =
  do v :=: x     <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (\ a -> v :=: ctx a, g, bs', hole)
 ++
   -- HOLE; e
  do x :>: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:>: e) . ctx, g :>: e, bs', hole)
 ++
   -- e; HOLE
  do e :>: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :>:) . ctx, e :>: g, bs', hole)
 ++
   -- Exi y HOLE
  do EXI y x <- [lhs]
     (ctx, g, bs', hole) <- execEX (BExi y : bs) x
     pure (EXI y . ctx, g, bs', hole)   -- y should be visible to e in g |- e
 ++
   -- Uni y HOLE
  do UNI y x <- [lhs]
     (ctx, g, bs', hole) <- execEX (BUni y : bs) x
     pure (UNI y . ctx, g, bs', hole)   -- y should be visible to e in g |- e
 ++
   -- ONE HOLE
  do One x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (One . ctx, g, bs', hole)
 ++
  do All x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (All . ctx, g, bs', hole)
 ++
  do x :|: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:|: e) . ctx, g, bs', hole)
 ++
  do e :|: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :|:) . ctx, g, bs', hole)
 ++
  do Lam (Bind y x) <- [lhs]
     (ctx, g, bs', hole) <- execEX (BLam y : bs) x
     pure (Lam . Bind y . ctx, Assume (Var y) :>: g, bs', hole)  -- y should be visible to e in g |- e
 ++
  do x :@: e <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((:@: e) . ctx, g, bs', hole)
 ++
  do e :@: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :@:) . ctx, g, bs', hole)
 ++
  do If x e1 e2 <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (\a -> If (ctx a) e1 e2, g, bs', hole)
 ++
  do Assert x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (Assert . ctx, g, bs', hole)
 ++
  do Verify x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (Verify . ctx, g, bs', hole)
