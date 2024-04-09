{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}

module Rules.OldVerifier( icfpeVerifier, generalizedIcfpRules ) where
import TRS.Bind
import TRS.TRS hiding (step)
import Rules.Core
import Rules.ICFP (systemICFPE, execX, defX, choiceX, ltExpr, execX1, hasStore, isChoiceFree)
import Control.Monad (guard)
import Data.List( intersect, isInfixOf )
import Data.Maybe (mapMaybe)
import qualified Rules.TRS2024 as TRS2024

-- | Top-level "Verifier" rewrite system based on ICFP rules -------------------------

icfpeVerifier :: TRSystem Expr
icfpeVerifier = icfp
  { sname = "ICFPEverify"
  , description = "ICFPE + extra verifier rules"
  , rules = (rules icfp -= "EQN-FLOAT" -= "SUBST" -= "U-LIT" -= "U-FAIL"  -= "FAIL-ELIM" )
              <> generalizedIcfpRules
              <> fancySubstRules
              <> uniRules
              <> (assumeAssertRules -= "suc-seq")
              <> verifierRules
              <> directRules
  , displayRules = if True then const True else isInfixOf "MODULO"
  }
  where icfp = systemICFPE

--------------------------------------------------------------------------------------
-- | The "Context" in which a subsumption must hold; Tim's "G" -- set of "known facts"
--------------------------------------------------------------------------------------

type QContext = Expr

--------------------------------------------------------------------------------
-- | Abstract Rules
--------------------------------------------------------------------------------
type VRule = Rule Expr

uniRules :: VRule
uniRules _env lhs =
  -- Assume { uni x . e } ----> uni x . Assume {e}
  "asm-uni" `name`
  do Assume (Uni (Bind x e)) <- [lhs]
     pure (Uni (Bind x (Assume e)))
  ++
  -- X[uni x. e] ---> uni x. X[e]
  "uni-float" `name`    -- TODO(RJ): Duplicate of UNI-FLOAT
  do (ctx, _, _, UNI x e) <- evalX lhs  -- Note: Store not allowed in ctx
     -- guard (hasStore (ctx Fail) <= isChoiceFree e)  -- <= is implication for booleans
     let freeX = free ctx
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (UNI x' (ctx (subst [(x,Var x')] e)))
       else pure (UNI x (ctx e))
  ++
  -- exi x. uni y. e ---> uni y. exi x. e
  "uni-swap" `name`
  do  EXI x (UNI y e) <- [lhs]
      guard (x /= y)
      pure (UNI y (EXI x e))

-- | ICFP rules generalized to remove the trailing `e :>: ...` pattern
generalizedIcfpRules :: VRule
generalizedIcfpRules _ lhs =
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
                            (Lam _, Lam _)  -> False  -- LAM comparisons "stuck"
                            _               -> True)
     pure Fail
  ++
   -- copied from ICFP (but the variant in L2R make `TRSVerify.ex0` fail...?)
   -- restricted/effect-compatible variants of FAIL-ELIM
  "FAIL-L" `name`
  do Fail :>: _ <- [lhs]
     pure Fail
  ++
  "GUARD-FAIL-L" `name`
  do Fail :>>: _ <- [lhs]
     pure Fail
  ++
  "FAIL-R" `name`
  do e :>: Fail <- [lhs]
     guard (TRS2024.isEffectFree e)
     pure Fail
  ++
  -- Generalize `CHOOSE` to use lambda as an SX
  -- \z.CX[e1|e2] --> \z.CX[e1]|CX[e2]
  "CHOOSE-GEN" `name`
  do LAM z e <- [lhs]
     (cx, e1 :|: e2) <- choiceX e
     pure (LAM z (cx e1 :|: cx e2))
  ++
  "GUARD-ELIM" `name`
  do Val _ :>>: e <- [lhs]
     pure e

fancySubstRules :: VRule
fancySubstRules env lhs =
  "SUBST-MODULO-ASM" `name` -- tim-style "dominator"-based DEF-ELIM
  do EXI x e <- [lhs]
     (ctx, Var x' :=: Val v) <- defX x e
     guard (x == x')
     let freeX = free ctx
         freeV = free v
         freeM = freeModAssume (ctx (Arr []))
     guard (x `notElem` freeV)
     guard (x `notElem` freeM)
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     pure (EXI x (substGen Full sub (ctx (Var x0 :=: Val v))))
  ++
  "SUBST-GEN" `name`
  do (ctx, Var x :=: Val v) <- execX lhs
     let freeX = free ctx
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `notElem` definedVars (bndVars env))
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     -- TODO: guard x is not uni-bound
     -- pure (subst sub (ctx (Var x0 :=: Val v)))
     -- pure (substGen Full [(x0, Var x)] (substGen Asm [(x, v)] (ctx(Var x0 :=: Val v))))
     pure (substGen Asm sub (ctx (Var x0 :=: Val v)))
   ++
  "SUBST-GEN-ASM" `name`
  do (ctx, Assume (Var x :=: Val v)) <- execX lhs
     let freeX = free ctx
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     -- TODO: guard x is not uni-bound
     -- pure (subst sub (ctx (Assume (Var x0 :=: Val v))))
     pure (substGen Full sub (ctx (Assume (Var x0 :=: Val v))))

-- | Rules for `Assume` and `Assert` -------------------------------------------

assumeAssertRules :: VRule
assumeAssertRules _env lhs =
  -- ASSUME --
  "ASM-ELIM" `name`
  do Assume (Val v) <- [lhs]
     pure v
  ++
  -- Assume {e1; e2} ---> Assume e1; Assume e2
  "asm-seq" `name`
  do Assume (e1 :>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  -- Assume {e1;; e2} ---> Assume e1; Assume e2
  "asm-guard-seq" `name`
  do Assume (e1 :>>: e2) <- [lhs]
     pure (Assume e1 :>: Assume e2)
  ++
  -- Assert {e1; e2} ---> Assert e1; Assert e2
  "suc-seq" `name`
  do Assert (e1 :>: e2) <- [lhs]
     pure (Assert e1 :>: Assert e2)
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
  do Assume (Verify {}) <- [lhs]
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
  "verify-elim" `name`
  do Verify _ _ e <- [lhs]
     let verified (Assert _) = False
         verified (Decide _) = False
         verified _          = True
     guard (collect verified (&&) e)
     -- (old-style)
     pure (Val (Arr []))
     -- pure e
  ++
  -- Verify{ E [ Assume(e1 | e2) ]  ----> Verify{ E [Assume e1] } ; Verify{ E [Assume e2] }
  "verify-cas" `name`
  do Verify rs as e                 <- [lhs]
     (cx, _, _, Assume (e1 :|: e2)) <-  eX e
     pure (Verify rs as (cx (Assume e1)) :>: Verify rs as (cx (Assume e2)))
  ++
  "UNI-FLOAT" `name`
  do (ctx, UNI x e) <- execX1 lhs  -- Note: Store not allowed in ctx
     guard (hasStore (ctx Fail) <= isChoiceFree e)  -- <= is implication for booleans
     let freeX = free ctx
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (UNI x' (ctx (subst [(x,Var x')] e)))
       else pure (UNI x (ctx e))


mustSucceed :: QContext -> [BndVar] -> Expr -> Bool
mustSucceed _ bvars = go (definedVars bvars)
  where
   go _  (Int _)          = True
   go _  (Char _)         = True
   go _  (Path _)         = True
   go _bs (Arr _as)       = True -- all (go bs) as
   -- go bs (Arr as)      = all (go bs) as
   go _  (Lam _)          = True
   go bs (Var x)          = x `elem` bs
   go _  (Assume _)       = True
   go _  (Fails _)        = True
   -- go _  (Assume Fail :>: _) = True       -- alternative to "implies-fail"
   go bs (e1 :>: e2)      = go bs e1 && go bs e2
   go bs (Uni (Bind x e)) = go (x:bs) e
   go bs (One e)          = go bs e
   go bs (All e)          = go bs e
   go bs (e1 :|: e2)      = go bs e1 || go bs e2
   go bs (Exi (Bind _ e)) = go bs e
   go _  _                = False

definedVars :: [BndVar] -> [Ident]
definedVars = mapMaybe definedVar
  where
     definedVar :: BndVar -> Maybe Ident
     definedVar (BLam x) = Just x
     definedVar (BUni x) = Just x
     definedVar _        = Nothing

mustDecide :: [BndVar] -> Expr -> Bool
mustDecide bs e = {- Debug.trace ("mustDecide: " ++ prettyShow (e, res)) -} res
  where
    res = go e
    defBs          = definedVars bs
    go (Int _)     = True
    go (Char _)    = True
    go (Path _)    = True
    go (Arr as)    = all go as
    go (Lam _)     = True
    go (Var x)     = x `elem` defBs
    go (Assume _)  = True
    -- go (One e)     = go e
    go (e1 :|: e2) = go e1 && go e2
    go (e1 :>: e2) = go e1 && go e2
    go (e1 :=: e2) = go e1 && go e2    -- TODO:COMPARE-ANY!
    go (e1 :@: e2) = go e1 && go e2 && isDecideOp e1
    go (Op _)      = True
    go Fail        = True
    go (Exi (Bind _ e1)) = go e1
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
   "implies-r" `name`
   do e :>: rhs <- [lhs]
      (ctx1, _, bs1, Assert e') <- eX rhs
      (ctx2, _, bs2, e1 :>: e2) <- eX e'
      --- TODO:see "tricky example L3"  ??? guard (mustDecide (bndVars env) e)
      guard (null (free e1 `intersect` bndIds (bs1 ++ bs2)))
      guard (implies e e1)
      guard (e /= Fail)
      pure (e :>: ctx1 (Assert (ctx2 e2)))


-- | Rules to "prove" an `Assert` (succeeds) using `Assume` (context G) --------------------
verifierRules :: VRule
verifierRules env lhs =
   "implies-r-asm" `name`
   -- asm{e}; X[e1; e2] ----> asm{e}; X[e2]   if   fv(e1) disjoint from bvars(X) and e |- e1
   do (Assume e) :>: rhs <- [lhs]
      (ctx, _, bs, e1 :>: e2) <- eX rhs
      guard (null (free e1 `intersect` bndIds bs))
      guard (implies e e1)
      guard (e /= Fail)
      pure (Assume e :>: ctx e2)
   ++
   "implies-l" `name`
   -- e1; X[asm{e}]  ----> X[asm{e}]   if   fv(e) disjoint from bvars(X) and e |- e1
   do e1 :>: rhs <- [lhs]
      (_, _, bs, Assume e) <- eX rhs
      guard (null (free e `intersect` bndIds bs))
      guard (implies e e1)
      guard (e /= Fail)
      pure rhs
   ++
   "implies-fail" `name`
   do e@(Assume Fail) :>: rhs <- [lhs]
      (ctx, _, _, Fail) <- eX rhs
      pure (e :>: ctx (Arr []))
   -- ASSERT --
   ++
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
   do (ctx, _, _, Decide e) <- eX lhs
      guard (mustDecide (bndVars env) e)
      pure (ctx e)
   ++
   -- -- Verify{CTX[exi xs. if e1 e2 e3]} ---> Verify{CTX[exi xs. assume{e1} ; e2]}; Verify{CTX(Fails (exis xs e1); e3)} IF CTX + xs `mustDecide` e1
   -- "verify-if" `name`
   -- do Verify rs as e <- [lhs]
   --    (ctx, _, bs, e') <- eX e
   --    (xs, If e1 e2 e3) <- splitIf e'
   --    let bs0 = bndVars env
   --    guard (mustDecide (bs0 ++ bs ++ (BLam <$> xs)) e1)  -- TODO: new binder type for if-definitions
   --    pure (Verify (ctx (exis xs (Assume e1 :>: e2))) :>: Verify (ctx (Fails (exis xs e1) :>: e3)))
   -- ++
   -- Verify{uni r. V[r=<v1...vn>]} ---> Verify{uni r. V[fail]}; Verify{uni r. uni r1..rn. asm{r=<r1...rn>}; V[r1=v1;...;rn=vn;<>]}
   -- "decides-split-tup" `name`
   -- do Verify (UNI r e) <- [lhs]
   --    (ctx, _, bs, Var r' :=: Arr vs) <- eX e
   --    guard (not (null vs))
   --    guard (r == r')
   --    let xs  = bndIds bs ++ free e
   --    let rs  = take (length vs) (identsNotIn xs)
   --    let rvs = foldr1 (:>:) [ Var r :=: v | (r, v) <- rs `zip` vs ]
   --    pure (Verify (UNI r (ctx Fail)) :>:
   --          Verify (UNI r (unis rs ( Assume (Var r :=: Arr (Var <$> rs)) :>: ctx rvs))))
   -- ++
   -- -- Verify{\r. V[r=<v1...vn>]} ---> Verify{\r. V[fail]}; Verify{\r. uni r1..rn. asm{r=<r1...rn>}; V[r1=v1;...;rn=vn;<>]}
   -- "decides-split-tup" `name`
   -- do Verify (LAM r e) <- [lhs]
   --    (ctx, _, bs, Var r' :=: Arr vs) <- eX e
   --    guard (not (null vs))
   --    guard (r == r')
   --    let xs  = bndIds bs ++ free e
   --    let rs  = take (length vs) (identsNotIn xs)
   --    let rvs = foldr1 (:>:) [ Var x :=: v | (x, v) <- rs `zip` vs ]
   --    pure (Verify (LAM r (ctx Fail)) :>:
   --          Verify (LAM r (unis rs ( Assume (Var r :=: Arr (Var <$> rs)) :>: ctx rvs))))
   -- ++
   -- -- Verify{uni r. V[r=k]} ---> Verify{uni r. V[fail]}; Verify{uni r. asm{r=k}; V[<>]}
   -- "decides-split-lit" `name`
   -- do Verify (LAM b (UNI r e)) <- [lhs]
   --    (ctx, _, _, Var r' :=: Int k) <- eX e
   --    guard (r == r')
   --    pure (Verify (LAM b (UNI r (ctx Fail))) :>:
   --          Verify (LAM b (UNI r ( Assume (Var r :=: Int k) :>: ctx (Arr[])))))
   -- ++
   -- Verify{uni r. V[r=k]} ---> Verify{uni r. V[fail]}; Verify{uni r. asm{r=k}; V[<>]}
   -- "decides-split-lit" `name`
   -- do Verify (UNI r e) <- [lhs]
   --    (ctx, _, _, Var r' :=: Int k) <- eX e
   --    guard (r == r')
   --    pure (Verify (UNI r (ctx Fail)) :>:
   --          Verify (UNI r ( Assume (Var r :=: Int k) :>: ctx (Arr[]))))
   -- ++
   -- "decides-split-var" `name`
   -- do Verify (UNI r e) <- [lhs]
   --    (ctx, _, bs, Var r' :=: Var r'') <- eX e  ----------- SPLIT IN ASSERT needs NEGATION yuck.
   --    guard (r == r')
   --    guard (r'' `elem` [ b | BUni b <- bs ])
   --    pure (Verify (UNI r ( Fails  (Var r' :=: Var r'') :>: ctx Fail))
   --          :>:
   --          Verify (UNI r ( Assume (Var r' :=: Var r'') :>: ctx (Arr[]))))
   -- ++
   -- Fails {hnf} ---> Assume {fail}
   "fails-hnf" `name`
   do Fails (HNF _) <- [lhs]
      pure (Assume Fail)
   -- Fails {fail} ---> ()
   ++
   "fails-fail" `name`
   do Fails Fail <- [lhs]
      pure (Arr [])

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
  | e1  == e2                       = True
  | e1' == Fail                     = True
  | INT  a <- e1, (b1 :=: b2) <- e2 = a == b1 && a == b2
  | CHAR a <- e1, (b1 :=: b2) <- e2 = a == b1 && a == b2
  | otherwise                       = False
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



evalX :: Expr -> [(EContext, QContext, [BndVar], Expr)]
evalX = evalEX []

evalEX :: [BndVar] -> Expr -> [(EContext, QContext, [BndVar], Expr)]
evalEX bs lhs = evalEX1 bs lhs ++ [(id, Arr [], bs, lhs)]

evalEX1 :: [BndVar] -> Expr -> [(EContext, QContext, [BndVar], Expr)]
evalEX1 bs lhs =
   -- v = E
   do v :=: x     <- [lhs]
      (ctx, g, bs', hole) <- evalEX bs x
      pure (\ a -> v :=: ctx a, g, bs', hole)
   ++
   -- E; e
   do x :>: e <- [lhs]
      (ctx, g, bs', hole) <- evalEX bs x
      pure ((:>: e) . ctx, g :>: e, bs', hole)
   ++
   -- e; E
   do e :>: x <- [lhs]
      (ctx, g, bs', hole) <- evalEX bs x
      pure ((e :>:) . ctx, e :>: g, bs', hole)
   ++
   -- Exi y E
   do EXI y x <- [lhs]
      (ctx, g, bs', hole) <- evalEX (BExi y : bs) x
      pure (EXI y . ctx, g, bs', hole)   -- y should be visible to e in g |- e
   ++
   -- Uni y E
   do UNI y x <- [lhs]
      (ctx, g, bs', hole) <- evalEX (BUni y : bs) x
      pure (UNI y . ctx, g, bs', hole)   -- y should be visible to e in g |- e
   ++
   -- E;; e
   do x :>>: e <- [lhs]
      (ctx, g, bs', hole) <- evalEX bs x
      pure ((:>>: e) . ctx, g :>>: e, bs', hole)
   ++
   -- e;; E
   do e :>>: x <- [lhs]
      (ctx, g, bs', hole) <- evalEX bs x
      pure ((e :>>:) . ctx, e :>>: g, bs', hole)


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
  -- TODO: this `e` should be `ef` means "can fail or have choice but not loop or do I/O"
  -- e; HOLE
  do e :>: x <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure ((e :>:) . ctx, e :>: g, bs', hole)
 ++
 -- NOTE: only terms on LEFT of ;; to affect RIGHT
 do x :>>: e <- [lhs]
    (ctx, g, bs', hole) <- execEX bs x
    pure ((:>>: e) . ctx, g :>>: e, bs', hole)

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
  do Verify rs as x  <- [lhs]
     (ctx, g, bs', hole) <- execEX bs x
     pure (Verify rs as . ctx, g, bs', hole)
