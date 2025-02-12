{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}

module Core.Verifier(
    verificationRules
  ) where

import Core.Bind
import Core.Expr
import Core.TRS2024 as TRS2024
import Epic.Print hiding ( (<>) )

import Data.Maybe ( isJust )
import Control.Monad (guard)
import Core.Solver (unsat)

--------------------------------------------------------
--
--         The verifier's rules
--
--------------------------------------------------------

verificationRules ::  Rule
verificationRules
  = everywhere (verificationStep
                <> splitStep
                <> recStep)

verificationStep :: Rule
verificationStep =  TRS2024.runtimeAndVerificationStep
                 <> guardStep
                 <> verifyStep
                 <> arrStep

--------------------------------------------------------------------------------
guardStep :: Rule
guardStep env lhs =
   "GUARD-ELIM" `labelRuleWith`
   do v :>>: e <- [lhs]
      guard (skolValue (skolVars env) v)
      pure (pPrintSmallExpr v, e)

{- Guards only have values to the left
   ToDo: check in 'valid'
   ++
   "GUARD-FAIL" `labelRule`
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
--   verify(R,n;A){ P[ x = choose(n){some(\i. inrange[i,n])}; x ] }
arrStep env lhs =
  "DD-DODGY" `labelBigRule`   -- v = DotDot[i,n];e  --> i = v; _ = DotDot[v,n]; e
                      -- Dodgy because it overlaps with DD-NARROW
  do (v :=: Op DotDot :@: Tup [i, n]) :>: e <- [lhs]
     guard (skolValue (skolVars env) v)
     pure (coreSeq [ i :=: v
                   , Var underscore :=: Op DotDot :@: Tup [v,n]
                   , e ])
  ++
  "DD-NARROW" `labelBigRuleWith`
  -- exists x. C[ dotdot$[x,n] ] --> exists x. C[ x = CHOOSE(n){some(inrange(n))}; x ]
  do (exis, ctx, e1@(Op DotDot :@: Tup [Var x, v])) <- evalCtxLift (free lhs) lhs
     -- Use this rule when v is not a literal.
     guard (x `elem` exis)
     pure (pPrint e1, wrapExis exis $
                      ctx <@ ((Var x :=: Choose v (Some (inRangeType v)))
                              :>: Var x))
  ++
  "DD-INRANGE" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, e1@(Op DotDot :@: Tup [i, sz]))) <- proofX all_rs e
      guard (isJust (groundValue all_rs i))
      pure (pPrint e1, Verify $ bindList rs
                         (as, ctx <@ inRange i sz))
  ++
  "ARR-MAP" `labelBigRuleWith`   -- ArrMap$[f,a] --> for(x:a){f[x]}
  do Op ArrMap :@: Tup [f, arr@(Arr _ _)] <- [lhs]
     let x:i:_ = identsNotIn (free (f,arr))
     pure (pPrint arr, Iter IterFor
             ( Exi $ bind x $
                 (Var x :=: Exi (bind i (arr :@: Var i))) :>:
                 lamUnderscore (f :@: Var x)
             ) (Tup [])
          )
  ++
  "ARR-APP" `labelBigRuleWith`  -- (Arr n e)[v] --> Dotdot$[v,n]; some(\_.e)
  do arr@(Arr sz e) :@: v <- [lhs]
     pure (pPrint arr, (Op DotDot :@: Tup [v,sz]) >>> someUnderscore e )
  ++
  "CHOOSE0" `labelBigRule`
  -- choose(0){e}  -->   fail
  do Choose (LitInt 0) _ <- [lhs]
     pure Fail
  ++
  "CHOOSE1" `labelBigRule`
  -- choose(1){e}  -->   some(e)
  do Choose (LitInt 1) e <- [lhs]
     pure (someUnderscore e)
  ++
  "ITER-CHOOSE" `labelBigRuleWith`
  do Iter f body e0 <- [lhs]
     (exis, ctx, Choose n e) <- evalCtxLift [] body
     guard (ctx /= HOLE)
     guard (choiceFreeLH ctx)
     guard (free n `disjointFrom` exis)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)

     let k = identNotIn (free lhs ++ exis)
     pure ( text (show f)
          , Exi $ bind k $
            (Var k :=: mkSize n (mkExis exis $ ctx <@ someUnderscore e)) :>:
            Iter f (Choose (Var k) (mkExis exis $ ctx <@ e)) e0 )

{- Alernative, not quite working version
   See MaxVerse9, Feb 10 "Avoiding duplication in ITER-CHOOSE"
     let (k:a:ae:x:_) = identsNotIn (free lhs ++ exis)
     pure ( text (show f)
          , Exi $ bind k $ Exi $ bind a $
            (Var a :=: mkAll (someUnderscore e)) :>:
            (Var k :=: Iter IterOne (   (Exi $ bind ae $ (Var a :=: Tup [Var ae]) :>: n)
                                    :|: ((Var a :=: Tup []) :>: Lit (LInt 0)) )
                             (Some (Lam (bind x (Op Gt :@: Tup [Var x, n])))) ) :>:
            Iter f (Choose (Var k) (mkExis exis $ ctx <@ (Var a :@: Lit (LInt 0)))) e0 )
-}

  ++
  "ONE-CHOOSE" `labelBigRule`
  do Iter IterOne (Choose n e) e0 <- [lhs]
     pure $ mkIf ((n :=: Lit (LInt 0)) :>: lamUnderscore e0)
                 (someUnderscore e)
  ++
  "IF-CHOOSE" `labelBigRule`
  do Iter IterIf (Choose n e) e0 <- [lhs]
     pure $ mkIf ((n :=: Lit (LInt 0)) :>: lamUnderscore e0)
                 (mkApp (someUnderscore e) (Tup []))
  ++
  "ALL-CHOOSE" `labelBigRule`
  do Iter IterAll (Choose n e) e0 <- [lhs]
     let ys:zs:_ = identsNotIn (free lhs)
     pure $
       mkDef ys e0 $ \ys' ->
         Exi $ bind zs $
           (Op ArrApp :@: Tup [ Arr n e, ys', Var zs ]) >>>
           Var zs
  ++
  "FOR-CHOOSE" `labelBigRule`
  do Iter IterFor (Choose n e) e0 <- [lhs]
     let k:xs:ys:zs:_ = identsNotIn (free lhs)
     pure $
       Exi $ bind k $ Exi $ bind xs $
         (Var k  :=: mkSizeX n
                       ( mkApp (someUnderscore e) (Tup [])
                       )) :>:
         (Var xs :=: Choose (Var k) (Arr n (mkApp e (Tup [])))) :>:
         (mkDef ys e0 $ \ys' ->
           Exi $ bind zs $
             (Op ArrApp :@: Tup [ Var xs, ys', Var zs ]) >>>
             Var zs)
  ++
  "VERIFY-CHOOSE" `labelBigRule`
  do (_skols, rs, as, body) <- matchVerify env lhs
     (exis, ctx, Choose n e) <- evalCtxLift [] body
     guard (ctx /= HOLE)
     guard (choiceFreeLH ctx)
     --guard (free n `disjointFrom` exis) --  <-- needed for the solution using SIZE below
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     pure $
       Verify $ bindList rs
       ( as
       , mkExis exis $ ctx <@ ((Op Gt :@: Tup [n, Lit (LInt 0)]) >>> someUnderscore e)
       )

{-
     -- this seems to be correct but uses SIZE and is complicated
     let k = identNotIn (free lhs ++ exis)
     pure $
       Verify $ bindList rs
       ( as
       , Exi $ bind k $
           (Var k :=: mkSize n (mkExis exis $ ctx <@ someUnderscore e)) :>:
           (mkExis exis $ ctx <@ e)
       )
-}

{-
  ++
  "FOR-CHOOSE" `labelBigRule`
     -- FOR{ C[ choose(v){e} ] }
     -- --> n := size(v){ C[ some(\_.e) ] } ;
     --     Arr(n){ C[e] }
     -- if boundvars(C) disjoint from freevars(v)
  do Iter IterFor for_body e0 <- [lhs]
     (exis, ctx, Choose sz e) <- evalCtxLift [] for_body
     guard (free sz `disjointFrom` exis)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
       -- This guard seems to make no difference either way
     let k:n:a:b:c:x:_ = identsNotIn $ free for_body
     pure ( Exi $ bind n $ Exi $ bind k $
            (Var k :=: Some (Lam $ bind x $ Op GEq :@: Tup [Var x, Lit (LInt 0)]))
            :>:
            (Var n :=: Some (Lam $ bind x $ Op GEq :@: Tup [Var x, Lit (LInt 0)]))
            :>:
            (Exi $ bind a $ Exi $ bind b $ Exi $ bind c $
              (Var a :=: Choose (Var k) (Arr (Var n) (wrapExis exis (ctx <@ e)))) :>: 
              (Var b :=: e0) :>:
              (Op ArrApp :@: Tup [Var a,Var b,Var c]) >>>
              Var c
            )
          )
-}
{-    CHOOSE-X does not seems to work quite right, and is much slower
-}
{-
  ++
  "ALL-CHOOSE" `labelBigRule`
     -- all{ C[ choose(v){e} ] }
     -- --> Arr(n){ C[e] }
     -- if boundvars(C) disjoint from freevars(v)
  do All (Choose n e) <- [lhs]
     pure ( Arr n e )
  ++
  "CHOOSE-X" `labelBigRule`
     -- C[ choose(v){e} ]
     -- --> n := size(v){ C[ some(\_.e) ] } ;
     --     choose(n){ C[e] }
     -- if boundvars(C) disjoint from freevars(v)
  do let lhs_fvs = free lhs
     (exis, ctx, Choose sz e) <- evalCtxLift lhs_fvs lhs
     guard (ctx /= HOLE)
     guard (free sz `disjointFrom` exis)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
        -- Need 'blkd' to avoid CHOOSE-X applying repeatedly in
        --    C[choose(v){e}] --> exists n. n = size(v){C[some(\_.e)]}
        --                                  choose(n){C[e]}
        -- Don't engulf just the "n=size" part!!
     let n = identNotIn lhs_fvs
     pure ( Exi $ bind n $
            (Var n :=: mkSize sz (wrapExis exis $
                                ctx <@ Some (Lam $ bind underscore e)))
            :>:
            (Choose (Var n) (wrapExis exis (ctx <@ e))) )
-}
 ++
  "U-ARR" `labelBigRule`  -- Arr n1 e1 = Arr n2 e2; e
                    -- --> (n1=n2); one{ n1=0 | some(\_.e1) = some(\_.e2) }; e
  do (Arr n1 e1 :=: Arr n2 e2) :>: e <- [lhs]
     pure (coreSeq [ n1 :=: n2
                   , Var underscore
                       :=: mkOne (((n1 :=: litIntZero) :>: Tup [])
                                  :|:
                                  mkEqual (someUnderscore e2)
                                          (someUnderscore e1)
                                          (Tup []))
                   , e ])
 ++
  "U-TUP-ARR" `labelBigRule`  -- <v1,..,vk> = Arr n e1; e
                    -- --> (n=k); v1=some{\_.e1}; ..; vk=some{\_.e1}; e
  do (Tup vs :=: Arr n e1) :>: e <- [lhs]
     pure ((Lit (LInt (fromIntegral (length vs))) :=: n) :>: foldr (\v -> ((v :=: e1) :>:)) e vs)
 ++
  "U-ARR-TUP" `labelBigRule`  -- <v1,..,vk> = Arr n e1; e
                    -- --> (n=k); v1=some{\_.e1}; ..; vk=some{\_.e1}; e
  do (Arr n e1 :=: Tup vs) :>: e <- [lhs]
     pure ((Lit (LInt (fromIntegral (length vs))) :=: n) :>: foldr (\v -> ((v :=: e1) :>:)) e vs)

{-  (vs, n, e1, e) <- [ (vs, n, e1, e)
                       | 
                       , (Tup vs, Arr n e1) <- [ (a1,a2), (a2,a1) ]
                       ]
     pure ( (n :=: Lit (LInt (fromIntegral (length vs))))
        :>: foldr (:>:) e
            [ v :=: Some (lamUnderscore e1) | v <- vs ]
          )
-}

{-
 ++
  "SIZE0" `labelBigRule`  -- Size(0){e} --> 0
  do Size (LitInt 0) _ <- [lhs]
     pure (litInt 0)
 ++
  "SIZE-VAL" `labelBigRule`  -- Size(n){v} --> n
  do Size n v <- [lhs]
     guard (isVal n)
     guard (isVal v)
     pure n
 ++
  "SIZE-FAIL" `labelBigRule`  -- Size(n){fail} --> 0
  do Size n Fail <- [lhs]
     pure (LitInt 0)
 ++
  "SIZE-CHOICE" `labelBigRule`  -- Size(n){C[e1|e2]} --> Size(n){C[e1]} + Size(n){C[e2]}
  do Size n e <- [lhs]
     guard (isVal n)
     (ctx, e1 :|: e2) <- evalCtx [] e
     guard (choiceFreeLH ctx)
     guard (blocked ctx)  -- was: e
     let x:y:_ = identsNotIn (free e)
     pure $ Exi $ bind x $ Exi $ bind y $
           (Var x :=: Size n (ctx <@ e1))
       :>: (Var y :=: Size n (ctx <@ e2))
       :>: (Op Add :@: Tup [Var x, Var y])
-}
{-
 ++
  "SIZE0" `labelBigRule`  -- Size(0){e} --> 0
  do Size (LitInt n) _ <- [lhs]
     guard (n==0)
     pure (litInt 0)
 ++
  "SIZEn" `labelBigRule`  -- Size(n){v1 | v2 | ...} --> some(nat) -- rules SIZE1, SIZEF0, SIZEFn, SIZEN
  do Size n e <- [lhs]
     case multiplicity e of
       MDunno -> []      -- Rule does not apply
       MMany  -> pure someNat
       MOne   -> pure n
       MZero | LitInt 1 <- n -> pure (litInt 0)
             | otherwise     -> pure (Some (inRangeType n))

------------------------------
data Mult = MZero   -- Exactly zero
          | MOne    -- Exactly one
          | MMany   -- Definitely more than one
          | MDunno

choiceMult :: Mult -> Mult -> Mult
choiceMult MZero m   = m
choiceMult MOne  MZero  = MOne
choiceMult MOne  MOne   = MMany
choiceMult MOne  MMany  = MMany
choiceMult MOne  MDunno = MDunno
choiceMult MMany  _     = MMany
choiceMult MDunno MMany = MMany
choiceMult MDunno _     = MDunno

multiplicity :: Expr -> Mult
multiplicity Fail          = MZero
multiplicity (e1 :|: e2)   = multiplicity e1 `choiceMult` multiplicity e2
multiplicity e | isVal e   = MOne
               | otherwise = MDunno
-}

mkSize :: Val -> Expr -> Expr
mkSize n e =
  Iter IterOne
   (  ((n :=: Lit (LInt 0)) :>: Lit (LInt 0))
  :|: ( Exi $ bind k $ (Var k :=: mkCount e) :>:
        (  ((Var k :=: Lit (LInt 0))
             :>: Some (Lam $ bind x $ (Op IsInt :@: Var x) >>> (Op LEq :@: Tup [Lit (LInt 0),Var x]) >>> (Op Lt :@: Tup [Var x,n]) >>> Var x))
       :|: ((Var k :=: Lit (LInt 1))
             :>: n)
        )
      )
   ) ( Some (Lam $ bind x $ (Op IsInt :@: Var x) >>> (Op Lt :@: Tup [n,Var x]) >>> Var x))
 where
  k:x:_ = identsNotIn (free (n,e))

mkSizeX :: Val -> Expr -> Expr
mkSizeX n e =
  Iter IterOne
   (  ((n :=: Lit (LInt 0)) :>: Lit (LInt 1))
  :|: ( Exi $ bind k $ (Var k :=: mkCount e) :>:
        (  ((Var k :=: Lit (LInt 0))
             :>: Lit (LInt 0))
       :|: ((Var k :=: Lit (LInt 1))
             :>: Lit (LInt 1))
        )
      )
   ) ( Some (Lam $ bind x $ (Op IsInt :@: Var x) >>> (Op Lt :@: Tup [Lit (LInt 1),Var x]) >>> Var x))
 where
  k:x:_ = identsNotIn (free (n,e))

--------------------------------------------------------------------------------
verifyStep :: Rule
verifyStep env lhs =
   "VERIFY-VAL" `labelBigRule`
   do (_skols, _rs, _as, v) <- matchVerify env lhs
      guard (isVal v)
      pure (Tup [])
   ++
   "VERIFY-FAIL" `labelBigRule`
   do (_skols, _rs, _as, Fail) <- matchVerify env lhs
      pure (Tup [])
   ++
   "VERIFY-ERR" `labelBigRule`
   do (_skols, _rs, _as, Err s) <- matchVerify env lhs
      pure (Err s)
   ++
   "VERIFY-CHOICE" `labelBigRule`
   do (_skols, rs, as, e) <- matchVerify env lhs
      (ctx, e1 :|: e2) <- evalCtx [] e
      guard (blocked ctx)
      pure (     (Verify $ bindList rs (as,ctx <@ e1))
             >>> (Verify $ bindList rs (as,ctx <@ e2)) )
   ++
   "SOLVER" `labelBigRuleWith`
   do (_skols, rs, as, _e) <- matchVerify env lhs
      let env' = extendRuleEnv env rs as
      case unsat env' of
        Just reason -> pure (pPrint reason, Tup [])
        Nothing     -> []
   ++
   "SKOLEMIZE" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Some v)) <- proofX all_rs e
      guard (skolValue all_rs v)
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
   "SPLIT-V" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, (Var r :=: v) :>: rest)) <- proofX all_rs e
      guard (r `elem` all_rs)
      Just gv <- [groundValue all_rs v]
      pure ( pPrint r <+> text "=" <+> pPrint v
           , caseSplit rs (A_GVEq r gv) as ctx rest )

   ++
   "SPLIT-OP" `labelBigRuleWith`
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
   "SPLIT-ISARR" `labelBigRuleWith`
       -- verify(R,r;A){ P[ isArr$[r] ] }
       --  --> verify(R,r,n;A,isArr$[r],isInt$[n], n=arrLen$[r], n>=0){ P[ Arr(.){some(any)} ] }
       --      ..and the fail case..
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Op IsArr :@: Var r)) <- proofX all_rs e
      guard (r `elem` all_rs)   -- r is a skolem
      let n        = skolNotIn all_rs
          r_asm    = A_RelOp IsArr (GVVar r)
          n_asms   = [ A_RelOp IsInt (GVVar n)
                     , A_RelOp GEq (GVArr [GVVar n, GVLit (LInt 0)]) ]
          neg_asms = [A_Neg r_asm]
          pos_asms = A_PrimOp n (AO_Prim ArrLen) (GVVar r) : map A_Pos (r_asm:n_asms)
      pure ( pPrint r
           , (Verify (bindList rs (neg_asms ++ as, ctx <@ Fail)))
             >>>
             (Verify (bindList (n:rs) (pos_asms ++ as, ctx <@ Arr (Var n) someAny))) )
   ++
   "SPLIT-TUP" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Var r :=: Tup vs :>: rest)) <- proofX all_rs e
      guard (r `elem` all_rs)
      let rs'  = take (length vs) (skolsNotIn all_rs)
          rvs' = foldr (:>:) rest [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
          asm    = A_GVEq r (GVArr (map GVVar rs'))
      pure (pPrint asm, caseSplit (rs ++ rs') asm as ctx rvs')

   ++
   "SPLIT-TRU" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Var r :=: Tru v :>: rest)) <- proofX all_rs e
      guard (r `elem` all_rs)
      let r'  = skolsNotIn all_rs !! 0
          rv' = (Var r' :=: v) :>: rest
          asm    = A_GVEq r (GVTru (GVVar r'))
      pure (pPrint asm, caseSplit (rs ++ [r']) asm as ctx rv')

{- SPJ: I am not sure if we need SPLIT-APP at all.
        So I am commenting it out for now.
   -- Verify(rs ; as){ P[r[s]] }
   -- ---> Verify (r:rs ; r'=r[s], as) { P [r'] }  if r, s are skol, r' fresh
   ++
   "SPLIT-APP" `labelBigRuleWith`
   do (all_rs, rs, as, e) <- matchVerify env lhs
      (ctx, (_, Var r :@: s)) <- proofX all_rs e
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
  = Verify (bindList rs (A_Neg a : as, ctx <@ Fail))
    >>>
    Verify (bindList rs (A_Pos a : as, ctx <@ e))

--------------------------------------------------------------------------------
-- | Contexts ------------------------------------------------------------------
--------------------------------------------------------------------------------

proofX :: [Ident] -> Expr -> [( Context    -- The context
                              , ([Ident]   -- Flexible existentials bound by context
                              ,  Expr ))]  -- The expression in the hole
-- P context
proofX bs lhs
  = do { (ctx, stuff) <- go_px (LX { exi_flexi = [], exi_rigid = bs }) lhs
       ; guard (blocked ctx)
       --; guard (blkd (LX { exi_flexi = [], exi_rigid = bs}) ctx)
       ; pure (ctx, stuff) }

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
-- ++
--  do Size sz e <- [lhs]
--     (ctx, hole) <- go_px lx e
--     pure (Size sz ctx, hole)
-- ++
--  do Check fx x <- [lhs]
--     (ctx, hole) <- go_px lx x
--     pure (Check fx ctx, hole)
-- ++
--  do All x <- [lhs]
--     (ctx, hole) <- go_px (makeRigid lx) x
--     pure (All ctx, hole)
 ++
  do Iter f e e0 <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) e
     pure (Iter f ctx e0, hole)
 ++
  do Iter f e e0 <- [lhs]
     (ctx, hole) <- go_px (makeRigid lx) e0
     pure (Iter f e ctx, hole)
