{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}

module Core.Verifier(
    verificationRules
  ) where

import Data.Maybe ( isJust )
import Control.Monad (guard)
import Control.Applicative( (<|>) )

import Core.Bind
import Core.Expr
import Core.Rules
import Core.Rule
import Core.Blocked
import Core.Solver (unsat)
import Epic.Print hiding ( (<>) )

--------------------------------------------------------------------------------
-- the verifier's rules

verificationRules :: Rule Expr
verificationRules = (runtimeRules `without` "DOTDOT-EXPAND")
                <|> verifyRules
                <|> splitRules
                <|> guardRules
                <|> arrayRules
                <|> chooseRules

--------------------------------------------------------------------------------
-- aux functions

groundValue :: [SkolIdent] -> Expr -> Maybe GroundVal
-- Like skolValue, but no lambdas
groundValue _  (Lit l)               = Just (GVLit l)
groundValue rs (Var v) | v `elem` rs = Just (GVVar v)
groundValue rs (Tup vs)              = do { gvs <- mapM (groundValue rs) vs; Just (GVArr gvs) }
groundValue rs (Tru v)               = do gv <- groundValue rs v; Just (GVTru gv)
groundValue _  _                     = Nothing

--------------------------------------------------------------------------------
-- guard

guardRules :: Rule Expr
guardRules =
   do label "GUARD-ELIM"
      v :>>: e <- lhs
      skols <- skolems
      guard (skolValue skols v)
      labelArg (pPrintSmallExpr v)
      pure e

--------------------------------------------------------------------------------
-- array

arrayRules :: Rule Expr
arrayRules =
  do label "DD-DODGY"
     interest 2
     (v :=: Op DotDot :@: Tup [i, n]) :>: e <- lhs
     skols <- skolems
     guard (skolValue skols v)
     pure (mkSeq [ i :=: v
                 , Op DotDot :@: Tup [v,n]
                 , e
                 ])
 <|>
  do label "DD-NARROW"
     interest 2
     e <- lhs
     (exis, ctx, e1@(Op DotDot :@: Tup [Var x, v])) <- evalCtxExis (free e) e
     guard (x `elem` exis)
     labelArg (pPrint e1)
     pure (mkExis exis $
             ctx <@ ((Var x :=: Choose v (Some (inRangeType v)))
                       :>: Var x))
 <|>
  do label "DD-INRANGE"
     interest 2
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, e1@(Op DotDot :@: Tup [i, sz]))) <- proofX all_rs e
     guard (isJust (groundValue all_rs i))
     labelArg (pPrint e1)
     pure (Verify $ bindList rs (as, ctx <@ inRange i sz))
 <|>
  do label "ARR-MAP"
     interest 2
     Op ArrMap :@: Tup [f, arr@(Arr _ _)] <- lhs
     let x:i:_ = identsNotIn (free (f,arr))
     labelArg (pPrint arr)
     pure (Iter IterFor
             ( Exi $ bind x $
                 (Var x :=: Exi (bind i (arr :@: Var i))) :>:
                 lamUnderscore (f :@: Var x)
             ) (Tup [])
          )
 <|>
  do label "ARR-APP"
     interest 2
     arr@(Arr sz e) :@: v <- lhs
     labelArg (pPrint arr)
     pure ((Op DotDot :@: Tup [v,sz]) >>> someUnderscore e)
 <|>
  -- Arr n1 e1 = Arr n2 e2; e
  -- --> (n1=n2); one{ n1=0 | some(\_.e1) = some(\_.e2) }; e
  do label "U-ARR"
     (Arr n1 e1 :=: Arr n2 e2) :>: e <- lhs
     pure (mkSeq [ n1 :=: n2
                 , mkOne (  ((n1 :=: litIntZero) :>: Tup [])
                        :|: mkEqual (someUnderscore e2)
                                    (someUnderscore e1)
                                    (Tup []) )
                 , e ])
 <|>
  -- <v1,..,vk> = Arr n e1; e
  -- --> (n=k); v1=some{\_.e1}; ..; vk=some{\_.e1}; e
  do label "U-TUP-ARR"
     (Tup vs :=: Arr n e1) :>: e <- lhs
     pure ((Lit (LInt (fromIntegral (length vs))) :=: n) :>: foldr (\v -> ((v :=: e1) :>:)) e vs)
 <|>
  do label "U-ARR-TUP"
     (Arr n e1 :=: Tup vs) :>: e <- lhs
     pure ((Lit (LInt (fromIntegral (length vs))) :=: n) :>: foldr (\v -> ((v :=: e1) :>:)) e vs)

chooseRules :: Rule Expr
chooseRules =
  do label "CHOOSE0"
     Choose (LitInt 0) _ <- lhs
     pure Fail
 <|>
  do label "CHOOSE1"
     Choose (LitInt 1) e <- lhs
     pure (someUnderscore e)
 <|>
  do label "ITER-CHOOSE"
     interest 2
     Iter f body e0 <- lhs
     (exis, ctx, Choose n e) <- evalCtxExis (free body) body
     guard (ctx /= HOLE)
     guard (n `notElem` [Lit (LInt 0), Lit (LInt 1)])
     guard (choiceFreeLH ctx)
     guard (free n `disjointFrom` exis)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     let k = identNotIn (free (body,e0) ++ exis)
     labelArg (text (show f))
     pure ( Exi $ bind k $
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
 <|>
  do label "ONE-CHOOSE"
     interest 2
     Iter IterOne (Choose n e) e0 <- lhs
     pure $ mkIf ((n :=: Lit (LInt 0)) :>: lamUnderscore e0)
                 (someUnderscore e)
 <|>
  do label "IF-CHOOSE"
     interest 2
     Iter IterIf (Choose n e) e0 <- lhs
     pure $ mkIf ((n :=: Lit (LInt 0)) :>: lamUnderscore e0)
                 (mkApp (someUnderscore e) (Tup []))
 <|>
  do label "ALL-CHOOSE"
     interest 2
     Iter IterAll (Choose n e) e0 <- lhs
     let ys:zs:_ = identsNotIn (free (n,e,e0))
     pure $
       mkDef ys e0 $ \ys' ->
         Exi $ bind zs $
           (Op ArrApp :@: Tup [ Arr n e, ys', Var zs ]) >>>
           Var zs
 <|>
  do label "FOR-CHOOSE"
     interest 2
     Iter IterFor (Choose n e) e0 <- lhs
     let k:xs:ys:zs:_ = identsNotIn (free (n,e,e0))
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
 <|>
  do label "VERIFY-CHOOSE"
     interest 2
     (_skols, rs, as, body) <- matchVerify =<< lhs
     (exis, ctx, Choose n e) <- evalCtxExis (free body) body
     guard (ctx /= HOLE)
     guard (choiceFreeLH ctx)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     pure $
       Verify $ bindList rs
       ( as
       , mkExis exis $ ctx <@ ((Op Gt :@: Tup [n, Lit (LInt 0)]) >>> someUnderscore e)
       )

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

verifyRules :: Rule Expr
verifyRules =
   do label "VERIFY-VAL"
      (_skols, _rs, _as, v) <- matchVerify =<< lhs
      guard (isVal v)
      pure (Tup [])
  <|>
   do label "VERIFY-FAIL"
      (_skols, _rs, _as, Fail) <- matchVerify =<< lhs
      pure (Tup [])
  <|>
   do label "VERIFY-ERR"
      (_skols, _rs, _as, Err s) <- matchVerify =<< lhs
      pure (Err s)
  <|>
   do label "VERIFY-CHOICE"
      interest 2
      (_skols, rs, as, e) <- matchVerify =<< lhs
      (ctx, e1 :|: e2) <- evalCtx [] e
      guard (blocked ctx)
      pure (     (Verify $ bindList rs (as,ctx <@ e1))
             >>> (Verify $ bindList rs (as,ctx <@ e2)) )
  <|>
   do label "SOLVER"
      interest 2
      (_skols, _rs, as, _e) <- matchVerify =<< lhs
      asms <- assumps
      Just reason <- pure (unsat (asms ++ as))
      labelArg (pPrint reason)
      pure (Tup [])
  <|>
   do label "SKOLEMIZE"
      interest 2
      (all_rs, rs, as, e) <- matchVerify =<< lhs
      (ctx, (_, Some v)) <- proofX all_rs e
      guard (skolValue all_rs v)
      let x  = identNotIn (occurs ctx)
          r  = skolNotIn all_rs
      labelArg ( sep [ text "r=" <> pPrint r, text "x=" <> pPrint x
                     , text "rs=" <> pPrint rs ] )
      pure (Verify $ bindList (r:rs)
                 (as, Exi $ bind x $
                    Var x :=: (v :@: Var r) :>: (ctx <@ Var x) ))

--------------------------------------------------------------------------------

splitRules :: Rule Expr
splitRules =
  do label "DROP-VERIFY"
     interest 2
     (_, rs, as, e) <- matchVerify =<< lhs
     (ctx, Verify inner_bind) <- evalCtx [] e
     guard (blocked ctx)
     labelArg (pPrint (fst (unsafeUnbindList inner_bind)))
        -- Identify the site by showing the binders of the inner verify
     pure (Verify (bindList rs (as, ctx <@ Tup [])))
 <|>
  do label "SPLIT-V"
     interest 2
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, (Var r :=: v) :>: rest)) <- proofX all_rs e
     guard (r `elem` all_rs)
     Just gv <- pure (groundValue all_rs v)
     labelArg (pPrint r <+> text "=" <+> pPrint v)
     pure ( caseSplit rs (A_GVEq (GVVar r) gv) as ctx rest )

 <|>
  do label "SPLIT-OP"
     interest 2
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, Op op :@: arg)) <- proofX all_rs e
     guard (op /= IsArr && op /= DotDot)
       -- TODO: this is a bit awkward
       -- Can't split on DotDot because it produces many results
     Just gv <- pure (groundValue all_rs arg)
     guard (free gv `intersects` all_rs)
     let r    = skolNotIn all_rs
         asm  = A_PrimOp r (AO_Prim op) gv
         asmF = A_RelOp op gv
     if primOpCanFail op then
       do labelArg (pPrint asmF)
          pure (caseSplit (r:rs) asmF as ctx (Var r))
      else
       do labelArg (pPrint asm)
          pure (Verify (bindList (r:rs) (asm : as, ctx <@ Var r)))
 <|>
  do interest 2
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, Var f :@: arg)) <- proofX all_rs e
     guard (f `elem` all_rs)   -- f is a skolem
     case groundValue all_rs arg of
       Just gv -> do { let r    = skolNotIn all_rs
                           asm  = A_PrimOp r AO_Apply (GVArr [GVVar f, gv])
                     ; label "SPLIT-APP"
                     ; labelArg (pPrint asm)
                     ; pure (Verify (bindList (r:rs) (asm : as, ctx <@ Var r))) }
       Nothing | Lam b <- arg
               , all_rs `includes` free b
               -> do { let r = skolNotIn all_rs
                     ; label "SPLIT-APP-HO"
                     ; pure (Verify (bindList (r:rs) (as, ctx <@ Var r))) }
               | otherwise
               -> Core.Rule.empty   -- Fails in the Rule monad
 <|>
  do label "SPLIT-ISARR"
     interest 2
       -- verify(R,r;A){ P[ isArr$[r] ] }
       --  --> verify(R,r,n;A,isArr$[r],isInt$[n], n=arrLen$[r], n>=0){ P[ Arr(.){some(any)} ] }
       --      ..and the fail case..
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, Op IsArr :@: Var r)) <- proofX all_rs e
     guard (r `elem` all_rs)   -- r is a skolem
     let n        = skolNotIn all_rs
         r_asm    = A_RelOp IsArr (GVVar r)
         n_asms   = [ A_RelOp IsInt (GVVar n)
                    , A_RelOp GEq (GVArr [GVVar n, GVLit (LInt 0)]) ]
         neg_asms = [A_Pred $ A_Neg r_asm]
         pos_asms = A_PrimOp n (AO_Prim ArrLen) (GVVar r) : map (A_Pred . A_Pos) (r_asm:n_asms)
     labelArg (pPrint r)
     pure ( (Verify (bindList rs (neg_asms ++ as, ctx <@ Fail)))
           >>>
            (Verify (bindList (n:rs) (pos_asms ++ as, ctx <@ Arr (Var n) someAny))) )
 <|>
  do label "SPLIT-TUP"
     interest 2
     (all_rs, rs, as, e) <- matchVerify =<< lhs
     (ctx, (_, Var r :=: Tup vs :>: rest)) <- proofX all_rs e
     guard (r `elem` all_rs)
     let rs'  = take (length vs) (skolsNotIn all_rs)
         rvs' = foldr (:>:) rest [ Var r' :=: v | (r', v) <- rs' `zip` vs ]
         asm  = A_GVEq (GVVar r) (GVArr (map GVVar rs'))
     labelArg (pPrint asm)
     pure (caseSplit (rs ++ rs') asm as ctx rvs')
 <|>
   do label "SPLIT-TRU"
      interest 2
      (all_rs, rs, as, e) <- matchVerify =<< lhs
      (ctx, (_, Var r :=: Tru v :>: rest)) <- proofX all_rs e
      guard (r `elem` all_rs)
      let r'  = skolsNotIn all_rs !! 0
          rv' = (Var r' :=: v) :>: rest
          asm    = A_GVEq (GVVar r) (GVTru (GVVar r'))
      labelArg (pPrint asm)
      pure (caseSplit (rs ++ [r']) asm as ctx rv')

matchVerify :: Expr -> Rule ([SkolIdent], [SkolIdent], [Assump], Expr)
matchVerify e =
  do Verify bnd <- pure e
     env_rs <- skolems
     let (new_rs, (as, body)) = alphaRenameVerify env_rs bnd
     pure (new_rs ++ env_rs, new_rs, as, body)

caseSplit :: [Ident] -> FailableAssump -> [Assump] -> Context -> Expr -> Expr
caseSplit rs a as ctx e
  = Verify (bindList rs (A_Pred (A_Neg a) : as, ctx <@ Fail))
    >>>
    Verify (bindList rs (A_Pred (A_Pos a) : as, ctx <@ e))

--------------------------------------------------------------------------------
-- | Contexts ------------------------------------------------------------------
--------------------------------------------------------------------------------

proofX :: [Ident] -> Expr -> Rule ( Context    -- The context
                                  , ( [Ident]  -- Flexible existentials bound by context
                                    , Expr ))  -- The expression in the hole
-- P context
proofX bs e =
  do (ctx, stuff) <- proto_proofX (LX { exi_flexi = [], exi_rigid = bs }) e
     guard (blocked ctx)
     -- guard (blkd (LX { exi_flexi = [], exi_rigid = bs}) ctx)
     pure (ctx, stuff)

proto_proofX :: LocalExis -> Expr -> Rule (Context, ([Ident], Expr))
proto_proofX lx e =
  do pure (HOLE, (exi_flexi lx, e))
 <|>
  do x :>: e2 <- pure e
     (ctx, hole) <- proto_proofX lx x
     pure (ctx :>: e2, hole)
 <|>
  do eq :>: x <- pure e
     guard (choiceFreeLH eq)
     (ctx, hole) <- proto_proofX lx x
     pure (eq :>: ctx, hole)
 <|>
  do v :=: x <- pure e
     (ctx, hole) <- proto_proofX lx x
     pure (v :=: ctx, hole)
 <|>
  do Exi bnd <- pure e
     let (x,e1) = alphaRename (allExis lx) bnd
     (ctx, hole) <- proto_proofX (addFlexi lx x) e1
     pure (Exi (bind x ctx), hole)
 <|>
  do x :|: e2 <- pure e
     (ctx, hole) <- proto_proofX (makeRigid lx) x
     pure (ctx :|: e2, hole)
 <|>
  do e1 :|: x <- pure e
     (ctx, hole) <- proto_proofX (makeRigid lx) x
     pure (e1 :|: ctx, hole)
 <|>
  do x :>>: e2 <- pure e
     (ctx, hole) <- proto_proofX lx x
     pure (ctx :>>: e2, hole)
 <|>
  do Iter f e1 e0 <- pure e
     (ctx, hole) <- proto_proofX (makeRigid lx) e1
     pure (Iter f ctx e0, hole)
 <|>
  do Iter f e1 e0 <- pure e
     (ctx, hole) <- proto_proofX (makeRigid lx) e0
     pure (Iter f e1 ctx, hole)
