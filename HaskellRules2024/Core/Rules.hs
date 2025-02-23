{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Core.Rules
  ( runtimeRules
  , labelArg
  , without
  , skolValue
  , evalCtxExis
  , evalCtx
  )
 where

import Prelude

import Core.Expr
import Core.Bind
import Core.Rule
import Core.Blocked
import Epic.Print hiding ( (<>), empty )

import Control.Monad( guard )
import Control.Applicative( Alternative(..) )
import Data.List( (\\), isPrefixOf )

--------------------------------------------------------------------------------
-- runtime rules in one rule

runtimeRules :: Rule Expr
runtimeRules = applicationRules
           <|> unificationRules
           <|> normalizationRules
           <|> substitutionRules  -- SUBST, REC, EXI-APP
           <|> failAndErrRules
           <|> iterRules
           <|> dotDotRules        -- remove DOTDOT-EXPAND for verification!

--------------------------------------------------------------------------------
-- Rule-specific auxiliary functions

labelArg :: Doc -> Rule ()
labelArg d = label (render (parens d))

without :: Rule a -> String -> Rule a
r `without` lab = only (\s -> not ((lab++"(") `isPrefixOf` (s++"("))) r

--------------------------------------------------------------------------------
-- auxiliary functions

skolValue :: [SkolIdent] -> Expr -> Bool
-- A value whose only free vars are skolems
skolValue rs e = isVal e && null (free e \\ rs)

--------------------------------------------------------------------------------
-- application rules

applicationRules :: Rule Expr
applicationRules =
      appLambda
  <|> appTruth
  <|> appArray
  <|> appPrimOp

appLambda :: Rule Expr
appLambda =
  do label "APP-LAMBDA"
     Lam bnd :@: v <- lhs
     let (x,e) = alphaRename (free v) bnd
         body  = (Var x :=: v) :>: e
     pure (if isUnderscore x
           then body
           else Exi (bind x body))

appTruth :: Rule Expr
appTruth =
  do label "APP-TRUTH"
     Tru a :@: v <- lhs
     pure ((v :=: a) :>: a)

appArray :: Rule Expr
appArray =
  do label "APP-TUP0"
     -- <>[k] --> fail
     interest 0
     Tup [] :@: _v <- lhs
     pure Fail
 <|>
{-
  do label "APP-TUPK"
     -- <v1,..,vn>[k] --> vk
     -- This rule isn't needed, but it makes the reduction
     -- sequence much shorter when indexing with a constant
     -- (it's also unsound/not confluent with our current way of doing choice,
     -- which is why it is commented out for now)
     Tup vs :@: Lit (LInt i) <- lhs
     guard (all isVal vs)
     let i' = fromInteger i
     if 0 <= i' && i' < length vs then
       pure (vs !! i')
      else
       pure Fail
 <|>
-}
  do label "APP-TUP"
     -- <v1,..,vn>[v] --> (v=1;v1) | .. | (v=n;vn)
     -- This does narrrowing
     Tup vs@(_:_) :@: v <- lhs
     pure (foldr1 (:|:) [ (v :=: LitInt i) :>: vi | (i,vi) <- [0..] `zip` vs ])

appPrimOp :: Rule Expr
appPrimOp =
  do label "APP"
     interest 0
     choices
       -- negation
       [ do Op Neg :@: LitInt k <- lhs
            label "-NEG"
            pure (LitInt (-k))

       -- binary arithmetic functions
       , do Op op :@: Tup [LitInt k1, LitInt k2] <- lhs
            (lab, ans) <- case op of
                            Add         -> pure ("ADD", k1+k2)
                            Sub         -> pure ("SUB", k1-k2)
                            Mul         -> pure ("MUL", k1*k2)
                            Div | k2/=0 -> pure ("DIV", k1 `div` k2)
                            _           -> empty
            label ("-" ++ lab)
            pure (LitInt ans)

       -- binary arithmetic predicates
       , do Op op :@: Tup [LitInt k1, LitInt k2] <- lhs
            (lab, ans) <- case op of
                            Lt  -> pure ("LT",  k1<k2)
                            Gt  -> pure ("GT",  k1>k2)
                            LEq -> pure ("LEQ", k1<=k2)
                            GEq -> pure ("GEQ", k1>=k2)
                            NEq -> pure ("NEQ", k1/=k2)
                            _   -> empty
            label ("-" ++ lab)
            case ans of
              True  -> pure (LitInt k1)
              False -> label "-FAIL" >> pure Fail

       -- unary predicates
       , do Op op :@: a <- lhs
            (lab, ans) <- case op of
                            IsInt  -> pure ("ISINT",  [a|Lit(LInt _)<-[a]])
                            IsStr  -> pure ("ISSTR",  [a|Lit(LStr _)<-[a]])
                            IsChar -> pure ("ISCHAR", [a|Lit(LChar _)<-[a]])
                            IsComp -> pure ("ISCOMP", [a|isComparable a])
                            IsArr  -> pure ("ISARR",  [a|Tup{}<-[a]]++[a|Arr{}<-[a]])
                            _      -> empty
            label ("-" ++ lab)
            case ans of
              [v]          -> pure v
              [] | isHNF a -> label "-FAIL" >> pure Fail
              _            -> empty

       -- isGround (not used anywhere right now, we could remove this Op and rule)
       , do label "-ISGROUND"
            Op IsGround :@: a <- lhs
            skols <- skolems
            guard (skolValue skols a)
            pure a

       -- array operations
       , do label "-LENGTH"
            Op ArrLen :@: a <- lhs
            case a of
              Tup xs   -> pure (LitInt (fromIntegral (length xs)))
              Arr sz _ -> pure sz
              _        -> empty   -- No match here

       , do label "-ARRMAP"
            Op ArrMap :@: arg@(Tup [f, arr@(Tup vs)]) <- lhs
            let prs = (identsNotIn $ free arg) `zip` vs
                bind_one (x,v) e = Exi $ bind x $ (Var x :=: (f :@: v)) :>: e
            labelArg (pPrint arr)
            pure (foldr bind_one (Tup [Var x | (x,_) <- prs]) prs)

       , do label "-ARRAPP"
            Op ArrApp :@: Tup [a1,a2,res] <- lhs
            choices
              -- <vs1>++<vs2> = res  -->  <vs1++vs2> = res
              [ do Tup vs1 <- pure a1
                   Tup vs2 <- pure a2
                   pure $ (Tup (vs1++vs2) :=: res) :>: res

              -- <>++a2 = res  -->  a2 = res
              -- a1++<> = res  -->  a1 = res
              , do label "-TUP0-L"
                   Tup [] <- pure a1
                   pure $ (a2 :=: res) :>: res

              , do label "-TUP0-R"
                   Tup [] <- pure a2
                   pure $ (a1 :=: res) :>: res

              -- a1++a2 = <>  -->  a1=<>; a2=<>
              , do label "-TUP0"
                   Tup [] <- pure res
                   pure $ (a1 :=: Tup []) :>: (a2 :=: Tup []) :>: res

              -- <v,vs>++a2 = <w,ws>  -->  v=w; <vs>++a2 = <ws>; <w,ws>
              -- a1++<vs,v> = <ws,w>  -->  a1++<vs> = <ws>; v=w; <ws,w>
              , do label "-TUP-L"
                   Tup (v:vs) <- pure a1
                   Tup (w:ws) <- pure res
                   pure ( (v :=: w)
                      :>: (Op ArrApp :@: Tup [Tup vs,a2,Tup ws])
                      >>> res
                        )
              
              , do label "-TUP-R"
                   Tup (vs@(_:_)) <- pure a2
                   Tup (ws@(_:_)) <- pure res
                   pure ( (Op ArrApp :@: Tup [a1,Tup (init vs),Tup (init ws)])
                      >>> ((last vs :=: last ws)
                      :>: res)
                        )
              ]
       ]

--------------------------------------------------------------------------------
-- unification rules

unificationRules :: Rule Expr
unificationRules =
  do label "U-LIT"
     (Lit l1 :=: Lit l2) :>: e <- lhs
     pure $ if l1 == l2
       then e
       else Fail
 <|>
  do label "U-TUP"
     (Tup vs :=: Tup vs') :>: e <- lhs
     pure $ if length vs == length vs'
       then foldr (:>:) e [ v :=: v' | (v,v') <- vs `zip` vs' ]
       else Fail
 <|>
  do label "U-TRU"
     (Tru v :=: Tru v') :>: e <- lhs
     pure ((v :=: v') :>: e)
 <|>
  do label "U-FAIL"
     (a1 :=: a2) :>: _ <- lhs
     guard (isHNF a1 && isHNF a2)
     guard $
       case (a1, a2) of
         (Lit {}, Lit {}) -> False  -- Handled by U-LIT
         (Tup {}, Tup {}) -> False  -- Handled by U-TUP
         (Tru {}, Tru {}) -> False  -- Handled by U-TRU
         (Tup {}, Arr {}) -> False  -- }
         (Arr {}, Tup {}) -> False  -- } Handled by U-ARR (in Verify.hs)
         (Arr {}, Arr {}) -> False  -- }
         (_,      _)      -> True
     pure Fail
 <|>
  do label "U-OCCURS"
     (x@(Var _) :=: v) :>: _ <- lhs
     guard (v /= x)
     (_ctx, e) <- valueCtx v
     guard (e == x)
     pure Fail
 <|>
  do label "U-SWAP"
     (a :=: Var x) :>: e <- lhs
     guard (isHNF a)
     pure ((Var x :=: a) :>: e)

--------------------------------------------------------------------------------
-- normalization rules

normalizationRules :: Rule Expr
normalizationRules =
  do label "SEQ-ASSOC"
     interest 0
     (v2 :=: ((v1 :=: e1) :>: e2)) :>: e3 <- lhs
     pure ((v1 :=: e1) :>: ((v2 :=: e2) :>: e3))
 <|>
  do label "UNDERSCORE-ELIM"
     interest 0
     (Var u :=: v) :>: e <- lhs
     guard (isUnderscore u)
     guard (isVal v)
     pure e
 <|>
  do label "EXI-ELIM"
     interest 0
     (exis,x,e) <- matchExi_alphaRename [] =<< lhs
     guard (x `notElem` free e)
     labelArg (pPrint x)
     pure (exis <@ e)
 <|>
  do label "EXI-FLOAT"   -- v=(∃x.e1);e2 --> ∃x.v=e1;e2
     interest 0
     (v :=: exi_x_e1) :>: e2 <- lhs
     (exis,x,e1) <- matchExi_alphaRename (free (v,e2)) exi_x_e1
     labelArg (pPrint x)
     pure (Exi (bind x ((v:=:(exis <@ e1)):>:e2)))
 <|>
     -- EXI-PUSH is necessary to let us do
     --    exi x. f[y]; x=3; 3+1; blah
     -- Here we want to substitute for x despite the intervening f[y]
  do label "EXI-PUSH"   -- ∃x.v=e1;e2 --> v=e1;∃x.e2 
     interest 0
     (exis,x,(v :=: e1) :>: e2) <- matchExi_alphaRename [] =<< lhs
     guard (x `notElem` free (v,e1))
     labelArg (pPrint x)
     pure (exis <@ ((v :=: e1) :>: Exi (bind x e2)))

--------------------------------------------------------------------------------
-- substitution rules

substitutionRules :: Rule Expr
substitutionRules =
  do label "EXI-SUBST"
     e0 <- lhs
     (exis, ctx, x_eq_v :>: e) <- evalCtxExis (free e0) e0 
     (Var x,v) <- matchEq x_eq_v
     guard (x `elem` exis)
     guard (isVal v)
     guard (x `notElem` free v)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     labelArg (pPrint x <+> text ":=" <+> pPrintSmallExpr v)
     pure (mkExis (exis \\ [x]) $ subst [(x,v)] (ctx <@ e))
 <|>
  -- x=V[\y.body]  --> x = V[\y. exists x. x=V[\y.body]; body]
  --   if x/=y, and x free in body
  do label "REC"
     interest 2
     Var x :=: v <- lhs
     (ctx, Lam bnd) <- valueCtx v
     let (y,body) = alphaRename [x] bnd
     guard (x `elem` free body)
     pure (Var x :=: ctx <@ (Lam $ bind y $
                             Exi $ bind x $
                             (Var x :=: v) :>: body))

 <|>
  do label "EXI-APP"
     interest 2
     (exis,f,e) <- matchExi_alphaRename [] =<< lhs
     guard (f `elem` free e)
     guard (onlyApps f e)
     labelArg (pPrint f)
     let x     = ident "x"
     let dummy = lamUnderscore (Exi (bind x (Var x)))
     pure (exis <@ Exi (bind f ((Var f :=: dummy) :>: e)))

onlyApps :: Ident -> Expr -> Bool
-- (onlyApps f e) returns True if the only occurrences of
-- `f` in `e` are in applications f[v]
onlyApps f orig_e = go orig_e
  where
    go (Lit {})     = True
    go (Op {})      = True
    go Fail         = True
    go (Err {})     = True
    go HOLE         = True

    go (Var x)      = x /= f

    go (Tup es)     = all go es
    go (Tru e)      = go e
    go (Lam bnd)    = go_bind bnd
    go (e1 :=: e2)  = go e1 && go e2
    go (e1 :>: e2)  = go e1 && go e2
    go (e1 :|: e2)  = go e1 && go e2
    go (e1 :@: e2)  = e1 == Var f || (go e1 && go e2)
    go (Iter _ e e0) = all go [e,e0]
    go (Some e)       = go e
    --go (All e)        = go e
    go (e1 :>>: e2)   = go e1 && go e2
    --go (Check _ e)    = go e
    go (Arr    sz e)  = go sz && go e
    --go (Size   sz e)  = go sz && go e
    go (Choose sz e)  = go sz && go e
    go (Exi bnd)      = go_bind bnd

    -- ToDo: Lennart thought this was impossible. Why?
    go (Verify bnd) = go e
                    where
                      (_,(_,e)) = alphaRenameVerify [f] bnd

    go_bind bnd = f == x || go e
                where
                  (x,e) = alphaRename [f] bnd

--------------------------------------------------------------------------------

failAndErrRules :: Rule Expr
failAndErrRules =
  do label "FAIL"
     (ctx, Fail) <- evalCtx [] =<< lhs
     guard (ctx /= HOLE)
     guard (blocked ctx)
     pure Fail
 <|>
  do label "ERR"
     interest 2
     (ctx, Err s) <- evalCtx [] =<< lhs
     guard (ctx /= HOLE)
     guard (blocked ctx)
     pure (Err s)
 <|>
  do label "ITER-ERR"
     interest 2
     Iter _f (Err s) _e0 <- lhs
     pure (Err s)

--------------------------------------------------------------------------------

iterRules :: Rule Expr
iterRules =
  -- iter(f){ fail }{e0}  -->  e0
  do label "ITER-FAIL"
     Iter f Fail e0 <- lhs
     labelArg (text (show f))
     pure e0
 <|>
  -- iter(f){ v }{e0}  -->  f[v][\_.e0]
  do label "ITER-VALUE"
     Iter f v e0 <- lhs
     labelArg (text (show f))
     guard (isVal v)
     pure (iterApply f v e0)
 <|>
  -- iter(f){ C[e1] | C[e2] }{e0}  -->  iter(f){ C[e1] }{ iter(f){ C[e2} }{e0} }
  do label "ITER-CHOICE"
     Iter f e e0 <- lhs
     labelArg (text (show f))
     (ctx, e1 :|: e2) <- evalCtx [] e
     guard (choiceFreeLH ctx)
     guard (blocked ctx)
     pure (Iter f (ctx <@ e1) (Iter f (ctx <@ e2) e0))

--------------------------------------------------------------------------------

dotDotRules :: Rule Expr
dotDotRules =
  do label "DOTDOT-CONST"
     Op DotDot :@: Tup [k@(Lit {}), n] <- lhs
     labelArg (pPrint k)
     pure ((Var underscore :=: inRange k n) :>: Tup [])
 <|>
  -- Used only for evaluation, not verification!
  -- Also, try it /last/ so that DOTDOT-CONST gets first dibs
  do label "DOTDOT-EXPAND"
     Op DotDot :@: Tup [v, Lit (LInt k)] <- lhs
     labelArg (pPrint k)
     let the_choice = foldr ((:|:) . Lit . LInt) Fail [0..(k-1)]
     pure ((v :=: the_choice) :>: Tup [])

{- Here is a more conservative version, but we probably don't need it
   because DotDot$ is rare except during verification.

  -- Expand DotDot$[i,100] only if you really have to;
  -- i.e. i is an existential we are blocked on
  "DOTDOT-EXPAND" `labelRuleWith`
  do (exis, ctx, Op DotDot :@: Tup [Var x, Lit (LInt k)]) <- evalCtxExis (free lhs) lhs
     guard (x `elem` exis)
     guard (blkd (LX { exi_flexi = exis, exi_rigid = [] }) ctx)
     let the_choice = foldr ((:|:) . Lit . LInt) Fail [0..(k-1)]
     pure (pPrint k, (Var x :=: the_choice) :>: Tup [])
-}

--------------------------------------------------------------------------------
-- structural rules matching

matchExi_alphaRename :: [Ident] -> Expr -> Rule (Context, Ident, Expr)
-- matches C[e] (with ∃x somewhere in C) returning (C\\(∃x), x, e)
--    where x is properly alpha-renamed
matchExi_alphaRename orig_zs orig_e = choices (map pure (go orig_zs orig_e))
 where
  go zs e =
    [ cxe
    | Exi bnd <- [e]
    , let (x,ex) = alphaRename zs bnd
          cxes   = go (x:zs) ex
    , cxe <- -- just add "bind x" to the exis
             [ (Exi (bind x ctx),y,ey)
             | (ctx,y,ey) <- cxes
             ]
             -- add a case where "bind x" is the variable we're matching on
          ++ case cxes of
               [] -> [ (HOLE,x,ex) ]
               _  -> [ (Exi (bind y ctx),x,ey)
                     | (ctx,y,ey) <- [head cxes]
                     ]
    ]

matchEq :: Expr -> Rule (Expr,Expr)
-- matches (v = e), and also (v1 = v2) returning (v2 = v1)
matchEq e =
  choices
  [ pure (v', e1')
  | v :=: e1 <- [e]
  , (v',e1') <- (v,e1) : [ (Var y, Var x)
                         | (Var x, Var y) <- [(v,e1)]
                         ]
  ]

--------------------------------------------------------------------------------
-- Contexts

valueCtx :: Expr -> Rule (Context, Expr)
-- V ::= HOLE | <e1,..,V,..en>
-- Moreover we only return pairs whose Expr is /not/ a tuple
valueCtx v = 
   do pure (HOLE,v)
  <|>
   do Tup es <- pure v
      choices [ do (ctx,h) <- valueCtx (es !! i)
                   pure (Tup (take i es ++ [ctx] ++ drop (i+1) es), h)
              | i <- [0..length es-1]
              ]
  <|>
   do Tru a <- pure v
      (ctx,h) <- valueCtx a
      pure (Tru ctx, h)

-- Evaluation contexts

evalCtx :: [Ident] -> Expr -> Rule (Context, Expr)
evalCtx zs e =
  do pure (HOLE, e)
 <|>
  do Exi bnd <- pure e
     let (x,e1) = alphaRename zs bnd
     (ctx, h) <- evalCtx (x:zs) e1
     pure (Exi (bind x ctx), h)
 <|>
  do (v :=: e1) :>: e2 <- pure e
     (ctx, h) <- evalCtx zs e1
     pure ((v :=: ctx) :>: e2, h)
 <|>
  do (v :=: e1) :>: e2 <- pure e
     (ctx, h) <- evalCtx zs e2
     pure ((v :=: e1) :>: ctx, h)

evalCtxExis :: [Ident] -> Expr
            -> Rule ( [Ident]  -- All the 'exists x' bits
                    , Context  -- All the other bits; does not bind any variables
                    , Expr     -- The expression in the middle
                    )
-- E.g.   evalCtxtExis (exi x. x=3; exi y. y=5; x+y)
--        returns  ( [x,y]
--                 , x=3; y=5; HOLE
--                 , x+y )
evalCtxExis zs e =
  do pure ([], HOLE, e)
 <|>
  do Exi bnd <- pure e
     let (x,e1) = alphaRename zs bnd
     (exis, ctx, h) <- evalCtxExis (x:zs) e1
     pure (x:exis, ctx, h)
 <|>
  do (v :=: e1) :>: e2 <- pure e
     (exis, ctx, h) <- evalCtxExis zs e1
     pure (exis, (v :=: ctx) :>: e2, h)
 <|>
  do (v :=: e1) :>: e2 <- pure e
     (exis, ctx, h) <- evalCtxExis zs e2
     pure (exis, (v :=: e1) :>: ctx, h)

--------------------------------------------------------------------------------
