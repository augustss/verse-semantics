module Rules.TRS2024 where

import Control.Monad( guard )
import TRS.Bind
import Rules.Core

import Data.List( (\\) )

--------------------------------------------------------------------------------

rules :: Rule
rules = rulesApplication
     <> rulesUnification
     <> rulesExistentials
     <> rulesNormalization
     <> rulesChoice
     <> rulesOneAndAll

--------------------------------------------------------------------------------

name :: String -> [Expr] -> [(String,Expr)]
name s es = [ (s,e) | e <- es ]

--------------------------------------------------------------------------------

-- value contexts
isV :: Expr -> Expr -> Bool
isV x e = x==e || case e of
                    Arr es -> any (isV x) es
                    _      -> False

-- evaluation contexts (no existentials anymore!)
evalCtx :: Expr -> [(Context, Expr)]
evalCtx lhs =
  do pure (HOLE, lhs)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx e1
     pure ((v :=: ctx) :>: e2, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx e2
     pure ((v :=: e1) :>: ctx, h)

-- valid/blkd
validC :: Ident -> [Ident] -> Context -> Bool
validC x ys HOLE        = True
validC x ys ((v :=: e) :>: ctx)
  | x `elem` zs = blkd (x:ys) ((v :=: e) :>: ctx)
  | otherwise   = (blkd (x:ys) e && validC x ys ctx)
                  || validC x (filter (`notElem` zs) ys) ctx
 where
  zs = free (v,e)
validC x ys _ = False

type Expr_or_Context = Expr

blkd :: [Ident] -> Expr_or_Context -> Bool
blkd xs HOLE                = True
blkd xs ((v :=: e1) :>: e2) = blkd xs e1 && (isContext e1 || blkd xs e2)
blkd xs (e1 :|: e2)         = blkd xs e1 && blkd xs e2
blkd xs (One e)             = blkd xs e
blkd xs (All e)             = blkd xs e
blkd xs (Exi bnd)           = blkd (x:xs) e where (x,e) = alphaRename xs bnd
blkd xs (v1 :@: v2)         = case v1 of
                                Var f -> f `elem` xs
                                Op op -> any ((`isV` v2) . Var) xs
                                _     -> False
blkd xs (v :>>: e)          = any (`elem` xs) (free v)
blkd xs (Verify _)          = True
blkd xs (Check fx e)        = blkd xs e
blkd xs _                   = False

-- choice-freeness
choiceFree :: Expr_or_Context -> Bool
choiceFree (e1 :|: e2)         = False
choiceFree ((v :=: e1) :>: e2) = choiceFree e1 && (isContext e1 || choiceFree e2)
choiceFree (v :>>: e)          = choiceFree e
choiceFree (Exi bnd)           = choiceFree e where (x,e) = unsafeUnbind bnd
choiceFree (v1 :@: v2)         = case v1 of
                                   Op _ -> True -- all ops we support are choice-free right now
                                   _    -> False
choiceFree _                   = True

-- scope contexts
scopeCtx :: Expr -> [(Context, Expr)]
scopeCtx (One e)     = [(One HOLE, e)]
scopeCtx (All e)     = [(All HOLE, e)]
scopeCtx (e1 :|: e2) = [(HOLE :|: e2, e1), (e1 :|: HOLE, e2)]
scopeCtx _           = []

--------------------------------------------------------------------------------

rulesApplication :: Rule
rulesApplication lhs =
  "APP-ADD" `name`
  do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1+k2))
 ++
  "APP-SUB" `name`
  do Op Sub :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1-k2))
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     guard (k1 > k2)
     pure (Int k1)
 ++
  "APP-GT-FAIL" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     guard (k1 <= k2)
     pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: a <- [lhs]
     guard (isHNF a)
     case a of
       Int _ -> pure a
       _     -> pure Fail
 ++
  "APP-LAM" `name`
  do Lam bnd :@: v <- [lhs]
     guard (isVal v)
     let (x,e) = alphaRename (free v) bnd
     pure (Exi (bind x ((Var x :=: v) :>: e)))
 ++
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     guard (isVal v && all isVal vs)
     pure (foldr1 (:|:) [ (v :=: Int i) :>: vi | (i,vi) <- [0..] `zip` vs ])
 ++
  "APP-TUP-0" `name`
  do Arr [] :@: v <- [lhs]
     guard (isVal v)
     pure Fail

--------------------------------------------------------------------------------

rulesUnification :: Rule
rulesUnification lhs =
  "U-LIT" `name`
  do (Int k1 :=: Int k2) :>: e <- [lhs]
     guard (k1 == k2)
     pure e
 ++
  "U-TUP" `name`
  do (Arr vs :=: Arr vs') :>: e <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) e [ v :=: v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-FAIL" `name`
  do (a1 :=: a2) :>: _ <- [lhs]
     guard (isHNF a1 && isHNF a2)
     guard $
       case (a1, a2) of
         (Int k1, Int k2)  -> k1 /= k2
         (Arr vs, Arr vs') -> length vs /= length vs'
         (_,      _)       -> True
     pure Fail
 ++
  "U-OCCURS" `name`
  do (x@(Var _) :=: v) :>: _ <- [lhs]
     guard (isV x v && v /= x)
     pure Fail
 ++
  "U-SWAP" `name`
  do (a :=: Var x) :>: e <- [lhs]
     guard (isHNF a)
     pure ((Var x :=: a) :>: e)

--------------------------------------------------------------------------------

rulesExistentials :: Rule
rulesExistentials lhs =
  "EXI-SUBST" `name`
  do (exis,x,ctx_xv_e) <- matchExi_alphaRename [] lhs
     (ctx, x_eq_v :>: e) <- evalCtx ctx_xv_e
     (Var x',v) <- matchEq x_eq_v
     guard (x == x')
     guard (isVal v)
     guard (x `notElem` free v)
     guard (validC x (bvs exis) ctx)
     pure (exis <@ (subst [(x,v)] (ctx <@ e)))
 ++
  "EXI-ELIM" `name`
  do (exis,x,e) <- matchExi_alphaRename [] lhs
     guard (x `notElem` free e)
     pure (exis <@ e)
 ++
  "EXI-FLOAT" `name`
  do (ctx,exi_x_e) <- evalCtx lhs
     guard (ctx /= HOLE)
     (exis,x,e) <- matchExi_alphaRename (free ctx) exi_x_e
     pure (Exi (bind x (ctx <@ (exis <@ e))))
 ++
  "EXI-CHOICE" `name`
  do (exis,x,e) <- matchExi_alphaRename [] lhs
     (ctx,e1 :|: e2) <- evalCtx e
     guard (x `notElem` free ctx)
     pure (exis <@ (ctx <@ (Exi (bind x e1) :|: Exi (bind x e2))))

--------------------------------------------------------------------------------

rulesNormalization :: Rule
rulesNormalization lhs =
  "SEQ-ASSOC" `name`
  do (v2 :=: ((v1 :=: e1) :>: e2)) :>: e3 <- [lhs]
     pure ((v1 :=: e1) :>: ((v2 :=: e2) :>: e3))
 ++
  "SEQ-ELIM" `name`
  [] -- we do not need SEQ-ELIM because we don't actually have _=e1;e2
 ++
  "REC" `name`
  [] -- TODO

--------------------------------------------------------------------------------

rulesChoice :: Rule
rulesChoice lhs =
  "CHOICE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
  "CHOICE-FAIL-L" `name`
  do Fail :|: e <- [lhs]
     pure e
 ++
  "CHOICE-FAIL-R" `name`
  do e :|: Fail <- [lhs]
     pure e
 ++
  "CHOICE" `name`
  do (sx, e) <- scopeCtx lhs
     (ctx, e1 :|: e2) <- evalCtx e
     guard (ctx /= HOLE)
     guard (choiceFree ctx)
     pure (sx <@ ((ctx <@ e1) :|: (ctx <@ e2)))
 ++
  "FAIL" `name`
  do let (exis, e) = unExis lhs
     (ctx, Fail) <- evalCtx e
     guard (ctx /= HOLE)
     guard (blkd (bvs exis) ctx)
     pure Fail

--------------------------------------------------------------------------------

rulesOneAndAll :: Rule
rulesOneAndAll lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-VALUE" `name`
  do One v <- [lhs]
     guard (isVal v)
     pure v
 ++
  "ONE-CHOICE" `name`
  do One (v :|: _) <- [lhs]
     guard (isVal v)
     pure v
 ++
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All e <- [lhs]
     let choices (e1 :|: e2) = choices e1 ++ choices e2
         choices e1          = [e1]
     let vs = choices e
     guard (all isVal vs)
     pure (Arr vs)

-----------------------------------------------------------------------------------
