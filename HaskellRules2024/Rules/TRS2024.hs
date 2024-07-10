module Rules.TRS2024 (
     evalRules
   , blocked, choiceFree
   , name, nameWith, iff
 ) where

import Prelude

import TRS.Bind
import Rules.Core
import Epic.Print hiding ( (<>) )

import Control.Monad( guard )
import Data.List( (\\) )

--------------------------------------------------------------------------------
--
--            The rules themselves
--
--------------------------------------------------------------------------------

evalRules :: Rule
-- Runtime evauation rules
evalRules = rulesApplication
          <> rulesUnification
          <> rulesExistentials
          <> rulesNormalization
          <> rulesChoice
          <> rulesOneAndAll

--------------------------------------------------------------------------------
rulesApplication :: Rule
rulesApplication _env lhs =
  "APP-ADD" `name`
  do Op Add :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1+k2))
 ++
  "APP-SUB" `name`
  do Op Sub :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1-k2))
 ++
  "APP-MUL" `name`
  do Op Mul :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     pure (LitInt (k1*k2))
 ++
  "APP-DIV" `name`
  do Op Div :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k2 /= 0)
     pure (LitInt (k1 `div` k2))
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 > k2)
     pure (LitInt k1)
 ++
  "APP-GT-FAIL" `name`
  do Op Gt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 > k2))
     pure Fail
 ++
  "APP-LT" `name`
  do Op Lt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 < k2)
     pure (LitInt k1)
 ++
  "APP-LT-FAIL" `name`
  do Op Lt :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 < k2))
     pure Fail
 ++
  "APP-LE" `name`
  do Op LEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 <= k2)
     pure (LitInt k1)
 ++
  "APP-LE-FAIL" `name`
  do Op LEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 <= k2))
     pure Fail
 ++
  "APP-GE" `name`
  do Op GEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 >= k2)
     pure (LitInt k1)
 ++
  "APP-GE-FAIL" `name`
  do Op GEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 >= k2))
     pure Fail
 ++
  "APP-NE" `name`
  do Op NEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (k1 /= k2)
     pure (LitInt k1)
 ++
  "APP-NE-FAIL" `name`
  do Op NEq :@: Arr [LitInt k1, LitInt k2] <- [lhs]
     guard (not (k1 /= k2))
     pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: a <- [lhs]
     case a of
       Lit (LInt _) -> pure a
       _            -> []
 ++
  "APP-ISSTR" `name`
  do Op IsStr :@: a <- [lhs]
     case a of
       Lit (LStr _) -> pure a
       _            -> []  -- ToDo: what is idomatic here?
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
     pure (foldr1 (:|:) [ (v :=: LitInt i) :>: vi | (i,vi) <- [0..] `zip` vs ])
 ++
  "APP-TUP-0" `name`
  do Arr [] :@: v <- [lhs]
     guard (isVal v)
     pure Fail

--------------------------------------------------------------------------------
rulesUnification :: Rule
rulesUnification _env lhs =
  "U-LIT" `name`
  do (LitInt k1 :=: LitInt k2) :>: e <- [lhs]
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
         (LitInt k1, LitInt k2) -> k1 /= k2
         (Arr vs, Arr vs')      -> length vs /= length vs'
         (_,      _)            -> True
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
rulesExistentials _env lhs =
  "UNDERSCORE-ELIM" `name`
  do { (Var u :=: v) :>: e <- [lhs]
     ; guard (u == underscore)
     ; guard (isVal v)
     ; pure e }
 ++
  "EXI-ELIM" `nameWith`
  do (exis,x,e) <- matchExi_alphaRename [] lhs
     guard (x `notElem` free e)
     pure (pPrint x, exis <@ e)
 ++
  "EXI-FLOAT" `nameWith`
  do (v :=: exi_x_e1) :>: e2 <- [lhs]
     (exis,x,e1) <- matchExi_alphaRename (free (v,e2)) exi_x_e1
     pure (pPrint x, Exi (bind x ((v:=:(exis <@ e1)):>:e2)))
 ++
  "EXI-PUSH" `nameWith`
  do (exis,x,(v :=: e1) :>: e2) <- matchExi_alphaRename [] lhs
     guard (x `notElem` free (v,e1))
     pure (pPrint x, exis <@ ((v :=: e1) :>: Exi (bind x e2)))
  ++
  -- Do this last: most complex and expensive
  "EXI-SUBST" `nameWith`
  do (exis, ctx, x_eq_v :>: e) <- evalCtxLift (free lhs) lhs
     (Var x,v) <- matchEq x_eq_v
     guard (isVal v)
     guard (x `elem` exis)
     guard (x `notElem` free v)
     guard (blkd exis ctx)
     pure ( pPrint x <+> text ":=" <+> pPrintSmallExpr v
          , wrapExis (exis \\ [x]) $
            subst [(x,v)] (ctx <@ e) )

--------------------------------------------------------------------------------
rulesNormalization :: Rule
rulesNormalization _env lhs =
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
rulesChoice _env lhs =
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
  do (ctx, e1 :|: e2) <- evalCtx [] lhs
     guard (ctx /= HOLE)
     guard (choiceFree ctx)
     guard (blocked ctx)
     pure ((ctx <@ e1) :|: (ctx <@ e2))
 ++
  "FAIL" `name`
  do (ctx, Fail) <- evalCtx [] lhs
     guard (ctx /= HOLE)
     guard (blocked ctx)
     pure Fail

--------------------------------------------------------------------------------
rulesOneAndAll :: Rule
rulesOneAndAll _env lhs =
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


--------------------------------------------------------------------------------
--
--            Auxiliary functions
--
--------------------------------------------------------------------------------

name :: String -> [Expr] -> [(String,Expr)]
name s es = [ (s,e) | e <- es ]

-- This is used to give rules names.
nameWith :: String -> [(Doc, a)] -> [(String, a)]
nameWith rulename as = [(rulename ++ render (parens doc), a) | (doc, a) <- as]

iff :: [Bool] -> [()]
iff conds = [()| and conds]


--------------------------------------------------------------------------------
--
--            Value contexts
--
--------------------------------------------------------------------------------

isV :: Expr -> Expr -> Bool
isV x e = x==e || case e of
                    Arr es -> any (isV x) es
                    _      -> False

--------------------------------------------------------------------------------
--
--            Evaluation contexts
--
--------------------------------------------------------------------------------

evalCtx :: [Ident] -> Expr -> [(Context, Expr)]
evalCtx zs lhs =
  do pure (HOLE, lhs)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename zs bnd
     (ctx, h) <- evalCtx (x:zs) e
     pure (Exi (bind x ctx), h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx zs e1
     pure ((v :=: ctx) :>: e2, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (ctx, h) <- evalCtx zs e2
     pure ((v :=: e1) :>: ctx, h)

evalCtxLift :: [Ident] -> Expr
            -> [( [Ident]  -- All the 'exists x' bits
                , Context  -- All the other bits
                , Expr)]   -- The expression in the middle
-- E.g.   evalCtxtLift (exi x. x=3; exi y. y=5; x+y)
--        returns  ( [x,y]
--                 , x=3; y=5; HOLE
--                 , x+y )
evalCtxLift zs lhs =
  do pure ([], HOLE, lhs)
 ++
  do Exi bnd <- [lhs]
     let (x,e) = alphaRename zs bnd
     (exis, ctx, h) <- evalCtxLift (x:zs) e
     pure (x:exis, ctx, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (exis, ctx, h) <- evalCtxLift zs e1
     pure (exis, (v :=: ctx) :>: e2, h)
 ++
  do (v :=: e1) :>: e2 <- [lhs]
     (exis, ctx, h) <- evalCtxLift zs e2
     pure (exis, (v :=: e1) :>: ctx, h)

wrapExis :: [Ident] -> Expr -> Expr
wrapExis xs orig_e = foldr wrap orig_e xs
  where
    wrap x e = Exi (bind x e)

--------------------------------------------------------------------------------
--
--            The 'blocked' and 'choice-free' predicates
--
--------------------------------------------------------------------------------

type Expr_or_Context = Expr

blocked :: Expr_or_Context -> Bool
blocked ec = blkd [] ec

blkd :: [Ident] -> Expr_or_Context -> Bool
blkd _  HOLE                = True
blkd xs ((_ :=: e1) :>: e2) = blkd xs e1 && (isContext e1 || blkd xs e2)
blkd xs (e1 :|: e2)         = blkd xs e1 && (isContext e2 || blkd xs e2)  -- ToDo: check
blkd xs (One e)             = blkd xs e
blkd xs (All e)             = blkd xs e
blkd xs (Exi bnd)           = blkd (x:xs) e where (x,e) = alphaRename xs bnd
blkd xs (v1 :@: v2)         = case v1 of
                                Var f -> f `elem` xs
                                Op {} -> any ((`isV` v2) . Var) xs
                                _     -> False
blkd xs (v :>>: _)          = any (`elem` xs) (free v)
blkd _  (Verify _)          = True
blkd xs (Check _ e)         = blkd xs e
blkd _  _                   = False

-- choice-freeness
choiceFree :: Expr_or_Context -> Bool
choiceFree (_ :|: _)           = False
choiceFree ((_ :=: e1) :>: e2) = choiceFree e1 && (isContext e1 || choiceFree e2)
choiceFree (_ :>>: e)          = choiceFree e
choiceFree (Exi bnd)           = choiceFree e where (_,e) = unsafeUnbind bnd
choiceFree (v1 :@: _)          = case v1 of
                                   Op _ -> True -- all ops we support are choice-free right now
                                   _    -> False
choiceFree _                   = True

