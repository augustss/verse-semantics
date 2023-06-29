{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-matches -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.LeftToRight(allSystemsLeftToRight) where

import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
import Rules.ICFP(rulesPrimOps, isChoiceFreeOp)
import Control.Monad( guard )
import Data.List( union )

--------------------------------------------------------------------------------

allSystemsLeftToRight :: [TRSystem Expr]
allSystemsLeftToRight =
  [ systemLeftToRight ]

systemLeftToRight :: TRSystem Expr
systemLeftToRight = TRSystem
  { sname               = "L2R"
  , description         = "Left-to-right evaluation rules"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check validE . expr)
  , postProcess         = const id
  , rules               = allRules
  , rules2              = \ _ _ -> []
  , rulesHaveStructural = True
  , confluenceRules     = \_ _ -> []
  , validExpr           = const validE
  , sortRewrites        = id
  }

-- Turn an expression into the subset of the grammar
expr :: Expr -> Expr
expr (Arr es)         = letExprs (map expr es) Arr
expr (fe :@: xe)      = letExprs (map expr [fe,xe]) $ \[f,x] -> f :@: x
expr (Lam (Bind x e)) = Lam (Bind x (expr e))
expr (e1 :=: e2)      = expr e1 =:= expr e2
expr (e1 :|: e2)      = expr e1 :|: expr e2
expr (e1 :>: e2)      = expr e1 =:>: expr e2
expr (Exi (Bind x e)) = Exi (Bind x (expr e))
expr (One e)          = One (expr e)
expr (All e)          = All (expr e)
expr e                = e

(=:=) :: Expr -> Expr -> Expr
Val v               =:= e     = (v :=: e) :>: v
e                   =:= Val v = (v :=: e) :>: v
((v :=: e1) :>: e2) =:= e     = (v :=: e1) :>: (e2 =:= e)
Exi bnd             =:= e     = Exi (Bind x (e1 =:= e)) where Bind x e1 = alphaRename (free e) bnd
e1                  =:= e     = letExpr e1 (\x -> (x :=: e) :>: x)

(=:>:) :: Expr -> Expr -> Expr
Val v               =:>: e = e
((v :=: e1) :>: e2) =:>: e = (v :=: e1) :>: (e2 =:>: e)
Exi bnd             =:>: e = Exi (Bind x (e1 =:>: e)) where Bind x e1 = alphaRename (free e) bnd
e1                  =:>: e = Exi (Bind x ((Var x :=: e1) :>: e))
 where
  x = identNotIn (free (e1,e))

letExpr :: Expr -> (Expr -> Expr) -> Expr
letExpr e@(Val _)     f = f e
letExpr (Val v :=: e) f = (v :=: e) :>: f v
letExpr e             f = Exi (Bind x ((Var x :=: e) :>: f (Var x)))
 where
  fx0 = f (Var (ident ""))
  x   = identNotIn (free fx0)

letExprs :: [Expr] -> ([Expr] -> Expr) -> Expr
letExprs []     f = f []
letExprs (e:es) f = letExpr e (\x -> letExprs es (\xs -> f (x:xs)))

-- Check that an expression is in the subset defined by the grammar.
validE :: Expr -> Bool
validE = ok
 where
  ok (Arr xs)         = all isVal xs
  ok (Lam (Bind _ e)) = ok e
  ok (Exi (Bind _ e)) = ok e
  ok (_ :=: _)        = False
  ok (e1 :>: e2)      = okeq e1 && ok e2
  ok (e1 :|: e2)      = ok e1 && ok e2
  ok (f :@: x)        = isVal f && isVal x
  ok (One e)          = ok e
  ok (All e)          = ok e
  ok _                = True

  okeq (v :=: e)      = isVal v && ok e
  okeq _              = False 

--------------------------------------------------------------------------------

allRules :: ERule
allRules =
     rulesApplication
  <> rulesUnification
  <> rulesSubstitution
  <> rulesNormalization
  <> rulesChoice
  <> rulesOneAndAll

--------------------------------------------------------------------------------

-- contexts

-- V
isVctx :: Ident -> Expr -> Bool
isVctx x (Arr as) = Var x `elem` as || any (isVctx x) as
isVctx _ _        = False

-- E
evalX :: Expr -> [([Ident], Expr->Expr, Expr)]
evalX lhs =
  do pure ([], id, lhs)
 ++
  do (Val v :=: eh) :>: e <- [lhs]
     (xs, ctx, h) <- evalX eh
     pure (xs `union` free (v,e), (:>: e) . (v :=:) . ctx, h)
 ++
  do (Val v :=: ef) :>: eh <- [lhs]
     guard (effectFree ef)
     (xs, ctx, h) <- evalX eh
     pure (xs `union` free (v,ef), ((v :=: ef) :>:) . ctx, h)
 ++
  do Exi (Bind x eh) <- [lhs]
     (xs, ctx, h) <- evalX eh
     pure (xs `union` [x], Exi . Bind x . ctx, h)

-- ef
effectFree :: Expr -> Bool
effectFree (Val _)       = True
effectFree (One _)       = True
effectFree (All _)       = True
effectFree (Op op :@: _) = isChoiceFreeOp op
effectFree _             = False

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication env lhs =
  "APP-LAM" `name`
  do Lam bnd :@: Val v <- [lhs]
     let Bind x e = alphaRename (free v) bnd
     pure (Exi (Bind x ((Var x :=: v) :>: e)))
 ++
  "APP-TUP" `name`
  do Arr as :@: Val v <- [lhs]
     pure (foldr (:|:) Fail [ (v :=: Int i) :>: a | (i,a) <- [0..] `zip` as ])
 ++
  rulesPrimOps env lhs

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification _ lhs =
  "U-LIT" `name`
  do (Int k :=: Int k') :>: e <- [lhs]
     guard (k == k')
     pure e
 ++
  "U-TUP" `name`
  do (Arr as :=: Arr bs) :>: e <- [lhs]
     guard (length as == length bs)
     pure (foldr (:>:) e [ a :=: b | (a,b) <- as `zip` bs ])
 ++
  "U-FAIL" `name`
  do (HNF a :=: HNF b) :>: _ <- [lhs]
     guard (a =/= b)
     pure Fail
 ++
  "U-OCCURS" `name`
  do (Var x :=: Val v) :>: _ <- [lhs]
     guard (isVctx x v)
     pure Fail
 where
  Int k  =/= Int k' = k /= k'
  Arr as =/= Arr bs = length as /= length bs
  _      =/= _      = True

--------------------------------------------------------------------------------

rulesSubstitution :: ERule
rulesSubstitution _ lhs =
  "SUBST" `name`
  do (Var x :=: Val v) :>: e <- [lhs]
     guard (not (isVctx x v))
     pure ((Var x :=: v) :>: subst [(x,v)] e)
 ++
  "VAL-SWAP" `name`
  do (Val v :=: Var x) <- [lhs]
     pure (Var x :=: v)
 ++
  "EQN-SWAP" `name`
  do (Val v :=: ef) :>: ((Val v1 :=: Val v2) :>: e) <- [lhs]
     guard (effectFree ef)
     pure ((v1 :=: v2) :>: ((v :=: ef) :>: e))

--------------------------------------------------------------------------------

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "EXI-ELIM" `name`
  do Exi (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "EQN-ELIM" `name`
  do Exi (Bind x ec) <- [lhs]
     (xs, ctx, (Var x' :=: Val v) :>: e) <- evalX ec
     guard (x == x')
     guard (x `notElem` (xs `union` free (v, e)))
     pure (ctx e)
 ++
  "EXI-FLOAT" `name`
  do (xs, ctx, Exi bnd) <- evalX lhs
     let Bind x e = alphaRename xs bnd
     pure (Exi (Bind x (ctx e)))
{-
 ++
  "EXI-FLOAT-L" `name`
  do (v :=: Exi bnd) :>: e2 <- [lhs]
     let Bind x e1 = alphaRename (free (v,e2)) bnd
     pure (Exi (Bind x ((v :=: e1) :>: e2)))
 ++
  "EXI-FLOAT-R" `name`
  do Exi (Bind x (v_eq_e1 :>: e2)) <- [lhs]
     guard (x `notElem` free v_eq_e1)
     pure (v_eq_e1 :>: Exi (Bind x e2))
 ++
  "EXI-SWAP" `name`
  do Exi (Bind x (Exi (Bind y e))) <- [lhs]
     pure (Exi (Bind y (Exi (Bind x e))))
-}
 ++
  "SEQ-ASSOC" `name`
  do (v2 :=: ((v1 :=: e1) :>: e2)) :>: e3 <- [lhs]
     pure ((v1 :=: e1) :>: ((v2 :=: e2) :>: e3))

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "CHOICE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
  "CHOICE-FAIL-L" `name`
  do Fail :|: e2 <- [lhs]
     pure e2
 ++
  "CHOICE-FAIL-R" `name`
  do e1 :|: Fail <- [lhs]
     pure e1
 ++
  "FAIL" `name`
  do (_, _, Fail) <- evalX lhs
     pure Fail
 ++
  "CHOICE" `name`
  do (_, ctx, e1 :|: e2) <- evalX lhs
     pure (ctx e1 :|: ctx e2)

--------------------------------------------------------------------------------

rulesOneAndAll :: ERule
rulesOneAndAll _ lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-VAL" `name`
  do One (Val v) <- [lhs]
     pure v
 ++
  "ONE-CHOICE" `name`
  do One (Val v :|: _) <- [lhs]
     pure v
 ++
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All e <- [lhs]
     let as = choices e
     guard (all isVal as)
     pure (Arr as)
 where
  choices (e1 :|: e2) = choices e1 ++ choices e2
  choices e           = [e]

--------------------------------------------------------------------------------

