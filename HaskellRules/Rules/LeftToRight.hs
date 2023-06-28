{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-matches -Wno-name-shadowing #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.LeftToRight(allSystemsLeftToRight) where

import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
import Control.Monad( guard )

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
  }

-- Turn an expression into the subset of the grammar
expr :: Expr -> Expr
expr (Arr es)         = letExprs (map expr es) Arr
expr (fe :@: xe)      = letExprs (map expr [fe,xe]) $ \[f,x] -> f :@: x
expr (Lam (Bind x e)) = Lam (Bind x (expr e))
expr (e1 :=: e2)      = letExpr (expr e1) (\x -> (x :=: expr e2) :>: x)
expr (e1 :|: e2)      = expr e1 :|: expr e2
expr (e1 :>: e2)      = expr e1 =:>: expr e2
expr (Exi (Bind x e)) = Exi (Bind x (expr e))
expr (One e)          = One (expr e)
expr (All e)          = All (expr e)
expr e                = e

(=:>:) :: Expr -> Expr -> Expr
e1@(_ :=: _) =:>: e2 = e1 :>: e2
e1           =:>: e2 = Exi (Bind x ((Var x :=: e1) :>: e2))
 where
  x = identNotIn (free (e1,e2))

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
     rulesFail
  <> rulesSubst
  <> rulesUni
  <> rulesEqn
  <> rulesExi
  <> rulesAssoc
  <> rulesElim
  <> rulesApp
  <> rulesOne
  <> rulesAll
  <> rulesChoice

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL-ELIM" `name`
  do (_, Fail) <- choiceX lhs
     pure Fail
 ++
  "FAIL-CHOICE-R" `name`
  do e1 :|: Fail <- [lhs]
     pure e1
 ++
  "FAIL-CHOICE-L" `name`
  do Fail :|: e2 <- [lhs]
     pure e2

xX :: Expr -> [(Expr->Expr, Expr)]
xX lhs =
  do pure (id, lhs)
 ++
  do v :=: xe <- [lhs]
     (ctx, e) <- xX xe
     pure ((v :=:). ctx, e)
 ++
  do e1 :>: xe <- [lhs]
     (ctx, e) <- xX xe
     pure ((e1 :>:). ctx, e)
 ++
  do xe :>: e2 <- [lhs]
     (ctx, e) <- xX xe
     pure ((:>: e2). ctx, e)

--------------------------------------------------------------------------------

rulesSubst :: ERule
rulesSubst _ lhs =
  "SUBST" `name`
  do (Var x :=: Val v) :>: e <- [lhs]
     guard (not (isVctx x v))
     pure ((Var x :=: v) :>: subst [(x,v)] e)
 ++
  "EQN-MOVE" `name`
  do ce :>: ((Val v1 :=: Val v2) :>: e) <- [lhs]
     guard (isChoiceFree ce)
     pure ((v1 :=: v2) :>: (ce :>: e))
 ++
  "EQN-SWAP" `name`
  do (Val v :=: Var x) <- [lhs]
     pure (Var x :=: v)

isVctx :: Ident -> Expr -> Bool
isVctx x (Arr as) = Var x `elem` as || any (isVctx x) as
isVctx _ _        = False

--------------------------------------------------------------------------------

rulesUni :: ERule
rulesUni _ lhs =
  "UNI-ARR" `name`
  do (Arr as :=: Arr bs) :>: e <- [lhs]
     guard (length as == length bs)
     pure (foldr (:>:) e [ a :=: b | (a,b) <- as `zip` bs ])
 ++
  "UNI-INT" `name`
  do (Int k :=: Int k') :>: e <- [lhs]
     guard (k == k')
     pure e
 ++
  "UNI-OCCURS" `name`
  do (Var x :=: Val v) :>: _ <- [lhs]
     guard (isVctx x v)
     pure Fail
 ++
  "UNI-FAIL" `name`
  do (HNF a :=: HNF b) :>: _ <- [lhs]
     guard (a =/= b)
     pure Fail
 where
  Int k  =/= Int k' = k /= k'
  Arr as =/= Arr bs = length as /= length bs
  _      =/= _      = True

--------------------------------------------------------------------------------

rulesEqn :: ERule
rulesEqn _ lhs =
  "EQN-SEQ-ASSOC" `name`
  do (v2 :=: ((v1 :=: e1) :>: e2)) :>: e3 <- [lhs]
     pure ((v1 :=: e1) :>: (v2 :=: e2) :>: e3)

--------------------------------------------------------------------------------

rulesExi :: ERule
rulesExi _ lhs =
  "EXI-FLOAT" `name`
  do (ctx, Exi bnd) <- xX lhs
     let Bind x e = alphaRename (free (ctx (Arr []))) bnd
     pure (Exi (Bind x (ctx e)))
 ++
  "EXI-SWAP" `name`
  do Exi (Bind x (Exi (Bind y e))) <- [lhs]
     pure (Exi (Bind y (Exi (Bind x e))))

--------------------------------------------------------------------------------

rulesAssoc :: ERule
rulesAssoc _ lhs =
  "CHOICE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))

--------------------------------------------------------------------------------

rulesElim :: ERule
rulesElim _ lhs =
  "EXI-ELIM" `name`
  do Exi (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
-- ++
--  "EQN-ELIM" `name`
--  do Exi (Bind x ((Var x' :=: Val v) :>: e)) <- [lhs]
--     guard (x == x')
--     guard (x `notElem` free (v,e))
--     pure e
 ++
  "EQN-ELIM" `name`
  do Exi (Bind x xe) <- [lhs]
     (ctx, (Var x' :=: Val v) :>: e) <- xX xe
     guard (x == x')
     guard (x `notElem` free (ctx (Val v :>: e)))
     pure (ctx e)

--------------------------------------------------------------------------------

rulesApp :: ERule
rulesApp _ lhs =
  "APP-LAM" `name`
  do Lam bnd :@: Val v <- [lhs]
     let Bind x e = alphaRename (free v) bnd
     pure (Exi (Bind x ((Var x :=: v) :>: e)))
 ++
  "APP-ARR" `name`
  do Arr as :@: Val v <- [lhs]
     pure (foldr (:|:) Fail [ (v :=: Int i) :>: a | (i,a) <- [0..] `zip` as ])
 ++
  "APP-ADD" `name`
  do Op Add :@: Arr [Int i, Int j] <- [lhs]
     pure (Int (i+j))
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [Int i, Int j] <- [lhs]
     pure (if i > j then Int i else Fail)

--------------------------------------------------------------------------------

rulesOne :: ERule
rulesOne _ lhs =
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

rulesAll :: ERule
rulesAll _ lhs =
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

isChoiceFree :: Expr -> Bool
isChoiceFree (One _)       = True
isChoiceFree (All _)       = True
isChoiceFree (Val _)       = True
isChoiceFree (Op op :@: _) = True -- op `elem` [Add, Gt, ..]
isChoiceFree (e1 :>: e2)   = isChoiceFree e1 && isChoiceFree e2
isChoiceFree (e1 :=: e2)   = isChoiceFree e1 && isChoiceFree e2
isChoiceFree _             = False

rulesChoice :: ERule
rulesChoice _ lhs =
  "EQN-CHOICE" `name`
  do (Val v :=: (e1 :|: e2)) :>: e3 <- [lhs]
     pure (((v :=: e1) :>: e3) :|: ((v :=: e2) :>: e3))
 ++
  "SEQ-CHOICE" `name`
  do ce :>: (e1 :|: e2) <- [lhs]
     guard (isChoiceFree ce)
     pure ((ce :>: e1) :|: (ce :>: e2))
 ++
  "EXI-CHOICE" `name`
  do Exi (Bind x (e1 :|: e2)) <- [lhs]
     pure (Exi (Bind x e1) :|: Exi (Bind x e2))

choiceX :: Expr -> [(Expr->Expr, Expr)]
choiceX lhs =
  do pure (id, lhs)
 ++
  do Val v :=: xe <- [lhs]
     (ctx, e) <- choiceX xe
     pure ((v :=:) . ctx, e)
 ++
  do xe :>: e2 <- [lhs]
     (ctx, e) <- choiceX xe
     pure ((:>: e2) . ctx, e)
 ++
  do ce :>: xe <- [lhs]
     guard (isChoiceFree ce)
     (ctx, e) <- choiceX xe
     pure ((ce :>:) . ctx, e)
 ++
  do Exi (Bind x xe) <- [lhs]
     (ctx, e) <- choiceX xe
     pure ((Exi . Bind x) . ctx, e)

--------------------------------------------------------------------------------

