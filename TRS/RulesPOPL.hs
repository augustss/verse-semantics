{-# OPTIONS_GHC -Wno-unused-matches -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module RulesPOPL where

import TRS
import Bind
import TRSCore
import Control.Monad( guard )
import Data.List( sort )

--------------------------------------------------------------------------------
-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val v)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (HNF (Op op) :@: _) = isChoiceFreeOp op  -- NOTE: not in POPL submission
isChoiceFree _         = False
-- KC: what about @?

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp _ = True

isVar :: Expr -> Bool
isVar (VAR _) = True
isVar _       = False

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

-- scope contexts

execX, execX1 :: Expr -> [(Context, Expr)]
execX lhs = execX1 lhs ++ [(id,lhs)]

execX1 lhs =
  do x :=: e <- [lhs]
     (ctx, hole) <- execX x
     pure ((:=: e) . ctx, hole)
 ++
  do e :=: x <- [lhs]
     (ctx, hole) <- execX x
     pure ((e :=:) . ctx, hole)
 ++
  do x :>: e <- [lhs]
     (ctx, hole) <- execX x
     pure ((:>: e) . ctx, hole)
 ++
  do e :>: x <- [lhs]
     (ctx, hole) <- execX x
     pure ((e :>:) . ctx, hole)

defX :: Expr -> [(Context, Expr)]
defX lhs =
  do execX lhs
 ++
  do Def (Bind x dx) <- [lhs]
     (ctx, hole) <- defX dx
     return (Def . Bind x . ctx, hole)

-- choice contexts

choiceX, choiceX1 :: Expr -> [(Context, Expr)]
choiceX lhs = choiceX1 lhs ++ [(id,lhs)]

choiceX1 lhs =
  do cx :=: e <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((:=: e) . ctx, hole)
 ++
  do ce :=: cx <- [lhs]
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :=:) . ctx, hole)
 ++
  do cx :>: e <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((:>: e) . ctx, hole)
 ++
  do ce :>: cx <- [lhs]
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :>:) . ctx, hole)
 ++
  do Def (Bind x cx) <- [lhs]
     (ctx, hole) <- choiceX cx
     pure (Def . Bind x . ctx, hole) -- hopefully this is sound!

-- scope contexts

scopeX :: Expr -> [(Context, Expr)]
scopeX lhs =
  do hole :|: e <- [lhs]
     pure ((:|: e), hole)
 ++
  do e :|: hole <- [lhs]
     pure ((e :|:), hole)
 ++
  do One hole <- [lhs]
     pure (One, hole)
 ++
  do All hole <- [lhs]
     pure (All, hole)

-- value contexts

valueX :: Value -> [(Value->Value, Value)]
valueX lhs =
  do pure (id, lhs)
 ++
  do VARR vs <- [lhs]
     i <- [0..length vs-1]
     pure (\v -> VARR (take i vs ++ [v] ++ drop (i+1) vs), vs!!i)

--------------------------------------------------------------------------------

rules = rulesPrimOps
    +++ rulesApplication
    +++ rulesUnification
    +++ rulesUnificationVariables
    +++ rulesSequencing
    +++ rulesChoice
    +++ rulesOne
    +++ rulesAll
    +++ rulesFail

--------------------------------------------------------------------------------

rulesPrimOps lhs =
  do ADD :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1+k2))
 ++
  do SUB :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1-k2))
 ++
  do MUL :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1*k2))
 ++
  do DIV :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k2 /= 0
       then pure (INT (k1+k2))
       else pure Fail
 ++
  do GRT :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 > k2
       then pure (INT k1)
       else pure Fail
 ++
  do GRE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 >= k2
       then pure (INT k1)
       else pure Fail
 ++
  do LST :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 < k2
       then pure (INT k1)
       else pure Fail
 ++
  do LSE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 <= k2
       then pure (INT k1)
       else pure Fail
 ++
  do NEQ :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 /= k2
       then pure (INT k1)
       else pure Fail
 ++
  do IsINT :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure (ARR [])
       _     -> pure Fail
 ++
  do MAPAP :@: VARR vs <- [lhs]
     pure (mapAp vs)

mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
      unit = HNF (Arr [])
  in  defs xs $ seqs $ zipWith (\ x v -> VAR x :=: (v :@: unit)) xs vs ++ [ARR $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr (\ x e -> Def (Bind x e)) e vs

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication lhs =
  do VARR vs :@: v <- [lhs]
     pure (foldr (:|:) Fail [ (Val v :=: INT i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])
 ++
  do VLAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = Def (Bind y ((VAR y :=: Val v) :>: b))
     if x `notElem` freeV then
       pure (beta x e)
      else do
       -- The x has to be renamed to avoid capture
       let freeE = free e
           x' = identNotIn (freeV ++ freeE)
           e' = subst [(x, Var x')] e
       pure (beta x' e')

--------------------------------------------------------------------------------

rulesUnification lhs =
  do INT k1 :=: INT k2 <- [lhs]
     if k1 == k2
       then pure (INT k1)
       else pure Fail
 ++
  do ARR vs :=: ARR vs' <- [lhs]
     if length vs == length vs'
       then pure (foldr (:>:) (ARR vs) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
       else pure Fail
 ++
  do INT k :=: ARR vs <- [lhs]
     pure Fail
 ++
  do ARR vs :=: INT k <- [lhs]
     pure Fail
 ++
  do Val (HNF (Op _)) :=: Val (HNF _) <- [lhs]
     pure Fail
 ++
  do Val (HNF _) :=: Val (HNF (Op _)) <- [lhs]
     pure Fail
 ++
  do VAR x :=: Val val <- [lhs]
     guard (val /= Var x && x `elem` free val)
     pure Fail
 
--------------------------------------------------------------------------------

rulesUnificationVariables lhs =
  do Def (Bind x a) <- [lhs]
     (ctx, VAR x' :=: Val v) <- defX a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  do Def (Bind x a) <- [lhs]
     (ctx, Val v :=: VAR x') <- defX a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  do (ctx, Def (Bind x e)) <- execX1 lhs
     let freeX = free (ctx blob)
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (Def (Bind x' (ctx (subst [(x,Var x')] e))))
       else pure (Def (Bind x (ctx e)))
 ++
  do Val (HNF hnf) :=: VAR x <- [lhs]
     pure (VAR x :=: Val (HNF hnf))
 ++
  do (ctx, VAR x :=: Val v) <- execX lhs
     let freeX = free (ctx blob)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx (VAR x0 :=: Val v)))
 where
  blob = Fail -- just something to plug the hole in the context so we can look at it

--------------------------------------------------------------------------------

rulesSequencing lhs =
  do Val v :>: e <- [lhs]
     pure e
 ++
  do (e1 :>: e2) :=: e3 <- [lhs]
     pure (e1 :>: (e2 :=: e3))
 ++
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (Val v :=: e2))
 ++
  do (e1 :=: e2) :=: e3 <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((VAR x :=: e1) :>: (VAR x :=: e2) :>: (VAR x :=: e3))))
 ++
  do e1 :=: (e2 :=: e3) <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((VAR x :=: e1) :>: (VAR x :=: e2) :>: (VAR x :=: e3))))

--------------------------------------------------------------------------------

rulesChoice lhs =
  do Fail :|: e <- [lhs]
     pure e
 ++
  do e :|: Fail <- [lhs]
     pure e
 ++
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))
 ++
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))

--------------------------------------------------------------------------------

rulesOne lhs =
  do One (Val v) <- [lhs]
     pure (Val v)
 ++
  do One (Val v :|: e) <- [lhs]
     pure (Val v)
 ++
  do One Fail <- [lhs]
     pure Fail

{-
rulesAll lhs =
  do All es <- [lhs]
     vs     <- choiceVals es
     let xs = identsNotIn (free vs)
     pure (foldr (\(x,v) -> (Def . Bind x . ((VAR x :=: (v :@: VARR [])) :>:)))
                 (ARR [ Var x | (x,_) <- xs `zip` vs ])
                 (zip xs vs))
 where
  choiceVals :: Expr -> [[Value]]
  choiceVals Fail      = [[]]
  choiceVals (a :|: b) = [ vs1 ++ vs2 | vs1 <- choiceVals a, vs2 <- choiceVals b ]
  choiceVals (Val v)   = [[v]]
  choiceVals _         = []
-}

rulesAll lhs =
  do All es <- [lhs]
     vs     <- choiceVals es
     pure (ARR vs)
 where
  choiceVals :: Expr -> [[Value]]
  choiceVals Fail      = [[]]
  choiceVals (a :|: b) = [ vs1 ++ vs2 | vs1 <- choiceVals a, vs2 <- choiceVals b ]
  choiceVals (Val v)   = [[v]]
  choiceVals _         = []

--------------------------------------------------------------------------------

rulesFail lhs =
  do (cx, Fail) <- execX1 lhs
     pure Fail
 ++
  do Def (Bind x Fail) <- [lhs]
     pure Fail
  
--------------------------------------------------------------------------------

(=~=) :: Expr -> Expr -> Bool
a =~= b = norm a == norm b

norm :: Expr -> Expr
norm (Val (Var x) :=: Val (Var y)) | y > x = Val (Var y) :=: Val (Var x)
norm (a :>: b) = norm a :>: norm b
norm (a :=: b) = norm a :=: norm b
norm (a :|: b) = norm a :|: norm b
norm a@(Def _) = normDef [] a
norm (One a)   = One (norm a)
norm (All a)   = All (norm a)
norm e         = e

normDef :: [Ident] -> Expr -> Expr
normDef xs (Def (Bind x a)) = normDef (x:xs) a
normDef xs a                = defs (sort ys) (norm (subst sub a))
 where
  vs  = filter (`elem` xs) (free a)
  ys  = [ ident ("x" ++ show (index x vs)) | x <- xs ]
  sub = [ (x, Var y) | (x,y) <- xs `zip` ys ]
  
  index x zs = head $ [ i | (z,i) <- zs `zip` [1..], z == x ] ++ [0]
  
  defs []     a = a
  defs (x:xs) a = Def (Bind x (defs xs a))
  
