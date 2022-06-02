module Rules where

import TRS
import Bind
import Core
import Control.Monad( guard )

--------------------------------------------------------------------------------
-- (=), (;), (|) are associative

-- normalizes associative operators on top-level
norm :: Expr -> Expr
norm ((a :=: b) :=: c) = norm (a :=: (b :=: c))
norm ((a :>: b) :>: c) = norm (a :>: (b :>: c))
norm ((a :|: b) :|: c) = norm (a :|: (b :|: c))
norm (a :=: b)         = a :=: norm b
norm (a :>: b)         = a :>: norm b
norm (a :|: b)         = a :|: norm b
norm a                 = a

-- mangles associative operators on top-level
assocs :: Expr -> [Expr]
assocs e@(a :=: (b :=: c)) = e : assocs ((a :=: b) :=: c)
assocs e@(a :>: (b :>: c)) = e : assocs ((a :>: b) :>: c)
assocs e@(a :|: (b :|: c)) = e : assocs ((a :|: b) :|: c)
assocs e                   = [e]

-- matcher to use for associative operators on top-level
assoc :: Expr -> [Expr]
assoc = assocs . norm

--------------------------------------------------------------------------------
-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val v)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree _         = False
-- KC: what about @?

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
  do x :=: e <- assoc lhs
     (ctx, hole) <- execX x
     pure ((:=: e) . ctx, hole)
 ++
  do e :=: x <- assoc lhs
     (ctx, hole) <- execX x
     pure ((e :=:) . ctx, hole)
 ++
  do x :>: e <- assoc lhs
     (ctx, hole) <- execX x
     pure ((:>: e) . ctx, hole)
 ++
  do e :>: x <- assoc lhs
     (ctx, hole) <- execX x
     pure ((e :>:) . ctx, hole)

-- choice contexts

choiceX, choiceX1 :: Expr -> [(Context, Expr)]
choiceX lhs = choiceX1 lhs ++ [(id,lhs)]

choiceX1 lhs =
  do cx :=: e <- assoc lhs
     (ctx, hole) <- choiceX cx
     pure ((:=: e) . ctx, hole)
 ++
  do ce :=: cx <- assoc lhs
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :=:) . ctx, hole)
 ++
  do cx :>: e <- assoc lhs
     (ctx, hole) <- choiceX cx
     pure ((:>: e) . ctx, hole)
 ++
  do ce :>: cx <- assoc lhs
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :>:) . ctx, hole)
 ++
  do Def (Bind x cx) <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((Def . Bind x) . ctx, hole) -- hopefully this is sound!

-- scope contexts

scopeX :: Expr -> [(Context, Expr)]
scopeX lhs =
  do hole :|: e <- assoc lhs
     pure ((:|: e), hole)
 ++
  do e :|: hole <- assoc lhs
     pure ((e :|:), hole)
 ++
  do One hole <- [lhs]
     pure (One, hole)
 ++
  do All hole <- [lhs]
     pure (All, hole)

--------------------------------------------------------------------------------

rulesChoice lhs =
  do Fail :|: e <- assoc lhs
     pure e
 ++
  do e :|: Fail <- assoc lhs
     pure e
 ++
  do (sx, e)     <- scopeX lhs
     (cx, e1_e2) <- choiceX1 e
     e1 :|: e2   <- assoc e1_e2
     pure (sx (cx e1 :|: cx e2))

--------------------------------------------------------------------------------

rulesPrimOps lhs =
  do ADD :@: ARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1+k2))
 ++
  do GRT :@: ARR [VINT k1, VINT k2] <- [lhs]
     if k1 > k2
       then pure (INT k1)
       else pure Fail
 ++
  do IsINT :@: (Val (HNF hnf)) <- [lhs]
     case hnf of
       Int _ -> pure (ARR [])
       _     -> pure Fail

--------------------------------------------------------------------------------

rulesSequencing lhs =
  do Val v :>: e <- assoc lhs
     pure e
 ++
  do e1_e2 :=: e3 <- assoc lhs
     e1 :>: e2    <- assoc e1_e2
     pure (e1 :>: (e2 :=: e3))
 ++
  do Val v :=: e1_e2 <- assoc lhs
     e1 :>: e2       <- assoc e1_e2
     pure (e1 :>: (Val v :=: e2))

--------------------------------------------------------------------------------

rulesApplication lhs =
  do ARR vs :@: Val v <- [lhs]
     pure (foldr (:|:) Fail [ (Val v :=: INT i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])

--------------------------------------------------------------------------------

rulesOne lhs =
  do One (Val v) <- [lhs]
     pure (Val v)
 ++
  do One v_e <- [lhs]
     Val v :|: e <- assoc v_e
     pure (Val v)
 ++
  do One Fail <- [lhs]
     pure Fail

rulesAll lhs =
  do All es <- [lhs]
     vs     <- choiceVals es
     let xs = identsNotIn (free vs)
     pure (foldr (\(x,v) -> (Def . Bind x . ((VAR x :=: Val v :@: ARR []) :>:)))
                 (ARR [ Var x | (x,_) <- xs `zip` vs ])
                 (zip xs vs))
 where
  choiceVals :: Expr -> [[Value]]
  choiceVals Fail      = [[]]
  choiceVals (a :|: b) = [ vs1 ++ vs2 | vs1 <- choiceVals a, vs2 <- choiceVals b ]
  choiceVals (Val v)   = [[v]]
  choiceVals _         = []

--------------------------------------------------------------------------------

rulesUnification lhs =
  do e :=: VAR x <- assoc lhs
     guard (not (isVar e))
     pure (VAR x :=: e)
 ++
  do INT k1 :=: INT k2 <- assoc lhs
     if k1 == k2
       then pure (INT k1)
       else pure Fail
 ++
  do ARR vs :=: ARR vs' <- assoc lhs
     if length vs == length vs'
       then pure (foldr (:>:) (ARR vs) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
       else pure Fail
 ++
  do INT k :=: ARR vs <- assoc lhs
     pure Fail
 ++
  do ARR vs :=: INT k <- assoc lhs
     pure Fail

--------------------------------------------------------------------------------

rulesDef lhs =
  do (ctx, VAR x :=: Val v) <- execX lhs -- execX already applies assoc, so I don't
     let freeX = free (ctx blob)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx (VAR x0 :=: Val v)))
 ++
  do Def (Bind x a) <- [lhs]
     (ctx, VAR x :=: Val v) <- execX a
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  do (ctx, Def (Bind x e)) <- execX1 lhs
     let freeX = free (ctx blob)
         x'    = identNotIn freeX
     if x `elem` freeX
       then pure (Def (Bind x' (ctx (subst [(x,Var x')] e))))
       else pure (Def (Bind x (ctx e)))
 where
  blob = Fail -- just something to plug the hole in the context so we can look at it

--------------------------------------------------------------------------------

rulesFail lhs =
  do Fail :>: e <- assoc lhs
     pure Fail
 ++
  do e :>: Fail <- assoc lhs
     pure Fail
 ++
  do Fail :=: e <- assoc lhs
     pure Fail
 ++
  do e :=: Fail <- assoc lhs
     pure Fail
 ++
  do Def (Bind x Fail) <- [lhs]
     pure Fail

--------------------------------------------------------------------------------

