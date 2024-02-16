{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-unused-top-binds #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.TRS2024(allSystemsTRS2024) where
import Control.Monad( guard )

--import Epic.Print(prettyShow)
--import qualified Epic.SIntMap as IM
import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
--import Debug.Trace

--------------------------------------------------------------------------------

allSystemsTRS2024 :: [TRSystem Expr]
allSystemsTRS2024 = [ systemTRS2024 ]

systemTRS2024 :: TRSystem Expr
systemTRS2024 = TRSystem
  { sname               = "TRS2024"
  , description         = "TRS2024, as specified in our internal document"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check valid . anf)
  , postProcess         = const id
  , rules               = allRules
  , rules2              = \ _ _ -> []
  , rulesHaveStructural = False
  , confluenceRules     = \ _ _ -> []
  , validExpr           = const valid
  , sortRewrites        = id
  }

-- Check that an expression is valid as defined by the grammar
valid :: Expr -> Bool
valid = expr
  where
    -- left out for now: assume, check, verify, havoc, olam
    expr (v  :=: e)       = value v && expr e
    expr (e1 :>: e2)      = expr e1 && expr e2
    expr (e1 :>>: e2)     = expr e1 && expr e2
    expr (e1 :|: e2)      = expr e1 && expr e2
    expr Fail             = True
    expr (Exi (Bind _ e)) = expr e
    expr (v1 :@: v2)      = value v1 && value v2
    expr (One e)          = expr e
    expr (All e)          = expr e
    expr (Uni (Bind _ e)) = expr e
    expr v                = value v
    
    value (Var _) = True
    value r       = hnf r
    
    hnf (Int _)          = True
    hnf (Op _)           = True
    hnf (Arr vs)         = all value vs
    hnf (Lam (Bind _ e)) = expr e
    hnf _                = False    

-- Make the expression obey the grammar; i.e. valid (anf e) == True
anf :: Expr -> Expr
anf = expr
  where
    expr (e1 :=: e2)      = makeValue e1 (\v -> v :=: expr e2)
    expr (e1 :>: e2)      = expr e1 :>: expr e2
    expr (e1 :>>: e2)     = expr e1 :>>: expr e2
    expr (e1 :|: e2)      = expr e1 :|: expr e2
    expr Fail             = Fail
    expr (Exi (Bind x e)) = Exi (Bind x (expr e))
    expr (e1 :@: e2)      = makeValues [e1,e2] (\[v1,v2] -> v1 :@: v2)
    expr (One e)          = One (expr e)
    expr (All e)          = All (expr e)
    expr (Uni (Bind x e)) = Uni (Bind x (expr e))
    expr (Arr es)         = makeValues es (\vs -> Arr vs)
    expr (Lam (Bind x e)) = Lam (Bind x (expr e))
    expr (Int k)          = Int k
    expr (Op op)          = Op op
    expr _                = Int 13 -- what to do here??

    value v = valid (v :=: Int 0)

    makeValue v f
      | value v   = f v
      | otherwise = Exi (Bind x ((Var x :=: expr v) :>: f (Var x)))
     where
      x:_ = identsNotIn (free (f v))
    
    makeValues []       f = f []
    makeValues (v':vs') f = makeValue v' (\v -> makeValues vs' (\vs -> f (v:vs)))
    
--------------------------------------------------------------------------------

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)          = True
isChoiceFree (a :=: b)        = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b)        = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>>: b)       = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)          = True
isChoiceFree (All _)          = True
isChoiceFree (Op op :@: _)    = isChoiceFreeOp op
isChoiceFree (Exi (Bind _ e)) = isChoiceFree e
isChoiceFree (Uni (Bind _ e)) = isChoiceFree e
isChoiceFree Fail             = True
isChoiceFree _                = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _     = True

isEffectFree :: Expr -> Bool
isEffectFree (Val _)          = True
isEffectFree (a :=: b)        = isEffectFree a && isEffectFree b
isEffectFree (a :>: b)        = isEffectFree a && isEffectFree b
isEffectFree (a :>>: b)       = isEffectFree a && isEffectFree b
isEffectFree (a :|: b)        = isEffectFree a && isEffectFree b
isEffectFree (One _)          = True
isEffectFree (All _)          = True
isEffectFree (Op op :@: _)    = isEffectFreeOp op
isEffectFree (Exi (Bind _ e)) = isEffectFree e
isEffectFree (Uni (Bind _ e)) = isEffectFree e
isEffectFree Fail             = True
isEffectFree _                = False

isEffectFreeOp :: Op -> Bool
isEffectFreeOp IsInt = True
isEffectFreeOp Add   = True
isEffectFreeOp Gt    = True
isEffectFreeOp _     = False

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

emptyX :: Expr -> [(Context, Expr)]
emptyX lhs = pure (id, lhs)

{-
valueX, valueX1 :: Expr -> [(Context, Expr)]
valueX lhs = emptyX lhs ++ valueX1 lhs
valueX1 lhs =
  do Arr vs <- [lhs]
     (v,i) <- vs `zip` [0..]
     (ctx, hole) <- valueX v
     pure (Arr . (take i vs ++) . (: drop (i+1) vs) . ctx, hole)
-}

isValueX :: Expr -> Ident -> Bool
isValueX (Arr vs) x = any (`isValueX` x) vs
isValueX (Var y)  x = x == y
isValueX _        _ = False

substX, substX1 :: Expr -> [(Context, Expr)]
substX lhs = emptyX lhs ++ substX1 lhs
substX1 lhs =
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- substX e1
     return ((:>: e2) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     guard (isEffectFree e1)
     (ctx, hole) <- substX e2
     return ((e1 :>:) . ctx, hole)
 ++
  do v :=: e <- [lhs]
     (ctx, hole) <- substX e
     return ((v :=:) . ctx, hole)
 ++
  do e1 :>>: e2 <- [lhs]
     (ctx, hole) <- substX e1
     return ((:>>: e2) . ctx, hole)

evalX, evalX1 :: Expr -> [(Context, Expr)]
evalX lhs = emptyX lhs ++ evalX1 lhs
evalX1 lhs =
  do v :=: e <- [lhs]
     (ctx, hole) <- evalX e
     return ((v :=:) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- evalX e2
     return ((e1 :>:) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- evalX e1
     return ((:>: e2) . ctx, hole)
 ++
  do Exi (Bind x e) <- [lhs]
     (ctx, hole) <- evalX e
     return (Exi . Bind x . ctx, hole)
 ++
  do Uni (Bind x e) <- [lhs]
     (ctx, hole) <- evalX e
     return (Uni . Bind x . ctx, hole)
 ++
  do e1 :>>: e2 <- [lhs]
     (ctx, hole) <- evalX e1
     return ((:>>: e2) . ctx, hole)
 ++
  do e1 :>>: e2 <- [lhs]
     (ctx, hole) <- evalX e2
     return ((e1 :>>:) . ctx, hole)
 
choiceX, choiceX1 :: Expr -> [(Context, Expr)]
choiceX lhs = emptyX lhs ++ choiceX1 lhs
choiceX1 lhs =
  do v :=: e <- [lhs]
     (ctx, hole) <- choiceX e
     return ((v :=:) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- choiceX e1
     return ((:>: e2) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     guard (isChoiceFree e1)
     (ctx, hole) <- choiceX e2
     return ((e1 :>:) . ctx, hole)
 ++
  do e1 :>>: e2 <- [lhs]
     (ctx, hole) <- choiceX e1
     return ((:>>: e2) . ctx, hole)
 ++
  do Exi (Bind x e) <- [lhs]
     (ctx, hole) <- choiceX e
     return (Exi . Bind x . ctx, hole)

--------------------------------------------------------------------------------

allRules :: ERule
allRules =  rulesPrimOps
         <> rulesApplication
         <> rulesUnification
         <> rulesSubstitution
         <> rulesNormalization
         <> rulesChoice
         <> rulesOneAndAll
         <> rulesGuard

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps _ lhs =
  "APP-ADD" `name`
  do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1+k2))
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     guard (k1 > k2)
     pure (Int k1)
 ++
  "APP-GT-FAIL" `name`
  do Op Gt :@: a <- [lhs]
     guard (case a of
              Arr [Int k1, Int k2] -> not (k1 > k2)
              _                    -> True)
     pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure hnf
       _     -> pure Fail

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-LAM" `name`
  do Lam bnd :@: Val v <- [lhs]
     let Bind x body = alphaRename (free v) bnd
     pure (Exi (Bind x ((Var x :=: v) :>: body)))
 ++
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     pure (foldr1 (:|:) [ (Val v :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])
 ++
  "APP-TUP-0" `name`
  do Arr [] :@: _ <- [lhs]
     pure Fail

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification _ lhs =
  "U-LIT" `name`
  do Int k1 :=: Int k2 <- [lhs]
     guard (k1 == k2)
     pure (Arr [])
 ++
  "U-TUP" `name`
  do Arr vs :=: Arr vs' <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) (Arr []) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-FAIL" `name`
  do HNF hnf1 :=: HNF hnf2 <- [lhs]
     guard $
       case (hnf1, hnf2) of
         (Int k1, Int k2)  -> k1 /= k2
         (Arr vs, Arr vs') -> length vs /= length vs'
         (_,      _)       -> True
     pure Fail
 ++
  "U-OCCURS" `name`
  do Var x :=: v <- [lhs]
     let isVar (Var _) = True
         isVar _       = False
     guard (not (isVar v) && isValueX v x)
     pure Fail

--------------------------------------------------------------------------------

rulesSubstitution :: ERule
rulesSubstitution _ lhs =
  "SUBST" `name`
  do (s, Var x :=: Val v) <- substX lhs
     guard (not (isValueX v x))
     let z = identNotIn (allVars lhs) -- z is placeholder
     pure (subst [(x,v),(z,Var x :=: v)] (s (Var z))) 

--------------------------------------------------------------------------------

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "EXI-ELIM1" `name`
  do Exi (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "EXI-ELIM2" `name`
  do Exi bnd <- [lhs]
     let Bind x e = alphaRename (allVars lhs) bnd
     (ctx, Var x' :=: Val v) <- evalX e
     guard (x == x')
     guard (x `notElem` allVars (ctx (Arr [])))
     guard (x `notElem` free v)
     pure (ctx (Arr []))
 ++
  "EXI-FLOAT" `name`
  do Val v :=: Exi bnd <- [lhs]
     let Bind x e = alphaRename (free v) bnd
     pure (Exi (Bind x (v :=: e)))
{-
  do (ctx, Exi bnd) <- evalX1 lhs
     let Bind x e = alphaRename (allVars (ctx (Arr []))) bnd
     pure (Exi (Bind x (ctx e)))
-}
 ++
  "SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "SEQ-FLOAT" `name`
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (v :=: e2))
 ++
  "SEQ-ELIM" `name`
  do Val _ :>: e <- [lhs]
     pure e
 ++
  -- not in the document right now, but considered OK
  "EQ-FLOAT" `name`
  do Val v1 :=: (Val v2 :=: e) <- [lhs]
     pure ((v2 :=: e) :>: (v1 :=: Arr []))
 ++
  "EQ-SWAP" `name`
  do Val v :=: Var x <- [lhs]
     pure (Var x :=: v)
 ++
  "EQ-RESULT" `name`
  do (Val v :=: e) :>: Arr [] <- [lhs]
     pure (v :=: e)
 -- nu-normalization
 ++
  "UNI-ELIM1" `name`
  do Uni (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "UNI-FLOAT" `name`
  do (ctx, Uni bnd) <- evalX1 lhs
     let Bind x e = alphaRename (allVars (ctx (Arr []))) bnd
     pure (Uni (Bind x (ctx e)))
     
--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
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
  do (c, e1 :|: e2) <- choiceX lhs
     pure (c e1 :|: c e2)
 ++
  "FAIL" `name`
  do (_, Fail) <- choiceX lhs
     pure Fail

--------------------------------------------------------------------------------

rulesOneAndAll :: ERule
rulesOneAndAll _ lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-VALUE" `name`
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
     let choices (e1 :|: e2) = choices e1 ++ choices e2
         choices e1          = [e1]
     let vs = choices e
     guard (all isVal vs)
     pure (Arr vs)

--------------------------------------------------------------------------------

rulesGuard :: ERule
rulesGuard _ lhs =
  "GUARD-ELIM" `name`
  do Val _ :>>: e <- [lhs]
     pure e
 ++
  "GUARD-FAIL" `name`
  do Fail :>>: _ <- [lhs]
     pure Fail

-----------------------------------------------------------------------------------

