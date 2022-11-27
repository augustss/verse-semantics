{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.POPL(systemPOPL, systemVPOPL) where

import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
import Control.Monad( guard )
--import Data.Functor.Classes (Show1(liftShowList))
--import Debug.Trace

--------------------------------------------------------------------------------

systemPOPL :: TRSystem Expr
systemPOPL = TRSystem
  { sname               = "POPL"
  , description         = "POPL submission"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = anf
  , postProcess         = id
  , rules               = allRules
  , rulesHaveStructural = False
  , confluenceRules     = \ _ _ -> []  -- XXX temporary
  }

systemVPOPL :: TRSystem Expr
systemVPOPL = systemPOPL
  { sname               = "VPOPL"
  , description         = "POPL submission + DEF-ELIMV"
  , rules               = allRules <> rulesElimV
  }

anf :: Expr -> Expr
anf = id -- XXX

--------------------------------------------------------------------------------

-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Op op :@: _) = isChoiceFreeOp op  -- NOTE: not in POPL submission
isChoiceFree Split{}   = True
isChoiceFree Wrong     = True
isChoiceFree _         = False
-- KC: what about @?

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _ = True

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

-- scope contexts

-- X ::= * | X = e | e = X | X;e | e;X

execX, execX1 :: Expr -> [(Context, Expr)]
-- X context
execX lhs = execX1 lhs ++ [(id,lhs)]
-- X context, X /= hole
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

-- X context, or exist x . X
defX :: Ident -> Expr -> [(Context, Expr)]
defX xx lhs =
  do execX lhs
 ++
  do Def (Bind x dx) <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- defX xx dx
     return (Def . Bind x . ctx, hole)

-- choice contexts

choiceX, choiceX1 :: Expr -> [(Context, Expr)]
-- CX context
choiceX lhs = choiceX1 lhs ++ [(id,lhs)]
-- CX context, CX /= hole
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
-- SX context
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
 ++
  do Split hole f g <- [lhs]
     pure (\ e -> Split e f g, hole)

-- value contexts
-- V context
valueX, valueX1 :: Value -> [(Value->Value, Value)]
valueX lhs = valueX1 lhs ++ [(id, lhs)]

valueX1 lhs =
  do Arr vs <- [lhs]
     i <- [0..length vs-1]
     pure (\v -> Arr (take i vs ++ [v] ++ drop (i+1) vs), vs!!i)

--------------------------------------------------------------------------------

allRules :: ERule
allRules = rulesPrimOps
         <> rulesApplication
         <> rulesUnification
         <> rulesUnificationVariables
         <> rulesSequencing
         <> rulesChoice
         <> rulesOne
         <> rulesAll
         <> rulesFail
         <> rulesSplit

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps _ lhs =
  "P-ADD" `name`
  do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1+k2))
 ++
  "P-SUB" `name`
  do Op Sub :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1-k2))
 ++
  "P-MUL" `name`
  do Op Mul :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1*k2))
 ++
  "P-DIV" `name`
  do Op Div :@: Arr [Int k1, Int k2] <- [lhs]
     if k2 /= 0
       then pure (Int (k1 `div` k2))
       else pure Fail
 ++
  "P-NEG" `name`
  do Op Neg :@: Int k <- [lhs]
     pure (Int k)
 ++
  "P-PLUS" `name`
  do Op Plus :@: Int k <- [lhs]
     pure (Int k)
 ++
  "P-GRT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 > k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-GRE" `name`
  do Op Ge :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 >= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-LST" `name`
  do Op Lt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 < k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-LSE" `name`
  do Op Le :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 <= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-NEQ" `name`
  do Op Ne :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 /= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-IsInt" `name`
  do Op IsInt :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure (Arr [])
       _     -> pure Fail
 ++
  "P-MAPAP" `name`
  do Op MapAp :@: Arr vs <- [lhs]
     pure (mapAp vs)
 ++
  "P-CONS" `name`
  do Op Cons :@: Arr [v, Arr vs] <- [lhs]
     pure (Arr (v:vs))

-- Turn array{f1, ... fn} into array{f1(), ... fn()}
mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> Var x :=: (v :@: unit)) xs vs ++ [Arr $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr (\ x -> Def . Bind x) e vs

unit :: Value
unit = Arr []

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do LAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = DEF y ((Var y :=: Val v) :>: b)
     if x `notElem` freeV then
       pure (beta x e)
      else do
       -- The x has to be renamed to avoid capture
       let freeE = free e
           x' = identNotIn (freeV ++ freeE)
           e' = subst [(x, Var x')] e
       pure (beta x' e')
 ++
  "APP-TUP" `name`
  do Arr vs :@: v <- [lhs]
     pure (foldr (:|:) Fail [ (Val v :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])

--------------------------------------------------------------------------------
rulesUnification :: ERule
rulesUnification = rulesUnificationNoOcc
                <> rulesUnificationOcc

rulesUnificationNoOcc :: ERule
rulesUnificationNoOcc _ lhs =
  "ULIT" `name`
  do Int k1 :=: Int k2 <- [lhs]
     if k1 == k2
       then pure (Int k1)
       else pure Fail
 ++
  "UTUP" `name`
  do Arr vs :=: Arr vs' <- [lhs]
     if length vs == length vs'
       then pure (foldr (:>:) (Arr vs) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
       else pure Fail
 ++
  "UX1" `name`
  do Int _k :=: Arr _vs <- [lhs]
     pure Fail
 ++
  "UX2" `name`
  do Arr _vs :=: Int _k <- [lhs]
     pure Fail
 ++
  "UX3" `name`
  do Val (LAM _ _) :=: Val (HNF _) <- [lhs]
     pure Fail
 ++
  "UX4" `name`
  do Val (HNF _) :=: Val (LAM _ _) <- [lhs]
     pure Fail
 ++
  "UX5" `name`
  do Val (HNF (Op _)) :=: Val (HNF _) <- [lhs]
{-
     if h1 == h2 then  -- To make it compatible with the PLDI rules
       pure (Val h1)
      else
-}
     pure Fail
 ++
  "UX6" `name`
  do Val (HNF _) :=: Val (HNF (Op _)) <- [lhs]
     pure Fail

rulesUnificationOcc :: ERule
rulesUnificationOcc _ lhs =
   "UX-OCCURS" `name`
   do Var x :=: Val v <- [lhs]
      (_, Var x') <- valueX1 v
      guard (x == x')
      pure Fail

--------------------------------------------------------------------------------

rulesUnificationVariables :: ERule
rulesUnificationVariables _ lhs =
  "SUBST" `name`
  do (ctx, Var x :=: Val v) <- execX lhs
     let freeX = free (ctx blob)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx (Var x0 :=: Val v)))
 ++
  "SUBST-REC" `name`
  do Var x :=: Val v <- [lhs]
     (ctx, LAM y e) <- valueX v
     guard (x `elem` free (LAM y e))
     pure (Var x :=: Val (ctx (LAM y (Def (Bind x (lhs :>: e))))))
 ++
  "DEF-ELIML" `name`
  do Def (Bind x a) <- [lhs]
     (ctx, Var x' :=: Val v) <- defX x a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  "DEF-ELIMR" `name`
  do Def (Bind x a) <- [lhs]
     (ctx, Val v :=: Var x') <- defX x a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  "SWAP" `name`
  do Val (HNF hnf) :=: Var x <- [lhs]
     pure (Var x :=: Val hnf)
 ++
  "DEF-FLOAT" `name`
  do (ctx, Def (Bind x e)) <- execX1 lhs
     let freeX = free (ctx blob)
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (Def (Bind x' (ctx (subst [(x,Var x')] e))))
       else pure (Def (Bind x (ctx e)))
 where
  blob = Fail -- just something to plug the hole in the context so we can look at it

rulesElimV :: ERule
rulesElimV _ lhs =
  "DEF-ELIMV" `name`
  do Def (Bind x a) <- [lhs]
     (ctx, Var y :=: Var x') <- defX x a
     guard (x == x')
     guard (x /= y)
     guard (y `elem` free a)
     pure (subst [(x, Var y)] (ctx (Var y)))

--------------------------------------------------------------------------------

rulesSequencing :: ERule
rulesSequencing _ lhs =
  "SEQ" `name`
  do Val _v :>: e <- [lhs]
     pure e
 ++
  "SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "UNIFY-SEQL" `name`
  do (e1 :>: e2) :=: e3 <- [lhs]
     pure (e1 :>: (e2 :=: e3))
 ++
  "UNIFY-SEQR" `name`
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (Val v :=: e2))
 ++
  "UNIFY-UNIFYR" `name`
  do (e1 :=: e2) :=: e3 <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((Var x :=: e1) :>: (Var x :=: e2) :>: (Var x :=: e3))))
 ++
  "UNIFY-UNIFYR" `name`
  do e1 :=: (e2 :=: e3) <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((Var x :=: e1) :>: (Var x :=: e2) :>: (Var x :=: e3) :>: Var x)))
  -- for FRESH
 ++ "CONJ-CST-DEFR" `name` -- e1 = (ex y. e2) --> ex y. e1 = e2
  do (e1 :=: Def (Bind y e2)) <- [lhs]
     let y' = identNotIn (free e2 ++ free e2)
     if y `elem` free e1
       then pure (Def (Bind y' (e1 :=: subst [(y,Var y')] e2)))
       else pure (Def (Bind y (e1 :=: e2)))
-- ++ "CONJ-SEQ-ASSOC" `name`
--  do (e1 :>: e2) :>: e3 <- [lhs]
--     pure (e1 :>: (e2 :>: e3))

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL-DEF" `name`
  do Def (Bind _x Fail) <- [lhs]
     pure Fail
 ++
  "FAIL" `name`
  do (_cx, Fail) <- execX1 lhs
     pure Fail

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "FAIL-L" `name`
  do (sx, fe) <- scopeX lhs
     Fail :|: e <- [fe]
     pure (sx e)
 ++
  "FAIL-R" `name`
  do (sx, ef) <- scopeX lhs
     e :|: Fail <- [ef]
     pure (sx e)
 ++
  "ASSOC-CHOICE" `name`
  do (sx, e) <- scopeX lhs
     (e1 :|: e2) :|: e3 <- [e]
     pure (sx (e1 :|: (e2 :|: e3)))
 ++
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))

--------------------------------------------------------------------------------

rulesOne :: ERule
rulesOne _ lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (Val v :|: _e) <- [lhs]
     pure (Val v)
 ++
  "ONE-VAL" `name`
  do One (Val v) <- [lhs]
     pure (Val v)

rulesAll :: ERule
rulesAll _ lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All ves@(_ :|: _) <- [lhs]
     let choiceVals (Val v) = [[v]]
         choiceVals (Val v :|: es) = [ v : vs | vs <- choiceVals es ]
         choiceVals _ = []
     vs <- choiceVals ves
     pure (Arr vs)
 ++
  "ALL-VAL" `name`
  do All (Val v) <- [lhs]
     pure (Arr [v])

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f _g <- [lhs]
     pure (f :@: Arr [])
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x e
     pure (Def (Bind h (gv :>: hlam)))
 ++
  "SPLIT-VAL" `name`
  do Split (Val v) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x Fail
     pure (Def (Bind h (gv :>: hlam)))

--------------------------------------------------------------------------------

