module Rules.TRS2024 where

import Control.Monad( guard )
import TRS.Bind
import Rules.Core

--------------------------------------------------------------------------------

rules :: Rule
rules = rulesApplication
     <> rulesUnification
     <> rulesSubstitution
     <> rulesNormalization
     <> rulesChoice
     <> rulesOneAndAll

--------------------------------------------------------------------------------

name :: String -> [Expr] -> [(String,Expr)]

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
       Int _ -> pure hnf
       _     -> pure Fail
 ++
  "APP-LAM" `name`
  do Lam bnd :@: Val v <- [lhs]
     let (x,body) = alphaRename (free v) bnd
     pure (Exi (bind x ((Var x :=: v) :>: body)))
 ++
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     pure (foldr1 (:|:) [ (Val v :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])
 ++
  "APP-TUP-0" `name`
  do Arr [] :@: _ <- [lhs]
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
     pure (foldr (:>:) e [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-FAIL" `name`
  do a1 :=: a2 <- [lhs]
     guard (isHNF a1 && isHNF a2)
     guard $
       case (a1, a2) of
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
  "SUBST1" `name`
  do Exi (Bind x e) <- [lhs]
     (s, Var x' :=: Val v) <- substX e
     guard (x == x')
     guard (not (isValueX v x))
     pure ((substCtx [(x,v)] s) (Arr []))

--------------------------------------------------------------------------------

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "EXI-ELIM" `name`
  do Exi (Bind x e) <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "EXI-FLOAT" `name`
  do (ctx, zs, Exi bnd) <- evalX1 [] lhs
     let Bind x e = alphaRename (zs ++ free (ctx (#))) bnd
     guard (x `notElem` free (ctx (#)))
     pure (Exi (Bind x (ctx e)))
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
  "EQ-FLOAT" `name`
  do Val v1 :=: (Val v2 :=: e) <- [lhs]
     pure ((v2 :=: e) :>: (v1 :=: Arr []))
{-
 ++
  "DEF-MOVE" `name`
  do (Var x :=: Val v) :>: e <- [lhs]
     (e1,e2) <- [ (e1,e2) | e1 :>: e2 <- [e], isEffectFree e1 ]
             ++ [ (Var y :=: w, Arr []) | Var y :=: Val w <- [e] ]
     pure (e1 :>: ((Var x :=: v) :>: e2))
-}
 ++
  "EQ-SWAP" `name`
  do Val v :=: Var x <- [lhs]
     pure (Var x :=: v)
 ++
  "EQ-RESULT" `name`
  do (Val v :=: e) :>: Arr [] <- [lhs]
     pure (v :=: e)

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
  do (ctx, _, e1 :|: e2) <- choiceX [] lhs
     pure (ctx e1 :|: ctx e2)
 ++
  "FAIL" `name`
  do (_, _, Fail) <- choiceX [] lhs
     pure Fail
 ++
  "CHOICE-EXI" `name`
  do Exi (Bind x ctx_e) <- [lhs]
     (ctx, _, e1 :|: e2) <- evalX [x] ctx_e
     guard (x `notElem` free (ctx (#)))
     pure (ctx (Exi (Bind x (e1 :|: e2))))

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
