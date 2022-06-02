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
     pure ((Def . Bind x) . ctx, hole)

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
  do Fail :|: e <- [lhs]
     pure e
 ++
  do e :|: Fail <- [lhs]
     pure e
 ++
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))
 
--------------------------------------------------------------------------------

