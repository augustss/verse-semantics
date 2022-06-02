module Main where

import TRS
import Control.Monad( guard )

--------------------------------------------------------------------------------

type Name = String

data Expr
  = Num Integer
  | Var Name
  | Expr :+: Expr
  | Let (Name,Expr) Expr
 deriving ( Show, Eq, Ord )

instance Rec Expr where
  rec r (a :+: b)     = [ a' :+: b | a' <- r a ]
                     ++ [ a :+: b' | b' <- r b ]
  rec r (Let (x,a) b) = [ Let (x,a') b | a' <- r a ]
                     ++ [ Let (x,a) b' | b' <- r b ]
  rec r _             = []

subst :: (Name, Expr) -> Expr -> Expr
subst (x,w) (a :+: b)        = subst (x,w) a :+: subst (x,w) b
subst (x,w) (Let (y,a) b)    = Let (y, subst (x,w) a)
                                   (if x == y then b else subst (x,w) b) -- WRONG!!
subst (x,w) (Var y) | x == y = w
subst (x,w) a                = a

--------------------------------------------------------------------------------

assoc :: Rule Expr
assoc e =
  do (x :+: y) :+: z <- [e]
     pure (x :+: (y :+: z))

commut e =
  do x :+: y <- [e]
     guard (x > y)
     pure (y :+: x)
 ++
  do x :+: (y :+: z) <- [e]
     guard (x > y)
     pure (y :+: (x :+: z))

compute e =
  do Num i :+: Num j <- [e]
     pure (Num (i+j))

let_in e =
  do Let (x,a) b <- [e]
     guard (isValue a)
     pure (subst (x,a) b)     

isValue (Num _) = True
isValue _       = False

trs = assoc +++ commut +++ compute +++ let_in

--------------------------------------------------------------------------------

t0 = (Var "x" :+: Num 1) :+: Var "y"
t1 = Let ("x", t0) (Var "x" :+: Var "x")
t2 = Let ("x",Num 3) (Let ("y",Num 4) t1)

main = print (normalForms trs t2)

--------------------------------------------------------------------------------

