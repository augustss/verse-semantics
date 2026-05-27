{-# LANGUAGE GADTs #-}
module Timbda2 where

import Prelude hiding ( pi )

---------------------------------------------------------------------------
-- identifiers and environments

data Ident env a where
  This :: Ident (env,a) a
  That :: Ident env a -> Ident (env,b) a

data Env env where
  Empty :: Env ()
  Add   :: a -> Env env -> Env (env,a)

look :: Ident env a -> Env env -> a
look This     (Add a _)   = a
look (That x) (Add _ env) = look x env

---------------------------------------------------------------------------
-- semantic types

-- naturals
type Nat   = Int

-- function types
type a:->b = [(a,b)]

pi3 :: [(a,b)] -> (b->[(c,d)]) -> [a:->(b,c,d)]
pi3 []          f = [ [] ]
pi3 ((a,b):abs) f = [ (a,(b,c,d)):abcds | (c,d) <- f b, abcds <- pi3 abs f ]

---------------------------------------------------------------------------
-- expressions in Timbda calculus

data Expr env a b where
  Con   :: Nat -> Expr env Nat Nat
  Var   :: Ident env a -> Expr env a a
  Nat   :: Expr env (Nat:->Nat) (Nat:->Nat)
  Lam   :: Expr env a b -> Expr (env,b) c d -> Expr env (b:->c) (a:->d)
  App   :: Eq a => Expr env in1 (a:->b) -> Expr env in2 a -> Expr env b b
  Typ   :: Expr env in1 (a:->b) -> Expr env a b -- :e
  Fail  :: Expr env a b
  (:|:) :: Expr env a b -> Expr env a b -> Expr env a b
  --Fix   :: Eq a => Expr env (a:->a) -> Expr env a
  
eval :: Env env -> Expr env a b -> [(a,b)]
eval env (Con k)     = [(k,k)]
eval env (Var x)     = [(a,a)|let a=look x env]
eval env Nat         = [(h,h)|let h=[(a,a)|a<-[0..5]]] -- 5 is the largest number
eval env (Lam e1 e2) = [ ( [(b,c)|(_,(b,c,_)) <- h] 
                         , [(a,d)|(a,(_,_,d)) <- h]
                         )
                       | h <- pi3 (eval env e1)
                                  (\b -> eval (Add b env) e2)
                       ]
eval env (App e1 e2) = [(b,b) | (_,f) <- eval env e1, (a,b) <- f, (_,a') <- eval env e2, a==a']
eval env (Typ e)     = [(a,b) | (_,h) <- eval env e, (a,b) <- h]
eval env Fail        = []
eval env (e1 :|: e2) = eval env e1 ++ eval env e2
--eval env (Fix e)     = [ a | f <- eval env e, (a,a') <- f, a==a' ]

---------------------------------------------------------------------------
-- examples

ex1 = Lam (Typ Nat) (Typ Nat)

---------------------------------------------------------------------------

