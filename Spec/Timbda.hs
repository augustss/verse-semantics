{-# LANGUAGE GADTs #-}
module Timbda where

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

pi :: [a] -> (a->[b]) -> [a:->b]
pi []     f = [ [] ]
pi (a:as) f = [ (a,b):abs | b <- f a, abs <- pi as f ]

---------------------------------------------------------------------------
-- expressions in Timbda calculus

data Expr env a where
  Con   :: Nat -> Expr env Nat
  Var   :: Ident env a -> Expr env a
  Nat   :: Expr env (Nat:->Nat)
  Lam   :: Expr env a -> Expr (env,a) b -> Expr env (a:->b)
  App   :: Eq a => Expr env (a:->b) -> Expr env a -> Expr env b
  Img   :: Expr env (a:->b) -> Expr env b -- :e
  Fail  :: Expr env a
  (:|:) :: Expr env a -> Expr env a -> Expr env a
  Fix   :: Eq a => Expr env (a:->a) -> Expr env a
  
eval :: Env env -> Expr env a -> [a]
eval env (Con k)     = [k]
eval env (Var x)     = [look x env]
eval env Nat         = [[(a,a)|a<-[0..5]]] -- 5 is the largest number
eval env (Lam e1 e2) = pi (eval env e1) (\a -> eval (Add a env) e2)
eval env (App e1 e2) = [b | f <- eval env e1, (a,b) <- f, a' <- eval env e2, a==a']
eval env (Img e)     = [b | f <- eval env e, (a,b) <- f]
eval env Fail        = []
eval env (e1 :|: e2) = eval env e1 ++ eval env e2
eval env (Fix e)     = [ a | f <- eval env e, (a,a') <- f, a==a' ]

---------------------------------------------------------------------------
-- examples

ex1 = Lam (Img Nat) (Img Nat)

---------------------------------------------------------------------------

