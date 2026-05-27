{-# LANGUAGE GADTs #-}
module Timbda3 where

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

-- sets, relations
type Set a = [a]
type Rel a = Set a

-- function types
type a:->b = Rel (a,b)

pi :: Rel a -> (a -> Rel b) -> Rel (a:->b)
pi []      rng = [ [] ]
pi (a:dom) rng = [ (a,b):f | b <- rng a, f <- pi dom rng ]

---------------------------------------------------------------------------
-- expressions in Timbda calculus

data Expr env env' a b where
  -- k
  Con   :: Nat
        -> Expr env env Nat Nat

  -- x
  Var   :: Ident env u
        -> Expr env env u u

  -- nat
  Nat   :: Expr env env (Nat:->Nat) (Nat:->Nat)

  -- fun(e1){e2}
  Fun   :: Expr env0 env1 a b
        -> Expr env1 env2 c d
        -> Expr env0 env0 (b:->c) (a:->d)

  -- x:=e  (but x is only visible towards the right) (x is a deBrijn-index)
  Def   :: Expr env1 env2 u v
        -> Expr env1 (env2,v) u v

  -- e1;e2
  (:>:) :: Expr env1 env2 i j
        -> Expr env2 env3 u v
        -> Expr env1 env3 u v

  -- e1[e2]
  App   :: Eq a
        => Expr env1 env2 i (a:->b)
        -> Expr env2 env3 j a
        -> Expr env1 env3 b b

  -- <e1,e2>
  Tup   :: Expr env1 env2 a b
        -> Expr env2 env3 c d
        -> Expr env1 env3 (a,c) (b,d)

  -- :e
  Typ   :: Expr env1 env2 i (a:->b)
        -> Expr env1 env2 a b

  -- fail
  Fail  :: Expr env1 env2 a b

  -- e1|e2
  (:|:) :: Expr env0 env1 a b
        -> Expr env0 env2 a b
        -> Expr env0 env0 a b

  -- e1=e2
  (:=:) :: (Eq a,Eq b)
        => Expr env1 env2 a b
        -> Expr env2 env3 a b
        -> Expr env1 env3 a b

eval :: Env env -> Expr env env' u v -> Rel (Env env',u,v)
eval env (Con k)     = [(env,k,k)]
eval env (Var x)     = [(env,u,u)] where u=look x env
eval env Nat         = [(env,h,h)] where h=[(n,n)|n<-[0..5]]
eval env (Fun e1 e2) = [ ( env
                         , [(b,c) | ((_,_,b),(_,c,_)) <- h]
                         , [(a,d) | ((_,a,_),(_,_,d)) <- h]
                         )
                       | h <- pi (eval env e1)
                                 (\(env1,_,_) -> eval env1 e2)
                       ]
eval env (Def e)     = [ (Add v env1,u,v)
                       | (env1,u,v) <- eval env e
                       ]
eval env (e1:>:e2)   = [ (env2,u,v)
                       | (env1,_,_) <- eval env e1
                       , (env2,u,v) <- eval env1 e2
                       ]
eval env (App e1 e2) = [ (env2,b,b)
                       | (env1,_,f)  <- eval env e1
                       , (a,b)       <- f
                       , (env2,_,a') <- eval env1 e2
                       , a==a'
                       ]
eval env (Tup e1 e2) = [ (env2,(a,c),(b,d))
                       | (env1,a,b) <- eval env e1
                       , (env2,c,d) <- eval env1 e2
                       ]
eval env (Typ e)     = [ (env1,a,b)
                       | (env1,_,f) <- eval env e
                       , (a,b)      <- f
                       ]
eval env Fail        = []
eval env (e1:|:e2)   = [ (env,a,b) | (_,a,b) <- eval env e1 ] ++
                       [ (env,a,b) | (_,a,b) <- eval env e2 ]
eval env (e1:=:e2)   = [ (env2,u1,v2)
                       | (env1,u1,v1) <- eval env e1
                       , (env2,u2,v2) <- eval env1 e2
                       , u1==u2
                       , v1==v2
                       ]

---------------------------------------------------------------------------
-- examples

ex1 = Fun (Typ Nat) (Typ Nat)          -- fun(:nat){:nat}
ex2 = Fun (Def (Typ Nat)) (Var This)   -- fun(x:=(:nat)){x}
ex3 = Fun (Tup (Def (Typ Nat)) (Def (Typ Nat))) (Var (That This))
      -- fun(x:nat,y:nat){x}

---------------------------------------------------------------------------

