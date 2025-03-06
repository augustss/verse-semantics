module Main where

import Data.List
import qualified Data.Set as S

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------

data Expr
  = Const Integer
  | Var Ident
  | Tup [Expr]
  | Exi Ident Expr
  | Expr :=: Expr
  | Expr :>: Expr
  | Expr :|: Expr
  | Fail
  | All Expr
  | One Expr
 deriving ( Eq, Ord )

instance Show Expr where
  show (Const k)   = show k
  show (Var x)     = show x
  show (Tup es)    = "<" ++ intercalate "," (map show es) ++ ">"
  show (Exi x e)   = "∃" ++ show x ++ "." ++ show e
  show (e1 :=: e2) = show1 ""  e1 ++ "=" ++ show1 ""  e2
  show (e1 :>: e2) = show1 ";" e1 ++ ";" ++ show1 ";" e2
  show (e1 :|: e2) = show1 "|" e1 ++ "|" ++ show1 "|" e2
  show Fail        = "fail"
  show (All e)     = "all{" ++ show e ++ "}"
  show (One e)     = "one{" ++ show e ++ "}"

show1 :: String -> Expr -> String
show1 _  e@(Exi _ _) = showp e
show1 op e@(_ :>: _) = if op==";" then show e else showp e
show1 op e@(_ :|: _) = if op=="|" then show e else showp e
show1 _  e           = show e

showp :: Expr -> String
showp e = "(" ++ show e ++ ")"

free :: Expr -> [Ident]
free (Var x)     = [x]
free (Tup es)    = foldr union [] (map free es)
free (Exi x e)   = free e \\ [x]
free (e1 :=: e2) = free e1 `union` free e2
free (e1 :>: e2) = free e1 `union` free e2
free (e1 :|: e2) = free e1 `union` free e2
free (All e)     = free e
free (One e)     = free e
free _           = []

----------------------------------------------------------------------------------------

newtype Set a = Set (S.Set a)
 deriving ( Eq, Ord )

instance Show a => Show (Set a) where
  show (Set s) = "{" ++ intercalate "," (map show (S.toList s)) ++ "}"

set :: Ord a => [a] -> Set a
set xs = Set (S.fromList $ xs)

from :: Set a -> [a]
from (Set s) = S.toList s

(\/) :: Ord a => Set a -> Set a -> Set a
Set s1 \/ Set s2 = Set (s1 `S.union` s2)

----------------------------------------------------------------------------------------

flat :: Ord a => [Set [a]] -> Set [a]
flat []       = set[ [] ]
flat (s:sets) = set[ xs++ys | xs <-from$ s, ys <-from$ flat sets ]

----------------------------------------------------------------------------------------

data Pair a b
  = a :-> b
 deriving ( Eq, Ord )

instance (Show a, Show b) => Show (Pair a b) where
  show (x :-> y) = show x ++ "->" ++ show y

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun [Pair Value Value]
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k)    = show k
  show (Fun xys)
    | and [ x == Int i | (i,x:->_) <- [0..] `zip` xys ] = 
        "<" ++ intercalate "," [ show y | (_:->y) <- xys ] ++ ">"

    | otherwise =
        show xys

tup :: [Value] -> Value
tup ys = Fun [ Int i :-> y | (i,y) <- [0..] `zip` ys ]

----------------------------------------------------------------------------------------

univ :: Set Value
univ = set[ Int i | i <- [1..3] ]

type Env = Set (Pair Ident Value)

(?) :: Env -> Ident -> Value
env ? x = head [ v | (y :-> v) <-from$ env, y==x ]

del :: Ident -> Env -> Env
del x env = set[ p | p@(y:->_) <-from$ env, y/=x ]

univEnv :: [Ident] -> Set Env
univEnv []     = set[ set[] ]
univEnv (x:xs) = set[ set ((x :-> v):from env) | v <-from$ univ, env <-from$ univEnv xs ]

univEnvs :: [Ident] -> Set [Env]
univEnvs []     = set[ [set[]] ]
univEnvs (x:xs) = set[ [ set ((x :-> v):from env) | v <- vs, env <- envs ]
                     | vs <- permutations (from univ)
                     , envs <-from$ univEnvs xs
                     ]

----------------------------------------------------------------------------------------

sem :: Expr -> [Ident] -> Set [(Env,Value)]
sem (Const k) scope =
  set[ [ (env, Int k) | env <- envs ]
     | envs <-from$ univEnvs scope
     ]

sem (Var x) scope =
  set[ [ (env, env?x) | env <- envs ]
     | envs <-from$ univEnvs scope
     ]
{-
sem (Const k) scope =
  set[ [ (env, Int k) ]
     | env <-from$ univEnv scope
     ]

sem (Var x) scope =
  set[ [ (env, env?x) ]
     | env <-from$ univEnv scope
     ]
-}
sem (Exi x e) scope =
  set[ [ (del x env, v) | (env,v) <- s ]
     | s <-from$ sem e ([x] `union` scope)
     ]

sem (e1 :|: e2) scope =
  set[ s1++s2
     | s1 <-from$ sem e1 scope
     , s2 <-from$ sem e2 scope
     ]

sem (Var x :=: e) scope =
  set[ [ (env,v)
       | (env,v) <- s
       , env?x == v
       ]
     | s <-from$ sem e scope
     ]

sem (e1 :>: e2) scope =
  set[ s3
     | s1 <-from$ sem e1 scope
     , s3 <-from$ flat [ set[ [ (env,v2) | (env',v2)<-s2, env'==env ]
                            | s2 <-from$ sem e2 scope
                            ]
                       | (env,_) <- s1
                       ]
     ]

sem (One e) scope =
  set[ [(env,v) | env <- envs, v:_ <- [[ v | (env',v)<-s, env'==env ]] ]
     | s <-from$ sem e scope
     , envs <-from$ univEnvs scope
     ]

sem (All e) scope =
  set[ [(env, tup [ v | (env',v)<-s, env'==env ]) | env <- envs]
     | s <-from$ sem e scope
     , envs <-from$ univEnvs scope
     ]

sem e scope =
  error ("no semantics yet for " ++ show e)

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Expr -> IO ()
printSem e =
  do putStrLn ("> " ++ show e)
     putStrLn ("--> " ++ show (sem e (free e)))
     putStrLn ("(scope: " ++ show (free e) ++ ")")

----------------------------------------------------------------------------------------

x,y,z :: Expr
x = Var (Ident "x")
y = Var (Ident "y")
z = Var (Ident "z")

exi :: Expr -> Expr -> Expr
exi (Var x) e = Exi x e

instance Num Expr where
  fromInteger n = Const n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

examples :: [Expr]
examples =
  [ 1
  , x
  , x :>: 1
  , x :=: 1
  , x :=: y

  , 1 :|: 2
  , x :=: (1 :|: 2)
  -- , (1 :|: 2) :=: x
  -- , (2 :|: 1) :=: (1 :|: 2 :|: 3)
  , (x :=: (1 :|: 2)) :>: (x :=: y)
  , exi x $ (x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 2:|: 3))

  , One x
  , All x
  , One (exi x $ (x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 3)))
  , All (exi x $ (x :=: (2 :|: 1)) :>: (x :=: (1 :|: 2 :|: 2 :|: 3)))
  ]
  
----------------------------------------------------------------------------------------
