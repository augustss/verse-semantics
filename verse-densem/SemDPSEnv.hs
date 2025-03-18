module Main where

import Data.List( union, intercalate, (\\) )
import qualified Data.Set as S

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------

data Val
  = Const Integer
  | Var Ident
  | Tup [Val]
  | F
  | G
 deriving ( Eq, Ord )

instance Show Val where
  show (Const k) = show k
  show (Var x)   = show x
  show (Tup vs)  = "<" ++ intercalate "," (map show vs) ++ ">"
  show F         = "fun(x:=0|1){x+1}"
  show G         = "fun(x:=int){x+1}"

data Oper
  = Skip
  | Exi Ident
  | Ident :=: Val
  | Oper :>: Oper
  | Oper :|: Oper
  | Ident :=@ (Val,Val)
  | Fail
  | Scope Oper
  | If Oper Oper Oper
 deriving ( Eq, Ord )

instance Show Oper where
  show Skip             = "0"
  show (Exi x)          = "∃" ++ show x
  show (x :=: v)        = show x ++ "=" ++ show v
  show (op1 :>: op2)    = show1 ";" op1 ++ ";" ++ show1 ";" op2
  show (op1 :|: op2)    = show1 "|" op1 ++ "|" ++ show1 "|" op2
  show (x :=@ (v1,v2))  = show x ++ "=" ++ show v1 ++ "[" ++ show v2 ++ "]"
  show Fail             = "fail"
  show (Scope op)       = "{" ++ show op ++ "}"
  show (If op1 op2 op3) =
    "if(" ++ show op1 ++ "){" ++ show op2 ++ "}{" ++ show op3 ++ "}"

show1 :: String -> Oper -> String
show1 s op@(_ :>: _) = if s==";" then show op else showp op
show1 s op@(_ :|: _) = if s=="|" then show op else showp op
show1 _ op           = show op

showp :: Oper -> String
showp op = "(" ++ show op ++ ")"

freev :: Val -> [Ident]
freev (Var x)  = [x]
freev (Tup vs) = foldr union [] (map freev vs)
freev _        = []

free :: Oper -> [Ident]
free (Exi x)          = [x] -- yes
free (x :=: v)        = [x] `union` freev v
free (op1 :>: op2)    = free op1 `union` free op2
free (op1 :|: op2)    = free op1 `union` free op2
free (x :=@ (v1,v2))  = [x] `union` freev v1 `union` freev v2
free (Scope op)       = free op \\ exis op
free (If op1 op2 op3) = free (Scope (op1 :>: op2)) `union` free (Scope op3)
free _                = []

exis :: Oper -> [Ident]
exis (Exi x)       = [x]
exis (op1 :>: op2) = exis op1 `union` exis op2
exis (op1 :|: op2) = exis op1 `union` exis op2 -- is this correct?
exis _             = []

----------------------------------------------------------------------------------------

newtype Set a = Set (S.Set a)
 deriving ( Eq, Ord )

instance Show a => Show (Set a) where
  show (Set s) = "{" ++ intercalate "," (map show (S.toList s)) ++ "}"

set :: Ord a => [a] -> Set a
set xs = Set (S.fromList $ xs)

from :: Set a -> [a]
from (Set s) = S.toList s

(/\) :: Ord a => Set a -> Set a -> Set a
Set s1 /\ Set s2 = Set (s1 `S.intersection` s2)

(\/) :: Ord a => Set a -> Set a -> Set a
Set s1 \/ Set s2 = Set (s1 `S.union` s2)

(\-) :: Ord a => Set a -> Set a -> Set a
Set s1 \- Set s2 = Set (s1 `S.difference` s2)

elm :: Ord a => a -> Set a -> Bool
x `elm` Set s = x `S.member` s

seqUnion :: Ord a => [[Set a]] -> [Set a]
seqUnion []   = []
seqUnion seqs = foldr (\/) (set []) [ s | s:_ <- seqs ]
              : seqUnion [ ss | _:ss <- seqs, not (null ss) ]

----------------------------------------------------------------------------------------

data Pair a b
  = a :-> b
 deriving ( Eq, Ord )

instance (Show a, Show b) => Show (Pair a b) where
  show (x :-> y) = show x ++ "->" ++ show y

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun [Set (Pair Value Value)]
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k)   = show k
  show (Fun xys) = "Fun" ++ show xys

instance Num Value where
  fromInteger n = Int n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

----------------------------------------------------------------------------------------

univ :: Set Value
univ = set$ [ Int i | i <- [0..2] ]
         ++ [ Fun[set[0:->1],set[1:->2]]
            , Fun[set[0:->1,1:->2]]
            ]

type Env = Set (Pair Ident Value)

envs :: [Ident] -> Set Env
envs []     = set[ set[] ] 
envs (x:xs) = set[ set[x:->v] \/ env | env <-from$ envs xs, v <-from$ univ ]

(?) :: Env -> Ident -> Value
env ? x = head $ [ v | (y :-> v) <-from$ env, y==x ] ++ error (show x ++ " not in " ++ show env)

del :: Ident -> Env -> Env
del x env = set[ p | p@(y:->_) <-from$ env, y/=x ]

dels :: [Ident] -> Env -> Env
dels xs env = set[ p | p@(y:->_) <-from$ env, y `notElem` xs ]


{-
-- not needed for total environments
(~=) :: Env -> Env -> Bool
env1 ~= env2 = and [ v==w | (x:->v) <-from$ env1, (y:->w) <-from$ env2, x==y ]
-}

----------------------------------------------------------------------------------------

semVal :: Val -> Env -> Value
semVal (Const k) env = Int k
semVal (Var x)   env = env ? x
semVal (Tup vs)  env = Fun [ set[Int i:->semVal v env] | (i,v) <- [0..] `zip` vs ]
semVal F         env = Fun [ set[Int 0:->Int 1], set[Int 1:->Int 2] ]
semVal G         env = Fun [ set[Int i:->Int (i+1) | Int i <-from$ univ, Int (i+1) `elm` univ] ]

----------------------------------------------------------------------------------------

clean :: Ord a => [Set a] -> [Set a]
clean = filter (not . null . from)

----------------------------------------------------------------------------------------

sem :: Oper -> [Ident] -> [Set Env]
sem Skip xs =
  [ envs xs ]

sem (Exi x) xs =
  [ envs xs ]

sem (x :=: v) xs =
  clean
  [ set[ env
       | env <-from$ envs xs
       , env ? x == semVal v env
       ]  
  ]

sem (y :=@ (vf,vx)) xs =
  clean$
  seqUnion
  [ [ set[ env
         | env <-from$ envs xs
         , Fun fs' <- [semVal vf env]
         , fs' == fs
         , (semVal vx env :-> (env ? y)) `elm` f
         ]
    | f <- fs
    ]
  | Fun fs <-from$ univ
  ]

sem (op1 :|: op2) xs =
  sem op1 xs ++ sem op2 xs

sem (op1 :>: op2) xs =
  clean
  [ s1 /\ s2
  | s1 <- sem op1 xs
  , s2 <- sem op2 xs
  ]

sem Fail xs =
  []

sem (Scope op) xs =
  [ set[ dels ys env
       | env <-from$ s
       ]
  | s <- sem op (xs `union` ys)
  ]
 where
  ys = exis op

sem (If op1 op2 op3) xs =
  seqUnion
  [ [ set[ dels ys env
         | env <-from$ s1 /\ s2
         ]
    | s2 <- sem (Scope op2) (xs `union` ys)
    ]
  , [ set[ env
         | env <-from$ s3 \- set[ dels ys env | env <-from$ s1 ] 
         ]
    | s3 <- sem (Scope op3) xs
    ]
  ]
 where
  s1 = one ys (sem op1 (xs `union` ys))
  ys = exis op1

sem op xs =
  error ("no semantics yet for " ++ show op)

----------------------------------------------------------------------------------------

one :: [Ident] -> [Set Env] -> Set Env
one xs []     = set[]
one xs (s:ss) = s \/ set[ env
                        | env <-from$ one xs ss
                        , all (\env' ->
                                dels xs env /= dels xs env' 
                              )
                              (from s)
                        ]

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn ("univ = " ++ show univ)
     putStrLn ""
     sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Oper -> IO ()
printSem op =
  do putStrLn ("> " ++ show op)
     putStrLn ("--> " ++ show (sem (Scope op) (free (Scope op))))
     putStrLn ("scope = " ++ show (free (Scope op)))

----------------------------------------------------------------------------------------

x,y,z,f,g :: Val
x = Var (Ident "x")
y = Var (Ident "y")
z = Var (Ident "z")
r = Var (Ident "r")
f = Var (Ident "f")
g = Var (Ident "g")

exi :: Val -> Oper
exi (Var x) = Exi x

(=:) :: Val -> Val -> Oper
Var x =: v = x :=: v

(=@) :: Val -> (Val,Val) -> Oper
Var y =@ (f,x) = y :=@ (f,x)

instance Num Val where
  fromInteger n = Const n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

examples :: [Oper]
examples =
  [ (x=:1) :|: (x=:2)
  , ((x=:1) :|: (x=:2)) :>: (x=:y)
  , ((x=:1) :|: (x=:2) :|: (x=:1)) :>: ((x=:2) :|: (x=:1) :|: (x=:0))

  , If (exi z :>: ((z=:x) :>: (z=:y))) (r=@(F,z)) (r=:0)
  , If (exi z :>: ((z=:x) :|: (z=:y))) (r=@(F,z)) (r=:0)
  , If (exi z :>: ((x=:Tup[z,y]) :|: (z=:x))) (r=:z) (r=:0)

  , y =@ (F,x)
  , exi x :>: (y =@ (F,x))
  , y =@ (G,x)
  , exi x :>: (y =@ (G,x))

  , y =@ (f,x)
  ]

----------------------------------------------------------------------------------------
