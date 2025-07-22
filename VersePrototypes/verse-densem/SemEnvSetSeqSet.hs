module Main where

import Data.List
import qualified Data.Set as S

----------------------------------------------------------------------------------------

newtype Ident = Ident String
 deriving ( Eq, Ord )

instance Show Ident where
  show (Ident x) = x

----------------------------------------------------------------------------------------

data Oper
  = Ident :=: Ident
  | Ident := Integer
  | Ident :- (Integer,Integer)
  | Exi Ident
  | Oper :>: Oper
  | Oper :|: Oper
  | Fail
  | Scope Oper
  | If Oper Oper Oper
 deriving ( Eq, Ord )

instance Show Oper where
  show (x :=: y)        = show x ++ "=" ++ show y
  show (x := k)         = show x ++ "=" ++ show k
  show (x :- (a,b))     = show a ++ "<=" ++ show x ++ "<=" ++ show b
  show (Exi x)          = "∃" ++ show x
  show (op1 :>: op2)    = show1 ";" op1 ++ ";" ++ show1 ";" op2
  show (op1 :|: op2)    = show1 "|" op1 ++ "|" ++ show1 "|" op2
  show Fail             = "fail"
  show (Scope op)       = "{" ++ show op ++ "}"
  show (If op1 op2 op3) = "if(" ++ show op1 ++ "){" ++ show op2 ++ "}else{" ++ show op3 ++ "}"

show1 :: String -> Oper -> String
show1 op e@(_ :>: _) = if op==";" then show e else showp e
show1 op e@(_ :|: _) = if op=="|" then show e else showp e
show1 _  e           = show e

showp :: Oper -> String
showp e = "(" ++ show e ++ ")"

free :: Oper -> [Ident]
free (x :=: y)        = nub [x,y]
free (x := k)         = [x]
free (x :- _)         = [x]
free (Exi x)          = [x]
free (op1 :>: op2)    = free op1 `union` free op2
free (op1 :|: op2)    = free (Scope op1) `union` free (Scope op2)
free Fail             = []
free (Scope op)       = free op \\ exis op
free (If op1 op2 op3) = free (Scope op1) `union` free (Scope op2) `union` free (Scope op3)

exis :: Oper -> [Ident]
exis (Exi x)       = [x]
exis (op1 :>: op2) = exis op1 `union` exis op2
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

(\/) :: Ord a => Set a -> Set a -> Set a
Set s1 \/ Set s2 = Set (s1 `S.union` s2)

(/\) :: Ord a => Set a -> Set a -> Set a
Set s1 /\ Set s2 = Set (s1 `S.intersection` s2)

(\\-) :: Ord a => Set a -> Set a -> Set a
Set s1 \\- Set s2 = Set (s1 `S.difference` s2)

----------------------------------------------------------------------------------------

data Pair a b
  = a :-> b
 deriving ( Eq, Ord )

instance (Show a, Show b) => Show (Pair a b) where
  show (x :-> y) = show x ++ "->" ++ show y

----------------------------------------------------------------------------------------

data Value
  = Int Integer
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k) = show k

instance Num Value where
  fromInteger n = Int n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

----------------------------------------------------------------------------------------

univ :: Set Value
univ = set$[ Int i | i <- [1..3] ]

{-
type Env = Set (Pair Ident Value)

(?) :: Env -> Ident -> Value
env ? x = head $ [ v | (y :-> v) <-from$ env, y==x ] ++ error (show x ++ " not in " ++ show env)

dels :: [Ident] -> Env -> Env
dels xs env = set[ p | p@(y:->_) <-from$ env, y `notElem` xs ]

envs :: [Ident] -> Set Env
envs []     = set[ set[] ]
envs (x:xs) = set[ set ((x :-> v):from env) | v <-from$ univ, env <-from$ envs xs ]
-}

newtype ENV = ENV [ [(Ident,Value)] ] -- disj (conj pair)
 deriving ( Eq, Ord )

instance Show ENV where
  show (ENV []) = "fail"
  show (ENV es) = concat (intersperse "/" (map showE es))

showE :: [(Ident,Value)] -> String
showE []  = "()"
showE xvs = concat (intersperse ";" [ show x ++ "=" ++ show v | (x,v) <- xvs ])

(%=:) :: Ident -> Ident -> ENV
x %=: y
  | x <= y    = ENV [ [(x,v),(y,v)] | v <-from$ univ ]
  | otherwise = y %=: x

(%=) :: Ident -> Value -> ENV
x %= v = ENV [ [(x,v)] ]

(%-) :: Ident -> (Integer,Integer) -> ENV
x %- (a,b) = ENV [ [(x,v)] | v@(Int k)<-from$ univ, a<=k, k<=b ]

univE :: ENV
univE = ENV [ [] ]

failE :: ENV
failE = ENV []

hide :: [Ident] -> ENV -> ENV
hide xs (ENV xvss) =
  ENV (usort [ usort [ (x,v) | (x,v)<-xvs, x `notElem` xs ]| xvs <- xvss ])

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

compl :: ENV -> ENV
compl (ENV [])   = ENV [ [] ]
compl (ENV xvss) =
  foldr1 (%/\) [ ENV (usort [ [(x,v')] | (x,v)<-xvs, v'<-from$ univ, v/=v' ])
               | xvs <- xvss
               ]

(%/\) :: ENV -> ENV -> ENV
ENV xvss %/\ ENV yvss =
  ENV (usort [ zvs
             | xvs <- xvss
             , yvs <- yvss
             , zvs <- xvs `merge` yvs
             ])
 where
  []          `merge` yvs         = [yvs]
  xvs         `merge` []          = [xvs]
  ((x,v):xvs) `merge` ((y,w):yvs) =
    case x `compare` y of
      LT -> [ (x,v):zvs | zvs <- merge xvs ((y,w):yvs) ]
      EQ -> [ (x,v):zvs | v==w, zvs <- merge xvs yvs ]
      GT -> [ (y,w):zvs | zvs <- merge ((x,v):xvs) yvs ]

(%\/) :: ENV -> ENV -> ENV
ENV xvss %\/ ENV yvss = ENV (usort (xvss ++ yvss))

----------------------------------------------------------------------------------------

cleanC :: [ENV] -> [ENV]
cleanC ss = [ s | s <- ss, s /= ENV [] ]

cleanS :: Ord a => Set [a] -> Set [a]
cleanS ss = set[ s | s <-from$ ss, not (null s) ]

sem :: Oper -> Set [ENV]
sem (x := k) =
  set[ [ x %= Int k ] ]
     
sem (x :=: y) =
  set[ [ x %=: y ] ]
     
sem (x :- (a,b)) =
  set[ [ x %- (a,b) ] ]
     
sem (Exi x) =
  set[ [ univE ] ]

sem (op1 :|: op2) =
  set[ s1++s2
     | s1 <-from$ sem op1
     , s2 <-from$ sem op2
     ]

sem (op1 :>: op2) =
  cleanS $
  set[ cleanC (concat s)
     | s1 <-from$ sem op1
     , s <-from$ cleanS $ flatten
          [ cleanS $ set[ cleanC [s %/\ s' | s'<-s2]
               | s2 <-from$ sem op2
               ]
          | s <- s1
          ]
     ]
 where
  flatten []     = set[ [] ]
  flatten (s:ss) = set[ x:xs | x <-from$ s, xs <-from$ flatten ss ]

sem Fail =
  set[ [] ]
  
sem (Scope op) =
  set[ [ hide ys es
       | es <- s
       ]
     | s <-from$ sem op
     ]
 where
  ys = exis op
  
sem (If op1 op2 op3) =
  cleanS $
  set[ cleanC s23
     | s1 <-from$ sem op1
     , let s = first ys s1
     , s23 <- [ [hide ys (s %/\ s')|s'<-s2]
              | s2 <-from$ sem op2
              ] ++
              [ [s' %/\ compl (hide ys s)|s'<-s3]
              | s3 <-from$ sem op3
              ]
     ]
 where
  ys = exis op1
 
  first ys []       = failE
  first ys (se:ses) = se %\/ first ys [se' %/\ compl (hide ys se) | se'<-ses]

sem op =
  error ("no semantics yet for '" ++ show op ++ "'")

----------------------------------------------------------------------------------------

main :: IO ()
main =
  do putStrLn ("univ = " ++ show univ)
     putStrLn ""
     sequence_ [ printSem e >> putStrLn "" | e <- examples ]

printSem :: Oper -> IO ()
printSem op =
  do putStrLn ("> " ++ show op)
     putStrLn ("--> " ++ show (sem op))

----------------------------------------------------------------------------------------

x,y,z,f,g :: Ident
x = Ident "x"
y = Ident "y"
z = Ident "z"
f = Ident "f"
g = Ident "g"

examples :: [Oper]
examples =
  [ x :- (1,2)
  , (x:=1):|:(x:=2)
  , (x :- (1,2)) :>: ((x:=1):|:(x:=2))
  , ((x:=1):|:(x:=2)) :>: (x :- (1,2))
  , (x :- (1,2)) :>: (y :- (2,3))
  , ((x:=1):|:(x:=2)) :>: (y :- (2,3))
  , (x :- (1,2)) :>: ((y:=2):|:(y:=3))
  , ((x:=1):|:(x:=2)) :>: ((y:=2):|:(y:=3))
  , If(x:=1)(x:=1)(x:=2) :>: ((x:=1):|:(x:=2))
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(x:=1)(x:=2)
  , If(x:=1)(y:=2)(y:=3) :>: ((x:=1):|:(x:=2))
  , ((x:=1):|:(x:=2)) :>: If(x:=1)(y:=2)(y:=3) 
  , If(((y:=1) :|: (y:=2))) (x:=1)(x:=2)
  , If(Exi y :>: ((y:=1) :|: (y:=2))) (x:=:y)(x:=2)
  ]

----------------------------------------------------------------------------------------
