{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.String
import Debug.Trace

---------------------
--      Expressions
---------------------

type Ident = String

-- e ::= x | k |  (e1 | e2)  |  (e = k)  |  defrec { x := e } in e | (e1,e2) | fst(e) | snd(e)
data Exp = Var Ident | Con Integer |
           Alt Exp Exp | Fail |
           Pair Exp Exp | Fst Exp | Snd Exp |
           Def Ident Exp Exp |
           Equal Exp Exp |
           Plus Exp Exp
  deriving (Show)

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = Plus
  fromInteger = Con

instance IsString Exp where
  fromString = Var

infixl 3 |||
(|||) :: Exp -> Exp -> Exp
(|||) = Alt

infixl 2 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal  

def :: Ident -> Exp -> Exp -> Exp
def = Def

---------------------
--      Types for semantics
---------------------

data Value = VCon Integer | VPair Value Value
  deriving (Eq)

instance Show Value where
  show (VCon i) = show i
  show (VPair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"

type Env = [(Ident, Value)]

type Ext = (Ident, Value)  -- Environment extension in a Def.  Only one variable for now
type Res = Ext -> Value

---------------------
--      Semantics
---------------------

eval :: Exp -> Env -> [Res]
eval (Var i) rho = evalVar i rho
eval (Con k) _ = [const $ VCon k]
eval (Alt e1 e2) rho = eval e1 rho ++ eval e2 rho
eval Fail _ = []
eval (Equal e1 e2) rho =
  [ const v1 | fv1 <- eval e1 rho, fv2 <- eval e2 rho,
               let v1 = fv1 empty, let v2 = fv2 empty,
               v1 == v2 ]
eval (Pair e1 e2) rho = [ \ext -> VPair (fv1 ext) (fv2 ext) | fv1 <- eval e1 rho, fv2 <- eval e2 rho ]
eval (Fst e1) rho = map (vfst .) (eval e1 rho)
  where vfst (VPair v _) = v
        vfst v = error $ "vfst " ++ show v
eval (Snd e1) rho = map (vsnd .) (eval e1 rho)
  where vsnd (VPair _ v) = v
        vsnd v = error $ "vsnd " ++ show v
eval (Plus e1 e2) rho = [ \ext -> vplus (fv1 ext) (fv2 ext) | fv1 <- eval e1 rho, fv2 <- eval e2 rho ]
  where vplus (VCon i1) (VCon i2) = VCon (i1 + i2)
        vplus v1 v2 = error $ "vplus " ++ show (v1, v2)
eval (Def x r b) rho =
  -- Why does it work with const?
  [ const v | xv <- xvs, v <- evalV b ((x,xv):rho) ]
  where rfs = eval r rho
        xvs = aux rfs xvs
        aux [] _ = []
        aux (f:fs) ~(v:vs) = f (x,v) : aux fs vs

evalVar :: Ident -> Env -> [Res]
evalVar i rho = [\ ext ->
  case lookup i (ext:rho) of
    Nothing -> error $ "not found " ++ show i
    Just v -> v
  ]

empty :: Ext
empty = ("",undefined)

evalV :: Exp -> Env -> [Value]
evalV e rho = [f empty | f <- eval e rho]

---------------------
--      Tests
---------------------
ixy = "xy"
xy = Var ixy
x = Fst xy
y = Snd xy

-- ex1:           def { xy = ( 1|2, 2|3 ) } in xy
-- Equivalently:  def { (x,y) = ( 1|2, 2|3 ) } in (x,y)
-- Tim:           [(1,2),(1,3),(2,2),(2,3)]
-- Rec:           [(1,2),(1,3),(2,2),(2,3)]
-- eval ex1 [] == [(1,2),(1,3),(2,2),(2,3)]
ex1 = Def ixy (Pair (Con 1 `Alt` Con 2) (Con 2 `Alt` Con 3)) xy

-- ex1a:  def { (x,y) = ( 1|2, 2|3 ) } in (y,x)
-- eval ex1a [] == [(2, 1),(3, 1),(2, 2),(3, 2)]
ex1a = Def ixy (Pair (Con 1 `Alt` Con 2) (Con 2 `Alt` Con 3)) (Pair (Snd xy) (Fst xy))

-- ex2:   def { (x,y) = ( (y+1)|3, 1|2 ) in (x,y)
-- Tim            [(2,1),(3,2),(3,1),(3,2)]
-- Rec            [(2,1),(3,2),(3,1),(3,2)]
-- eval ex2 [] == [(2,1),(3,2),(3,1),(3,2)]
ex2 = Def ixy (Pair (Plus y (Con 1) `Alt` Con 3) (Con 1 `Alt` Con 2)) xy

-- ex5: def { xy = ((y=4)|2), (3|4) } in xy
-- Tim            [(4,4),(2,3),(2,4)]
-- Rec            bottom
-- eval ex5 [] == bottom (error, xy unbound)
ex5 = Def ixy (Pair (Equal y (Con 4) `Alt` Con 2) (Con 3 `Alt` Con 4)) xy

-- ex7: def { x = 1 | x | 2 } in x
-- Tim            [1,..,2]
-- Rec            [1,       -- loops, but length ex7 == 3
-- eval ex7 [] == [1,       -- loops, but length ex7 == 3
ex7 = Def "x" (Con 1 `Alt` Var "x" `Alt` Con 2) (Var "x")

-- ex8: def { x = 1|2 } in def { y = 3|4 } in (x, y)
-- eval ex8 [] == [(1,3),(1,4),(2,3),(2,4)]
ex8 = Def "x" (Con 1 `Alt` Con 2) $
        Def "y" (Con 3 `Alt` Con 4) $
          Pair (Var "x") (Var "y")
          
------ Sugared examples
ex1s = def "xy" (1 ||| 2 # 2 ||| 3) "xy"

ex2s = def "xy" (Snd "xy" + 1 ||| 3 # 1 ||| 2) "xy"

ex8s = def "x" (1 ||| 2) $ def "y" (3 ||| 4) $ "x" # "y"
