{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
import Data.Maybe
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
           Set Ident Exp |
           Equal Exp Exp |
           Plus Exp Exp
  deriving (Show)

infix 1 :=

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

pattern (:=) :: Ident -> Exp -> Exp
pattern (:=) x e = Set x e

-- Sequencing, evaluate both and return second
infixl 0 `semi`
semi :: Exp -> Exp -> Exp
semi x y = Snd (Pair x y)

---------------------
--      Types for semantics
---------------------

data Value = VInt Integer | VPair Value Value
  deriving (Eq)

instance Show Value where
  show (VInt i) = show i
  show (VPair v1 v2) = "(" ++ show v1 ++ "," ++ show v2 ++ ")"

type Env = [(Ident, Lenient)]
type Ext = Env  -- Environment extension

empty :: Ext
empty = []

data Lenient = Delay (Env -> Lenient)
             | Done Value  -- (Done v) is equivalent to (Delay (\_. v))
-- This version uses strict left-to-right evaluation

type Res = (Env, Lenient)

instance Show Lenient where
  show (Done v)  = "Done " ++ show v
  show (Delay f) = "Delay!"

equalLenient :: Lenient -> Lenient -> Bool
equalLenient (Done v1) (Done v2) = v1==v2
equalLenient l1 l2 = error ("equalLenient " ++ show l1 ++ "; " ++ show l2)

---------------------
--      Semantics
---------------------

-- NOTE: This assumes all variable names are unique.
-- If the are not unique the bound variable from a Def might be found
-- in the outer environment.
-- A fix for this would be to delete x from rho in the Def case.

eval :: Exp -> Env -> [Res]
-- In a call (eval e rho), the domain of the Env in the returned Res
-- is precisely the variables bound in 'e'.

eval (Var i) rho = [(empty, evalVar i rho)]
eval (Con k) _   = [(empty, Done (VInt k))]
eval Fail    _   = []

eval (Fst e1) rho = [ (ext1, liftL1 vfst fv1) | (ext1, fv1) <- eval e1 rho ]
eval (Snd e1) rho = [ (ext1, liftL1 vsnd fv1) | (ext1, fv1) <- eval e1 rho ]

eval (Plus e1 e2) rho =
  [ (ext1 ++ ext2, liftL2 vplus fv1 fv2)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho) ]

eval (Pair e1 e2) rho =
  [ (ext1 ++ ext2, liftL2 VPair fv1 fv2)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho) ]

eval (Alt e1 e2) rho =
  tieKnot (eval e1 rho) ++ tieKnot (eval e2 rho)

eval (Set x e) rho =
  [ (extendEnv x fv1 ext1, fv1) | (ext1, fv1) <- eval e rho ]

eval (Equal e1 e2) rho =
  [ (ext1 ++ ext2, fv1)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho)
  , fv1 `equalLenient` fv2
  ]

evalVar :: Ident -> Env -> Lenient
evalVar i rho =
  case lookupEnv i rho of
    Just v  -> v
    Nothing -> delay
       where
          delay :: Lenient
          delay = Delay $ \ ext -> case lookupEnv i ext of
                                      Just len -> len
                                      Nothing  -> delay

tieKnot :: [Res] -> [Res]
tieKnot vs = [ (empty, apply v ext) | (ext, v) <- vs ]
   where
     apply :: Lenient -> Env -> Lenient
     apply (Done v)  _   = Done v
     apply (Delay f) ext = f ext

---------------------
--      Auxiliary semantic operations
---------------------

extendEnv :: Ident -> Lenient -> Env -> Env
extendEnv x v rho = (x,v) : rho

unionEnv :: Env -> Env -> Env
-- If both envs bind the same variable 'ext2' wins
unionEnv ext1 ext2 = ext1 ++ ext2

lookupEnv :: Ident -> Env -> Maybe Lenient
lookupEnv = lookup

vplus :: Value -> Value -> Value
vplus (VInt i1) (VInt i2) = VInt (i1 + i2)
vplus v1 v2 = error $ "vplus " ++ show (v1, v2)

vfst :: Value -> Value
vfst (VPair a _) = a
vfst v           = error $ "vfst " ++ show v

vsnd :: Value -> Value
vsnd (VPair _ v) = v
vsnd v           = error $ "vsnd " ++ show v

liftL1 :: (Value -> Value) -> Lenient -> Lenient
liftL1 g (Delay f) = Delay (\ext -> liftL1 g (f ext))
liftL1 g (Done v)  = Done (g v)

liftL2 :: (Value -> Value -> Value) -> Lenient -> Lenient -> Lenient
liftL2 g (Delay f1) (Delay f2) = Delay (\ ext -> liftL2 g (f1 ext) (f2 ext))
liftL2 g (Delay f1) (Done v2)  = Delay (\ ext -> liftL2 g (f1 ext) (Done v2))
liftL2 g (Done v1) (Delay f2)  = Delay (\ ext -> liftL2 g (Done v1) (f2 ext))
liftL2 g (Done v1) (Done v2)   = Done (v1 `g` v2)


---------------------
--      Tests
---------------------

ev :: Exp -> [Value]
ev e = map get (tieKnot (eval e []))
  where
    get (_, Done v)  = v
    get (_, Delay f) = case f empty of
                         Done v -> v
                         Delay {} -> error "Top level delay"

-- Works [1,2]
test1 = 1 ||| 2

-- Works [2,3,3,4]
test2 = (1 ||| 2) + (1 ||| 2)

-- Works [2,4]
test3 = ("x" := 1 ||| 2) + "x"

-- Should fail, since variables in ||| do not escape
-- Fails
test4 = (("x" := 1) ||| 2) + "x"

-- works [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))]
test5 = ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

-- works [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))]
test6 = ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

-- works [4]
test7 = ("x" := 1 ||| 2) + ("x" === 2)

-- works [(1,1),(2,2)]
test8 = Pair "x" ("x" := 1 ||| 2)

-- Works [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))]
test9 = Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
-- test10 works [((1,7),1)]
test10 = Pair ("x" := (Pair 1 7 |||
                       Pair "y" ("y" := 2)))
              (Fst "x" === 1)

-- x's value depends on recurively bound z, so we get stuck.
-- Fails
test11 = Pair ("x" := (Pair 1 "z" |||
                        Pair "y" ("y" := 2)))
               (Pair ("x" === 1) ("z" := 3))

-- Fails (top level delay)
test12 = "t" := Pair 1 (Fst "t")

-- Works [(1,1)]
test13 = "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test14 = "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")
