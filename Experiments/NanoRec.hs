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

data Lenient = Delay (Env -> Value)
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
  [ (extendEnv x fv1 rho, fv1) | (ext1, fv1) <- eval e rho ]

eval (Equal e1 e2) rho =
  [ (ext1 ++ ext2, fv1)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho)
  , fv1 `equalLenient` fv2
  ]

evalVar :: Ident -> Env -> Lenient
evalVar i rho =
  case lookupEnv i rho of
    Nothing -> Delay $ \ ext -> case lookupEnv i ext of
                                  Just (Done v) -> v
                                  mlen -> error ("evalVar " ++ show i ++ " " ++ show mlen)
    Just v  -> v

extractValues :: [Res] -> [Value]
extractValues vs = [ apply v ext | (ext, v) <- vs ]
   where
     apply (Done v)  _   = v
     apply (Delay f) ext = f ext

wrapValues :: [Value] -> [Res]
wrapValues vs = [ (empty, Done v) | v <- vs ]

tieKnot :: [Res] -> [Res]
tieKnot = wrapValues . extractValues

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
liftL1 g (Delay f) = Delay (g . f)
liftL1 g (Done v) = Done (g v)

liftL2 :: (Value -> Value -> Value) -> Lenient -> Lenient -> Lenient
liftL2 g (Delay f1) (Delay f2) = Delay (\ ext -> f1 ext `g` f2 ext)
liftL2 g (Delay f1) (Done v2) = Delay (\ ext -> f1 ext `g` v2)
liftL2 g (Done v1) (Delay f2) = Delay (\ ext -> v1 `g` f2 ext)
liftL2 g (Done v1) (Done v2) = Done (v1 `g` v2)


---------------------
--      Tests
---------------------

ev :: Exp -> [Value]
ev e = extractValues (eval e [])

test1 = 1 ||| 2

test2 = (1 ||| 2) + (1 ||| 2)

test3 = ("x" := 1 ||| 2) + "x"

-- should fail, since variables in ||| do not escape
test4 = (("x" := 1) ||| 2) + "x"

test5 = ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

test6 = ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

test7 = ("x" := 1 ||| 2) + ("x" === 2)

test8 = Pair "x" ("x" := 1 ||| 2)

test9 = Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))
