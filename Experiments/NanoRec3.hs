{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
import Data.List
import Data.Maybe
import Data.String
import Debug.Trace

---------------------
--      Expressions
---------------------

type Ident = String

{- BNF syntax for the language
   e ::= x
      |  k
      |  (e1 | e2)
      |  (e = k)
      |  x := e
      |  (e1,e2)
      |  fst(e)
      |  snd(e)
      |  e1 + e2
      |  :false
-}

data Exp = Var Ident
         | Con Integer
         | Alt Exp Exp
         | Equal Exp Exp
         | Set Ident Exp
         | Pair Exp Exp
         | Fst Exp
         | Snd Exp
         | Plus Exp Exp
         | Fail
         | Error  -- to test strictness
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

data Value = VInt Integer | VPair Lenient Lenient

instance Show Value where
  show (VInt i) = show i
  show (VPair v1 v2) = "(" ++ show' v1 ++ "," ++ show' v2 ++ ")"
    where show' (Done v) = show v
          show' l = show l

type Env = [(Ident, Lenient)]
type Ext = Env  -- Environment extension

empty :: Ext
empty = []

data Lenient = Delay String (Env -> Lenient)  -- The string is just for debugging
             | Done Value  -- (Done v) is equivalent to (Delay (\_. v))
-- This version uses strict left-to-right evaluation

type Res = (Env, Lenient)

instance Show Lenient where
  show (Done v)  = "(Done " ++ show v ++ ")"
  show (Delay s f) = "Delay-" ++ s ++ "!"

equalLenient :: Lenient -> Lenient -> Bool
equalLenient v1 v2 =
  case cmpL v1 v2 of
    Equ -> True
    NotEqu -> False
    Dunno -> error $ "equalLenient: not evaluated: " ++ show (v1, v2)

data IsEqual = Equ | NotEqu | Dunno
  deriving (Show)

cmp :: Value -> Value -> IsEqual
cmp (VInt i1) (VInt i2) = if i1 == i2 then Equ else NotEqu
cmp (VInt _) (VPair _ _) = NotEqu
cmp (VPair _ _) (VInt _) = NotEqu
cmp (VPair x1 x2) (VPair y1 y2) =
  case (cmpL x1 y1, cmpL x2 y2) of
    (Equ, Equ) -> Equ
    (NotEqu, _) -> NotEqu
    (_, NotEqu) -> NotEqu
    _ -> Dunno

cmpL :: Lenient -> Lenient -> IsEqual
cmpL (Done v1) (Done v2) = cmp v1 v2
cmpL _ _ = Dunno

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

eval (Fst e1) rho = [ (ext1, liftLL1 "Fst" vfst fv1) | (ext1, fv1) <- eval e1 rho ]
eval (Snd e1) rho = [ (ext1, liftLL1 "Snd" vsnd fv1) | (ext1, fv1) <- eval e1 rho ]

eval (Plus e1 e2) rho =
  [ (ext1 ++ ext2, liftL2 "Plus" vplus fv1 fv2)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho) ]

eval (Pair e1 e2) rho =
  [ (ext1 ++ ext2, Done $ VPair fv1 fv2)
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

eval Error _ = error "eval: Error"

evalVar :: Ident -> Env -> Lenient
evalVar i rho = case lookupEnv i rho of
                  Just v  -> v
                  Nothing -> Delay (dly "Var" [i]) (evalVar i)

tieKnot :: [Res] -> [Res]
--tieKnot vs | trace ("tieKnot: " ++ show vs) False = undefined
tieKnot vs = [ (empty, apply v ext) | (ext, v) <- vs ]
   where
     apply :: Lenient -> Env -> Lenient
     apply (Done v)    ext = Done (applyV v ext)
     apply (Delay s f) ext = f ext

     applyV :: Value -> Env -> Value
     applyV v@(VInt _)    _   = v
     applyV (VPair l1 l2) ext = VPair (apply l1 ext) (apply l2 ext)

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

vfst :: Value -> Lenient
vfst (VPair a _) = a
vfst v           = error $ "vfst " ++ show v

vsnd :: Value -> Lenient
vsnd (VPair _ v) = v
vsnd v           = error $ "vsnd " ++ show v

liftLL1 :: String -> (Value -> Lenient) -> Lenient -> Lenient
liftLL1 s g (Delay s' f) = Delay (dly s [s']) (\ext -> liftLL1 s g (f ext))
liftLL1 _ g (Done v)  = g v

--liftL1 :: (Value -> Value) -> Lenient -> Lenient
--liftL1 g (Delay f) = Delay (\ext -> liftL1 g (f ext))
--liftL1 g (Done v)  = Done (g v)

liftL2 :: String -> (Value -> Value -> Value) -> Lenient -> Lenient -> Lenient
liftL2 s g (Delay s' f1) (Delay s'' f2) = Delay s (\ ext -> liftL2 (dly s [s',s'']) g (f1 ext) (f2 ext))
liftL2 s g (Delay s' f1) (Done v2)  = Delay s (\ ext -> liftL2 (dly s [s,"_"]) g (f1 ext) (Done v2))
liftL2 s g (Done v1) (Delay s' f2)  = Delay s (\ ext -> liftL2 (dly s ["_",s]) g (Done v1) (f2 ext))
liftL2 _ g (Done v1) (Done v2)   = Done (v1 `g` v2)

-- Combine Delay messages
dly :: String -> [String] -> String
dly s ss = s ++ "(" ++ intercalate "," ss ++ ")"

---------------------
--      Tests
---------------------

evalTop :: Exp -> [Res]
evalTop e = tieKnot (eval e [])

ev :: Exp -> [Value]
ev e = map get (evalTop e)
  where
    get (_, Done v)  = v
    get (_, Delay _ f) = case f empty of
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

-- x's value depends on recursively bound z, so we get stuck.
-- Produces []
-- LA: Is this the intended test.
test11 = Pair ("x" := (Pair 1 "z" |||
                        Pair "y" ("y" := 2)))
               (Pair ("x" === 1) ("z" := 3))

-- Works [(1,1)]
test12 = "t" := Pair 1 (Fst "t")

-- Works [(1,1)]
test13 = "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test14 = "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Generates an error, as it should
test15 = Error

-- Generates an error, as it should
test16 = Error `semi` 1

-- Generates an error, as it should
test17 = (2 # Error) `semi` 1
