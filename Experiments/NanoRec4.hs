{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
import Data.List
import Data.Maybe
import Data.String
import Debug.Trace
import Ex

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
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(e1){e2}
-}

data Exp = Var Ident
         | Con Integer
         | Alt Exp Exp
         | Equal Exp Exp
         | Set Ident Exp
         | Array [Exp]
         | Sel Exp Int
         | Plus Exp Exp
         | Fail
         | For Exp Exp
         | Error  -- to test strictness
  deriving (Show)

infix 2 :=

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = Plus
  fromInteger = Con

instance IsString Exp where
  fromString = Var

infixl 4 |||
(|||) :: Exp -> Exp -> Exp
(|||) = Alt

infixl 3 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal

pattern (:=) :: Ident -> Exp -> Exp
pattern (:=) x e = Set x e

pattern Fst e = Sel e 0
pattern Snd e = Sel e 1
pattern Pair e1 e2 = Array [e1, e2]

-- Sequencing, evaluate both and return second
infixl 1 `semi`
semi :: Exp -> Exp -> Exp
semi x y = Snd (Pair x y)

-- Sequencing, evaluate both and return first
infix 1 `wher`
wher :: Exp -> Exp -> Exp
wher x y = Fst (Pair x y)

---------------------
--      Types for semantics
---------------------

data Value = VInt Integer | VArray [Lenient]

instance Show Value where
  show (VInt i) = show i
  show (VArray vs) = "(" ++ intercalate "," (map show' vs) ++ ")"
    where show' (Done v) = show v
          show' l = show l

type Env = [(Ident, Lenient)]
type Ext = Env  -- Environment extension

empty :: Ext
empty = []

data Lenient = Delay String (Env -> Lenient)  -- The string is just for debugging
             | Done Value  -- (Done v) is equivalent to (Delay (\_. v))

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
cmp (VInt _) (VArray _) = NotEqu
cmp (VArray _) (VInt _) = NotEqu
cmp (VArray xs) (VArray ys) | length xs /= length ys = NotEqu
                           | otherwise = foldl' iand Equ $ zipWith cmpL xs ys
  where iand Equ Equ = Equ
        iand NotEqu _ = NotEqu
        iand _ NotEqu = NotEqu
        iand _ _ = Dunno

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

eval (Sel e1 i) rho = [ (ext1, liftLL1 ("Sel-"++show i) (vsel i) fv1) | (ext1, fv1) <- eval e1 rho ]

eval (Plus e1 e2) rho =
  [ (ext1 ++ ext2, liftL2 "Plus" vplus fv1 fv2)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 ++ rho) ]

eval (Array es) arho = [ (ext, Done $ VArray fvs) | (ext, fvs) <- evalArray es arho ]
  where evalArray [] rho = [(empty, [])]
        evalArray (e:es) rho =
          [ (ext' ++ ext'', fv : fvs)
          | (ext', fv) <- eval e rho
          , (ext'', fvs) <- evalArray es (ext' ++ rho)
          ]

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

eval (For e1 e2) rho = map mkArr $ sequence
  [ tieKnot (eval e2 (unionEnv ext1' rho))
  | (ext1, _) <- eval e1 rho
  -- ext1 has delay for its own variables
  -- Note the recursice use of ext1', without it we would need several passes.
  , let ext1' = tieKnotExt ext1
  ]
  where mkArr :: [Res] -> Res
        mkArr rs = (empty, Done $ VArray $ map snd rs)

eval Error _ = error "eval: Error"

evalVar :: Ident -> Env -> Lenient
evalVar i rho = case lookupEnv i rho of
--                  Just v  -> v
-- This is a possible fix for test19
                  Just v -> withExtL rho v
                  Nothing -> Delay (dly "Var" [i]) (evalVar i)

tieKnot :: [Res] -> [Res]
tieKnot vs = [ (empty, withExtL (tieKnotExt ext) v)
             | (ext, v) <- vs
             ]

withExtL :: Env -> Lenient -> Lenient
withExtL ext (Delay s f) = f ext
withExtL ext (Done v)    = Done (withExtV ext v)
  where
     withExtV :: Env -> Value -> Value
     withExtV _ v@(VInt _)      = v
     withExtV ext (VArray ls) = VArray (map (withExtL ext) ls)

tieKnotExt :: Ext -> Ext
tieKnotExt ext = rec_ext
  -- Here is where we tie the knot.  Consider an example like
  --   x:=y; y:=z; z:=3; z
  -- we get an 'ext' that binds x and y to Delays, and we
  -- must resolve both at once when we tie the knot
  where rec_ext = mapEnv (withExtL rec_ext) ext
 
---------------------
--      Auxiliary semantic operations
---------------------

extendEnv :: Ident -> Lenient -> Env -> Env
extendEnv x v rho = (x,v) : rho

mapEnv :: (Lenient -> Lenient) -> Env -> Env
mapEnv f env = [ (i, f v) | (i,v) <- env ]

unionEnv :: Ext -> Env -> Env
-- If both envs bind the same variable 'ext1' wins
unionEnv ext1 ext2 = ext1 ++ ext2

lookupEnv :: Ident -> Env -> Maybe Lenient
lookupEnv = lookup

vplus :: Value -> Value -> Value
vplus (VInt i1) (VInt i2) = VInt (i1 + i2)
vplus v1 v2 = error $ "vplus " ++ show (v1, v2)

vsel :: Int -> Value -> Lenient
vsel i (VArray as) | i >= 0 && i < length as = as !! i
                   | otherwise = error $ "vsel: out of bounds " ++ show (as, i)
vsel _ v           = error $ "vsel: not an array " ++ show v

liftLL1 :: String -> (Value -> Lenient) -> Lenient -> Lenient
liftLL1 s g (Delay s' f) = Delay (dly s [s']) (\ext -> liftLL1 s g (f ext))
liftLL1 _ g (Done v)  = g v

liftL2 :: String -> (Value -> Value -> Value) -> Lenient -> Lenient -> Lenient
liftL2 s g (Delay s' f1) (Delay s'' f2) = Delay s (\ ext -> liftL2 (dly s [s',s'']) g (f1 ext) (f2 ext))
liftL2 s g (Delay s' f1) (Done v2)  = Delay s (\ ext -> liftL2 (dly s [s,"_"]) g (f1 ext) (Done v2))
liftL2 s g (Done v1) (Delay s' f2)  = Delay s (\ ext -> liftL2 (dly s ["_",s]) g (Done v1) (f2 ext))
liftL2 _ g (Done v1) (Done v2)   = Done (v1 `g` v2)

-- Combine Delay messages
dly :: String -> [String] -> String
dly s ss = s ++ "(" ++ intercalate "," ss ++ ")"

evalTop :: Exp -> [Res]
evalTop e = tieKnot (eval e [])

ev :: Exp -> [Value]
ev e = map get (evalTop e)
  where
    get (_, Done v)  = v
    get (_, Delay _ f) = case f empty of
                           Done v -> v
                           Delay {} -> error "Top level delay"

---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (show $ ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (show $ ev e)

test1 = ok "test1" [1,2] $
  1 ||| 2

test2 = ok "test2" [2,3,3,4] $
  (1 ||| 2) + (1 ||| 2)

test3 = ok "test3" [2,4] $
  ("x" := 1 ||| 2) + "x"

-- Should fail, since variables in ||| do not escape
-- Fails
test4 = bad "test4" $
  (("x" := 1) ||| 2) + "x"

test5 = ok "test5" [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

test6 = ok "test6" [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

test7 = ok "test7" [4] $
  ("x" := 1 ||| 2) + ("x" === 2)

test8 = ok "test8" [(1,1),(2,2)] $
  Pair "x" ("x" := 1 ||| 2)

test9 = ok "test9" [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))] $
  Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
test10 = ok "test10" [((1,7),1)] $
         Pair ("x" := (Pair 1 7 |||
                       Pair "y" ("y" := 2)))
              (Fst "x" === 1)

-- x's value depends on recursively bound z, so we get stuck.
-- Produces []
-- LA: Is this the intended test?
test11 = ok "test11" ([]::[()]) $
         Pair ("x" := (Pair 1 "z" |||
                        Pair "y" ("y" := 2)))
               (Pair ("x" === 1) ("z" := 3))

test12 = ok "test12" [(1,1)] $
  "t" := Pair 1 (Fst "t")

test13 = ok "test13" [(1,1)] $
  "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test14 = bad "test14" $
  "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Generates an error, as it should
test15 = bad "test15"
  Error

-- Generates an error, as it should
test16 = bad "test16" $
  Error `semi` 1

-- Generates an error, as it should
test17 = bad "test17" $
   (2 # Error) `semi` 1

-- Cascaded forward references
test18 = ok "test18" [3,7,2,2] $
         "x" := ("y" ||| 2)  `semi`
         "y" := (3 ||| "z")  `semi`
         "z" := 7            `semi`
         "x"

-- Test that when evaluating z the x is fully determined.
-- BUG: This test does not pass.
test19 = ok "test19" [5] $
  "x" := "y" `semi` "y" := 5 `semi` "z" := ("x"===5)

-- Check that mutual recursion fails
test20 = bad "test20" $
  "x" := "y" `semi` "y" := "x"

-- Nested delays
test21 = ok "test21" [(1,(2,3))] $
  "x" := (1 # "y") `semi`
  "y" := (2 # "z") `semi`
  "z" := 3 `semi`
  "x"

test22 = ok "test19" [(5,5,5)] $
  For (1|||2|||3) 5

test23 = ok "test20" [(1,2,3)] $
  For ("x" := 1|||2|||3) "x"

test24 = ok "test21" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  For ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test25 = ok "test22" [((1,4),(2,4),(3,4)),
                      ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` For ("x" := 1|||2|||3) ("x" # "y")

test26 = ok "test23" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  For ("y" := 4|||5) $ For ("x" := 1|||2|||3) ("x" # "y")

test27 = ok "test24" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  For ("x" := 1|||2|||3) ("x" ||| 99)

test28 = ok "test25" [(1,2,3)] $
  For ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test29 = ok "test26" [(2,3,4)] $
  For ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)


testAll = mapM_ testEx
  [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,
   test11,test12,test13,test14,test15,test16,test17,test18,
   test19,test20,test21,test22,test23,test24,test25,test26,
   test27,test28,test29
  ]

