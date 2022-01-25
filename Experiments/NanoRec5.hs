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

--
-- NOTE: All expressions are assumed to be scope correct, i.e.,
-- variables assigned in a scope must appear in a def

type Name = String

{- BNF syntax for the language
   e ::= x
      |  k
      |  (s1 | s2)
      |  (e = k)
      |  x := e
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(s1){e2}
      |  do{s}
   s ::= def {x1,...} in e
-}

data Exp = Var Name
         | Con Integer
         | Alt SExp SExp
         | Equal Exp Exp
         | Set Name Exp
         | Array [Exp]
         | Sel Exp Int
         | Plus Exp Exp
         | Fail
         | For SExp Exp
         | Do SExp
         | Error  -- to test strictness
  deriving (Show)

data SExp = Def [Name] Exp
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
x ||| y = Alt (addDef x) (addDef y)

infixl 3 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal

pattern (:=) :: Name -> Exp -> Exp
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

for :: Exp -> Exp -> Exp
for e1 e2 = For (addDef e1) e2

doo :: Exp -> Exp
doo e = Do (addDef e)

-- Add all 
addDef :: Exp -> SExp
addDef e | xs /= nub xs = scopeError $ "Duplicate := " ++ show (e, xs)
         | otherwise = Def xs e
  where xs = findSet e

findSet :: Exp -> [Name]
findSet (Var _) = []
findSet (Con _) = []
findSet (Alt _ _) = []
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (Array es) = concatMap findSet es
findSet (Sel e _) = findSet e
findSet (Plus e1 e2) = findSet e1 ++ findSet e2
findSet Fail = []
findSet (For _ _) = []
findSet (Do _) = []
findSet Error = []

---------------------
--      Types for semantics
---------------------

data Value = VInt Integer | VArray [Lenient]
  deriving (Show)
{-
instance Show Value where
  show (VInt i) = show i
  show (VArray vs) = "(" ++ intercalate "," (map show' vs) ++ ")"
    where show' (Done v) = show v
          show' l = show l
-}

type Id = Int

type Env = [(Name, Id)]
--type Ext = [(Id, Lenient)]
-- All Id in use should be in the mapping.
-- An unset Id maps to Nothing.
newtype Binds = Binds [(Id, Maybe Lenient)]
  deriving (Show)

emptyEnv :: Env
emptyEnv = []

emptyBinds :: Binds
emptyBinds = Binds []

data Lenient = Delay String (Binds -> Lenient)  -- The string is just for debugging
             | Done Value  -- (Done v) is equivalent to (Delay (\_. v))

type Res = (Binds, Lenient)

instance Show Lenient where
  show (Done v)  = "(Done " ++ show v ++ ")"
  show (Delay s f) = "Delay-" ++ s ++ "!"

equalLenient :: Lenient -> Lenient -> Bool
equalLenient v1 v2 =
  case cmpL v1 v2 of
    Equ -> True
    NotEqu -> False
    Dunno -> whatError $ "equalLenient: not evaluated: " ++ show (v1, v2)

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

eval :: Exp -> Env -> Binds -> [Res]
-- In a call (eval e rho), the domain of the Env in the returned Res
-- is precisely the variables bound in 'e'.

eval (Var i) rho bnd = [(emptyBinds, evalVar i rho bnd)]
eval (Con k) _   _   = [(emptyBinds, Done (VInt k))]
eval Fail    _   _   = []

eval (Sel e1 i) rho bnd = [ (ext1, liftLL1 ("Sel-"++show i) (vsel i) fv1) | (ext1, fv1) <- eval e1 rho bnd ]

eval (Plus e1 e2) rho bnd =
  [ (ext1 `appBinds` ext2, liftL2 "Plus" vplus fv1 fv2)
  | (ext1, fv1) <- eval e1 rho bnd
  , (ext2, fv2) <- eval e2 rho (ext1 `appBinds` bnd) ]

eval (Array es) rho abnd = [ (ext, Done $ VArray fvs) | (ext, fvs) <- evalArray es abnd ]
  where evalArray :: [Exp] -> Binds -> [(Binds, [Lenient])]
        evalArray [] bnd = [(emptyBinds, [])]
        evalArray (e:es) bnd =
          [ (ext' `appBinds` ext'', fv : fvs)
          | (ext', fv) <- eval e rho bnd
          , (ext'', fvs) <- evalArray es (ext' `appBinds` bnd)
          ]

eval (Alt e1 e2) rho bnd =
  evalS e1 rho bnd ++ evalS e2 rho bnd

eval (Set x e) rho bnd =
  [ (aBind (lookupEnv x rho) fv1, fv1) | (ext1, fv1) <- eval e rho bnd ]

eval (Equal e1 e2) rho bnd =
  [ (ext1 `appBinds` ext2, fv1)
  | (ext1, fv1) <- eval e1 rho bnd
  , (ext2, fv2) <- eval e2 rho (ext1 `appBinds` bnd)
  , fv1 `equalLenient` fv2
  ]

{-
eval (For (Def xs1 e1) e2) rho = map mkArr $ sequence
  [ tieKnot (eval e2 (unionEnv ext1' rho))
  | (ext1, _) <- eval e1 rho
  -- ext1 has delay for its own variables
  -- Note the recursice use of ext1', without it we would need several passes.
  , let ext1' = tieKnotExt ext1
  ]
  where mkArr :: [Res] -> Res
        mkArr rs = (empty, Done $ VArray $ map snd rs)
-}

eval (Do e) rho bnd = evalS e rho bnd

eval Error _ _ = expectedError "eval: Error"

evalS :: SExp -> Env -> Binds -> [Res]
evalS (Def ns e) rho bnd = tieKnot $ eval e rho' bnd'
  where (rho', bnd') = extend ns rho bnd

evalVar :: Name -> Env -> Binds -> Lenient
evalVar n rho bnd = evalId n (lookupEnv n rho) bnd

-- NOTE: The Name is used only for debugging.
evalId :: Name -> Id -> Binds -> Lenient
evalId n i bnd =
  case lookupBinds i bnd of
    Just v -> v
    Nothing -> Delay (dly "Var" [n ++ "#" ++ show i]) (evalId n i)

tieKnot :: [Res] -> [Res]
tieKnot vs | trace ("\ntieKnot " ++ show vs ++ "\n") False = undefined
tieKnot vs | bad:_ <- filter badRes vs = error $ "tieKnot: badRes " ++ show bad
  where badRes (Binds ext, _) = nub (map fst ext) /= map fst ext
tieKnot vs = [ (emptyBinds, withBindsL (tieKnotBinds ext) v)
             | (ext, v) <- vs
             ]

withBindsL :: Binds -> Lenient -> Lenient
withBindsL ext (Delay s f) = f ext
withBindsL ext (Done v)    = Done (withBindsV ext v)
  where
     withBindsV :: Binds -> Value -> Value
     withBindsV _ v@(VInt _)      = v
     withBindsV ext (VArray ls) = VArray (map (withBindsL ext) ls)

tieKnotBinds :: Binds -> Binds
tieKnotBinds ext =
  trace ("\ntieKnotBinds: " ++ show ext ++ "\n") $
  rec_ext
  -- Here is where we tie the knot.  Consider an example like
  --   x:=y; y:=z; z:=3; z
  -- we get an 'ext' that binds x and y to Delays, and we
  -- must resolve both at once when we tie the knot
  where rec_ext = mapBinds (withBindsL rec_ext) ext
 
---------------------
--      Auxiliary semantic operations
---------------------

-- Environment
extendEnv :: [(Name, Id)] -> Env -> Env
extendEnv xvs rho = xvs ++ rho

--unionEnv :: Ext -> Env -> Env
-- If both envs bind the same variable 'ext1' wins
--unionEnv ext1 ext2 = ext1 ++ ext2

lookupEnv :: Name -> Env -> Id
lookupEnv n rho =
  case lookup n rho of
    Nothing -> scopeError $ "Not in scope " ++ show n
    Just i  -> i

-- Bindings
-- Set a binding.
-- This implementation is overly cautious. :)
aBind :: Id -> Lenient -> Binds
aBind i v = Binds [(i, Just v)]

-- Extend with n unbound Ids.
-- The new Ids will be numbered higher than any existing Ids.
extendBinds :: Int -> Binds -> ([Id], Binds)
extendBinds n (Binds bnd) = (is, Binds $ zip is (repeat Nothing) ++ bnd)
  where is = take n [m+1..]
        m = maximum (0 : map fst bnd)

mapBinds :: (Lenient -> Lenient) -> Binds -> Binds
mapBinds f (Binds ext) = Binds [ (i, fmap f v) | (i, v) <- ext ]

lookupBinds :: Id -> Binds -> Maybe Lenient
lookupBinds i (Binds bnd) =
  case lookup i bnd of
    Nothing -> internalError $ "Id not found " ++ show i
    Just r -> r

-- Extend the environment and bindings with the given identifiers.
extend :: [Name] -> Env -> Binds -> (Env, Binds)
extend xs env bnd =
  trace ("\nextend " ++ show (zip xs is) ++ "\n") $
  (env', bnd')
  where (is, bnd') = extendBinds (length xs) bnd
        env' = extendEnv (zip xs is) env

appBinds :: Binds -> Binds -> Binds
appBinds (Binds ext1) (Binds ext2)
  | not $ null $ intersect (map fst ext1) (map fst ext2) = error $ "appBinds: " ++ show (ext1, ext2)
  | otherwise = Binds $ ext1 ++ ext2

---------------------
-- Value operations
vplus :: Value -> Value -> Value
vplus (VInt i1) (VInt i2) = VInt (i1 + i2)
vplus v1 v2 = wrong $ "vplus " ++ show (v1, v2)

vsel :: Int -> Value -> Lenient
vsel i (VArray as) | i >= 0 && i < length as = as !! i
                   | otherwise = wrong $ "vsel: out of bounds " ++ show (as, i)
vsel _ v           = wrong $ "vsel: not an array " ++ show v

-- Lifting
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

-- Top level evaluation
evalTop :: Exp -> [Res]
evalTop e = evalS (addDef e) emptyEnv emptyBinds

-- Ensure all delays are gone.
ev :: Exp -> [Value]
ev e = map get (evalTop e)
  where
    get (_, Done v)  = v
    get (_, Delay _ f) = case f emptyBinds of
                           Done v -> v
                           Delay {} -> whatError "Top level delay"

---------------------
--      Different kinds of errors
---------------------

-- Error in the implementation of the semantics.
internalError :: String -> a
internalError s = error $ "internalError: " ++ s

-- Some scope problem
scopeError :: String -> a
scopeError s = error $ "scopeError: " ++ s

-- Semantic error (should be caught by the verifier)
wrong :: String -> a
wrong s = error $ "wrong: " ++ s

-- Use of Error
expectedError :: String -> a
expectedError s = error $ "expectedError: " ++ s

-- I'm not sure
whatError :: String -> a
whatError s = error $ "whatError: " ++ s


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

ttt = 
  ("x" := 1 ) + ("y" := 3 ||| 4) # "y" -- # ("x" # "y")

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

test19 = ok "test19" [(5,5,5)] $
  for (1|||2|||3) 5

test20 = ok "test20" [(1,2,3)] $
  for ("x" := 1|||2|||3) "x"

test21 = ok "test21" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  for ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test22 = ok "test22" [((1,4),(2,4),(3,4)),
                      ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` for ("x" := 1|||2|||3) ("x" # "y")

test23 = ok "test23" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test24 = ok "test24" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test25 = ok "test25" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test26 = ok "test26" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

test27 = ok "test27" [(1,(2,3))] $
  "x" := (1 # "y") `semi` "y" := (2 # "z") `semi` "z" := 3 `semi` "x"

test28 = bad "test28" $
  "x" := 1 `semi` "x" := 2

-- This test fails because we don't handle scope properly yet
test29 = ok "test29" [(1,2)] $
  "x" := 2 `semi` (doo ("x" `wher` "x" := 1) # "x")

testAll = mapM_ testEx
  [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,
   test11,test12,test13,test14,test15,test16,test17,test18,
   test19,test20,test21,test22,test23,test24,test25,test26,test27,
   test28 --, test29
  ]

