{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module NanoRec6(testAll) where
import Data.List
--import Data.Maybe
import Data.String
--import Debug.Trace
import GHC.Stack
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
      |  :e
   s ::= def {x1,...} in e
-}

data Exp = Var Name
         | Con Integer
         | Alt SExp SExp
         | Equal Exp Exp
         | Set Name Exp
         | Array [Exp]   -- (e1, ..., en)  aka  array{e1, ..., en}
         | Sel Exp Int   -- The Int needs to be generalized to Exp
         | Plus Exp Exp
         | Fail
         | For SExp SExp
         | Do SExp
         | Range Exp     -- :e
         | Error  -- to test strictness
  deriving (Show)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression

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

pattern Fst :: Exp -> Exp
pattern Fst e = Sel e 0
pattern Snd :: Exp -> Exp
pattern Snd e = Sel e 1
pattern Pair :: Exp -> Exp -> Exp
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
for e1 e2 = For (addDef e1) (addDef e2)

doo :: Exp -> Exp
doo e = Do (addDef e)

-- Add all variables defined in the current scope.
addDef :: HasCallStack => Exp -> SExp
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
findSet (Range e) = findSet e
findSet Error = []

---------------------
--      Types for semantics
---------------------

data Value  -- A head normal form
  = VInt Integer
  | VArray [Lenient]

data Lenient
  = Done Value  -- A head normal form
                -- (Done v) is equivalent to (Delay (\_. v))
  | Delay String (Ext -> Lenient)  -- The string is just for debugging


newtype Env = Env [(Name, Lenient)]
  deriving (Show)

-- Same as Env, but used for new bindings
newtype Ext = Ext [(Name, Lenient)]
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env []

emptyExt :: Ext
emptyExt = Ext []

equalLenient :: HasCallStack => Lenient -> Lenient -> Bool
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

type Res = (Ext, Lenient)

eval :: HasCallStack => Exp -> Env -> [Res]
-- Invariants:
-- * In a call (eval e rho), the domain of the Ext in the
--   returned Res is precisely the Names bound in 'e'.
-- * Moreover the domain of the returned Ext is disjoint
--   from the domain of the Env passed in.

eval (Var i) rho = [(emptyExt, evalVar i rho)]
eval (Con k) _   = [(emptyExt, Done (VInt k))]
eval Fail    _   = []

eval (Sel e1 i) rho
  = [ (ext1, liftLL1 ("Sel-"++show i) (vsel i) fv1)
    | (ext1, fv1) <- eval e1 rho ]

eval (Plus e1 e2) rho =
  [ (ext1 `appExt` ext2, liftL2 "Plus" vplus fv1 fv2)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 `updExt` rho) ]

eval (Array as) arho =
--  trace ("\neval 1 Array " ++ show (es, rho, abnd) ++ "\n") $
--  trace ("\neval 2 Array " ++ show (evalArray es abnd) ++ "\n") $
  [ (ext, Done $ VArray fvs) | (ext, fvs) <- evalArray as arho ]
  where
    evalArray :: [Exp] -> Env -> [(Ext, [Lenient])]
    evalArray [] _ = [(emptyExt, [])]
    evalArray (e:es) rho =
      [ (ext1 `appExt` ext2, fv : fvs)
      | (ext1, fv)  <- eval e rho
      , (ext2, fvs) <- evalArray es (ext1 `updExt` rho)
      ]

eval (Alt e1 e2) rho =
  evalS e1 rho ++ evalS e2 rho

eval (Set x e) rho =
  [ (aExt x fv1 `appExt` ext1, fv1) | (ext1, fv1) <- eval e rho ]

eval (Equal e1 e2) rho =
  [ (ext1 `appExt` ext2, fv1)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 `updExt` rho)
  , fv1 `equalLenient` fv2
  ]

eval (For (Def xs1 e1) e2) arho =
  checkShadow xs1 arho $
  map mkArr $ sequence
  [ evalS e2 (updExt ext1' rho)
  | (ext1, _) <- eval e1 rho
  -- ext1 has delay for its own variables
  -- Note the recursive use of ext1', without it we would need several passes.
  , let ext1' = tieKnotExt xs1 ext1
  ]
  where mkArr :: [Res] -> Res
        mkArr rs = (emptyExt, Done $ VArray $ map snd rs)
        rho = extend xs1 arho

eval (Do e) rho = evalS e rho

eval (Range e) rho =
  [ (ext, fv)
  | (ext, av) <- eval e rho
  , fv <- unArray (withExtL ext av)
  ]

eval Error _ = expectedError "eval: Error"

evalS :: HasCallStack => SExp -> Env -> [Res]
-- Evaluate a new scope:
--   * check for shadowing
--   * bring the variables into scope
--   * tie the knot
evalS (Def ns e) rho =
  --trace ("evalS " ++ show (ns, e, rho', bnd')) $
  checkShadow ns rho $
  tieKnot ns $ eval e rho'
  where
    rho' = extend ns rho

evalVar :: HasCallStack => Name -> Env -> Lenient
evalVar n rho = lookupEnv n rho

checkShadow :: HasCallStack => [Name] -> Env -> a -> a
checkShadow ns (Env nvs) a | ds@(_:_) <- intersect ns (map fst nvs) = wrong $ "Duplicate defs " ++ show (ds, ns, nvs)
                           | otherwise = a

tieKnot :: HasCallStack => [Name] -> [Res] -> [Res]
--tieKnot _ vs | trace ("\ntieKnot " ++ show vs ++ "\n") False = undefined
tieKnot _ vs
  | err:_ <- filter badRes vs = error $ "tieKnot: badRes, multiple Set " ++ show err
  where
    badRes (Ext ext, _) = nub (map fst ext) /= map fst ext

tieKnot ids vs = [ (emptyExt, withExtL (tieKnotExt ids ext) v)
                 | (ext, v) <- vs
                 ]

withExtL :: Ext -> Lenient -> Lenient
withExtL aext (Delay _ f) = f aext
withExtL aext (Done av)   = Done (withExtV aext av)
  where
     withExtV :: Ext -> Value -> Value
     withExtV _   v@(VInt _)  = v
     withExtV ext (VArray ls) = VArray (map (withExtL ext) ls)

-- The first argument is the set of names for the current scope.
-- The second argument is the bindings we have accumulated
-- during evaluation.  If some defined variables are not assigned then those variables
-- in names will be missing in ext.  Since we need to eliminate all names from the
-- current scope, we need to add bindings for those.
tieKnotExt :: HasCallStack => [Name] -> Ext -> Ext
tieKnotExt ids (Ext new) =
  --trace ("\ntieKnotExt: " ++ show (ids, new) ++ "\n") $
  rec_ext
  -- Here is where we tie the knot.  Consider an example like
  --   x:=y; y:=z; z:=3; z
  -- we get an 'ext' that binds x and y to Delays, and we
  -- must resolve both at once when we tie the knot
  where
    rec_ext = mapExt (withExtL rec_ext) ext
    missing = ids \\ map fst new  -- brought into scope, but not assigned.
    ext = Ext $ [(x, runtimeError $ "Unset Id " ++ show x) | x <- missing] ++ new

---------------------
--      Auxiliary semantic operations
---------------------

-- Environment
aExt :: Name -> Lenient -> Ext
aExt x v = Ext [(x, v)]

lookupEnv :: HasCallStack => Name -> Env -> Lenient
lookupEnv n (Env rho) =
  case lookup n rho of
    Nothing -> scopeError $ "Not in scope " ++ show n
    Just i  -> withExtL (Ext rho) i

lookupExt :: HasCallStack => Name -> Ext -> Lenient
lookupExt n (Ext rho) =
  case lookup n rho of
    Nothing -> Delay "lookupExt" $ lookupExt n
    Just i  -> i

mapExt :: (Lenient -> Lenient) -> Ext -> Ext
mapExt f (Ext ext) = Ext [ (i, f v) | (i, v) <- ext ]

-- Extend the environment and bindings with the given identifiers.
extend :: [Name] -> Env -> Env
extend xs (Env env) = Env $ ext ++ env
  where
    ext = [(x, Delay "extend" $ lookupExt x) | x <- xs ]

appExt :: HasCallStack => Ext -> Ext -> Ext
-- Precondition: the two Ext have disjoint domains
appExt (Ext ext1) (Ext ext2)
  | not $ null $ intersect (map fst ext1) (map fst ext2)
  = error $ "appExt: " ++ show (ext1, ext2)  -- Check precondition
  | otherwise
  = Ext $ ext1 ++ ext2

updExt :: HasCallStack => Ext -> Env -> Env
updExt (Ext ext1) (Env ext2)
  | not $ null $ ks1 \\ ks2 = error $ "updExt: " ++ show (ext1, ext2)
  | otherwise = Env $ ext1 ++ [ (k, mv) | (k, mv) <- ext2, k `notElem` ks1 ]
 where ks1 = map fst ext1
       ks2 = map fst ext2

---------------------
-- Top level
---------------------

-- Top level evaluation
evalTop :: HasCallStack => Exp -> [Res]
evalTop e = evalS (addDef e) emptyEnv

-- Ensure all delays are gone.
ev :: HasCallStack => Exp -> [Value]
ev e = map get (evalTop e)
  where
    get (_, Done v)  = v
    get (_, Delay _ f) = case f emptyExt of
                           Done v -> v
                           Delay {} -> internalError "Top level delay"

---------------------
-- Value operations
---------------------

vplus :: Value -> Value -> Value
vplus (VInt i1) (VInt i2) = VInt (i1 + i2)
vplus v1 v2 = wrong $ "vplus " ++ show (v1, v2)

vsel :: Int -> Value -> Lenient
vsel i (VArray as) | i >= 0 && i < length as = as !! i
                   | otherwise = wrong $ "vsel: out of bounds " ++ show (as, i)
vsel _ v           = wrong $ "vsel: not an array " ++ show v

unArray :: Lenient -> [Lenient]
unArray (Done (VArray vs)) = vs
unArray v = wrong $ "unArray: " ++ show v

-- Lifting
liftLL1 :: String -> (Value -> Lenient) -> Lenient -> Lenient
liftLL1 s g (Delay s' f) = Delay (dly s [s']) (\ext -> liftLL1 s g (f ext))
liftLL1 _ g (Done v)  = g v

liftL2 :: String -> (Value -> Value -> Value) -> Lenient -> Lenient -> Lenient
liftL2 s g (Delay s' f1) (Delay s'' f2) = Delay s (\ ext -> liftL2 (dly s [s',s'']) g (f1 ext) (f2 ext))
liftL2 s g (Delay s' f1) (Done v2)  = Delay s (\ ext -> liftL2 (dly s [s',"_"]) g (f1 ext) (Done v2))
liftL2 s g (Done v1) (Delay s' f2)  = Delay s (\ ext -> liftL2 (dly s ["_",s']) g (Done v1) (f2 ext))
liftL2 _ g (Done v1) (Done v2)   = Done (v1 `g` v2)

-- Combine Delay messages
dly :: String -> [String] -> String
dly s ss = s ++ "(" ++ intercalate "," ss ++ ")"

---------------------
--      Showing things
---------------------

instance Show Value where
  show (VInt i) = show i
  show (VArray vs) = "(" ++ intercalate "," (map show' vs) ++ ")"
    where show' (Done v) = show v
          show' l = show l

instance Show Lenient where
  show (Done v)  = "(Done " ++ show v ++ ")"
  show (Delay s _) = "Delay-" ++ s ++ "!"


---------------------
--      Different kinds of errors
---------------------

-- Error in the implementation of the semantics.
internalError :: HasCallStack => String -> a
internalError s = error $ "internalError: " ++ s

-- Some scope problem
scopeError :: HasCallStack => String -> a
scopeError s = error $ "scopeError: " ++ s

-- Semantic error (should be caught by the verifier)
wrong :: HasCallStack => String -> a
wrong s = error $ "wrong: " ++ s

-- Semantic error, not caught by the verifier
runtimeError :: HasCallStack => String -> a
runtimeError s = error $ "runtimeError: " ++ s

-- Use of Error
expectedError :: HasCallStack => String -> a
expectedError s = error $ "expectedError: " ++ s

-- I'm not sure
whatError :: HasCallStack => String -> a
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

test22 = ok "test22" [(5,5,5)] $
  for (1|||2|||3) 5

test23 = ok "test23" [(1,2,3)] $
  for ("x" := 1|||2|||3) "x"

test24 = ok "test24" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  for ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test25 = ok "test25" [((1,4),(2,4),(3,4)),
                      ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` for ("x" := 1|||2|||3) ("x" # "y")

test26 = ok "test26" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test27 = ok "test27" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test28 = ok "test28" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test29 = ok "test29" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

test30 = bad "test30" $
  "x" := 1 `semi` "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test31 = ok "test31" [(1,2)] $
  "x" := 2 `semi` (doo ("x1" `wher` "x1" := 1) # "x")

test32 = ok "test32" [(1,2,3)] $
  for ("x" := Range (Array [1,2,3])) "x"

test33 = ok "test33" [(102,103,104)] $
  "xs" := for ("x" := 1|||2|||3) ("x" + 1) `semi`
  for ("y" := Range "xs") ("y" + 100)

-- The variable x gets Id 2, and so does y.
-- If eval does not resolve all the variables in the Def
-- then the x will wrongly be resolved as 2.
test34 = bad "test34" $
  "xs" := Do (Def ["x"] (1 # "x")) `semi`
  doo ("y" := 2 `semi` Snd "xs")

test35 = ok "test35" [((1,2),(1,4),(1,5))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 1) "xy"

test36 = ok "test36" ([]::[()]) $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test37 = ok "test37" [((2,3),(2,3))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#3, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

-- The t1 used to be t, but shadowning is not allowed.
test38 = ok "test38" [62,134] $
  "v" := "t" + 1 `semi`
  "x" := doo ( "z" := "v" + "t1" `semi` "t1" := 6 `semi` "z" ) `semi`
  "t" := 55 ||| 127 `semi`
  "x"

testAll :: IO ()
testAll = mapM_ testEx
  [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,
   test11,test12,test13,test14,test15,test16,test17,test18,
   test19,test20,test21,test22,test23,test24,test25,test26,
   test27,test28,test29,test30,test31,test32,test33,test34,
   test35,test36,test37,test38
  ]
