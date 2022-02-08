{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module NanoRecFun(testAll) where
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
      |  (e = e)
      |  x := e
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(s1){e2}
      |  do{s}
      |  :e
      |  lam{x}in e
      |  e(e)
      |  e[e]
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
         | Lam Name SExp
         | AppS Exp Exp  -- e(e)  exactly one result
         | AppI Exp Exp  -- e[e]  can iterate
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

lam :: Name -> Exp -> Exp
lam n e = Lam n (addDef e)

-- Add all variables defined in the current scope.
addDef :: HasCallStack => Exp -> SExp
addDef e | xs /= nub xs = scopeError $ "Duplicate := " ++ show (e, xs)
         | otherwise = Def xs e
  where xs = findSet e

findSet :: Exp -> [Name]
findSet Var {}   = []
findSet Con {}   = []
findSet Alt {}   = []
findSet Fail     = []
findSet For {}   = []
findSet Do {}    = []
findSet Error    = []
findSet Lam {}   = []
findSet (AppS  e1 e2) = findSet e1 ++ findSet e2
findSet (AppI  e1 e2) = findSet e1 ++ findSet e2
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (Array es) = concatMap findSet es
findSet (Sel e _) = findSet e
findSet (Plus e1 e2) = findSet e1 ++ findSet e2
findSet (Range e) = findSet e

---------------------
--      Types for semantics
---------------------

data Value  -- A head normal form
  = VInt Integer
  | VArray [Lenient]
  | VFun String -- Debugging only
         Env    -- Captured when lambda is evaluated
         (Env -> Lenient -> [Lenient])

data Lenient
  = Done Value  -- A head normal form
                -- (Done v) is equivalent to (Delay (\_. v))
  | Delay String (Ext -> Lenient)  -- The string is just for debugging


-- There should be no duplicates in the domain of the Env;
-- that is, we do not allow shadowing.
newtype Env = Env [(Name, Lenient)]
  deriving (Show)

-- Same as Env, but used for new bindings
-- There should be no duplicates in the domain of Ext
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
cmp (VFun {}) (VFun {}) = Dunno   -- Not sure about this
cmp (VInt i1) (VInt i2) = if i1 == i2 then Equ else NotEqu
cmp (VArray xs) (VArray ys) | length xs /= length ys = NotEqu
                            | otherwise = foldl' iand Equ $ zipWith cmpL xs ys
  where
    iand Equ Equ = Equ
    iand NotEqu _ = NotEqu
    iand _ NotEqu = NotEqu
    iand _ _ = Dunno
cmp _ _ = NotEqu -- Different constructors

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

eval Error       _   = expectedError "eval: Error"
eval Fail        _   = []
eval (Con k)     _   = [(emptyExt, Done (VInt k))]
eval (Var i)     rho = [(emptyExt, evalVar i rho)]
eval (Alt e1 e2) rho = evalS2 e1 rho ++ evalS2 e2 rho
eval (Do e)      rho = evalS2 e rho

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
  [ evalS2 e2 (updExt ext1' rho)
  | (ext1, _) <- eval e1 rho
  -- ext1 has delay for its own variables
  -- Note the recursive use of ext1', without it we would need several passes.
  , let ext1' = tieKnotExt xs1 ext1
  ]
  where mkArr :: [Res] -> Res
        mkArr rs = (emptyExt, Done $ VArray $ map snd rs)
        rho = extend xs1 arho

eval (Range e) rho =
  [ (ext, fv)
  | (ext, av) <- eval e rho
  , fv <- unArray (withExtL ext av)
  ]

eval (Lam x e) rho = [(emptyExt, Done (VFun str rho fn))]
  where
    fn :: Env -> Lenient -> [Lenient]
    fn rho_fn v = evalS e (bindEnv x v rho_fn)
    str = "{" ++ show (Lam x e) ++ "}"

eval (AppS e1 e2) rho =
  [ (ext1 `appExt` ext2, liftLL1 "AppS" (vapp fv2) fv1)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 `updExt` rho) ]

eval (AppI e1 e2) rho =
  [ (ext1 `appExt` ext2, fv3)
  | (ext1, fv1) <- eval e1 rho
  , (ext2, fv2) <- eval e2 (ext1 `updExt` rho)
  , fv3 <- case fv1 of
             Done (VFun _ rho_fn fn) -> fn rho_fn fv2
             Done _   -> wrong ("eval:AppI:non-function) " ++ show fv1)
             Delay {} -> wrong ("eval:AppI:not evaluated " ++ show fv1)
  ]

evalS2 :: HasCallStack => SExp -> Env -> [Res]
-- Add an empty extension
evalS2 e rho = [(emptyExt, v) | v <- evalS e rho]

evalS :: HasCallStack => SExp -> Env -> [Lenient]
-- Evaluate a new scope:
--   * check for shadowing
--   * bring the variables into scope
--   * tie the knot
evalS (Def ns e) rho =
  --trace ("evalS " ++ show (ns, e, rho', bnd')) $
  checkShadow ns rho $
  tieKnot ns         $
  eval e rho'
  where
    rho' = extend ns rho

evalVar :: HasCallStack => Name -> Env -> Lenient
evalVar n rho = lookupEnv n rho

tieKnot :: HasCallStack => [Name] -> [Res] -> [Lenient]
--tieKnot _ vs | trace ("\ntieKnot " ++ show vs ++ "\n") False = undefined
tieKnot ids vs
  = checkResultInvariants vs $
    [ withExtL (tieKnotExt ids ext) v
    | (ext, v) <- vs ]

withExtL :: Ext -> Lenient -> Lenient
withExtL aext (Delay _ f) = f aext
withExtL aext (Done av)   = Done (withExtV aext av)
  where
     withExtV :: Ext -> Value -> Value
     withExtV _   v@(VInt _)      = v
     withExtV ext (VArray ls)     = VArray (map (withExtL ext) ls)
     withExtV ext (VFun s rho fn) = VFun s (ext `updExt` rho) fn

-- The first argument is the set of names for the current scope.
-- The second argument is the bindings we have accumulated
-- during evaluation.  If some defined variables are not assigned then those variables
-- in names will be missing in ext.  Since we need to eliminate all names from the
-- current scope, we need to add bindings for those.
tieKnotExt :: HasCallStack => [Name] -> Ext -> Ext
tieKnotExt ids (Ext new) = rec_ext
  --trace ("\ntieKnotExt: " ++ show (ids, new) ++ "\n") $
  -- Here is where we tie the knot.  Consider an example like
  --   x:=y; y:=z; z:=3; z
  -- we get an 'ext' that binds x and y to Delays, and we
  -- must resolve both at once when we tie the knot
  where
    rec_ext = mapExt (withExtL rec_ext) full_new

    full_new      = Ext $ (missing_binds ++ new)
    missing_binds = [(x, runtimeError $ "Unset Id " ++ show x)
                    | x <- ids \\ map fst new ]
       -- missing_binds are the ones brought into scope by Def,
       -- but never actually given a value.  They may be entirely
       -- unsued, but if we do happen to use one, we should blow up.
       --   e.g. Def{x} in x  should blow up

checkShadow :: HasCallStack => [Name] -> Env -> a -> a
-- Checks that the newly-bound names don't shadow any existing names
checkShadow ns (Env nvs) a
  | ds@(_:_) <- intersect ns (map fst nvs)
  = wrong $ "Duplicate defs " ++ show (ds, ns, nvs)
  | otherwise
  = a

checkResultInvariants :: [Res] -> a -> a
checkResultInvariants vs thing_inside
  | err:_ <- filter badRes vs
  = wrong $ "checkResultInvariants: multiple Set " ++ show err
  | otherwise
  = thing_inside
  where
    badRes (Ext ext, _) = nub bound_names /= bound_names
       where
         bound_names = map fst ext

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

bindEnv :: Name -> Lenient -> Env -> Env
-- Extend the environment with a single binding
bindEnv x v (Env env) = Env ((x,v):env)

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
-- ext1 overrides ext2
updExt (Ext ext1) (Env ext2)
  | not $ null $ ks1 \\ ks2 = error $ "updExt: " ++ show (ext1, ext2)
  | otherwise = Env $ ext1 ++ [ (k, mv) | (k, mv) <- ext2, k `notElem` ks1 ]
 where ks1 = map fst ext1
       ks2 = map fst ext2

---------------------
-- Top level
---------------------

-- Top level evaluation
evalTop :: HasCallStack => Exp -> [Lenient]
evalTop e = evalS (addDef e) emptyEnv

-- Ensure all delays are gone.
ev :: HasCallStack => Exp -> [Value]
ev e = map get (evalTop e)
  where
    get (Done v)    = v
    get (Delay _ f) = case f emptyExt of
                        Done v -> v
                        Delay {} -> internalError "Top level delay"

---------------------
-- Value operations
---------------------

vplus :: Value -> Value -> Value
vplus (VInt i1) (VInt i2) = VInt (i1 + i2)
vplus v1 v2 = wrong $ "vplus " ++ show (v1, v2)

vsel :: Int -> Value -> Lenient
--vsel i x | trace ("+++vsel: " ++ show (i, x) ++ "***") False = undefined
vsel i (VArray as) | i >= 0 && i < length as = as !! i
                   | otherwise = wrong $ "vsel: out of bounds " ++ show (as, i)
vsel _ v           = wrong $ "vsel: not an array " ++ show v

vapp :: Lenient -> Value -> Lenient
vapp fv2 (VFun _ rho_fn fn) =
  case fn rho_fn fv2 of
    [fv] -> fv
    _ -> wrong "vapp:AppI: not a singleton"
vapp _ fv1 = wrong $ "vapp:AppI:non-function) " ++ show fv1

unArray :: Lenient -> [Lenient]
unArray (Done (VArray vs)) = vs
unArray v = wrong $ "unArray: " ++ show v

-- Lifting
liftLL1 :: String -> (Value -> Lenient) -> Lenient -> Lenient
liftLL1 s g (Delay s' f) = Delay (dly s [s']) (\ext -> liftLL1 s g (f ext))
liftLL1 _ g (Done v)  = g v

liftL2 :: String -> (Value -> Value -> Value)
       -> Lenient -> Lenient -> Lenient
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
  show (VFun s env _fn) = "VFun{" ++ s ++ show env ++ "}"
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

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (show $ ev e)

---------------------
-- Simple, single valued tests.
---------------------
test101 = ok "test101" [5] $
  5

test102 = ok "test102" [42] $
  5 + 37

test103 = ok "test103" [(5,37)] $
  5 # 37

test104 = ok "test104" [(1,2,3,4)] $
  Array [1,2,3,4]

---------------------
-- Variable scopes
---------------------
test201 = ok "test201" [(5,5)] $
  ("x" := 5) # "x"

test202 = ok "test202" [(5,5)] $
  "x" # ("x" := 5)

test203 = ok "test203" [(7,6)] $
  "x"+1 # ("x" := 6)

test204 = ok "test204" [(7,6,6,5)] $
  Array ["x"+1, "x" := "y", "y" := "z"+1, "z" := 5]

test205 = bad "test205" $
  ("x" := 1) # ("x" := 2)

test206 = bad "test206" $
  "x"

test207 = ok "test207" [(3,4)] $
  3 # doo ("x":= 4)

test208 = bad "test208" $
  "x" # doo ("x":= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  "x" := "y" `semi` "y" := "x"

test210 = ok "test210" [(1,(2,3))] $
  "x" := (1 # "y") `semi`
  "y" := (2 # "z") `semi`
  "z" := 3 `semi`
  "x"

test211 = bad "test211" $
  "x" := 1 `semi` "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" [(1,2)] $
  "x" := 2 `semi` (doo ("x1" `wher` "x1" := 1) # "x")

---------------------
-- 0/1 results
---------------------

test301 = ok "test301" [(3,3)] $
  ("x" := 3) # ("x" === 3)

test302 = ok "test302" [3] $
  ("x" := 1+"y") `semi` "y" := 2 `semi` ("x" === 3)

test303 = bug "test303" [(3,3)] $
  ("x" === 3) # ("x" := 3)

test304 = ok "test304" [20] $
  ("a" := Array [10,20,30]) `semi` Sel "a" 1

test305 = ok "test305" [20] $
  Sel "a" 1 `wher` ("a" := Array [10,20,30])

test306 = bug "test306" ([]::[()]) $
  ("a" := Array [10,20,30]) `semi` Sel "a" 3

test307 = ok "test307" [(1,1)] $
  "t" := Pair 1 (Fst "t")

-- Test that when evaluating z the x is fully determined.
test308 = ok "test308" [5] $
  "x" := "y" `semi` "y" := 5 `semi` "z" := ("x"===5)

---------------------
-- Multi-valued
---------------------

test401 = ok "test401" [1,2] $
  1 ||| 2

test402 = ok "test402" [2,3,3,4] $
  (1 ||| 2) + (1 ||| 2)

test403 = ok "test403" [2,4] $
  ("x" := 1 ||| 2) + "x"

-- Should fail, since variables in ||| do not escape
test404 = bad "test404" $
  (("x" := 1) ||| 2) + "x"

test405 = ok "test405" [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

test406 = ok "test406" [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

test407 = ok "test407" [4] $
  ("x" := 1 ||| 2) + ("x" === 2)

test408 = ok "test408" [(1,1),(2,2)] $
  Pair "x" ("x" := 1 ||| 2)

test409 = ok "test409" [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))] $
  Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
test410 = ok "test410" [((1,7),1)] $
         Pair ("x" := (Pair 1 7 |||
                       Pair "y" ("y" := 2)))
              (Fst "x" === 1)

test411 = ok "test411" [(1,1)] $
  "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test412 = bug "test412" [(1,1)] $
  "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Cascaded forward references
test413 = ok "test413" [3,7,2,2] $
         "x" := ("y" ||| 2)  `semi`
         "y" := (3 ||| "z")  `semi`
         "z" := 7            `semi`
         "x"

---------------------
-- Error/strictness
---------------------

-- Generates an error, as it should
test501 = bad "test501"
  Error

-- Generates an error, as it should
test502 = bad "test502" $
  Error `semi` 1

-- Generates an error, as it should
test503 = bad "test504" $
   (2 # Error) `semi` 1

---------------------
-- for
---------------------

test601 = ok "test601" [(5,5,5)] $
  for (1|||2|||3) 5

test602 = ok "test602" [(1,2,3)] $
  for ("x" := 1|||2|||3) "x"

test603 = ok "test603" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  for ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test604 = ok "test604" [((1,4),(2,4),(3,4)),
                      ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` for ("x" := 1|||2|||3) ("x" # "y")

test605 = ok "test605" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test606 = ok "test606" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test607 = ok "test607" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test608 = ok "test608" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

---------------------
-- Functions
---------------------

test701 = ok "test701" [5] $
  "f" := lam "v" ("v" + 1) `semi`
  AppS "f" 4

test702 = ok "test702" [11] $
  "w" := 7 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  AppS "f" 4

test703 = ok "test703" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  AppS "f" 4

test704 = ok "test704" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

-- f is called before it is defined
test705 = ok "test705" [11] $
  "y" := AppS "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test706 = ok "test706" [11] $
  "f" := doo ("w" := 7 `semi` lam "v" ("w" + "v")) `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

test707 = bad "test707" $
  "y" := AppI "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test708 = ok "test708" [10,11] $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppI "f" 10

test709 = bad "test709" $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppS "f" 10

---------------------
-- Not yet sorted
---------------------

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

-- This test doesn't work, but it should.
test46 = bug "test46" [(2,3)] $
  "a" := for ("x" := Range "xs") ("x" + 1) `semi`
  "xs" := (1#2) `semi`
  "a"

testAll :: IO ()
testAll = mapM_ testEx
  [test101,test102,test103,test104,
   test201,test202,test203,test204,test205,test206,test207,test208,test209,test210,test211,test212,
   test301,test302,test303,test304,test305,test306,test307,test308,
   test401,test402,test403,test404,test405,test406,test407,test408,test409,test410,test411,test412,test413,
   test501,test502,test503,
   test601,test602,test603,test604,test605,test606,test607,test608,
   test701,test702,test703,test704,test705,test706,test707,test708,test709,

   test32,test33,test34,test35,test36,test37,test38,test46
  ]
