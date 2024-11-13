{-# OPTIONS_GHC -Wall #-}
module Main where
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
--import Debug.Trace

--------------------
---- Abstract syntax

type Ident = String

data Exp
  = Var Ident | Int Integer | Prim Op | App Exp Exp | Equ Exp Exp
  | Seq Exp Exp | Def Ident Exp | Colon Exp | Fail | Tup [Exp]
  | If Exp Exp Exp | Fun OC Exp Exp
  deriving (Eq, Ord, Show)

data Op = Oint | Ogt | Oadd
  deriving (Eq, Ord, Show)

data OC = Open | Closed
  deriving (Eq, Ord, Show)


--------------------
---- Values

data Val = VInt Integer | VTup [Val] | VFcn (Fcn Val Val)
  deriving (Eq, Ord)

data RVal = RVal Val | Wrong String

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VTup vs) = showsPrec p vs
  showsPrec p (VFcn f) = showsPrec p f

instance Show RVal where
  showsPrec p (RVal v) = showsPrec p v
  showsPrec _ (Wrong s) = showString $ "Wrong(" ++ s ++ ")"

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

--------------------
---- Functions as tables
-- All functions have a unique name

data Fcn a b = Fcn String (M.Map a b)    -- mapping from a to b

mkFcn :: (Ord a) => String -> [(a, b)] -> Fcn a b
mkFcn s xys = Fcn s (M.fromList xys)

instance Eq (Fcn a b) where
  Fcn f _ == Fcn f' _  =  f == f'

instance Ord (Fcn a b) where
  Fcn f _ `compare` Fcn f' _  =  f `compare` f'

instance Show (Fcn a b) where
  show (Fcn s _) = s

-- Domain test
inDom :: Ord a => a -> Fcn a b -> Bool
inDom x (Fcn _ xys) = M.member x xys

-- Application when the argument is in the domain
ap :: (Show a, Ord a) => Fcn a b -> a -> b
ap (Fcn f xys) x =
  fromMaybe (error $ "ap: outside domain " ++ f ++ " " ++ show x) $
  M.lookup x xys

--------------------
---- Sets

type Set a = S.Set a

unSet :: Set a -> [a]
unSet = S.toList

mkSet :: (Ord a) => [a] -> Set a
mkSet = S.fromList

sUnion :: (Ord a) => [Set a] -> Set a
sUnion = S.unions

isect :: Ord a => Set a -> Set a -> Set a
isect = S.intersection

sing :: a -> Set a
sing = S.singleton

unSing :: Set a -> a
unSing s =
  case unSet s of
    [x] -> x
    _   -> error "unSing"

empty :: Set a
empty = S.empty

isEmpty :: Set a -> Bool
isEmpty = S.null

sIn :: Ord a => a -> Set a -> Bool
sIn = S.member

-- Check if a predicate holds for all values in the set
forAll :: Set a -> (a -> Bool) -> Bool
forAll xs p = all p (unSet xs)

forAllL :: [a] -> (a -> Bool) -> Bool
forAllL xs p = all p xs

existsL :: [a] -> (a -> Bool) -> Bool
existsL xs p = any p xs

--------------------
---- Environment

type Env = M.Map Ident Val

lookupEnv :: Ident -> Env -> WS
lookupEnv x rho = sing $ fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $ M.lookup x rho

-- Initial environment
rho0 :: Env
rho0 = M.fromList $
  [ (n, unSing (dO o)) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", VFcn fsucc), ("pred", VFcn fpred) ] ++
  [ ("false", VTup []) ]

--------------------
---- "Universal" set of values
-- This is a carefully selected set of values to make
-- the examples work.

maxVInt :: Integer
maxVInt = 4

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allWs :: WS
allWs = S.fromList $
  nonFcn ++
  [ unSing (dO o) | o <- [Oint, Ogt, Oadd] ] ++
  map VFcn [ id0, id1, id01, f01, const0, const1, const2, const3, fsucc, fsucc2, fpred, comp, ho1, ho2, ho3 ]
  where
    nonFcn =
      allInts ++
      [VTup [x, y] | x <- allInts, y <- allInts]
    id0 = mkFcn "id0" [(VInt 0, VInt 0)]
    id1 = mkFcn "id1" [(VInt 1, VInt 1)]
    id01 = mkFcn "id01" [(VInt 0, VInt 0), (VInt 1, VInt 1)]
    f01 = mkFcn "f01" [(VInt 0, VInt 0), (VInt 1, VInt 2)]
    const0 = mkFcn "const0" [(x, VInt 0) | x <- allInts]
    const1 = mkFcn "const1" [(x, VInt 1) | x <- allInts]
    const2 = mkFcn "const2" [(x, VInt 2) | x <- allInts]
    const3 = mkFcn "const3" [(x, VInt 3) | x <- allInts]
    comp = mkFcn "comparable" [(w, w) | w <- nonFcn ]
    -- The function that accepts f:int->int as an argument and returns f[1]
    ho1 = mkFcn "ho1" [(VFcn fsucc, VInt 2), (VFcn fpred, VInt 0), (VFcn fint, VInt 1),
                       (VFcn fsucc2, VInt 3), (VFcn comp, VInt 1),
                       (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                      ]
    ho2 = mkFcn "ho2" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                       (VFcn fsucc2, VInt 0), (VFcn comp, VInt 2),
                       (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                      ]
    ho3 = mkFcn "ho3" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                       (VFcn fsucc2, VInt 0), (VFcn comp, VInt 2),
                       (VFcn const0, VInt 1), (VFcn const1, VInt 2), (VFcn const2, VInt 3), (VFcn const3, VInt 0)
                      ]

fint :: Fcn Val Val
fint = mkFcn "int" [(x, x) | x <- allInts ]

fsucc :: Fcn Val Val
fsucc = mkFcn "succ" [(x, vadd x (VInt 1)) | x <- allInts ]

fsucc2 :: Fcn Val Val
fsucc2 = mkFcn "succ2" [(x, vadd x (VInt 2)) | x <- allInts ]

fpred :: Fcn Val Val
fpred = mkFcn "pred" [(x, vadd x (VInt 3)) | x <- allInts ]

--------------------
---- Aux

type W = Val
type WS = Set W

-- Given an initial environment, rho, and some identifiers,
-- generate all environments where rho has been extended with
-- the given identifiers bound to all possible value.
genRhos :: Env -> [Ident] -> [Env]
genRhos rho xs = 
  let exts = sequence $ map (\ x -> map (x,) (unSet allWs)) xs
  in  map (foldr (uncurry M.insert) rho) exts

-- Generate all possible environment extensions and then
-- evaluate using the given semantic function, ev.
-- Finally, take the union of all those.
tryAll :: Env -> [Ident] -> (Env -> WS) -> WS
tryAll rho xs ev = sUnion $ map ev $ genRhos rho xs

-- Verse function application, handles tuples and functions.
-- Returns a singleton set on success.
-- If the function argument is neither of those, return the empty set.
-- NOTE: if the non-function case gives an error, then the way
-- we deal with existentials will not work since it generates
-- a lot of non-functions.
apply :: Val -> Val -> Set Val
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = sing (ws !! k')  where k' = fromInteger k
apply (VFcn (Fcn _ xys)) w = maybe empty sing $ M.lookup w xys
apply _ _ = empty

--------------------
---- Find all identifiers defined by := in this scope

dI :: Exp -> [Ident]
dI = checkDup . sort . dI'
  where
    checkDup (x:x':xs) | x == x' = error $ "Duplicate definition of " ++ x
                       | otherwise = x : checkDup (x':xs)
    checkDup xs = xs

dI' :: Exp -> [Ident]
dI' (App e1 e2) = dI' e1 ++ dI' e2
dI' (Equ e1 e2) = dI' e1 ++ dI' e2
dI' (Seq e1 e2) = dI' e1 ++ dI' e2
dI' (Tup es) = concat (map dI' es)
dI' (Def i e) = i : dI' e
dI' (Colon e) = dI' e
dI' _ = []

--------------------
---- Primitive functions

dO :: Op -> WS
dO Oint = sing $ VFcn $ mkFcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = sing $ VFcn $ mkFcn "gt"  [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = sing $ VFcn $ mkFcn "add" [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

--------------------
---- Semantic equations, valuation

-- P, top level program
dP :: Exp -> RVal
dP e =
  case unSet $ dD e rho0 of
    [w] -> RVal w
    ws  -> Wrong $ show ws

-- P', top level program using dL
dP' :: Exp -> RVal
dP' e =
  case unSet $ sUnion [ dL e w rho0 | w <- unSet allWs ] of
    [w] -> RVal w
    ws  -> Wrong $ show ws

-- D, expression in a scope
dD :: Exp -> Env -> WS
dD e rho = tryAll rho (dI e) (dE e)

-- E, expression
dE :: Exp -> Env -> WS
-- Use the next line to avoid having equations for E.
-- It is a massive slowdown:  5s to 8m
--dE e rho = sUnion [ dM e w rho | w <- unSet allWs ]
dE (Var x) rho = lookupEnv x rho
dE (Int k) _rho = sing $ VInt k
dE (Prim o) _rho = dO o
dE (App e1 e2) rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ]
dE (Equ e1 e2) rho = dD e1 rho `isect` dD e2 rho
dE (Seq e1 e2) rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dE e2 rho ]
dE (Def x e) rho = lookupEnv x rho `isect` dE e rho
dE (Colon e) rho = mkSet [ r | f <- unSet $ dE e rho, a <- unSet allWs, r <- unSet $ apply f a ]
dE Fail _rho = empty
dE (If e1 e2 e3) rho =
  case unSet $ dC e1 rho of
    [] -> dE e3 rho
    rhos -> sUnion [ dE e2 rho' | rho' <- rhos ]
dE (Tup es) rho = mkSet $ map VTup $ sequence $ map (\ e -> unSet (dE e rho)) es
-- Simon's version.
dE (Fun q e1 e2) rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs
           , forAll allWs $ \ x ->
               forAllL (genRhos rho xs) $ \ rho' ->
                 not (isEmpty (dM e1 x rho'))
                 `implies`
                (x `inDom` f && ap f x `sIn` dD e2 rho')
           , (q == Closed)
             `implies`
             (forAll allWs $ \ x ->
               (x `inDom` f) `implies`
                 (existsL (genRhos rho (dI e1)) (\ rho' -> not (isEmpty (dM e1 x rho'))))
             )
  ]
  where xs = dI e1
{-
dE (Fun q e1 e2) rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
        forAll allWs $ \ x ->
          let rhos = dB e1 x rho in
--            trace ("f,x=" ++ show (f, x) ++ " rhos=" ++ show rhos) $
            if isEmpty rhos then not (inDom x f) || q == Open
            else inDom x f && forAll rhos (\ rho' -> ap f x `sIn` dD e2 rho')
  ]
-}

-- Get all possible "solutions", i.e., assignments to the existentials in e.
dC :: Exp -> Env -> Set Env
dC e rho = mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dE e rho' ]

implies :: Bool -> Bool -> Bool
implies x y = not x || y

--------------------
---- Semantic equations, matching

-- L, expression matching in a scope
-- (Like D, but for M)
dL :: Exp -> W -> Env -> WS
dL e u rho = tryAll rho (dI e) (dM e u)

-- M, expression matching
-- Match the value u against the expression, returning all possible
-- values of the expression that makes it match u.
dM :: Exp -> W -> Env -> WS
dM (Var x) u rho = lookupEnv x rho `isect` sing u
dM (Int k) u _rho = sing (VInt k) `isect` sing u
dM (Prim o) u _rho = dO o `isect` sing u
dM (App e1 e2) u rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ] `isect` sing u
dM (Equ e1 e2) u rho = dL e1 u rho `isect` dL e2 u rho
dM (Seq e1 e2) u rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dM e2 u rho ]
dM (Def x e) u rho = lookupEnv x rho `isect` dM e u rho
dM (Colon e) u rho = mkSet [ r | f <- unSet $ dE e rho, r <- unSet $ apply f u ]
dM Fail _u _rho = empty
dM (If e1 e2 e3) u rho =
  case unSet $ dC e1 rho of
    [] -> dM e3 u rho
    rhos -> sUnion [ dM e2 u rho' | rho' <- rhos ]
dM (Tup es) u rho | VTup us <- u, length us == length us =
                      mkSet $ map VTup $ sequence $ zipWith (\ e v -> unSet $ dM e v rho) es us
                  | otherwise = empty
dM (Fun q e1 e2) u rho = mkSet
  [ VFcn f | VFcn g <- [u],
             VFcn f <- unSet allWs,
             forAll allWs $ \ x ->
               let rhos = dB e1 x rho in
                 if isEmpty rhos then not (x `inDom` f) || q == Open
                 else inDom x f && forAll rhos (\ rho' ->
                                                  forAll (dM e1 x rho')
                                                         (\ x' -> x' `inDom` g &&
                                                                  ap f x `sIn` dL e2 (ap g x') rho'))
  ]

-- Solve
-- (Like C, but for M)
dB :: Exp -> W -> Env -> Set Env
dB e u rho = mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dM e u rho' ]



--------------------
---- Examples

-- x:=2; y:=1; add[(x,y)]
exp1 :: Exp
exp1 = Def "x" (Int 2) `Seq` Def "y" (Int 1) `Seq` (App (Prim Oadd) (Tup [Var "x", Var "y"]))

ex1 :: RVal
ex1 = dP exp1

-- fun_c(x:int){x}
exp2 :: Exp
exp2 = Fun Closed (Def "x" (Colon (Var "int"))) (Var "x")

ex2 :: RVal
ex2 = dP exp2

-- fun_o(x:int){x}
exp3 :: Exp
exp3 = Fun Open (Def "x" (Colon (Var "int"))) (Var "x")

-- Goes wrong, as it should
ex3 :: RVal
ex3 = dP exp3

-- fun_c(x:int){add[(x,1)]}
exp4 :: Exp
exp4 = Fun Closed (Def "x" (Colon (Var "int"))) (App (Prim Oadd) (Tup [Var "x", Int 1]))

ex4 :: RVal
ex4 = dP exp4

exp5 :: Exp
exp5 = App exp4 (Int 2)

ex5 :: RVal
ex5 = dP exp5

exp6 :: Exp
exp6 = App exp3 (Int 1)

-- Using exp3 in its domain is fine
ex6 :: RVal
ex6 = dP exp6

-- fun_c(f := fun_c(:int){:int}){f[1]}
exp7 :: Exp
exp7 = Fun Closed arg (App (Var "f") (Int 1))
  where arg = Def "f" (Fun Closed cint cint)
        cint = Colon (Var "int")

exp8 :: Exp
exp8 = App exp7 (Var "succ")

ex8 :: RVal
ex8 = dP exp8

exp9 :: Exp
exp9 = App exp7 (Var "int")

ex9 :: RVal
ex9 = dP exp9

exp10 :: Exp
exp10 = App exp7 exp4

ex10 :: RVal
ex10 = dP exp10

-- fun_c(f := fun_c(:succ){:int}){f[1]}
exp11 :: Exp
exp11 = Fun Closed arg (App (Var "f") (Int 1))
  where arg = Def "f" (Fun Closed csucc cint)
        csucc = Colon (Var "succ")
        cint = Colon (Var "int")

ex11 :: RVal
ex11 = dP exp11

exp12 :: Exp
exp12 = App exp11 (Var "int")

ex12 :: RVal
ex12 = dP exp12

-- Should fail, function domain not large enough.
-- ex7[fun_c(0){0}]
exp13 :: Exp
exp13 = App exp7 (Fun Closed (Int 0) (Int 0))

ex13 :: RVal
ex13 = dP exp13

-- Should fail, function domain not large enough,
-- even though it handles the f[1].
-- ex7[fun_c(1){1}]
exp14 :: Exp
exp14 = App exp7 (Fun Closed (Int 1) (Int 1))

ex14 :: RVal
ex14 = dP exp14

exp15 :: Exp
exp15 = App exp7 (Fun Closed (Colon (Var "int")) (Int 0))

ex15 :: RVal
ex15 = dP exp15

exp16 :: Exp
exp16 = App exp11 (Fun Closed (Colon (Var "int")) (Int 0))

ex16 :: RVal
ex16 = dP exp16

-- fun_c(f := fun_c(:int){:succ}){f[1]}
exp17 :: Exp
exp17 = Fun Closed arg (App (Var "f") (Int 1))
  where arg = Def "f" (Fun Closed cint csucc)
        csucc = Colon (Var "succ")
        cint = Colon (Var "int")

ex17 :: RVal
ex17 = dP exp17

exp18 :: Exp
exp18 = App exp17 (Var "int")

ex18 :: RVal
ex18 = dP exp18

exp19 :: Exp
exp19 = App exp17 (Fun Closed (Colon (Var "int")) (Int 0))

ex19 :: RVal
ex19 = dP exp19

-- if (1=1){2}else{0}
exp20 :: Exp
exp20 = If (Int 1 `Equ` Int 1) (Int 2) (Int 0)

-- if (1=3){2}else{0}
exp21 :: Exp
exp21 = If (Int 1 `Equ` Int 3) (Int 2) (Int 0)

-- if (x:int){x}{999} = 3
exp22 :: Exp
exp22 = If (Def "x" (Colon (Var "int"))) (Var "x") (Int 999) `Equ` Int 3

allExps :: [Exp]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22
          ]

refExps :: String
refExps = "[3,int,Wrong([comparable,int]),succ,3,1,ho1,2,1,2,ho2,2,Wrong([]),Wrong([]),0,0,ho3,2,1,2,0,3]"

allOK :: Bool
allOK = show (map dP allExps) == refExps

allOK' :: Bool
allOK' = show (map dP' allExps) == refExps

main :: IO ()
main = do
  print allOK
  print allOK'
