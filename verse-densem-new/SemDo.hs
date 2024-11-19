{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}
module Main where
import Prelude(Show(..), Ord(..), Eq(..), Num(..), Integral(..),
               Bool(..), String, IO, Integer,
               sequence, error, uncurry, undefined, showString, traverse,
               ($), (.), not, (&&), (||), otherwise,
               putStrLn,
               )
import qualified Prelude
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Exp
import Examples
--import Debug.Trace

--------------------
---- Because of RebindableSyntax

ifThenElse :: Bool -> a -> a -> a
ifThenElse False _ x = x
ifThenElse True  x _ = x

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
  showsPrec _ (Wrong _) = showString $ "Wrong"

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

--sing :: a -> Set a
--sing = S.singleton

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

-- It's impossible to make Set a monad since there is an Ord constraint on the elements.
-- So we have to make do with RebindableSyntax and defining return, >>= and >>

return :: a -> Set a
return = S.singleton

(>>=) :: (Ord b) => Set a -> (a -> Set b) -> Set b
s >>= f = S.unions $ map f $ S.toList s

(>>) :: Set a -> Set b -> Set b
s >> t = if S.null s then S.empty else t

fail :: String -> Set a
fail _ = empty

guard :: Bool -> Set ()
guard False = empty
guard True  = return ()

ifEmpty :: Set a -> b -> (Set a -> b) -> b
ifEmpty s n f | S.null s  = n
              | otherwise = f s

mapM :: (Ord b) => (a -> Set b) -> [a] -> Set [b]
mapM f = S.fromList . traverse (S.toList . f)

infixl 4 <$>
(<$>) :: Ord b => (a -> b) -> Set a -> Set b
(<$>) = S.map

--------------------
---- Environment

type Env = M.Map Ident Val

lookupEnv :: Ident -> Env -> WS
lookupEnv x rho = return $ fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $ M.lookup x rho

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
genRhos :: Env -> [Ident] -> Set Env
genRhos rho xs = 
  let exts = sequence $ map (\ x -> map (x,) (unSet allWs)) xs
  in  mkSet $ map (foldr (uncurry M.insert) rho) exts

-- Generate all possible environment extensions and then
-- evaluate using the given semantic function, ev.
-- Finally, take the union of all those.
tryAll :: Env -> [Ident] -> (Env -> WS) -> WS
tryAll rho xs ev = do
  rho' <- genRhos rho xs
  ev rho'

-- Verse function application, handles tuples and functions.
-- Returns a singleton set on success.
-- If the function argument is neither of those, return the empty set.
-- NOTE: if the non-function case gives an error, then the way
-- we deal with existentials will not work since it generates
-- a lot of non-functions.
apply :: W -> W -> WS
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = return (ws !! k')  where k' = fromInteger k
apply (VFcn (Fcn _ xys)) w = maybe empty return $ M.lookup w xys
apply _ _ = empty

-- Apply a set of functions to a set of arguments.
applys :: WS -> WS -> WS
applys fs as = do
  f <- fs
  a <- as
  apply f a

--------------------
---- Primitive functions

dO :: Op -> WS
dO Oint = return $ VFcn $ mkFcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = return $ VFcn $ mkFcn "gt"  [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = return $ VFcn $ mkFcn "add" [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

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
dE (Int k) _rho = return $ VInt k
dE (Prim o) _rho = dO o
dE (App e1 e2) rho = applys (dE e1 rho) (dE e2 rho)
dE (Equ e1 e2) rho = dD e1 rho `isect` dD e2 rho
dE (Seq e1 e2) rho = do
  _ <- dE e1 rho
  dE e2 rho
dE (Where e1 e2) rho = do
  x <- dE e1 rho
  _ <- dE e2 rho
  return x
dE (Def x e) rho = lookupEnv x rho `isect` dE e rho
dE (Colon e) rho = applys (dE e rho) allWs
dE Fail _rho = empty
dE (If e1 e2 e3) rho =
  ifEmpty (dC e1 rho)
    (dE e3 rho)
    (\ rhos -> do
        rho' <- rhos
        dE e2 rho'
    )
dE (Tup es) rho = VTup <$> mapM (\ e -> dE e rho) es
dE (Fun q e1 e2) rho = do
  vf@(VFcn f) <- allWs
  guard $
    forAll allWs $ \ x ->
      let rhos = dB e1 x rho
      in  if isEmpty rhos then not (x `inDom` f) || q == Open
          else x `inDom` f &&
               forAll rhos (\ rho' -> ap f x `sIn` dD e2 rho')
  return vf
dE _ _ = undefined

-- Get all possible "solutions", i.e., assignments to the existentials in e.
dC :: Exp -> Env -> Set Env
dC e rho = do
  rho' <- genRhos rho (dI e)
  guard $ not $ isEmpty $ dE e rho'
  return rho'
  --mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dE e rho' ]


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
dM (Var x) u rho = lookupEnv x rho `isect` return u
dM (Int k) u _rho = return (VInt k) `isect` return u
dM (Prim o) u _rho = dO o `isect` return u
dM (App e1 e2) u rho = applys (dE e1 rho) (dE e2 rho) `isect` return u
dM (Equ e1 e2) u rho = dL e1 u rho `isect` dL e2 u rho
dM (Seq e1 e2) u rho = do { _ <- dE e1 rho; dM e2 u rho }
dM (Where e1 e2) u rho = do
  x <- dM e1 u rho
  _ <- dE e2 rho
  return x
dM (Def x e) u rho = lookupEnv x rho `isect` dM e u rho
dM (Colon e) u rho = do f <- dE e rho; apply f u
dM Fail _u _rho = empty
dM (If e1 e2 e3) u rho =
  ifEmpty (dC e1 rho)
    (dM e3 u rho)
    (\ rhos -> do
        rho' <- rhos
        dM e2 u rho'
    )
dM (Tup es) u rho | VTup us <- u, length es == length us = VTup <$> mapM (\ (e, v) -> dM e v rho) (zip es us)
                  | otherwise = empty
dM (Fun q e1 e2) u rho | VFcn g <- u = do
  vf@(VFcn f) <- allWs
  guard $
    forAll allWs $ \ x ->
      let rhos = dB e1 x rho
      in  if isEmpty rhos then not (x `inDom` f) || q == Open
          else x `inDom` f &&
               forAll rhos
                      (\ rho' -> forAll (dM e1 x rho')
                                        (\ x' -> x' `inDom` g &&
                                                 ap f x `sIn` dL e2 (ap g x') rho'))
  return vf
                       | otherwise = empty
dM _ _ _ = undefined

-- Solve
-- (Like C, but for M)
dB :: Exp -> W -> Env -> Set Env
dB e u rho = do
  rho' <- genRhos rho (dI e)
  guard $ not $ isEmpty $ dM e u rho'
  return rho'


allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp33, exp34, exp35
          ]

main :: IO ()
main = Prelude.do
  putStrLn "Start dP"
  runExamples dP allExps
  putStrLn "Start dP'"
  runExamples dP' allExps
