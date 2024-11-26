{-# OPTIONS_GHC -Wall #-}
module Main where
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import GHC.Stack
import Exp
import Val
import Set
import Examples
--import Debug.Trace

--------------------
---- Environment

type Env = M.Map Ident Val

lookupEnv :: HasCallStack => Ident -> Env -> WS
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

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allWs :: WS
allWs = S.fromList $
  nonFcn ++
  [ unSing (dO o) | o <- [Oint, Ogt, Oadd] ] ++
  map VFcn [ id0, id1, id01, f01, const0, const1, const2, const3, fsucc, fsucc2, fpred, comp, ho1, ho2, ho3, ho4 ]
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
    ho4 = mkFcn "ho4" [(VFcn id0, VInt 0)]

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

dX :: Exp -> Env -> Set Env
dX e rho = mkSet $ genRhos rho (dI e)

-- D, expression in a scope
dD :: Exp -> Env -> WS
dD e rho = mkSet [ r | rho' <- unSet $ dX e rho, r <- unSet $ dE e rho' ]

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
dE (Where e1 e2) rho = mkSet [ x | x <- unSet $ dE e1 rho, _y <- unSet $ dE e2 rho ]
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
               forAll (dX e1 rho) $ \ rho' ->
                 not (isEmpty (dM e1 x rho'))
                 `implies`
                (x `inDom` f && ap f x `sIn` dD e2 rho')
           , (q == Closed)
             `implies`
             (forAll allWs $ \ x ->
               (x `inDom` f) `implies`
                 (exists (dX e1 rho) (\ rho' -> not (isEmpty (dM e1 x rho'))))
             )
  ]
{- old version
dE (Fun q e1 e2) rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
        forAll allWs $ \ x ->
          let rhos = dB e1 x rho in
--            trace ("f,x=" ++ show (f, x) ++ " rhos=" ++ show rhos) $
            if isEmpty rhos then not (inDom x f) || q == Open
            else inDom x f && forAll rhos (\ rho' -> ap f x `sIn` dD e2 rho')
  ]
-}
dE _ _ = undefined

-- Get all possible "solutions", i.e., assignments to the existentials in e.
dC :: Exp -> Env -> Set Env
dC e rho = mkSet [ rho' | rho' <- unSet $ dX e rho, not (isEmpty (dE e rho')) ]

implies :: Bool -> Bool -> Bool
implies x y = not x || y

--------------------
---- Semantic equations, matching

-- L, expression matching in a scope
-- (Like D, but for M)
dL :: Exp -> W -> Env -> WS
dL e u rho = mkSet [ r | rho' <- unSet $ dX e rho, r <- unSet $ dM e u rho' ]

-- M, expression matching
-- Match the value u against the expression, returning all possible
-- values of the expression that makes it match u.
dM :: HasCallStack => Exp -> W -> Env -> WS
dM (Var x) u rho = lookupEnv x rho `isect` sing u
dM (Int k) u _rho = sing (VInt k) `isect` sing u
dM (Prim o) u _rho = dO o `isect` sing u
dM (App e1 e2) u rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ] `isect` sing u
dM (Equ e1 e2) u rho = dL e1 u rho `isect` dL e2 u rho
dM (Seq e1 e2) u rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dM e2 u rho ]
dM (Where e1 e2) u rho = mkSet [ x | x <- unSet $ dM e1 u rho, _y <- unSet $ dE e2 rho ]
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
  [ VFcn f | VFcn f <- unSet allWs
           , VFcn g <- [u]
           , forAll allWs $ \ x ->
               forAll (dX e1 rho) $ \ rho' ->
                 forAll (dM e1 x rho') $ \ y ->
                   (x `inDom` f) &&
                   (y `inDom` g) &&
                   (ap f x `sIn`
                      dL e2 (ap g y) rho')
           , (q == Closed)
             `implies`
             (forAll allWs $ \ x ->
               (x `inDom` f) `implies`
                 (exists (dX e1 rho) $ \ rho' ->
                    not (isEmpty (dM e1 x rho')))
             )
  ]

{-
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
-}
dM _ _ _ = undefined

-- Solve
-- (Like C, but for M)
dB :: Exp -> W -> Env -> Set Env
dB e u rho = mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dM e u rho' ]



-- f:=fun_c(g:=fun_c(0){0}){g[0]} ; f[fun_c(0){0}]
exp40 :: Exp
exp40 = Def "f" (Fun Closed (Def "g" (Fun Closed (Int 0) (Int 0))) (App (Var "g") (Int 0))) `Seq` App (Var "f") (Fun Closed (Int 0) (Int 0))

-- f:=fun_c(g:=fun_c(:int){:int}){g[0]} ; f[fun_c(0){0}]
exp41 :: Exp
exp41 = Def "f" (Fun Closed (Def "g" (Fun Closed cint cint)) (App (Var "g") (Int 0))) `Seq` App (Var "f") (Fun Closed (Int 0) (Int 0))
  where cint = Colon (Var "int")

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp33, exp34, exp35
          ]

main :: IO ()
main = do
  putStrLn "Start dP"
  runExamples dP allExps
  putStrLn "Start dP'"
  runExamples dP' allExps
