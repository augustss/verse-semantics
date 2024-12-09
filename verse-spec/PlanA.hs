{-# OPTIONS_GHC -Wall #-}
module Main where
import qualified Data.Map as M
import GHC.Stack
import Exp
import Val
import Set
import Env
import Examples
--import Debug.Trace

--------------------
---- Aux

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
dE (Var x) rho = sing $ lookupEnv x rho
dE (Int k) _rho = sing $ VInt k
dE (Prim o) _rho = sing $ dO o
dE (App e1 e2) rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ]
dE (Equ e1 e2) rho = dD e1 rho `isect` dD e2 rho
dE (Seq e1 e2) rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dE e2 rho ]
dE (Where e1 e2) rho = mkSet [ x | x <- unSet $ dE e1 rho, _y <- unSet $ dE e2 rho ]
dE (Def x e) rho = sing (lookupEnv x rho) `isect` dE e rho
dE (Def2 x y e) rho = sing (lookupEnv x rho) `isect` sing (lookupEnv y rho) `isect` dE e rho
dE (Colon (Var "any")) _ = allWs                         -- hack for any
dE (Colon e) rho = mkSet [ r | f <- unSet $ dE e rho, a <- unSet allWs, r <- unSet $ apply f a ]
dE Fail _rho = empty
dE (If e1 e2 e3) rho =
  case unSet $ dC e1 rho of
    [] -> dE e3 rho
    rhos -> sUnion [ dE e2 rho' | rho' <- rhos ]
dE (Tup es) rho = mkSet $ map VTup $ sequence $ map (\ e -> unSet (dE e rho)) es
-- Simon's version.
dE (Fun q e1 e2) rho = mkSet
  [ f | f <- unSet allWs, function f
           , forAll allWs $ \ x ->
               forAll (dX e1 rho) $ \ rho' ->
                 not (isEmpty (dM e1 x rho'))
                 `implies`
                ( x `inDomV` f &&
--                  trace ("good " ++ show (f, e1, x)) True &&
                  apV f x `sIn` dD e2 rho')
--           , trace ("all good " ++ show (ee, f)) True
           , (q == Closed)
             `implies`
             (forAll allWs $ \ x ->
               (x `inDomV` f) `implies`
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
dM (Var x) u rho = sing (lookupEnv x rho) `isect` sing u
dM (Int k) u _rho = sing (VInt k) `isect` sing u
dM (Prim o) u _rho = sing (dO o) `isect` sing u
dM (App e1 e2) u rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ] `isect` sing u
dM (Equ e1 e2) u rho = dL e1 u rho `isect` dL e2 u rho
dM (Seq e1 e2) u rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dM e2 u rho ]
dM (Where e1 e2) u rho = mkSet [ x | x <- unSet $ dM e1 u rho, _y <- unSet $ dE e2 rho ]
dM (Def x e) u rho = sing (lookupEnv x rho) `isect` dM e u rho
dM (Def2 x y e) u rho | lookupEnv x rho == u = sing (lookupEnv y rho) `isect` dM e u rho
                      | otherwise            = empty
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
  [ f | f <- unSet allWs, function f
           , g <- [u], function g

           , (q == Closed)
             `implies`
             (domV u == rangeE e1 rho)

           , forAll allWs $ \ x ->
               forAll (dX e1 rho) $ \ rho' ->
                 forAll (dM e1 x rho') $ \ y ->
                   (x `inDomV` f) &&
                   (y `inDomV` g) &&
                   (apV f x `sIn`
                      dL e2 (apV g y) rho')
           , (q == Closed)
             `implies`
             (forAll allWs $ \ x ->
               (x `inDomV` f) `implies`
                 (exists (dX e1 rho) $ \ rho' ->
                    not (isEmpty (dM e1 x rho')))
             )

--  , trace (show (f, (u, domV u), (e1, domE e1 rho), domV u == domE e1 rho)) True

--           , trace ("*** " ++ show f) True
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

--domE :: Exp -> Env -> WS
--domE e rho = mkSet [ x | x <- unSet allWs, rho' <- unSet $ dX e rho, not (isEmpty (dM e x rho') ) ]

rangeE :: Exp -> Env -> WS
rangeE e rho = mkSet [ r | x <- unSet allWs, rho' <- unSet $ dX e rho, r <- unSet $ dM e x rho' ]

close :: OC -> [W] -> [W]
close _ [] = []
close _ [f] = [f]
close Open fs = fs
close Closed fs =
  let r = [ f | f <- fs, forAllL fs (\ f' -> domV f `lessEq` domV f') ]
  in  --trace ("close " ++ show (fs, r))
      r


-- Solve
-- (Like C, but for M)
dB :: Exp -> W -> Env -> Set Env
dB e u rho = mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dM e u rho' ]

allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22,
           {-Choice: exp23,exp24,exp25,exp26,exp27,exp28,exp29,exp30,exp31,exp32,-}
           exp33, exp34, exp35,
           {-Choice: exp36, exp37, exp38, exp39, exp40, exp43, exp44, -}
           exp45, exp46, exp47, exp48, exp49, exp50,
           exp51, exp52, exp53, exp54, exp55
          ]

main :: IO ()
main = do
  putStrLn "Start dP"
  runExamples dP allExps
--  putStrLn "Start dP'"
--  runExamples dP' allExps
