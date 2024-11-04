{-# OPTIONS_GHC -Wall #-}
module Sem where
import Data.List hiding (find)
import qualified Data.Map as M
import Data.Maybe
--import Debug.Trace

type Ident = String

data Exp = Var Ident | Int Integer | Prim Op | App Exp Exp | Equ Exp Exp | Seq Exp Exp | Def Ident Exp | Colon Exp | Fail | Pair Exp Exp |
           FunC Exp Exp | FunO Exp Exp
  deriving (Eq, Ord, Show)

data Op = Oint | Ogt | Oadd
  deriving (Eq, Ord, Show)

data Val = VInt Integer | VPair Val Val | VFcn (Fcn Val Val)
  deriving (Eq, Ord)

instance Show Val where
  showsPrec p (VInt i) = showsPrec p i
  showsPrec p (VPair a b) = showsPrec p (a, b)
  showsPrec p (VFcn f) = showsPrec p f

data Fcn a b = Fcn String [(a, b)]    -- mapping from a to b
  deriving (Eq, Ord)

instance Show (Fcn a b) where
  show (Fcn s _) = s

inDom :: Ord a => a -> Fcn a b -> Bool
inDom x (Fcn _ xys) = x `elem` map fst xys

newtype Set a = Set { unSet :: [a] }

instance Show a => Show (Set a) where
  showsPrec p (Set s) = showsPrec p s

type Env = M.Map Ident Val

mkSet :: (Ord a) => [a] -> Set a
mkSet = Set . remdup . sort
  where remdup (x:x':xs) | x == x' = remdup (x:xs)
                         | otherwise = x: remdup (x':xs)
        remdup xs = xs

sUnion :: (Ord a) => [Set a] -> Set a
sUnion = mkSet . concatMap unSet

isect :: Ord a => Set a -> Set a -> Set a
isect s1 s2 = mkSet $ unSet s1 `intersect` unSet s2

sing :: a -> Set a
sing x = Set [x]

unSing :: Set a -> a
unSing (Set [x]) = x
unSing _ = error "unSing"

empty :: Set a
empty = Set []

isEmpty :: Set a -> Bool
isEmpty (Set xys) = null xys

sIn :: Ord a => a -> Set a -> Bool
sIn x (Set xs) = x `elem` xs

maxVInt :: Integer
maxVInt = 4

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

vadd :: Val -> Val -> Val
vadd (VInt x) (VInt y) = VInt ((x + y) `mod` maxVInt)
vadd _ _ = undefined

allWs :: WS
allWs = Set $
  nonFcn ++
  [ unSing (dO o) | o <- [Oint, Ogt, Oadd] ] ++
  map VFcn [ id0, id1, id01, f01, const0, const1, const2, const3, fsucc, fsucc2, fpred, comp, ho1, ho2, ho3 ]
  where
    nonFcn =
      allInts ++
      [VPair x y | x <- allInts, y <- allInts]
    id0 = Fcn "id0" [(VInt 0, VInt 0)]
    id1 = Fcn "id1" [(VInt 1, VInt 1)]
    id01 = Fcn "id01" [(VInt 0, VInt 0), (VInt 1, VInt 1)]
    f01 = Fcn "f01" [(VInt 0, VInt 0), (VInt 1, VInt 2)]
    const0 = Fcn "const0" [(x, VInt 0) | x <- allInts]
    const1 = Fcn "const1" [(x, VInt 1) | x <- allInts]
    const2 = Fcn "const2" [(x, VInt 2) | x <- allInts]
    const3 = Fcn "const3" [(x, VInt 3) | x <- allInts]
    comp = Fcn "comparable" [(w, w) | w <- nonFcn ]
    -- The function that accepts f:int->int as an argument and returns f[1]
    ho1 = Fcn "ho1" [(VFcn fsucc, VInt 2), (VFcn fpred, VInt 0), (VFcn fint, VInt 1),
                     (VFcn fsucc2, VInt 3), (VFcn comp, VInt 1),
                     (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                    ]
    ho2 = Fcn "ho2" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                     (VFcn fsucc2, VInt 0), (VFcn comp, VInt 2),
                     (VFcn const0, VInt 0), (VFcn const1, VInt 1), (VFcn const2, VInt 2), (VFcn const3, VInt 3)
                    ]
    ho3 = Fcn "ho3" [(VFcn fsucc, VInt 3), (VFcn fpred, VInt 1), (VFcn fint, VInt 2),
                     (VFcn fsucc2, VInt 0), (VFcn comp, VInt 2),
                     (VFcn const0, VInt 1), (VFcn const1, VInt 2), (VFcn const2, VInt 3), (VFcn const3, VInt 0)
                    ]

fint :: Fcn Val Val
fint = Fcn "int" [(x, x) | x <- allInts ]

fsucc :: Fcn Val Val
fsucc = Fcn "succ" [(x, vadd x (VInt 1)) | x <- allInts ]

fsucc2 :: Fcn Val Val
fsucc2 = Fcn "succ2" [(x, vadd x (VInt 2)) | x <- allInts ]

fpred :: Fcn Val Val
fpred = Fcn "pred" [(x, vadd x (VInt 3)) | x <- allInts ]

rho0 :: Env
rho0 = M.fromList $
  [ (n, unSing (dO o)) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", VFcn fsucc), ("pred", VFcn fpred) ]

type W = Val
type WS = Set W

forAll :: Set a -> (a -> Bool) -> Bool
forAll xs p = all p (unSet xs)

genRhos :: Env -> [Ident] -> [Env]
genRhos rho xs = 
  let exts = sequence $ map (\ x -> map (x,) (unSet allWs)) xs
  in  map (foldr (uncurry M.insert) rho) exts

tryAll :: Env -> [Ident] -> (Env -> WS) -> WS
tryAll rho xs ev = sUnion $ map ev $ genRhos rho xs

apply :: Val -> Val -> Set Val
apply (VPair w0 w1) (VInt k) | k == 0 = sing w0
                             | k == 1 = sing w1
apply (VFcn (Fcn _ xys)) w = maybe empty sing $ lookup w xys
apply _ _ = empty

ap :: Ord a => Fcn a b -> a -> b
ap (Fcn _ xys) x = fromMaybe undefined $ lookup x xys

find :: Ident -> Env -> WS
find x rho = sing $ fromMaybe (error $ "find: undefined " ++ show (x, rho)) $ M.lookup x rho

dI :: Exp -> [Ident]
dI (App e1 e2) = dI e1 <> dI e2
dI (Equ e1 e2) = dI e1 <> dI e2
dI (Seq e1 e2) = dI e1 <> dI e2
dI (Pair e1 e2) = dI e1 <> dI e2
dI (Def i e) = [i] <> dI e
dI (Colon e) = dI e
dI _ = []

dP :: Exp -> W
dP e =
  case dD e rho0 of
    Set [w] -> w
    Set ws -> error $ "dP: " ++ show ws

dP' :: Exp -> W
dP' e =
  case dL e allWs rho0 of
    Set [w] -> w
    Set ws -> error $ "dP: " ++ show ws

dD :: Exp -> Env -> WS
dD e rho = tryAll rho (dI e) (dE e)

dE :: Exp -> Env -> WS
--dE e rho | trace ("dE " ++ show (e, rho)) False = undefined
dE (Var x) rho = find x rho
dE (Int k) _rho = sing $ VInt k
dE (Prim o) _rho = dO o
dE (App e1 e2) rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ]
dE (Equ e1 e2) rho = dE e1 rho `isect` dE e2 rho
dE (Seq e1 e2) rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dE e2 rho ]
dE (Def x e) rho = find x rho `isect` dE e rho
dE (Colon e) rho = mkSet [ r | f <- unSet $ dE e rho, a <- unSet allWs, r <- unSet $ apply f a ]
dE Fail _rho = empty
dE (Pair e1 e2) rho = mkSet [ VPair x y | x <- unSet $ dE e1 rho, y <- unSet $ dE e2 rho ]
dE (FunC e1 e2) rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
        forAll allWs $ \ x ->
          let rhos = dB e1 (sing x) rho in
--            trace ("f,x=" ++ show (f, x) ++ " rhos=" ++ show rhos) $
            if isEmpty rhos then not (inDom x f)
            else inDom x f && forAll rhos (\ rho' -> ap f x `sIn` dD e2 rho')
  ]
dE (FunO e1 e2) rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
        forAll allWs $ \ x ->
          let rhos = dB e1 (sing x) rho in
            if isEmpty rhos then True
            else inDom x f && forAll rhos (\ rho' -> ap f x `sIn` dD e2 rho')
  ]

dO :: Op -> WS
dO Oint = sing $ VFcn $ Fcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = sing $ VFcn $ Fcn "gt"  [ (VPair x y, x) | x <- allInts, y <- allInts, x > y]
dO Oadd = sing $ VFcn $ Fcn "add" [ (VPair x y, vadd x y) | x <- allInts, y <- allInts]

dB :: Exp -> WS -> Env -> Set Env
dB e u rho = mkSet [ rho' | rho' <- genRhos rho (dI e), not $ isEmpty $ dM e u rho' ]

dL :: Exp -> WS -> Env -> WS
dL e u rho = tryAll rho (dI e) (dM e u)

dM :: Exp -> WS -> Env -> WS
dM (Var x) u rho = find x rho `isect` u
dM (Int k) u _rho = sing (VInt k) `isect` u
dM (Prim o) u _rho = dO o `isect` u
dM (App e1 e2) u rho = mkSet [ r | f <- unSet $ dE e1 rho, a <- unSet $ dE e2 rho, r <- unSet $ apply f a ] `isect` u
dM (Equ e1 e2) u rho = dM e1 u rho `isect` dM e2 u rho
dM (Seq e1 e2) u rho = mkSet [ y | _x <- unSet $ dE e1 rho, y <- unSet $ dM e2 u rho ]
dM (Def x e) u rho = find x rho `isect` dM e u rho
dM (Colon e) u rho = mkSet [ r | f <- unSet $ dE e rho, a <- unSet u, r <- unSet $ apply f a ]
dM Fail _u _rho = empty
dM (Pair e1 e2) u rho = mkSet [ VPair x y | VPair u1 u2 <- unSet u,
                                            x <- unSet $ dM e1 (sing u1) rho, y <- unSet $ dM e2 (sing u2) rho ]
dM (FunC e1 e2) u rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
             VFcn g <- unSet u,
             forAll allWs $ \ x ->
               let rhos = dB e1 (sing x) rho in
                 if isEmpty rhos then not (x `inDom` f)
                 else inDom x f && forAll rhos (\ rho' ->
                                                  forAll (dM e1 (sing x) rho')
                                                         (\ x' -> x' `inDom` g &&
                                                                  ap f x `sIn` dL e2 (sing (ap g x')) rho'))
  ]
dM (FunO e1 e2) u rho = mkSet
  [ VFcn f | VFcn f <- unSet allWs,
             VFcn g <- unSet u,
             forAll allWs $ \ x ->
               let rhos = dB e1 (sing x) rho in
                 if isEmpty rhos then True
                 else inDom x f && forAll rhos (\ rho' ->
                                                  forAll (dM e1 (sing x) rho')
                                                         (\ x' -> x' `inDom` g &&
                                                                  ap f x `sIn` dL e2 (sing (ap g x')) rho'))
  ]

-----

-- x:=2; y:=1; add[(x,y)]
ex1 :: Val
ex1 = dP $ Def "x" (Int 2) `Seq` Def "y" (Int 1) `Seq` (App (Prim Oadd) (Pair (Var "x") (Var "y")))

exp2 :: Exp
exp2 = FunC (Def "x" (Colon (Var "int"))) (Var "x")

-- fun_c(x:int){x}
ex2 :: Val
ex2 = dP exp2

-- fun_o(x:int){x}
exp3 :: Exp
exp3 = FunO (Def "x" (Colon (Var "int"))) (Var "x")

-- Goes wrong, as it should
ex3 :: Val
ex3 = dP exp3

-- fun_c(x:int){add[(x,1)]}
exp4 :: Exp
exp4 = FunC (Def "x" (Colon (Var "int"))) (App (Prim Oadd) (Pair (Var "x") (Int 1)))

ex4 :: Val
ex4 = dP exp4

ex5 :: Val
ex5 = dP $ App exp4 (Int 2)

-- Using exp3 in its domain is fine
ex6 :: Val
ex6 = dP $ App exp3 (Int 1)

-- fun_c(f := fun_c(:int){:int}){f[1]}
exp7 :: Exp
exp7 = FunC arg (App (Var "f") (Int 1))
  where arg = Def "f" (FunC cint cint)
        cint = Colon (Var "int")

ex7 :: Val
ex7 = dP $ App exp7 (Var "succ")

ex8 :: Val
ex8 = dP $ App exp7 (Var "int")

ex9 :: Val
ex9 = dP $ App exp7 exp4

-- fun_c(f := fun_c(:succ){:int}){f[1]}
exp10 :: Exp
exp10 = FunC arg (App (Var "f") (Int 1))
  where arg = Def "f" (FunC csucc cint)
        csucc = Colon (Var "succ")
        cint = Colon (Var "int")

ex10 :: Val
ex10 = dP $ App exp10 (Var "int")

-- Should fail, function domain not large enough.
-- ex7[fun_c(0){0}]
ex11 :: Val
ex11 = dP $ App exp7 (FunC (Int 0) (Int 0))

-- Should fail, function domain not large enough,
-- even though it handles the f[1].
-- ex7[fun_c(1){1}]
ex12 :: Val
ex12 = dP $ App exp7 (FunC (Int 1) (Int 1))

ex13 :: Val
ex13 = dP $ App exp7 (FunC (Colon (Var "int")) (Int 0))

ex14 :: Val
ex14 = dP $ App exp10 (FunC (Colon (Var "int")) (Int 0))

-- fun_c(f := fun_c(:int){:succ}){f[1]}
exp15 :: Exp
exp15 = FunC arg (App (Var "f") (Int 1))
  where arg = Def "f" (FunC cint csucc)
        csucc = Colon (Var "succ")
        cint = Colon (Var "int")

ex15 :: Val
ex15 = dP exp15

ex16 :: Val
ex16 = dP $ App exp15 (Var "int")

ex17 :: Val
ex17 = dP $ App exp15 (FunC (Colon (Var "int")) (Int 0))

allExs :: [Val]
allExs = [ex1, ex2, ex4, ex5, ex6, ex7, ex8, ex9, ex10, ex13, ex14, ex15, ex16, ex17]

allOK :: Bool
allOK = show allExs == "[3,int,succ,3,1,2,1,2,2,0,0,ho3,2,1]"
