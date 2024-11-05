{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

-- Use do notation for the set expressions.
-- Also, combine dE and dM in an efficient way.

module Main(main) where
import Prelude(Show(..), Ord(..), Eq(..), Num(..), Integral(..),
               Bool(..), String, IO, Integer,
               sequence, error, uncurry, undefined, showString, traverse,
               print, ($), (.), not, (&&), (||), otherwise, snd
               )
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

--------------------
---- Because of RebindableSyntax

ifThenElse :: Bool -> a -> a -> a
ifThenElse False _ x = x
ifThenElse True  x _ = x

--------------------
---- Abstract syntax

type Ident = String

data Exp
  = Var Ident | Int Integer | Prim Op | App Exp Exp | Equ Exp Exp
  | Seq Exp Exp | Def Ident Exp | Colon Exp | Fail | Tup [Exp]
  | If Exp Exp Exp | Fun OC Exp Exp
  | Choice Exp Exp | All Exp
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

isect :: Ord a => Set a -> Set a -> Set a
isect = S.intersection

sunion :: Ord a => Set a -> Set a -> Set a
sunion = S.union

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
-- So we have to make do with RebindableSyntax and defining return, >>=, >>, etc.

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

isectM :: WS -> Maybe Val -> WS
isectM s Nothing = s
isectM s (Just u) = S.filter (\ (_, x) -> x == u) s

traceS :: String -> Set ()
traceS s = trace s (return ())

--------------------
---- Environment

type Env = M.Map Ident Val

lookupEnv :: Ident -> Env -> Val
lookupEnv x rho =
  fromMaybe (error $ "lookupEnv: undefined " ++ show (x, rho)) $
  M.lookup x rho

-- Initial environment
rho0 :: Env
rho0 = M.fromList $
  [ (n, unSing (dO o)) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ] ++
  [ ("succ", VFcn fsucc), ("pred", VFcn fpred) ] ++
  [ ("false", VTup []) ]

unSing :: WS -> Val
unSing s =
  case unSet s of
    [(_,x)] -> x
    _   -> error "unSing"

--------------------
---- "Universal" set of values
-- This is a carefully selected set of values to make
-- the examples work.

maxVInt :: Integer
maxVInt = 4

allInts :: [Val]
allInts = [ VInt i | i <- [0 .. maxVInt - 1] ]

allWs :: WS
allWs = S.fromList $ map (noLbls,) $
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
---- Labels

type Lbls = [Lbl]
data Lbl = L | R
  deriving (Eq, Ord, Show)

noLbls :: Lbls
noLbls = []

(><) :: Lbls -> Lbls -> Lbls
(><) = (++)

concLbls :: [Lbls] -> Lbls
concLbls = foldr (><) noLbls

pre :: Lbl -> WS -> WS
pre lr s = (\ (l,x) -> (lr:l,x)) <$> s

unit :: Val -> WS
unit v = return (noLbls, v)

sortLbl :: WS -> [Val]
sortLbl = sortl . unSet
  where sortl [] = []
        sortl s =
          case [ w | ([], w) <- s ] of
            _:_:_ -> error "sortLbl"  -- > 1 element
            ws -> ws ++ sortl [ (l, w) | (L : l, w) <- s ] ++ sortl [ (l, w) | (R : l, w) <- s ]

--------------------
---- Aux

type W = (Lbls, Val)
type WS = Set W

-- Given an initial environment, rho, and some identifiers,
-- generate all environments where rho has been extended with
-- the given identifiers bound to all possible value.
genRhos :: Env -> [Ident] -> Set Env
genRhos rho xs = 
  let exts = sequence $ map (\ x -> map (x,) (map snd $ unSet allWs)) xs
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
apply :: Val -> Val -> Set Val
apply (VTup ws) (VInt k) | 0 <= k' && k' < length ws = return (ws !! k')  where k' = fromInteger k
apply (VFcn (Fcn _ xys)) w = maybe empty return $ M.lookup w xys
apply _ _ = empty

-- Apply a set of functions to a set of arguments.
applys :: WS -> WS -> WS
applys fs as = do
  (lf, f) <- fs
  (la, a) <- as
  let l = lf >< la
  (l,) <$> apply f a

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
dO Oint = unit $ VFcn $ mkFcn "int" [ (x, x) | x <- allInts ]
dO Ogt  = unit $ VFcn $ mkFcn "gt"  [ (VTup [x, y], x) | x <- allInts, y <- allInts, x > y]
-- add is a single function, not many as in the doc.
dO Oadd = unit $ VFcn $ mkFcn "add" [ (VTup [x, y], vadd x y) | x <- allInts, y <- allInts]

--------------------
---- Semantic equations, valuation

-- P, top level program
dP :: Exp -> RVal
dP e =
  case unSet $ dL e Nothing rho0 of
    [([],w)] -> RVal w
    ws  -> Wrong $ show ws

-- E, expression
dE :: Exp -> Env -> WS
dE e rho = dM e Nothing rho

--------------------
---- Semantic equations, matching

-- M, expression matching
-- Match the value u against the expression, returning all possible
-- values of the expression that makes it match u.
-- If u is Nothing, then match against all possible values.
dM :: Exp -> Maybe Val -> Env -> WS
dM (Var x) u rho = unit (lookupEnv x rho) `isectM` u
dM (Int k) u _rho = unit (VInt k) `isectM` u
dM (Prim o) u _rho = dO o `isectM` u
dM (App e1 e2) u rho = applys (dE e1 rho) (dE e2 rho) `isectM` u
dM (Equ e1 e2) u rho = dM e1 u rho `isect` dM e2 u rho
dM (Seq e1 e2) u rho = do (l, _) <- dE e1 rho; (l', w) <- dM e2 u rho; return (l >< l', w)
dM (Def x e) u rho = dM e u rho `isectM` Just (lookupEnv x rho)
dM (Colon e) (Just u) rho = do (l, f) <- dE e rho; r <- apply f u; return (l, r)
dM Fail _u _rho = empty
dM (If e1 e2 e3) u rho =
  ifEmpty (dB e1 Nothing rho)
    (dM e3 u rho)
    (\ rhos -> do
        rho' <- rhos
        dM e2 u rho'
    )
dM (Tup es) (Just u) rho | VTup us <- u, length us == length us =
                                vtup <$> mapM (\ (e, v) -> dM e (Just v) rho) (zip es us)
                              | otherwise = empty
  where vtup lvs = (concLbls ls, VTup vs) where (ls, vs) = unzip lvs
dM (Fun q e1 e2) (Just u) rho | VFcn g <- u = do
  vf@(_, VFcn f) <- allWs
  () <- traceS ("trying f=" ++ show f)
  guard $
    forAll allWs $ \ lx@(_,x) ->
      trace ("trying x=" ++ show (x, e1, dB e1 (Just x) rho)) (
      ifEmpty
        (dB e1 (Just x) rho)                -- possible ways x can match e1
        (not (x `inDom` f) || q == Open)     -- if none
        $ \ rhos ->                          -- if at least one
             trace ("x in e1 " ++ show (length rhos, x `inDom` f)) $
             x `inDom` f &&
             forAll rhos
                    (\ rho' -> forAll (dM e1 (Just x) rho')
                                      (\ (lx', x') -> x' `inDom` g &&
                                                      ap f x `sIn` (snd <$> dL e2 (Just (ap g x')) rho')))
      )
  return vf
                              | otherwise = empty

dM (Choice e1 e2) u rho =
  pre L (dL e1 u rho) `sunion` pre R (dL e2 u rho)
dM (All e) u rho = unit (VTup xs) `isectM` u
  where xs = sortLbl (dE e rho)

dM e Nothing rho = do  -- if nothing else matches then try all possible u
   (_,u) <- allWs
   dM e (Just u) rho

-- L, expression matching in a scope
dL :: Exp -> Maybe Val -> Env -> WS
dL e u rho = tryAll rho (dI e) (dM e u)

-- Solve
-- (Like C, but for M)
dB :: Exp -> Maybe Val -> Env -> Set Env
dB e u rho = do
  rho' <- genRhos rho (dI e)
  guard $ not $ isEmpty $ dM e u rho'
  return rho'


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

exp23 :: Exp
exp23 = All $ Choice (Int 1) (Int 2)

exp24 :: Exp
exp24 = Fun Closed (Def "x" (Choice (Int 0) (Int 1))) (Var "x")

allExps :: [Exp]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp23
          ]

refExps :: String
refExps = "[3,int,Wrong([([],comparable),([],int)]),succ,3,1,ho1,2,1,2,ho2,2,Wrong([]),Wrong([]),0,0,ho3,2,1,2,0,3,[1,2]]"

allOK :: Bool
allOK = show (map dP allExps) == refExps

_used :: [RVal]
_used = [ex1, ex2, ex3, ex4, ex5, ex6, ex8, ex9,
         ex10, ex11, ex12, ex13, ex14, ex15, ex16, ex17, ex18, ex19
        ]

main :: IO ()
main = Prelude.do
  print allOK
