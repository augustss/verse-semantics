{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

-- Use do notation for the set expressions.
-- Also, combine dE and dM in an efficient way.

module Main where
import Prelude(Show(..), Ord(..), Eq(..), Num(..), Integral(..),
               Bool(..), String, IO, Integer,
               sequence, error, uncurry, undefined, showString, traverse,
               ($), (.), not, (&&), (||), otherwise, snd, putStrLn,
               showParen,
               )
import qualified Prelude
import qualified Control.Monad as Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
--import Debug.Trace
import GHC.Stack

implies :: Bool -> Bool -> Bool
implies x y = not x || y

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
  | Choice Exp Exp | All Exp | For Exp Exp
  | Where Exp Exp
  deriving (Eq, Ord)

data Op = Oint | Ogt | Oadd
  deriving (Eq, Ord, Show)

data OC = Open | Closed
  deriving (Eq, Ord, Show)

instance Show Exp where
  showsPrec _ (Var s) = showString s
  showsPrec p (Int i) = showsPrec p i
  showsPrec _ (Prim o) = showString (drop 1 $ show o)
  showsPrec _ (App e1 e2) = showsPrec 11 e1 . showString "[" . showsPrec 0 e2 . showString "]"
  showsPrec p (Equ e1 e2) = showParen (p > 5) $ showsPrec 6 e1 . showString " = " . showsPrec 6 e2
  showsPrec p (Seq e1 e2) = showParen (p > 3) $ showsPrec 3 e1 . showString "; " . showsPrec 3 e2
  showsPrec p (Where e1 e2) = showParen (p > 1) $ showsPrec 3 e1 . showString " where " . showsPrec 3 e2
  showsPrec p (Def x e) = showParen (p > 5) $ showString x . showString " := " . showsPrec 6 e
  showsPrec _ (Colon e) = showString ":" . showsPrec 10 e
  showsPrec _ Fail = showString "fail"
  showsPrec _ (Tup es) = showString "<" . showString (intercalate "," $ map show es) . showString ">"
  showsPrec _ (If e1 e2 e3) = showString "if " . showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2) .
                              showBraces (showsPrec 0 e3)
  showsPrec p (Choice e1 e2) = showParen (p > 4) $ showsPrec 5 e1 . showString " | " . showsPrec 5 e2
  showsPrec _ (All e) = showString "all" . showBraces (showsPrec 0 e)
  showsPrec _ (For e1 e2) = showString "for" . showParen True (showsPrec 0 e1) . showBraces (showsPrec 0 e2)
  showsPrec _ (Fun q e1 e2) = showString (if q == Open then "fun_o" else "fun_c") .
                              showParen True (showsPrec 0 e1) .
                              showBraces (showsPrec 0 e2)

showBraces :: (String -> String) -> (String -> String)
showBraces a = showString "{" . a . showString "}"

--------------------
---- Values

data Val = VInt Integer | VTup [Val] | VFcn Fcn
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

data Fcn = Fcn String (M.Map W LW)

mkFcn :: String -> [(W, W)] -> Fcn
mkFcn s xys = mkFcn' s [(x, (noLbls, y)) | (x, y) <- xys]

mkFcn' :: String -> [(W, LW)] -> Fcn
mkFcn' s xys = Fcn s $ M.fromList xys

instance Eq Fcn where
  Fcn f _ == Fcn f' _  =  f == f'

instance Ord Fcn where
  Fcn f _ `compare` Fcn f' _  =  f `compare` f'

instance Show Fcn where
  show (Fcn s _) = s

fcnName :: Fcn -> String
fcnName (Fcn s _) = s

-- Domain test
inDom :: W -> Fcn -> Bool
inDom _ (Fcn "any" _) = True  -- ANY hack
inDom x (Fcn _ xys) = M.member x xys

-- Application when the argument is in the domain
ap :: HasCallStack => Fcn -> W -> LW
ap (Fcn "any" _) x = (noLbls, x)
ap (Fcn f xys) x =
  fromMaybe (error $ "ap: outside domain " ++ f ++ " " ++ show x) $
  M.lookup x xys

--------------------
---- Sets

newtype Set a = S { unS :: S.Set a }
  deriving (Eq, Ord)

instance Show a => Show (Set a) where
  show (S s) = "{" ++ init (drop 10 (show s)) ++ "}"

unSet :: Set a -> [a]
unSet = S.toList . unS

mkSet :: (Ord a) => [a] -> Set a
mkSet = S . S.fromList

isect :: Ord a => Set a -> Set a -> Set a
isect (S s1) (S s2) = S $ S.intersection s1 s2

sunion :: Ord a => Set a -> Set a -> Set a
sunion (S s1) (S s2) = S $ S.union s1 s2

empty :: Set a
empty = S S.empty

isEmpty :: Set a -> Bool
isEmpty = S.null . unS

sIn :: Ord a => a -> Set a -> Bool
sIn x = S.member x . unS

getSing :: Set a -> Maybe a
getSing s =
  case S.toList (unS s) of
    [x] -> Just x
    _   -> Nothing

-- Check if a predicate holds for all values in the set
forAll :: Set a -> (a -> Bool) -> Bool
forAll xs p = all p (unSet xs)

exists :: Set a -> (a -> Bool) -> Bool
exists xs p = any p (unSet xs)

-- It's impossible to make Set a monad since there is an Ord constraint on the elements.
-- So we have to make do with RebindableSyntax and defining return, >>=, >>, etc.

return :: a -> Set a
return = S . S.singleton

(>>=) :: (Ord b) => Set a -> (a -> Set b) -> Set b
s >>= f = S $ S.unions $ map (unS . f) $ unSet s

(>>) :: Set a -> Set b -> Set b
s >> t = if S.null (unS s) then empty else t

fail :: String -> Set a
fail _ = empty

guard :: Bool -> Set ()
guard False = empty
guard True  = return ()

ifEmpty :: Set a -> b -> (Set a -> b) -> b
ifEmpty s n f | S.null (unS s) = n
              | otherwise      = f s

mapM :: (Ord b) => (a -> Set b) -> [a] -> Set [b]
mapM f = mkSet . traverse (unSet . f)

infixl 4 <$>
(<$>) :: Ord b => (a -> b) -> Set a -> Set b
f <$> (S s) = S $ S.map f s

isectM :: WS -> Maybe Val -> WS
isectM s Nothing = s
isectM s (Just u) = S $ S.filter (\ (_, x) -> x == u) $ unS s

--traceS :: String -> Set ()
--traceS s = trace s (return ())

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
  [ (n, unSing (dO o)) | (n, o) <- [("int", Oint), ("gt", Ogt), ("add", Oadd) ] ]
  ++ [ ("succ", VFcn fsucc), ("pred", VFcn fpred) ]
  ++ [ ("false", VTup []) ]

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

allWs :: Set Val
allWs = mkSet $
  nonFcn ++
  [ unSing (dO o) | o <- [Oint, Ogt, Oadd] ] ++
  map VFcn [ id0, id1, f01, const0, const1, const2, const3, fsucc, fsucc2,
             fpred, comp, ho1, ho2, ho3,
             id01, id01LR, id01RL,
             id012, id310, id012x, id310x,
             f0L1, f0R2, f0t12
             -- ,fany
           ]
  where
    nonFcn =
      allInts ++
      [VTup []] ++
      [VTup [x] | x <- allInts] ++
      [VTup [x, y] | x <- allInts, y <- allInts]
-- ++      [VTup [x, y, z] | x <- allInts, y <- allInts, z <- allInts]
    id0 = mkFcn "id0" [(VInt 0, VInt 0)]
    id1 = mkFcn "id1" [(VInt 1, VInt 1)]
    id01 = mkFcn "id01" [(VInt 0, VInt 0), (VInt 1, VInt 1)]
    id01LR = mkFcn' "id01LR" [(VInt 0, (Lbls [L], VInt 0)), (VInt 1, (Lbls [R], VInt 1))]
    id01RL = mkFcn' "id01RL" [(VInt 0, (Lbls [R], VInt 0)), (VInt 1, (Lbls [L], VInt 1))]
    id012 = mkFcn' "id012" [(VInt 0, (Lbls [L,L], VInt 0)), (VInt 1, (Lbls [L,R], VInt 1)), (VInt 2, (Lbls [R], VInt 2))]
    id310 = mkFcn' "id310" [(VInt 3, (Lbls [L,L], VInt 3)), (VInt 1, (Lbls [L,R], VInt 1)), (VInt 0, (Lbls [R], VInt 0))]
    id012x = mkFcn' "id012x" [(VInt 0, (Lbls [L,L,L], VInt 0)), (VInt 1, (Lbls [L,R,L], VInt 1)), (VInt 2, (Lbls [L,R], VInt 2))]
    id310x = mkFcn' "id310x" [(VInt 3, (Lbls [R,L,L], VInt 3)), (VInt 1, (Lbls [R,R,L], VInt 1)), (VInt 0, (Lbls [R,R], VInt 0))]
    f01 = mkFcn "f01" [(VInt 0, VInt 0), (VInt 1, VInt 2)]
    f0L1 = mkFcn' "f0L1" [(VInt 0, (Lbls [L], VInt 1))]
    f0R2 = mkFcn' "f0R2" [(VInt 0, (Lbls [R], VInt 2))]
    f0t12 = mkFcn "f0t12" [(VInt 0, VTup [VInt 1, VInt 2])]
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
--    fany = mkFcn "any" [] -- ANY hack, see apply & inDom

fint :: Fcn
fint = mkFcn "int" [(x, x) | x <- allInts ]

fsucc :: Fcn
fsucc = mkFcn "succ" [(x, vadd x (VInt 1)) | x <- allInts ]

fsucc2 :: Fcn
fsucc2 = mkFcn "succ2" [(x, vadd x (VInt 2)) | x <- allInts ]

fpred :: Fcn
fpred = mkFcn "pred" [(x, vadd x (VInt 3)) | x <- allInts ]

--------------------
---- Labels

newtype Lbls = Lbls { unLbls :: [Lbl] }
  deriving (Eq, Ord)

instance Show Lbls where
  show (Lbls []) = "-"
  show (Lbls ls) = concatMap show ls

data Lbl = L | R
  deriving (Eq, Ord, Show)

noLbls :: Lbls
noLbls = Lbls []

(><) :: Lbls -> Lbls -> Lbls
x >< y = Lbls $ unLbls x ++ unLbls y

concLbls :: [Lbls] -> Lbls
concLbls = foldr (><) noLbls

pre :: Lbl -> WS -> WS
pre l s = (\ (Lbls ls,x) -> (Lbls (l:ls),x)) <$> s

unit :: Val -> WS
unit v = return (noLbls, v)

sortLbl :: Ord a => Set (Lbls, a) -> [Set a]
sortLbl = sortl . unSet
  where sortl [] = []
        sortl s =
          let ws = mkSet [ w | (Lbls [], w) <- s ]
              rest = sortl [ (Lbls l, w) | (Lbls (L : l), w) <- s ] ++ sortl [ (Lbls l, w) | (Lbls (R : l), w) <- s ]
          in  if isEmpty ws then rest else ws : rest

preLbls :: Lbls -> WS -> WS
preLbls l s = (\ (l',x) -> (l >< l',x)) <$> s

-- Common structure of a set of labels.
-- XXX Just prefix now.
commonLbls :: Set Lbls -> Lbls
commonLbls = Lbls . longestPrefix . map unLbls . unSet

longestPrefix :: Eq a => [[a]] -> [a]
longestPrefix [] = []
longestPrefix (x:xs) = last $ filter (\ p -> all (isPrefixOf p) xs) $ subsequences x

--------------------
---- Aux

type W = Val
type LW = (Lbls, W)
type WS = Set LW

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
apply :: W -> W -> Set LW
apply (VTup ws) (VInt k) | 0 <= k' && k' < l = return (lbl, ws !! k')
  where k' = fromInteger k
        l = length ws
        lbl = Lbls $ L : replicate k' R
apply (VFcn (Fcn "any" _)) w = unit w  -- ANY hack
apply (VFcn (Fcn _ xys)) w = maybe empty return $ M.lookup w xys
apply _ _ = empty

-- Apply a set of functions to a set of arguments.
applys :: WS -> WS -> WS
applys fs as = do
  (lf, f) <- fs
  (la, a) <- as
  (lr, r) <- apply f a
  return (lf >< la >< lr, r)

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
dI' (Where e1 e2) = dI' e1 ++ dI' e2
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
    [(Lbls [],w)] -> RVal w
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
dM (Equ e1 e2) u rho = dL e1 u rho `isect` dL e2 u rho
dM (Seq e1 e2) u rho = do (l, _) <- dE e1 rho; preLbls l $ dM e2 u rho
dM (Where e1 e2) u rho = do (l, x) <- dE e1 rho; (l', _) <- dM e2 u rho; return (l >< l', x)
dM (Def x e) u rho = dM e u rho `isectM` Just (lookupEnv x rho)
dM (Colon e) (Just u) rho = do (l, f) <- dE e rho; preLbls l $ apply f u
dM Fail _u _rho = empty
dM (If e1 e2 e3) u rho =
  case sortLbl $ dB' e1 Nothing rho of
    []     -> dL e3 u rho
    rhos:_ -> do    -- explicitely pick the first alternative
      rho' <- rhos
      dL e2 u rho'
dM (Tup es) (Just u) rho | VTup us <- u, length es == length us =
                            vtup <$> mapM (\ (e, v) -> dM e (Just v) rho) (zip es us)
                         | otherwise = empty
  where vtup lvs = (concLbls ls, VTup vs) where (ls, vs) = unzip lvs
{-
dM (Fun q e1 e2) Nothing rho = do
  vf@(VFcn f) <- allWs
--  () <- trace ("trying f=" ++ show f) (return ())
  let xs = dI e1
  guard $
            forAll allWs $ \ x ->
--               trace ("trying x=" ++ show x) $
               forAll (genRhos rho xs) $ \ rho' ->
--                 trace ("trying rho'=" ++ show rho') $
                 let w1 = dM e1 (Just x) rho' in
--                 trace ("w1=" ++ show w1) $
--                 trace ("x `in` dom(f)=" ++ show (x `inDom` f)) $
--                 trace ("f(x)=" ++ if x `inDom` f then show (ap f x) else "NA") $
                 not (isEmpty w1)
                 `implies`
                (x `inDom` f && ap f x `sIn` dD e2 rho')
  guard $
            (q == Closed)
            `implies`
            (forAll allWs $ \ x ->
              (x `inDom` f) `implies`
                (exists (genRhos rho (dI e1)) (\ rho' -> not (isEmpty (dM e1 (Just x) rho'))))
            )
  unit vf
-}
{-
dM (Fun q e1 e2) Nothing rho = do
  vf@(VFcn f) <- allWs
  guard $
    forAll allWs $ \ x ->
      let rhos = dB' e1 (Just x) rho
      in  if isEmpty rhos then not (x `inDom` f) || q == Open
          else let l = commonLbls (fst <$> rhos) in
               x `inDom` f &&
               forAll rhos (\ (_, rho') -> ap f x `sIn` (preLbls l $ dD e2 rho'))
  unit vf
-}
dM (Fun q e1 e2) (Just u) rho | VFcn g <- u = do
  vf@(VFcn f) <- allWs
--  () <- trace ("trying f,g=" ++ show (f,g)) (return ())
  guard $
    forAll allWs $ \ x ->
--      trace ("trying x=" ++ show x)
      (
      ifEmpty
        (dB' e1 (Just x) rho)                -- possible ways x can match e1
        (not (x `inDom` f) || q == Open)     -- if none
        $ \ rhos ->                          -- if at least one
--             let l = commonLbls (fst <$> rhos) in
--             trace ("x in e1 " ++ show (rhos, x `inDom` f, e1)) $
             x `inDom` f &&
-- This needs to change.  With multiple rhos we should maybe
-- intersect all the (dL e2)s
             forAll rhos
                    (\ (_, rho') -> forAll (dM e1 (Just x) rho')
                                      (\ (l, x') ->
--                                         trace ("e1(x) x,l,x'=" ++ show (x, l, x')) $
                                         x' `inDom` g &&
                                         (
--                                         trace ("f(x)=" ++ show (ap f x)) $
--                                         trace ("g(x')=" ++ show (ap g x')) $
--                                         trace ("e2(g(x'))=" ++ show (dL e2 (Just (snd $ ap g x')) rho')) $
                                         ap f x `sIn` (preLbls l $ dL e2 (Just (snd $ ap g x')) rho')
                                         )
                                      )
                    )
--             && trace ("success u,f" ++ show (u,f,ee)) True
      )
  unit vf
                              | otherwise = empty

dM (Choice e1 e2) u rho =
  pre L (dL e1 u rho) `sunion` pre R (dL e2 u rho)
dM (All e) u rho = unit tup `isectM` u
  where tup =
          case Monad.mapM getSing $ sortLbl (dE e rho) of
            Nothing -> error "All: multivalued"
            Just xs -> VTup xs
dM (For _e1 _e2) _u _rho = error "for not implemented"

dM e Nothing rho = do  -- if nothing else matches then try all possible u
   u <- allWs
   dM e (Just u) rho

dD :: Exp -> Env -> WS
dD e rho = tryAll rho (dI e) (dE e)

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

dB' :: Exp -> Maybe Val -> Env -> Set (Lbls,Env)
dB' e u rho = do
  rho' <- genRhos rho (dI e)
  (l, _) <- dM e u rho'
  return (l, rho')


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
exp24 = All $ Colon $ Tup [Int 2, Int 3]

-- fun_c(x:=(0|1)){x}
--  denotation id01LR = { [0->L0, 1->R1] }
exp25 :: Exp
exp25 = Fun Closed (Def "x" (Choice (Int 0) (Int 1))) (Var "x")

exp26 :: Exp
exp26 = All $ Colon exp25

-- fun_c(x:=(1|0)){x}
--  denotation id01RL = { [0->R0, 1->L1] }
exp27 :: Exp
exp27 = Fun Closed (Def "x" (Choice (Int 1) (Int 0))) (Var "x")

exp28 :: Exp
exp28 = All $ Colon exp27

-- if (1 | 2){2}else{0}
exp29 :: Exp
exp29 = If (Int 1 `Choice` Int 2) (Int 2) (Int 0)

-- fun_c(0){1|2}
exp30 :: Exp
exp30 = Fun Closed (Int 0) (Int 1 `Choice` Int 2)

-- all{exp30[0]}
exp31 :: Exp
exp31 = All $ App exp30 (Int 0)

-- fun_c(() where y:=1|2) := if (y = 1) then (1, _) else (_, 2)
exp32 :: Exp
exp32 = Fun Closed (Def "y" (Choice (Int 1) (Int 2)) `Seq` Int 0)
                   (If (Var "y" `Equ` Int 1) (Tup [Int 1, cint]) (Tup [cint, Int 2]))
  where cint = Colon (Var "int")

-- (1, :int) = (:int, 2)
exp33 :: Exp
exp33 = Tup [Int 1, cint] `Equ` Tup [cint, Int 2]
  where cint = Colon (Var "int")

-- fun_c(x:=:int; :int){0}
exp34 :: Exp
exp34 = Fun Closed (Def "x" cint `Seq` cint) (Int 0)
  where cint = Colon (Var "int")

-- fun_c((x:=a; x:int) where a:int){x}
exp35 :: Exp
exp35 = Fun Closed (((Var "x" `Equ` Var "a") `Seq` Def "x" cint) `Where` Def "a" cint) (Var "x")
  where cint = Colon (Var "int")

-- fun_c(a:=0|1; x:=a){x}
exp36 :: Exp
exp36 = Fun Closed ((Def "a" (Int 0 `Choice` Int 1)) `Seq` (Def "x" (Var "a"))) (Var "x")

-- fun_c(x:=0|1|2){x}
exp37 :: Exp
exp37 = Fun Closed (Def "x" (Int 0 `Choice` Int 1 `Choice` Int 2)) (Var "x")

-- fun_c(x:=3|1|0){x}
exp38 :: Exp
exp38 = Fun Closed (Def "x" (Int 3 `Choice` Int 1 `Choice` Int 0)) (Var "x")

-- fun_c(x:=0|1|2){x} = fun_c(x:=3|1|0){x}
-- denotation {}
exp39 :: Exp
exp39 = exp37 `Equ` exp38

-- fun_c(a:=0|1; x:=if(a=0)(0|1|2)else(3|1|0)){x}
-- 0->L,LL0, 1->L,RL1, 2->L,R2, 3->R,LL3, 1->R,RL1, 0->R,R0
exp40 :: Exp
exp40 = Fun Closed (Def "a" (Int 0 `Choice` Int 1) `Seq`
                    Def "x" (If (Var "a" `Equ` Int 0)
                                (Int 0 `Choice` Int 1 `Choice` Int 2)
                                (Int 3 `Choice` Int 1 `Choice` Int 0)))
                   (Var "x")

allExps :: [Exp]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp23, exp24, exp26, exp28, exp29, exp31, exp33,
           exp34, exp35
          ]

refExps :: String
refExps = "[3,int,Wrong([(-,comparable),(-,int)]),succ,3,1,ho1,2,1,2,ho2,2,Wrong([]),Wrong([]),0,0,ho3,2,1,2,0,3,[1,2],[2,3],[0,1],[1,0],2,[1,2],[1,2],const0,int]"

allRes :: [RVal]
allRes = map dP allExps

allOK :: Bool
allOK = show allRes == refExps

_used :: [RVal]
_used = [ex1, ex2, ex3, ex4, ex5, ex6, ex8, ex9,
         ex10, ex11, ex12, ex13, ex14, ex15, ex16, ex17, ex18, ex19
        ]

dens :: [(Exp, WS)]
dens =
  [(exp25, sfn "id01LR"), (exp27, sfn "id01RL")
  ,(exp30, sfn "f0L1" `sunion` sfn "f0R2")
  ,(exp32, sfn "f0t12")
  ]
  where fn :: String -> Val
        fn s = VFcn $ Fcn s M.empty
        sfn :: String -> WS
        sfn = unit . fn

check :: IO ()
check = do
  if (all (\ (e, v) -> dE e rho0 == v) dens) then
    putStrLn "OK"
   else Prelude.do
    let f (e, v) = Prelude.do
          let v' = dE e rho0
          if v == v' then
            Prelude.return ()
           else
            Prelude.print (e, v, v')
    Prelude.mapM_ f dens

main :: IO ()
main = Prelude.do
  putStrLn "Start"
  if allOK then
    putStrLn "Success"
   else Prelude.do
    putStrLn "Failure:"
    putStrLn $ refExps
    putStrLn $ show allRes
