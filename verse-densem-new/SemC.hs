{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE QualifiedDo #-}

-- Use do notation for the set expressions.
-- Also, combine dE and dM in an efficient way.

module Main where
import Prelude(Show(..), Ord(..), Eq(..), Num(..), Integral(..),
               Bool(..), String, IO, Integer,
               sequence, error, uncurry, undefined, showString, traverse,
               ($), (.), not, (&&), (||), otherwise, putStrLn,
               )
import qualified Prelude
import qualified Control.Monad as Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
--import Debug.Trace
import GHC.Stack
import Exp
import Examples

implies :: Bool -> Bool -> Bool
implies x y = not x || y

--------------------
---- Because of RebindableSyntax

ifThenElse :: Bool -> a -> a -> a
ifThenElse False _ x = x
ifThenElse True  x _ = x

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
  showsPrec _ (Wrong _) = showString $ "Wrong"

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

unLbl :: (Lbls, W) -> W
unLbl (_, w) = w

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
        (dB e1 (Just x) rho)                -- possible ways x can match e1
        (not (x `inDom` f) || q == Open)     -- if none
        $ \ rhos ->                          -- if at least one
--             let l = commonLbls (fst <$> rhos) in
--             trace ("x in e1 " ++ show (rhos, x `inDom` f, e1)) $
             x `inDom` f &&
-- This needs to change.  With multiple rhos we should maybe
-- intersect all the (dL e2)s
             forAll rhos
                    (\ rho' -> forAll (dM e1 (Just x) rho')
                                      (\ (l, x') ->
--                                         trace ("e1(x) x,l,x'=" ++ show (x, l, x')) $
                                         x' `inDom` g &&
                                         (
--                                         trace ("f(x)=" ++ show (ap f x)) $
--                                         trace ("g(x')=" ++ show (ap g x')) $
--                                         trace ("e2(g(x'))=" ++ show (dL e2 (Just (snd $ ap g x')) rho')) $
                                         ap f x `sIn` (preLbls l $ dL e2 (Just (unLbl $ ap g x')) rho')
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


allExps :: [Example]
allExps = [exp1, exp2, exp3, exp4, exp5, exp6, exp7, exp8, exp9,
           exp10, exp11, exp12, exp13, exp14, exp15, exp16, exp17, exp18, exp19,
           exp20, exp21, exp22, exp23, exp24, exp25, exp26, exp27, exp28, exp29, exp30, exp31, exp33,
           exp34, exp35
          ]

-- broken: exp32

{-
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
-}

main :: IO ()
main = Prelude.do
  putStrLn "Start"
  runExamples dP allExps
