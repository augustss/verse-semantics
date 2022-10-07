{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module DenSem where
import Control.Monad
import Data.List((\\))
import qualified Data.List as L
import Expr(Ident(..), noLoc)
import Core
import Print(prettyShow)

import Debug.Trace

denSem :: Core -> [Core]
denSem e | trace ("denSem: " ++ prettyShow e) False = undefined
denSem e = map (CValue . valueW) $ noAlts $ evalE emptyEnv e

valueW :: W -> Value
valueW (WInt i) = HNF (HInt i)
valueW (WTuple ws) = VArray (map valueW ws)
valueW (Wrong s) = Var (Ident noLoc ("WRONG: " ++ s))
valueW (WFunction f) = Var (Ident noLoc $ show f)
--valueW w = error $ "valueW " ++ show w

pattern CHasType :: Core -> Core -> Core
pattern CHasType e1 e2 <- CMacro (Ident _ "hastype") (CSeq [e1, e2])

newVars :: String -> [Ident] -> [Ident]
newVars s vs = [ Ident noLoc $ "$" ++ s ++ show n | n <- [0::Int ..] ] \\ vs

#if 0
type S a = [a]

unit :: a -> S a
unit a = [a]

empty :: S a
empty = []

union :: S a -> S a -> S a
union s1 s2 = s1 ++ s2

unions :: [S a] -> S a
unions ss = concat ss

isect :: Eq a => S a -> S a -> S a
isect s1 s2 = [ w1 | w1 <- s1, w2 <- s2, w1 == w2 ]

sequ :: S a -> S a -> S a
sequ s1 s2 = [ w2 | _w1 <- s1, w2 <- s2 ]

sOne :: S a -> S a
sOne [] = []
sOne (a:_) = [a]

sAll :: S W -> W
sAll ws = WTuple ws

succeeds :: S W -> S W
succeeds [x] = [x]
succeeds _ = [Wrong "succeeds"]

noAlts :: S W -> [W]
noAlts w = w

#else
data LR = L | R deriving (Eq, Ord, Show)

newtype S a = S [([LR], a)]

unit :: a -> S a
unit a = S [([],a)]

empty :: S a
empty = S []

nonEmpty :: S a -> Bool
nonEmpty (S []) = False
nonEmpty _ = True

member :: (Eq a) => a -> S a -> Bool
member x (S xs) = any ((x ==) . snd) xs

getSing :: S a -> Maybe a
getSing (S [(_, x)]) = Just x
getSing _ = Nothing

aset :: [a] -> S a
aset as = S [([], a) | a <- as]

union :: S a -> S a -> S a
union (S s1) (S s2) = S $ [(L:l,w) | (l,w) <- s1] ++ [(R:l,w) | (l,w) <- s2]

unions :: (Eq a) => [S a] -> S a
unions ss = S $ foldr (\ (S s) -> L.union s) [] ss

isect :: Eq a => (Eq a) => S a -> S a -> S a
isect (S s1) (S s2)  = S [ (l1,w1) | (l1,w1) <- s1, (_l2,w2) <- s2, w1 == w2 ]

sequ :: S a -> S a -> S a
sequ (S s1) (S s2) = S [ (l1 ++ l2, w2) | (l1,_w1) <- s1, (l2,w2) <- s2 ]

sOne :: S a -> S a
sOne (S []) = empty
sOne ws = unit $ head $ ssort ws

sAll :: S W -> W
sAll ws = WTuple $ ssort ws

ssort :: S a -> [a]
ssort (S []) = []
ssort (S ws) = [w | ([],w) <- ws ] ++
               ssort (S [(l,w) | (L:l,w) <- ws ]) ++
               ssort (S [(l,w) | (R:l,w) <- ws ])

succeeds :: S W -> S W
succeeds (S [x]) = S [x]
succeeds _ = unit $ Wrong "succeeds"

noAlts :: S W -> [W]
noAlts = ssort

eqSet :: (Eq a) => S a -> S a -> Bool
eqSet s1 s2 = subset s1 s2 && subset s2 s1

subset :: (Eq a) => S a -> S a -> Bool
subset (S s1) (S s2) = null (s1 \\ s2)

#endif

data W
  = WInt Integer
  | WTuple [W]
  | WFunction (Func W (S W))
  | Wrong String
  deriving (Eq, Show)

allW :: [W]
allW = allInts ++ allTuples ++ allFuncs

allInts :: [W]
allInts = [WInt i | i <- [0 .. 3]]

allTuples :: [W]
allTuples = [WTuple []] ++ [WTuple [w1,w2] | w1 <- allInts, w2 <- allInts]

allFuncs :: [W]
allFuncs = fcns
  where
    fcns = map WFunction $ [wAdd, wGt, wMul, wDiv, wId, wInc, wGt0, wIsInt, wFst, wSnd, wTy0] ++
                           [ func ("const" ++ show i) (const (unit w)) | w@(WInt i) <- allInts ]
    wId  = func "id"  $ unit
    wInc = func "inc" $ \case WInt x -> unit (WInt (x+1)); _ -> empty
    wGt0 = func "gt0" $ \case WInt x | x > 0 -> unit (WInt x); _ -> empty
    wFst = func "fst" $ \case WTuple [w,_] -> unit w; _ -> empty
    wSnd = func "snd" $ \case WTuple [_,w] -> unit w; _ -> empty
    wTy0 = func "ty0" $ \case WInt 0 -> unit (WInt 0); _ -> empty

data Func a b = Func !String !(a -> b)
instance Show (Func a b) where show (Func s _) = "Func:" ++ s
func :: String -> (a -> b) -> Func a b
func = Func
apFunc :: Func a b -> a -> b
apFunc (Func _ f) a = f a

instance Eq (Func W (S W)) where
  f@(Func sf _) == g@(Func sg _) =
    sf == sg ||
    and [eqs (apFunc f w) (apFunc g w) | w <- allW ]
    where
      eqs ws1 ws2 | length x1 == length x2 = and $ zipWith eq x1 x2
                  | otherwise = False
        where x1 = noAlts ws1
              x2 = noAlts ws2
      eq WFunction{} WFunction{} = True  -- pretend all function are equal
      eq w1 w2 = w1 == w2

type Env = Ident -> W
ext :: Ident -> W -> Env -> Env
ext x w r = \ x' -> if x == x' then w else r x'
exts :: Env -> [(Ident, W)] -> Env
exts = foldr (uncurry ext)
emptyEnv :: Env
emptyEnv x = error $ "emptyEnv " ++ show x

evalE :: Env -> Core -> S W
evalE r (CValue v) = unit $ evalV r v
evalE r (CBar e1 e2) = evalE r e1 `union` evalE r e2
evalE _ CFail = empty
evalE r (CUnify e1 e2) = evalE r e1 `isect` evalE r e2
evalE r (CSeq [e]) = evalE r e
evalE r (CSeq (e: es)) = evalE r e `sequ` evalE r (CSeq es)
evalE r (CApply v1 v2) = apply (evalV r v1) (evalV r v2)
evalE r (CDef [] e) = evalE r e
evalE r (CDef (x:xs) e) = unions [ evalE (ext x w r) (CDef xs e) | w <- allW ]
evalE r (COne e) = sOne (evalE r e)
evalE r (CAll e) = unit (sAll (evalE r e))
evalE r (CSucceeds e) = succeeds $ evalE r e
evalE r (CLambda i is e0 e1) = aset
  [ f | f <- allFuncs,
    forAll (1 + length is) $ \ ws@(w:_) ->
      let r' = exts r (zip (i:is) ws) in
      nonEmpty (evalE r' e0) `implies`
      maybe False (\ z -> z `member` evalE r' e1 ) (getSing (apply f w))
  ]
evalE _ e = error $ "evalE " ++ prettyShow e

evalV :: Env -> Value -> W
evalV r (Var i) = r i
evalV _ (HNF (HInt i)) = WInt i
evalV r f@(VLam x e) = WFunction $ func ("(" ++ prettyShow f ++ ")") $ \ w -> evalE (ext x w r) e
evalV r (VArray vs) = WTuple $ map (evalV r) vs
evalV _ (VPrim "in'+'") = WFunction wAdd
evalV _ (VPrim "in'>'") = WFunction wGt
evalV _ (VPrim "in'*'") = WFunction wMul
evalV _ (VPrim "in'/'") = WFunction wDiv
evalV _ (VPrim "isInt$") = WFunction wIsInt
evalV _ _ = undefined

wAdd :: Func W (S W)
wAdd = func "add" f
  where f (WTuple [WInt x, WInt y]) = unit $ WInt $ x+y
        f _ = empty

wGt :: Func W (S W)
wGt = func "gt" f
  where f (WTuple [WInt x, WInt y]) | x > y = unit $ WInt x
        f _ = empty

wMul :: Func W (S W)
wMul = func "mul" f
  where f (WTuple [WInt x, WInt y]) = unit $ WInt $ x*y
        f _ = empty

wDiv :: Func W (S W)
wDiv = func "div" f
  where f (WTuple [WInt x, WInt y]) | y /= 0 = unit $ WInt $ x `div` y
        f _ = empty

wIsInt :: Func W (S W)
wIsInt = func "isInt" f
  where f (WInt x) = unit $ WInt x
        f _ = empty

apply :: W -> W -> S W
apply WInt{} _ = unit (Wrong "apply WInt")
apply WTuple{} _ = unit (Wrong "apply WTuple") -- XXX
apply (WFunction f) w = apFunc f w
apply (Wrong s) _ = unit (Wrong $ "apply Wrong " ++ s)

forAll :: Int -> ([W] -> Bool) -> Bool
forAll n p = all p $ replicateM n allW

implies :: Bool -> Bool -> Bool
implies = (<=)
