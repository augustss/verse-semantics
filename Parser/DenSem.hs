{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.DenSem where
import Control.Applicative((<|>))
import Control.Monad
import Data.List((\\), intersect)
import qualified Data.List as L
import Data.Maybe
import Parser.Expr(Ident(..), noLoc)
import Parser.Core
import Parser.Print(Pretty(..), prettyShow, text)
import GHC.Stack

import Debug.Trace

traceDen :: Bool
traceDen = False

traceInput :: Bool
traceInput = False

-- Try to limit the number of values to iterate over
forallHack :: Bool
forallHack = True

trace' :: String -> a -> a
trace' s a = if s==s then trace s a else undefined

denSem :: Core -> Core
denSem = denSem' . semSimp

semSimp :: Core -> Core
semSimp = f
  where
    f (CApplyVV (VLam x (CSeq [CApplyVV is@(VPrim "isInt$") (Var x'), CVar x''])) a) | x == x' && x == x'' =
      f $ CApplyVV is a
    f e = composOp f e

denSem' :: Core -> Core
denSem' e | traceInput && trace ("-----\ndenSem':\n" ++ prettyShow e ++ "\n-----") False = undefined
denSem' e = flat $ map (CValue . valueW) $ noAlts $ evalE emptyEnv e
  where
    flat [] = CFail
    flat [x] = x
    flat xs = cBar xs

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

instance Pretty LR where pPrintPrec _ _ = text . show

newtype S a = S [([LR], a)] deriving (Show)

instance Pretty a => Pretty (S a) where pPrintPrec l p (S x) = pPrintPrec l p x

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
isect (S s1) (S s2)  = S [ (l1 ++ l2,w1) | (l1,w1) <- s1, (l2,w2) <- s2, w1 == w2 ]

sequ :: S a -> S a -> S a
sequ (S s1) (S s2) = S [ (l1 ++ l2, w2) | (l1,_w1) <- s1, (l2,w2) <- s2 ]

sOne :: S W -> S W
sOne (S []) = empty
sOne ws = unit $ either Wrong head $ ssort ws

sAll :: S W -> W
sAll ws = either Wrong WTuple $ ssort ws

ssort :: (Pretty a) => S a -> Either String [a]
ssort sa = srt sa
  where
    srt (S []) = pure []
    srt (S ws) = do
      s <- notMany [w | ([],w) <- ws]
      ls <- srt (S [(l,w) | (L:l,w) <- ws ])
      rs <- srt (S [(l,w) | (R:l,w) <- ws ])
      pure $ s ++ ls ++ rs
    notMany [] = pure []
    notMany [x] = pure [x]
    notMany _ = Left $ "ssort: multiple unlabelled" ++ prettyShow sa

succeeds :: S W -> S W
succeeds (S [x]) = S [x]
succeeds _ = unit $ Wrong "succeeds"

noAlts :: S W -> [W]
noAlts = either error id . ssort

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

instance Pretty W where pPrintPrec l p = pPrintPrec l p . valueW

allW :: [W]
allW = allInts ++ allTuples ++ allFuncs

allInts :: [W]
allInts = [WInt i | i <- [0 .. 3]]

allTuples :: [W]
allTuples =
     [WTuple []]
  ++ [WTuple [w1,w2] | w1 <- allInts, w2 <- allInts]
  ++ [WTuple [w1,w2,w3] | w1 <- allInts, w2 <- allInts, w3 <- allInts]
  ++ [WTuple [w] | w <- constFuncs]
  ++ [WTuple [w1,w2] | w1 <- constFuncs, w2 <- constFuncs]
  ++ [WTuple [w1,w2,w3] | w1 <- constFuncs, w2 <- constFuncs, w3 <- constFuncs]

allFuncs :: [W]
allFuncs = fcns ++ constFuncs
  where
    fcns = map WFunction $ [wAdd, wGt, wMul, wDiv, wId, wInc, wDec, wDbl, wGt0, wIsInt, wFst, wSnd, wTy0, wAp, wDblW, wFail, w0'1, wHack, wTy01]
    wId  = func "id"  $ unit
    wInc = func "inc" $ \case WInt x -> unit (WInt (x+1)); _ -> empty
    wDec = func "dec" $ \case WInt x -> unit (WInt (x-1)); _ -> empty
    wDbl = func "dbl" $ \case WInt x -> unit (WInt (x*2)); _ -> empty
    wGt0 = func "gt0" $ \case WInt x | x > 0 -> unit (WInt x); _ -> empty
    wFst = func "fst" $ \case WTuple [w,_] -> unit w; _ -> empty
    wSnd = func "snd" $ \case WTuple [_,w] -> unit w; _ -> empty
    wTy0 = func "ty0" $ \case WInt 0 -> unit (WInt 0); _ -> empty
    wAp  = func "ap"  $ \case WTuple [f,a] -> apply f a; _ -> empty
    wDblW= func "dblW" $ \case WInt x -> unit (WInt (x*2)); _ -> unit (Wrong "wDblW")
    wFail= func "fail" $ const empty
    w0'1 = func "0->1" $ \case WInt 0 -> unit (WInt 1); _ -> empty
    wHack= func "0->1" $ \case WInt 0 -> unit (WInt 1); WInt 1 -> unit (WInt 2); _ -> empty
    wTy01= func "ty0|1" $ \case WInt x | x==0 || x==1 -> unit (WInt x); _ -> empty

constFuncs :: [W]
constFuncs = [ WFunction $ func ("const" ++ show i) (const (unit w)) | w@(WInt i) <- allInts ]

data Func a b = Func !String !(a -> b)
instance Show (Func a b) where show (Func s _) = "Func:" ++ s
func :: String -> (a -> b) -> Func a b
func = Func
apFunc :: Func a b -> a -> b
apFunc (Func _ f) a = f a

instance Eq (Func W (S W)) where
  f@(Func sf _) == g@(Func sg _) =
    -- trace' ("eq " ++ show (sf, sg)) $
    sf == sg ||    -- compare function names
    and [eqs (apFunc f w) (apFunc g w) | w <- allW ]
    where
      eqs ws1 ws2 | length x1 == length x2 = and $ zipWith eq x1 x2
                  | otherwise = False
        where x1 = noAlts ws1
              x2 = noAlts ws2
      eq WFunction{} WFunction{} = True  -- pretend all function are equal
      eq w1 w2 = w1 == w2

type Env = [(Ident, W)]
ext :: Ident -> W -> Env -> Env
ext x w r = (x, w) : r
exts :: Env -> [(Ident, W)] -> Env
exts = foldr (uncurry ext)
emptyEnv :: Env
emptyEnv = []
lookEnv :: HasCallStack => Env -> Ident -> W
lookEnv r i = fromMaybe (error $ "lookEnv: " ++ show i) $ lookup i r

evalE :: Env -> Core -> S W
evalE r e | traceDen =
  let ws = evalE' r e
      msg = "evalE: " ++ prettyShow (e, ws)
  in  if msg==msg then trace msg ws else undefined
evalE r e = evalE' r e

evalE' :: Env -> Core -> S W
evalE' r (CValue v) = unit $ evalV r v
evalE' r (CBar e1 e2) = evalE r e1 `union` evalE r e2
evalE' _ CFail = empty
evalE' r (CUnify e1 e2) =
  --trace' ("unify " ++ prettyShow (e1, e2, evalE r e1, evalE r e2)) $
  evalE r e1 `isect` evalE r e2
evalE' r (CSeq [e]) = evalE r e
evalE' r (CSeq (e: es)) = evalE r e `sequ` evalE r (CSeq es)
evalE' r (CApplyVV v1 v2) = apply (evalV r v1) (evalV r v2)
evalE' r (CDef [] e) = evalE r e
evalE' r (CDef (x:xs) e) = unions [ evalE (ext x w r) (CDef xs e) | w <- ws ]
  where ws = possibleValues xs r x e
evalE' r (COne e) = sOne (evalE r e)
evalE' r (CAll e) =
--  trace' ("CAll " ++ prettyShow (e, evalE r e)) $
  unit (sAll (evalE r e))
evalE' r (CSucceeds e) = succeeds $ evalE r e
evalE' r (CLambda i is cov e0 e1) = aset
  [ f | f <- allFuncs,
    forAll (1 + length is) $ \ ws@(w:_) ->
      let r' = exts r (zip (i:is) ws)
--      trace ("CLambda " ++ prettyShow (f, w, e0, evalE r' e0, nonEmpty (evalE r' e0), evalE r' e1, getSing (apply f w), maybe False (\ z -> z `member` evalE r' e1 ) (getSing (apply f w)))) $
          cond1 = nonEmpty (evalE r' e0)
          cond2 = maybe False (\ z -> z `member` evalE r' e1 ) (getSing (apply f w))
      in
      if cov then
        if cond1 then cond2 else isNothing (getSing (apply f w))
      else cond1 `implies` cond2
  ]
evalE' _ e = error $ "evalE " ++ prettyShow e

evalV :: Env -> Value -> W
evalV r (Var i) = lookEnv r i
evalV _ (HNF (HInt i)) = WInt i
evalV r f@(VLam x e) = WFunction $ func ("(" ++ prettyShow f' ++ ")") $ \ w -> evalE (ext x w r) e
  where f' = foldr (\ (i, w) c -> substV i (valueW w) c) f r
evalV r (VArray vs) = WTuple $ map (evalV r) vs
evalV _ (VPrim "in'+'") = WFunction wAdd
evalV _ (VPrim "in'>'") = WFunction wGt
evalV _ (VPrim "in'*'") = WFunction wMul
evalV _ (VPrim "in'/'") = WFunction wDiv
evalV _ (VPrim "isInt$") = WFunction wIsInt
evalV _ (VPrim "mapAp$") = WFunction wMapAp
evalV _ (VPrim "pre'+'") = WFunction wIsInt
evalV _ v = error $ "evalV: " ++ prettyShow v

possibleValues :: [Ident] -> Env -> Ident -> Core -> [W]
--possibleValues is _ i ee | trace ("possibleValues " ++ prettyShow (i, is, ee)) False = undefined
possibleValues is r i ee | not forallHack = allW
                         | otherwise = maybe allW (vals . evalE r . hacking) (get (i:is) ee)
  where
    hacking e = {-trace ("traceHack " ++ prettyShow (i, e))-} e
    vals (S xs) = map snd xs
    get xs (CUnify (CVar i') e) | i == i' && null (intersect (fvs e) xs) = Just e
    get xs (CUnify e1 e2) = get xs e1 <|> get xs e2
    get xs (CSeq es) = foldr (<|>) Nothing $ map (get xs) es
    get xs (CDef vs e) = get (vs ++ xs) e
    get _ _ = Nothing

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

-- XXX Hack
wMapAp :: Func W (S W)
wMapAp = func "mapAp$" (\ _ -> error "wMapAp called")

apply :: W -> W -> S W
apply WInt{} _ = unit (Wrong "apply WInt")
apply (WTuple vs) w = bars $ zipWith one [0..] vs
  where
    one :: Integer -> W -> S W
    one i v = if w == WInt i then unit v else empty
    bars :: [S W] -> S W
    bars [] = empty
    bars [x] = x
    bars (x:xs) = x `union` bars xs
apply (WFunction (Func "mapAp$" _)) w =
  case w of
    WTuple fs -> unit $ WTuple $ map one fs
    _ -> undefined
  where
    one f =
      case apply f (WTuple []) of
        S [([], x)] -> x
        x -> error $ "mapAp$: " ++ prettyShow x
apply (WFunction f) w = apFunc f w
apply (Wrong s) _ = unit (Wrong $ "apply Wrong " ++ s)

forAll :: Int -> ([W] -> Bool) -> Bool
forAll n p = all p $ replicateM n allW

implies :: Bool -> Bool -> Bool
implies = (<=)
