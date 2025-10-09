{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
module ValueP(
  Z,
  Ident, fresh, freshList,
  Value(..), FUN,
  pattern Tuple,
  PartialFun(..), applyPF, fun, domPF, mkPFSet, mkPFList,
  numInt,
  allInts, allPFs, allFUNs, allValues, allValuesSet,
  allInts',
  funNegate, funInt, funGt, funLt,
  funAdd, funSub, funMul, funDiv,
  funEmpty,
  funDomain,
  funUnion, funConcat, tupConcat,
  allTuples, allTuplesLen, allTuplesLenV,
  tupleLen,
  ) where
import Control.Monad
import Data.List((\\), intercalate)
import qualified Map as M
import FrontEnd.Expr(Ident(..), noLoc)
import PomSet
import qualified Set

default ()

-- Use Int for speed
type Z = Int

-------------------------------

freshList :: String -> [Ident] -> [Ident]
freshList s xs = ys \\ xs
 where
  ys = [ Ident noLoc (s ++ {-"_" ++-} i) | i <- "" : map show [1 :: Integer ..] ]

fresh :: String -> [Ident] -> Ident
fresh s xs = head (freshList s xs)

-------------------------------

data Value
  = Int Z
  | Fun FUN
 deriving ( Eq, Ord )

instance Show Value where
  show (Int k)   = show k
  show (Tuple vs) = "<" ++ intercalate "," (map show vs) ++ ">"
  show (Fun fn) = "F(" ++ showFUN fn ++ ")"

-- Invariant: no overlapping domains among the partial functions
type FUN = P PartialFun

showFUN :: FUN -> String
showFUN Empty = "{}"
showFUN (Unit f) = show f
showFUN (f1 :\/ f2) = showFUN f1 ++ " u " ++ showFUN f2
showFUN (f1 :++ f2) = showFUN f1 ++ " ++ " ++ showFUN f2

pattern Tuple :: [Value] -> Value
pattern Tuple vs <- (getTuple -> Just vs)
  where
    Tuple xs = Fun $ fun $ zipWith (PFSing . Int) [0..] xs

getTuple :: Value -> Maybe [Value]
getTuple (Int _) = Nothing
getTuple (Fun p) =
  case canon p of
    st | Set.isEmpty st -> Just []
       | Just sq <- Set.getSing st,
         Just xys <- mapM getPFSing sq,
         map fst xys == map Int [0..length sq-1] -> Just $ map snd xys
       | otherwise -> Nothing

data PartialFun = PF { pfName :: String, pfMap :: M.Map Value Value }
-- deriving ( Show )

{-
instance Eq PartialFun where
  PF f1 _ == PF f2 _  =  f1 == f2

instance Ord PartialFun where
  PF f1 _ <= PF f2 _  =  f1 <= f2
  PF f1 _ <  PF f2 _  =  f1 <  f2
  PF f1 _ >= PF f2 _  =  f1 >= f2
  PF f1 _ >  PF f2 _  =  f1 >  f2
  PF f1 _ `compare` PF f2 _  =  f1 `compare` f2
-}
instance Eq PartialFun where
  PF _ f1 == PF _ f2  =  f1 == f2

instance Ord PartialFun where
  PF _ f1 <= PF _ f2  =  f1 <= f2
  PF _ f1 <  PF _ f2  =  f1 <  f2
  PF _ f1 >= PF _ f2  =  f1 >= f2
  PF _ f1 >  PF _ f2  =  f1 >  f2
  PF _ f1 `compare` PF _ f2  =  f1 `compare` f2

instance Show PartialFun where
  show (PF s _m) = s -- show m

applyPF :: PartialFun -> Value -> Maybe Value
applyPF (PF _ m) a = M.lookup a m

domPF :: PartialFun -> Set.Set Value
domPF (PF _ m) = Set.mkSetUnsafe $ M.keys m

-- Single point mapping
pattern PFSing :: Value -> Value -> PartialFun
pattern PFSing x y <- (getPFSing -> Just (x, y))
  where PFSing x y = PF (show (show x ++ "->" ++ show y)) (M.fromList [(x, y)])

getPFSing :: PartialFun -> Maybe (Value, Value)
getPFSing (PF _ m) =
  case M.toList m of
    [xy] -> Just xy
    _    -> Nothing

fun :: [PartialFun] -> FUN
fun = foldr (+++) Empty . map unit

-----------------------------

numInt :: Z
numInt = 4

allInts' :: [Int]
allInts' = [0 .. fromIntegral numInt - 1 ]

allInts'' :: [Z]
allInts'' = [0 .. numInt - 1 ]

allInts :: [Value]
allInts = map Int [0 .. numInt-1]

funNegate :: PartialFun
funNegate = PF "neg" $ M.fromList [(i, Int ((-k) `mod` numInt)) | i@(Int k) <- allInts ]

funInt :: PartialFun
funInt = PF "int" $ M.fromList [(i, i) | i <- allInts]

funGt :: PartialFun
funGt = PF "gt" $ M.fromList [(Tuple [i, j], i) | i <- allInts, j <- allInts, i > j ]

funLt :: PartialFun
funLt = PF "lt" $ M.fromList [(Tuple [i, j], i) | i <- allInts, j <- allInts, i < j ]

funAdd :: PartialFun
funAdd = PF "add" $ M.fromList [(Tuple [Int i, Int j], Int ((i+j) `mod` numInt)) | i <- allInts'', j <- allInts'' ]

funSub :: PartialFun
funSub = PF "sub" $ M.fromList [(Tuple [Int i, Int j], Int ((i-j) `mod` numInt)) | i <- allInts'', j <- allInts'' ]

funMul :: PartialFun
funMul = PF "mul" $ M.fromList [(Tuple [Int i, Int j], Int ((i*j) `mod` numInt)) | i <- allInts'', j <- allInts'' ]

funDiv :: PartialFun
funDiv = PF "div" $ M.fromList [(Tuple [Int i, Int j], Int (i`div`j)) | i <- allInts'', j <- allInts'', j /= 0 ]

funSucc :: PartialFun
funSucc = PF "succ" $ M.fromList [(Int i, Int ((i+1) `mod` numInt)) | i <- allInts'']

funPred :: PartialFun
funPred = PF "pred" $ M.fromList [(Int i, Int ((i-1) `mod` numInt)) | i <- allInts'']

funNegSucc :: PartialFun
funNegSucc = PF "negsucc" $ M.fromList [(Int i, Int ((-i+1) `mod` numInt)) | i <- allInts'']

funEmpty :: PartialFun
funEmpty = PF "empty" M.empty

{-
funX12_3 :: PartialFun
funX12_3 = PF "x12_3" $ M.fromList [(Int 1, Int 3), (Int 2, Int 3)]
funX0_1 :: PartialFun
funX0_1  = PF "x0_1"  $ M.fromList [(Int 0, Int 1)]
funXF :: FUN
funXF = fun[funX12_3, funX0_1]
-}

allPFs :: [PartialFun]
allPFs =
         [funNegate, funInt, funGt, funLt, funAdd, funSub, funMul, funDiv
         ,funSucc, funPred, funNegSucc]
         ++
         -- All int->int functions with singleton domains.
         -- Needed to make tuples.
         [ mkPFList' [(x, y)]
         | x <- allInts, y <- allInts ]
         ++
         -- All int->int functions with singleton range
         [ PF ("K" ++ show y) $ M.fromList [(x, y) | x <- allInts]
         | y <- allInts ]
         ++
         -- The {0,1,2}->{0} function
         [ mkPFList' [(Int 0, Int 0), (Int 1, Int 0), (Int 2, Int 0)] ]
         ++
         -- All {0,1}->{0,1} functions
         fun01s
         ++
         -- Sample HO functions
         [ho01 0, ho01 1]
         ++
         -- {<1>}->{2} function
         [ mkPFList' [(Tuple [Int 1], Int 2)] ]

fun01s :: [PartialFun]
fun01s =
         [ mkPFList' [(Int 0, Int 0), (Int 1, Int 0)]
         , mkPFList' [(Int 0, Int 1), (Int 1, Int 0)]
         , mkPFList' [(Int 0, Int 0), (Int 1, Int 1)]
         , mkPFList' [(Int 0, Int 1), (Int 1, Int 1)]
         ]

ho01 :: Int -> PartialFun
ho01 r = mkPFList' [(Fun (Unit f), Int r) | f <- fun01s ]

mkPFSet :: Set.Set (Value, Value) -> PartialFun
mkPFSet = mkPFList . Set.toList

-- Try to find a named function
mkPFList :: [(Value, Value)] -> PartialFun
mkPFList pairs =
  let m = M.fromList pairs
  in  head $ filter (\ pf -> pfMap pf == m) allPFs ++ [mkPFList' pairs]

mkPFList' :: [(Value, Value)] -> PartialFun
mkPFList' pairs = PF { pfName = showPairs pairs, pfMap = M.fromList pairs }

showPairs :: [(Value, Value)] -> String
showPairs xys = "{" ++ intercalate "," (map (\ (x,y) -> show x ++ "->" ++ show y) xys) ++ "}"

allFUNs :: [FUN]
allFUNs = -- [ fun[funNegate], fun[funInt], fun[funGt], fun[funLt], fun[funAdd], fun[funSub], fun[funMul], fun[funDiv] ]
          [ fun [f] | f <- allPFs ]
          ++
          [ f | Fun f <- allTuples ]

-- Integers and pairs of integers
allValues :: [Value]
allValues = allInts ++ map Tuple (allTuplesLen 2)
            ++ [ Fun (unit f) | f <- allPFs ]     -- all single lane functions

allValuesSet :: Set.Set Value
allValuesSet = Set.mkSetUnsafe allValues

maxTuples :: Int
maxTuples = 2

tupleLen :: Value -> Int
tupleLen (Tuple xs) = length xs
tupleLen _ = error "not a Tuple"

-- Just tuples of ints for now,
-- and just 0,1,2-tuples
allTuples :: [Value]
allTuples = concatMap allTuplesLenV [0..maxTuples]

allTuplesLen :: Int -> [[Value]]
allTuplesLen = allTuplesLen' allTupleElems

allTuplesLenV :: Int -> [Value]
allTuplesLenV = map Tuple . allTuplesLen' allTupleElems

allTuplesLen' :: [Value] -> Int -> [[Value]]
allTuplesLen' els n | n < 0 || n > maxTuples = error $ "allTuplesLen: bad " ++ show n
                    | otherwise = replicateM n els

allTupleElems :: [Value]
allTupleElems = --[Int 0, Int 1]
                allInts
-- TOO SLOW                ++ map Tuple (allTuplesLen' allInts 2)   -- all pairs on ints

-- concatenate tuples represented as functions.
tupConcat :: FUN -> FUN -> FUN
tupConcat x y = uncanon [ appTup xs ys | xs <- ne $ canon x, ys <- ne $ canon y ]
  where appTup :: [PartialFun] -> [PartialFun] -> [PartialFun]
        appTup xs ys = xs ++ map (shiftPF (length xs)) ys
        ne s | Set.isEmpty s = Set.singleton []
             | otherwise = s

{-
shift :: Int -> FUN -> FUN
shift o = fmap (shiftPF o)
-}

shiftPF :: Int -> PartialFun -> PartialFun
shiftPF o (PFSing (Int i) x) = PFSing (Int (i+o)) x
shiftPF _ _ = error "shiftPF: not a singleton tuple"

{-
domCheck :: String -> FUN -> FUN
domCheck msg f =
  let xs = concat . allLeaves . fmap (M.keys . pfMap) $ f
  in  if xs == nub xs then f else error $ "domCheck: " ++ msg ++ ": " ++ show f
-}

-- The FUN invariant makes the mkSetUnsafe safe
funDomain :: FUN -> Set.Set Value
funDomain = Set.mkSetUnsafe . concat . allLeaves . fmap (M.keys . pfMap)

funUnion :: FUN -> FUN -> FUN
funUnion s t | Set.isEmpty (Set.intersect (funDomain s) (funDomain t)) = s `union` t
             | otherwise = Empty

funConcat :: FUN -> FUN -> FUN
funConcat s t | Set.isEmpty (Set.intersect (funDomain s) (funDomain t)) = s +++ t
              | otherwise = Empty
