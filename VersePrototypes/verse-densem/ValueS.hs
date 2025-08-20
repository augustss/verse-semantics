{-# OPTIONS_GHC -Wno-x-partial #-}
module ValueS(
  Z,
  Ident, fresh, freshList,
  Value(..), FUN, PartialFun, applyPF,
  numInt,
  allInts, allFUNs, allValues,
  allInts',
  funNegate, funInt, funGt, funLt,
  funAdd, funSub, funMul, funDiv,
  allTuples, allTuplesLen,
  ) where
import Control.Monad
import Data.List((\\), intercalate)
import qualified Map as M
import FrontEnd.Expr(Ident(..), noLoc)

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
  | Tuple [Value]             -- temporary, until we do it with functions
 deriving ( Eq, Ord )

type FUN = [PartialFun]

instance Show Value where
  show (Int k)   = show k
  show (Fun fun) = show fun
  show (Tuple vs) = "<" ++ intercalate "," (map show vs) ++ ">"

data PartialFun = PF String (M.Map Value Value)
-- deriving ( Eq, Ord )

instance Eq PartialFun where
  PF f1 _ == PF f2 _  =  f1 == f2

instance Ord PartialFun where
  PF f1 _ <= PF f2 _  =  f1 <= f2
  PF f1 _ <  PF f2 _  =  f1 <  f2
  PF f1 _ >= PF f2 _  =  f1 >= f2
  PF f1 _ >  PF f2 _  =  f1 >  f2
  PF f1 _ `compare` PF f2 _  =  f1 `compare` f2

instance Show PartialFun where
  show (PF s _m) = s -- show m

applyPF :: PartialFun -> Value -> Maybe Value
applyPF (PF _ m) a = M.lookup a m

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

allFUNs :: [FUN]
allFUNs = [ [funNegate], [funInt], [funGt], [funLt], [funAdd], [funSub], [funMul], [funDiv] ]

-- Integers and pairs of integers
allValues :: [Value]
allValues = allInts ++ map Tuple (allTuplesLen 2)

maxTuples :: Int
maxTuples = 2

-- Just tuples of ints for now,
-- and just 0,1,2-tuples
allTuples :: [Value]
allTuples = concatMap (map Tuple . allTuplesLen) [0..maxTuples]

allTuplesLen :: Int -> [[Value]]
allTuplesLen = allTuplesLen' allTupleElems

allTuplesLen' :: [Value] -> Int -> [[Value]]
allTuplesLen' els n | n < 0 || n > maxTuples = error $ "allTuplesLen: bad " ++ show n
                    | otherwise = replicateM n els

allTupleElems :: [Value]
allTupleElems = allInts
-- TOO SLOW                ++ map Tuple (allTuplesLen' allInts 2)   -- all pairs on ints
