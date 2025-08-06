{-# OPTIONS_GHC -Wno-x-partial #-}
module ValueS(
  Ident, fresh, freshList,
  Value(..), FUN, PartialFun, applyPF,
  numInt,
  allInts, allFUNs, allValues,
  allInts',
  funNegate, funInt,
  ) where
import Data.List((\\))
import qualified Data.Map as M
import FrontEnd.Expr(Ident(..), noLoc)

freshList :: String -> [Ident] -> [Ident]
freshList s xs = ys \\ xs
 where
  ys = [ Ident noLoc (s ++ "_" ++ i) | i <- "" : map show [1 :: Integer ..] ]

fresh :: String -> [Ident] -> Ident
fresh s xs = head (freshList s xs)

-------------------------------

data Value
  = Int Integer
  | Fun FUN
 deriving ( Eq, Ord )

type FUN = [PartialFun]

instance Show Value where
  show (Int k)   = show k
  show (Fun fun) = show fun

newtype PartialFun = PF (M.Map Value Value)
 deriving ( Eq, Ord )

instance Show PartialFun where
  show (PF m) = show m

applyPF :: PartialFun -> Value -> Maybe Value
applyPF (PF m) a = M.lookup a m

-----------------------------

numInt :: Integer
numInt = 4

allInts' :: [Int]
allInts' = [0 .. fromInteger numInt - 1 ]

allInts :: [Value]
allInts = map Int [0 .. numInt-1]

funNegate :: PartialFun
funNegate = PF $ M.fromList [(i, Int ((-k) `mod` numInt)) | i@(Int k) <- allInts ]

funInt :: PartialFun
funInt = PF $ M.fromList [(i, i) | i <- allInts]

allFUNs :: [FUN]
allFUNs = [ [funNegate], [funInt] ]

-- Just ints for now
allValues :: [Value]
allValues = allInts
