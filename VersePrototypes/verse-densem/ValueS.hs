{-# OPTIONS_GHC -Wno-x-partial #-}
module ValueS(
  Ident, fresh, freshList,
  Value(..), FUN, PartialFun,
  numInt,
  funNegate, funInt,
  ) where
import Data.List((\\))
import qualified Data.Map as M
import FrontEnd.Expr(Ident(..), noLoc)

freshList :: [Ident] -> [Ident]
freshList xs = ys \\ xs
 where
  ys = [ Ident noLoc ("z" ++ show i) | i <- [1 :: Integer ..] ]

fresh :: [Ident] -> Ident
fresh xs = head (freshList xs)

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

-----------------------------

numInt :: Integer
numInt = 4

allInts :: [Value]
allInts = map Int [0 .. numInt-1]

funNegate :: PartialFun
funNegate = PF $ M.fromList [(i, Int ((-k) `mod` numInt)) | i@(Int k) <- allInts ]

funInt :: PartialFun
funInt = PF $ M.fromList [(i, i) | i <- allInts]
