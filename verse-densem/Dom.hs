module Dom(
  Value(..),
  (:->?)(..),
  )where

import Data.List( intercalate )

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun [Value :->? Value]
 deriving ( Ord, Eq )

instance Show Value where
  show (Int n)  = show n
  show (Fun fs) = show fs

instance Num Value where
  fromInteger n = Int n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

----------------------------------------------------------------------------------------

data a :->? b
  = PFun{ dom :: [a], apply :: a -> b }

instance (Ord a, Ord b) => Ord (a :->? b) where
  PFun dom1 f `compare` PFun dom2 g =
    [(x,f x)|x<-dom1] `compare` [(x,g x)|x<-dom2]

instance (Ord a, Ord b) => Eq (a :->? b) where
  x == y = compare x y == EQ

instance (Show a, Show b) => Show (a :->? b) where
  show (PFun dom f) =
    "{" ++ intercalate "," [ show x ++ "->" ++ show (f x) | x <- dom ] ++ "}"

----------------------------------------------------------------------------------------
