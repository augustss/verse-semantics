{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
module Dom(
  Value(..),
  pattern Tup,
  (:->?)(..),
  pointFcn,
  )where

import Data.List( intercalate )

----------------------------------------------------------------------------------------

data Value
  = Int Integer
  | Fun [Value :->? Value]
 deriving ( Ord, Eq )

instance Show Value where
  show (Int n)  = show n
  show (Tup vs) = "<" ++ intercalate "," (map show vs) ++ ">"
  show (Fun fs) = show fs

instance Num Value where
  fromInteger n = Int n
  (+)    = error "+"
  (-)    = error "-"
  (*)    = error "*"
  abs    = error "abs"
  signum = error "signum"

pattern Tup :: [Value] -> Value
pattern Tup vs <- (getTup -> Just vs)
  where Tup vs = mkTup vs

mkTup :: [Value] -> Value
mkTup vs = Fun [ pointFcn (Int i) v | (i, v) <- zip [0..] vs ]

getTup :: Value -> Maybe [Value]
getTup (Fun fs) | map dom fs == [ [Int $ toInteger i] | i <- [0..length fs - 1] ]
  = Just $ map (\ f -> apply f (head (dom f))) fs
getTup _ = Nothing

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

pointFcn :: Eq a => a -> b -> (a :->? b)
pointFcn a b = PFun { dom = [a], apply = \ x -> if x == a then b else undefined }

----------------------------------------------------------------------------------------
