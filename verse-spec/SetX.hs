{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SetX(
  SetX,
  intersect,
  union,
  unions,
  difference,
--  nub,
  isEmpty,
  empty,
  member,
  isSubsetOf,
  mkSet,
  sing,
  toList,
  forAll, forAllL,
  exists, existsL,
  maximumSet,
  minimumSet,
  foldSet,
  cartProd,
  ) where
import Control.Applicative
import Data.List(intercalate, sort)
import GHC.Stack

-- Sets as lists with duplicates so it can be a monad.

newtype SetX a = S [a]
  deriving (Functor, Applicative, Monad, Alternative, MonadFail)

instance Ord a => Eq (SetX a) where
  x == y  =  compare x y == EQ

instance Ord a => Ord (SetX a) where
  compare x y = compare (toList x) (toList y)

instance (Show a, Ord a) => Show (SetX a) where
  show s =
    case toList s of
      [] -> "{}"
      s@(a:_)
        | length (show a) < 25 -> "{" ++ intercalate ","   (map show s) ++ "}"
        | otherwise            -> "{" ++ intercalate ",\n" (map show s) ++ "}"

--empty :: SetX a
--empty = S []

isEmpty :: SetX a -> Bool
isEmpty (S []) = True
isEmpty _ = False

member :: Eq a => a -> SetX a -> Bool
member x (S xs) = x `elem` xs

--nub :: Eq a => SetX a -> SetX a

union :: SetX a -> SetX a -> SetX a
union (S a) (S b) = S (a ++ b)

unions :: [SetX a] -> SetX a
unions = foldr union empty

intersect :: Eq a => SetX a -> SetX a -> SetX a
intersect (S as) (S bs) = S [ a | a <- as, a `elem` bs ]

difference :: Eq a => SetX a -> SetX a -> SetX a
difference (S as) (S bs) = S [ a | a <- as, a `notElem` bs ]

isSubsetOf :: Eq a => SetX a -> SetX a -> Bool
isSubsetOf (S as) (S bs) = all (\ a -> a `elem` bs) as

mkSet :: [a] -> SetX a
mkSet = S

toList :: Ord a => SetX a -> [a]
toList (S xs) = unDup $ sort xs
  where
    unDup (x:y:xs) | x == y    =     unDup (y:xs)
                   | otherwise = x : unDup (y:xs)
    unDup xs = xs

-- Check if a predicate holds for all values in the set
forAll :: SetX a -> (a -> Bool) -> Bool
forAll (S a) f = forAllL a f

forAllL :: [a] -> (a -> Bool) -> Bool
forAllL xs p = all p xs

exists :: SetX a -> (a -> Bool) -> Bool
exists (S a) f = existsL a f

existsL :: [a] -> (a -> Bool) -> Bool
existsL xs p = any p xs

maximumSet :: Ord a => SetX a -> a
maximumSet (S a) = maximum a

minimumSet :: Ord a => SetX a -> a
minimumSet (S a) = minimum a

sing :: a -> SetX a
sing x = S [x]

cartProd :: [SetX a] -> SetX [a]
cartProd = sequence

-- Function should be commutative and associative.
-- Set should be non-empty
foldSet :: HasCallStack => (a -> a -> a) -> SetX a -> a
foldSet _ (S []) = error "foldSet"
foldSet f (S a) = foldl1 f a
