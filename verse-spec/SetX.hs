{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SetX(
  SetX,
  intersect,
  union,
  unions,
  difference,
  isEmpty,
  empty,
  member,
  isSubsetOf,
  mkSet, mkSetUnsafe,
  sing,
  getSing,
  toList,
  forAll, forAllL,
  exists, existsL,
  maximumSet,
  minimumSet,
  foldSet,
  mapMaybe,
  toListBy,
  maybeToSet,
  partitionSet,
  filterSet,
  anySet,
  cross,
  ) where
import Control.Applicative
import Data.List(intercalate, sort, groupBy, sortBy, partition)
import qualified Data.Maybe as M
import qualified Data.Set as S
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
      ss@(a:_)
        | length (show a) < 25 && length ss < 10 -> "{" ++ intercalate ","   (map show ss) ++ "}"
        | otherwise            -> "{" ++ intercalate ",\n" (map show ss) ++ "}"

--empty :: SetX a
--empty = S []

isEmpty :: SetX a -> Bool
isEmpty (S []) = True
isEmpty _ = False

member :: Eq a => a -> SetX a -> Bool
member x (S xs) = x `elem` xs

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

mkSet :: Ord a => [a] -> SetX a
mkSet as@[] = S as    -- small speedup
mkSet as@[_] = S as   -- small speedup
mkSet s = S (nub s)

-- Only use this if all elements of the list are distinct
mkSetUnsafe :: [a] -> SetX a
mkSetUnsafe = S

getSing :: SetX a -> Maybe a
getSing (S [a]) = Just a
getSing _ = Nothing

toList :: Ord a => SetX a -> [a]
toList (S axs) = unDup $ sort axs
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

-- Function should be commutative and associative.
-- Set should be non-empty
foldSet :: (HasCallStack, Ord a) => (a -> a -> a) -> SetX a -> a
foldSet _ (S []) = error "foldSet"
foldSet f s = foldl1 f (toList s)

mapMaybe :: (a -> Maybe b) -> SetX a -> SetX b
mapMaybe f (S xs) = S (M.mapMaybe f xs)

toListBy :: (a -> a -> Ordering) -> SetX a -> [SetX a]
toListBy cmp (S xs) = map S $ groupBy eq $ sortBy cmp xs
  where eq x y = cmp x y == EQ

maybeToSet :: Maybe a -> SetX a
maybeToSet Nothing = empty
maybeToSet (Just a) = sing a

partitionSet :: (a -> Bool) -> SetX a -> (SetX a, SetX a)
partitionSet p (S sx) = (S a, S b) where (a, b) = partition p sx

cross :: (Ord a) => SetX (SetX a) -> SetX (SetX a)
cross (S ss) = mkSet $ map mkSet $ sequence $ map toList ss

filterSet :: (a -> Bool) -> SetX a -> SetX a
filterSet p (S ss) = S (filter p ss)

anySet :: (a -> Bool) -> SetX a -> Bool
anySet p (S ss) = any p ss

-----

nub :: Ord a => [a] -> [a]
nub = go S.empty
 where
  go _seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

