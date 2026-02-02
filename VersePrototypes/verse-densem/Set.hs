{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Set(
  Set,
  singleton,
  add,
  intersect,
  bigIntersect,
  union,
  unions,
  bigUnion,
  difference,
  isEmpty,
  empty,
  member,
  isSubsetOf,
  mkSet, mkSetUnsafe,
  sing,
  getSing,
  toList,
  toList',
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
  unzip3Set,
  lookupSet,
  partitions,
  ) where
import Control.Applicative
import Control.Monad
import Data.List(intercalate, sort, groupBy, sortBy, partition)
import qualified Data.Maybe as M
import qualified Data.Set as S
import GHC.Stack
import GHC.Exts(IsList(..))

-- Sets as lists with duplicates so it can be a monad.

newtype Set a = S [a]
  deriving (Functor, Applicative, Monad, Alternative, MonadFail)

instance Ord a => Eq (Set a) where
  x == y  =  compare x y == EQ

instance Ord a => Ord (Set a) where
  compare x y = compare (toList x) (toList y)

instance (Show a, Ord a) => Show (Set a) where
  show s =
    case toList s of
      [] -> "{}"
      ss@(a:_)
        | length (show a) < 25 && length ss < 10 -> "{" ++ intercalate ","   (map show ss) ++ "}"
        | otherwise            -> "{" ++ intercalate ",\n" (map show ss) ++ "}"

-- Control.Applicative defines it
--empty :: Set a
--empty = S []

singleton :: a -> Set a
singleton a = S [a]

isEmpty :: Set a -> Bool
isEmpty (S []) = True
isEmpty _ = False

member :: Eq a => a -> Set a -> Bool
member x (S xs) = x `elem` xs

union :: Set a -> Set a -> Set a
union (S a) (S b) = S (a ++ b)

unions :: [Set a] -> Set a
unions = foldr union empty

bigUnion :: Set (Set a) -> Set a
bigUnion (S ss) = unions ss

intersect :: Eq a => Set a -> Set a -> Set a
intersect (S as) (S bs) = S [ a | a <- as, a `elem` bs ]

bigIntersect :: Eq a => Set (Set a) -> Set a
bigIntersect (S []) = error "bigIntersect: empty"
bigIntersect (S ss) = foldr1 intersect ss

difference :: Eq a => Set a -> Set a -> Set a
difference (S as) (S bs) = S [ a | a <- as, a `notElem` bs ]

isSubsetOf :: Eq a => Set a -> Set a -> Bool
isSubsetOf (S as) (S bs) = all (\ a -> a `elem` bs) as

add :: a -> Set a -> Set a
add a (S as) = S (a:as)

{-
mkSet :: Ord a => [a] -> Set a
mkSet as@[] = S as    -- small speedup
mkSet as@[_] = S as   -- small speedup
mkSet s = S (nub s)
-}

-- We don't care about duplicates
mkSet :: [a] -> Set a
mkSet = S

-- Only use this if all elements of the list are distinct
mkSetUnsafe :: [a] -> Set a
mkSetUnsafe = S

getSing :: (Ord a) => Set a -> Maybe a
getSing s =
  case toList s of
    [x] -> Just x
    _ -> Nothing

toList' :: Ord a => Set a -> [a]
toList' (S axs) = unDup $ sort axs
  where
    unDup (x:y:xs) | x == y    =     unDup (y:xs)
                   | otherwise = x : unDup (y:xs)
    unDup xs = xs

-- Check if a predicate holds for all values in the set
forAll :: Set a -> (a -> Bool) -> Bool
forAll (S a) f = forAllL a f

forAllL :: [a] -> (a -> Bool) -> Bool
forAllL xs p = all p xs

exists :: Set a -> (a -> Bool) -> Bool
exists (S a) f = existsL a f

existsL :: [a] -> (a -> Bool) -> Bool
existsL xs p = any p xs

maximumSet :: Ord a => Set a -> a
maximumSet (S a) = maximum a

minimumSet :: Ord a => Set a -> a
minimumSet (S a) = minimum a

sing :: a -> Set a
sing x = S [x]

-- Function should be commutative and associative.
-- Set should be non-empty
foldSet :: (HasCallStack, Ord a) => (a -> a -> a) -> Set a -> a
foldSet _ (S []) = error "foldSet"
foldSet f s = foldl1 f (toList s)

mapMaybe :: (a -> Maybe b) -> Set a -> Set b
mapMaybe f (S xs) = S (M.mapMaybe f xs)

toListBy :: (a -> a -> Ordering) -> Set a -> [Set a]
toListBy cmp (S xs) = map S $ groupBy eq $ sortBy cmp xs
  where eq x y = cmp x y == EQ

maybeToSet :: Maybe a -> Set a
maybeToSet Nothing = empty
maybeToSet (Just a) = sing a

partitionSet :: (a -> Bool) -> Set a -> (Set a, Set a)
partitionSet p (S sx) = (S a, S b) where (a, b) = partition p sx

cross :: (Ord a) => Set (Set a) -> Set (Set a)
cross (S ss) = mkSet $ map mkSet $ sequence $ map toList ss

filterSet :: (a -> Bool) -> Set a -> Set a
filterSet p (S ss) = S (filter p ss)

anySet :: (a -> Bool) -> Set a -> Bool
anySet p (S ss) = any p ss

-- Can produce duplicates, but that's ok
unzip3Set :: Set (a, b, c) -> (Set a, Set b, Set c)
unzip3Set (S ss) = (S as, S bs, S cs)
  where (as, bs, cs) = unzip3 ss

-----

nub :: Ord a => [a] -> [a]
nub = go S.empty
 where
  go _seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

instance (Ord a) => IsList (Set a) where
  type Item (Set a) = a
  fromList = mkSet
  toList = toList'

lookupSet :: Eq a => a -> Set (a, b) -> Set b
lookupSet x (S xys) = S [ y | (x', y) <- xys, x == x' ]

partitions :: Ord a => Set a -> Set (Set a, Set a)
partitions = S . map f . partitionM (const [False, True]) . toList'
  where f (xs, ys) = (S xs, S ys)

-- Should import from somewhere
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM f [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)
