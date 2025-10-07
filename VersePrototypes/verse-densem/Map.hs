{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Map where
import Prelude hiding (lookup, null)
#if 0
import qualified Data.Map as M

newtype Map a b = Map (M.Map a b)
  deriving (Eq, Ord)

empty :: forall a b . Map a b
empty = Map M.empty

insert :: Ord a => a -> b -> Map a b -> Map a b
insert a b (Map m) = Map $ M.insert a b m

fromList :: Ord a => [(a, b)] -> Map a b
fromList s = Map $ M.fromList s

null :: Map a b -> Bool
null (Map m) = M.null m

lookup :: Ord a => a -> Map a b -> Maybe b
lookup a (Map m) = M.lookup a m

toList :: Map a b -> [(a, b)]
toList (Map m) = M.toList m

member :: Ord a => a -> Map a b -> Bool
member a (Map m) = M.member a m

keys :: Map a b -> [a]
keys (Map m) = M.keys m

isSubmapOf :: (Ord a, Ord b) => Map a b -> Map a b -> Bool
isSubmapOf = M.isSubmapOf

#else
-- It's faster to use lists than Data.Map for
-- the very small maps we have here.

import qualified Prelude

newtype Map a b = Map [(a, b)]
  deriving (Eq, Ord, Show)

empty :: Map a b
empty = Map []

-- Keep the list sorted and keys unique, so Eq and Ord can be fast
insert :: Ord a => a -> b -> Map a b -> Map a b
insert a b (Map m) = Map $ ins m
  where ins [] = [(a, b)]
        ins abxs@(ab@(a', _):xs) =
             case compare a a' of
               LT -> (a, b) : abxs
               EQ -> (a, b) : xs
               GT -> ab : ins xs

fromListUnsafe :: Ord a => [(a, b)] -> Map a b
fromListUnsafe s = Map s

fromList :: Ord a => [(a, b)] -> Map a b
fromList = foldr (uncurry insert) empty

null :: Map a b -> Bool
null (Map m) = Prelude.null m

lookup :: Ord a => a -> Map a b -> Maybe b
lookup a (Map m) = Prelude.lookup a m

toList :: Map a b -> [(a, b)]
toList (Map m) = m

member :: Ord a => a -> Map a b -> Bool
member a (Map m) = any ((a ==) . fst) m

keys :: Map a b -> [a]
keys (Map m) = map fst m

isSubmapOf :: (Ord a, Ord b) => Map a b -> Map a b -> Bool
isSubmapOf (Map xs) (Map ys) = all (`elem` ys) xs

unions :: (Ord a) => [Map a b] -> Map a b
unions = fromList . concatMap toList

#endif
