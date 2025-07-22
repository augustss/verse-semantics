{-# OPTIONS_GHC -Wno-x-partial #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Epic.List(
    anySame, revTake, revDrop,
    pick, pickLR,
    pattern Snoc,
    nub, nubKey, (\\),
    takeUntil, dropUntil, groupKey,
    firstJust
  ) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List(inits, tails, (\\) )
import Data.Maybe (listToMaybe, catMaybes)
--------------------------------------------------------
--
--         General utilities on lists
--
--------------------------------------------------------

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------

revTake :: Int -> [a] -> [a]
revTake n = reverse . take n . reverse

revDrop :: Int -> [a] -> [a]
revDrop n = reverse . drop n . reverse

--------

-- All elements in order and the remaining list.
pick :: [a] -> [(a, [a])]
pick as = [(a, xs ++ ys) | (xs, a, ys) <- pickLR as]

-- Split the list in all possible way.
pickLR :: [a] -> [([a], a, [a])]
pickLR as = zip3 (inits as) as (tail (tails as))

--------

pattern Snoc :: [a] -> a -> [a]
pattern Snoc xs x <- (unSnoc -> Just (xs, x))
  where Snoc xs x = xs ++ [x]

unSnoc :: [a] -> Maybe ([a], a)
unSnoc [] = Nothing
unSnoc xs = Just (init xs, last xs)

---------

nub :: Ord a => [a] -> [a]
nub = go S.empty
 where
  go _seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

nubKey :: (Ord k) => (a -> k) -> [a] -> [a]
nubKey = go S.empty
 where
  go _seen _key []            = []
  go seen key (x:xs)
    | k `S.member` seen = go seen key xs
    | otherwise         = x : go (S.insert k seen) key xs
   where
    k = key x


groupKey :: (Ord k) => (a -> k) -> [a] -> [[a]]
groupKey key = go M.empty
  where
    go m []     = M.elems m
    go m (x:xs) = go (M.insertWith (++) (key x) [x] m) xs

---------

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []                 = []
takeUntil p (x:xs) | p x       = [x]
                   | otherwise = x : takeUntil p xs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []                 = []
dropUntil p (x:xs) | p x       = xs
                   | otherwise = dropUntil p xs

---------

firstJust :: [Maybe a] -> Maybe a
firstJust = listToMaybe . catMaybes
