{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Epic.List(
  anySame, revTake, revDrop,
  pick, pickLR,
  pattern Snoc,
  nub,
  ) where
import qualified Data.Set as S
import Data.List(inits, tails)

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

