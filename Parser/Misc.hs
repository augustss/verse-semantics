{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.Misc(
  anySame, revTake, revDrop,
  pick, pickLR,
  pattern Snoc,
  ) where
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

pickLR :: [a] -> [([a], a, [a])]
pickLR as = zip3 (inits as) as (tail (tails as))

--------

pattern Snoc :: [a] -> a -> [a]
pattern Snoc xs x <- (unSnoc -> Just (xs, x))
  where Snoc xs x = xs ++ [x]

unSnoc :: [a] -> Maybe ([a], a)
unSnoc [] = Nothing
unSnoc xs = Just (init xs, last xs)

