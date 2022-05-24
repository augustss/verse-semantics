{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Misc(
  anySame, revTake, revDrop,
  pattern Snoc,
  ) where

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------

revTake :: Int -> [a] -> [a]
revTake n = reverse . take n . reverse

revDrop :: Int -> [a] -> [a]
revDrop n = reverse . drop n . reverse

--------

pattern Snoc :: [a] -> a -> [a]
pattern Snoc xs x <- (unSnoc -> Just (xs, x))
  where Snoc xs x = xs ++ [x]

unSnoc :: [a] -> Maybe ([a], a)
unSnoc [] = Nothing
unSnoc xs = Just (init xs, last xs)
