{-# LANGUAGE BangPatterns #-}
module Tarjan where

import qualified Data.Map as M
import Data.Map( Map, (!) )
import qualified Data.Set as S
import Data.Set( Set )

-----------------------------------------------------------------------------------

-- Tarjans SCC algorithm, it's lazy in the graph, so you can use it just find the
-- first component and you don't have to build the whole graph!

-- reimplemented from
-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

tarjan :: Ord a => (a -> [a]) -> a -> [[a]]
tarjan f x = strongc 0 M.empty S.empty [] x (\_ _ _ _ -> [])
 where
  strongc !index state onStack stack v k =
    visit (index+1) (M.insert v (index,index) state) (S.insert v onStack) (v:stack) v (f v) k
  
  visit !index state onStack stack v [] k =
    if vindex == vlowlink then
      let xs = takeUntil (v==) stack in
        xs : k index state (foldr S.delete onStack xs) (dropUntil (v==) stack)
    else
      k index state onStack stack
   where
    (vindex, vlowlink) = state!v

  visit index state onStack stack v (w:ws) k =
    case M.lookup w state of
      Nothing ->
        strongc index state onStack stack w $
          \index' state' onStack' stack' ->
            let (vindex, vlowlink) = state' ! v
                (windex, wlowlink) = state' ! w
             in visit index' (M.insert v (vindex, vlowlink `min` wlowlink) state')
                      onStack' stack' v ws k

      Just (windex, wlowlink) ->
        if w `S.member` onStack then
          visit index (M.insert v (vindex, vlowlink `min` windex) state) onStack stack v ws k
        else
          visit index state onStack stack v ws k
       where
        (vindex, vlowlink) = state ! v

-----------------------------------------------------------------------------------
-- Grrr... I wish these were standard functions. like takeWhile/dropWhile, but 1 step more

takeUntil p []                 = []
takeUntil p (x:xs) | p x       = [x]
                   | otherwise = x : takeUntil p xs

dropUntil p []                 = []
dropUntil p (x:xs) | p x       = xs
                   | otherwise = dropUntil p xs

-----------------------------------------------------------------------------------

{-
main = print $ head $ tarjan f 1
f x | x < 1000000 = [x+1,3*x,x+100]
    | otherwise   = [x `div` 2]
-}

