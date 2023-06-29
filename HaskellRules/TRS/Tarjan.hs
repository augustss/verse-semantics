{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TRS.Tarjan(tarjan1, tarjan, tarjanAny) where

import qualified Data.Map as M
import Data.Map( (!) )
import qualified Data.Set as S

import Epic.List(takeUntil, dropUntil)

-----------------------------------------------------------------------------------

-- Tarjans SCC algorithm, it's lazy in the graph, so you can use it just find the
-- first component and you don't have to build the whole graph!

-- reimplemented from
-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

type Fuel = Int
type Kont a = Fuel -> Int -> M.Map a (Int,Int) -> S.Set a -> [a] -> Maybe [[a]]

-- Return one normal form.
tarjan1 :: forall a . Ord a => Fuel -> (a -> [a]) -> a -> Maybe [a]
tarjan1 afuel nexts a = hd <$> tarjanAny True afuel nexts a
  where hd [x] = x
        hd _ = undefined  -- This should never happen

-- Return all normal forms.
tarjan :: forall a . Ord a => Fuel -> (a -> [a]) -> a -> Maybe [[a]]
tarjan = tarjanAny False

-- If justOne is True then the returned list is always a singleton.
tarjanAny :: forall a . Ord a => Bool -> Fuel -> (a -> [a]) -> a -> Maybe [[a]]
tarjanAny justOne afuel nexts x = strongc afuel 0 M.empty S.empty [] x (\ _ _ _ _ _ -> Nothing)
 where
  strongc :: Fuel -> Int -> M.Map a (Int,Int) -> S.Set a -> [a] -> a -> Kont a -> Maybe [[a]]
  strongc 0 _ _ _ _ _ _ = Nothing
  strongc fuel !index state onStack stack v k =
    visit (fuel-1) (index+1) (M.insert v (index,index) state) (S.insert v onStack) (v:stack) v (nexts v) k
  
  visit :: Fuel -> Int -> M.Map a (Int,Int) -> S.Set a -> [a] -> a -> [a] -> Kont a -> Maybe [[a]]
  visit !fuel !index state onStack stack v [] k =
    if vindex == vlowlink then
      let xs = takeUntil (v==) stack in
        if justOne then
          Just [xs]
        else
          (xs :) <$> k fuel index state (foldr S.delete onStack xs) (dropUntil (v==) stack)
    else
      k fuel index state onStack stack
   where
    (vindex, vlowlink) = state!v

  visit fuel index state onStack stack v (w:ws) k =
    case M.lookup w state of
      Nothing ->
        strongc fuel index state onStack stack w $
          \ fuel' index' state' onStack' stack' ->
            let (vindex, vlowlink) = state' ! v
                (_windex, wlowlink) = state' ! w
             in visit fuel' index' (M.insert v (vindex, vlowlink `min` wlowlink) state')
                      onStack' stack' v ws k

      Just (windex, _wlowlink) ->
        if w `S.member` onStack then
          visit fuel index (M.insert v (vindex, vlowlink `min` windex) state) onStack stack v ws k
        else
          visit fuel index state onStack stack v ws k
       where
        (vindex, vlowlink) = state ! v


{-
main = print $ head $ tarjan f 1
f x | x < 1000000 = [x+1,3*x,x+100]
    | otherwise   = [x `div` 2]
-}

