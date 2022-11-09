module TRSGraph where

import qualified Data.Set as S
import qualified Data.Map as M

import TRS
import Graph

data Traced a = a :<- [String]

instance Eq a => Eq (Traced a) where
  (x :<- _) == (y :<- _) = x == y

instance Ord a => Ord (Traced a) where
  (x :<- _) `compare` (y :<- _) = x `compare` y

trsGraphFuel :: (Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Graph (Maybe a)
trsGraphFuel env fuel rule x = M.fromListWith (++) (go S.empty fuel [x])
 where
  go seen _ [] =
    []

  go seen 0 xs =
    [ (Just x, ys)
    | (x,ys) <- xys
    ] ++
    [ (Nothing, [])
    | any (\(_,ys) -> Nothing `elem` ys) xys
    ]
   where
    xys = [ (x,nub [ if y `S.member` seen then Just y else Nothing
                   | y <- map snd (step rule env x)
                   ])
          | x <- xs
          ]
              
  go seen fuel (x:xs) =
    [ (Just x, map Just ys) ] ++
    go (S.insert x seen)
       (fuel-1)
       ([y | y <- ys, not (y `S.member` seen)] ++ xs) -- depth first
   where
    ys = map snd (step rule env x)

