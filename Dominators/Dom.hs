module Main where

import Test.QuickCheck
import Data.List( nub, sort, (\\) )
import Data.Char

type Node  = Int
type Graph = [(Node,Node)]

nodes :: Graph -> [Node]
nodes g = nub [ z | (x,y) <- g, z <- [x,y] ]

nexts :: Graph -> Node -> [Node]
nexts g x = [ y | (x',y) <- g, x' == x ]

reach :: Graph -> Node -> [Node]
reach g x = go [] [x]
 where
  go seen []        = []
  go seen (x:xs)
    | x `elem` seen = go seen xs
    | otherwise     = x : go (x:seen) (nexts g x ++ xs)

dom1 :: Graph -> [(Node,Node)]
dom1 g =
  nub . sort $
  [ (x,u)
  | x <- nodes g
  , let reach_x = reach g x
  , u <- reach_x
  , all (\y -> let reach_y = reach g y in 
                 not (u `elem` reach_y) || x `elem` reach_y || y `elem` reach_x)
        (nodes g)
  ]

dom2 :: Graph -> [(Node,Node)]
dom2 g = gfix [ (x,y) | x <- nodes g, y <- reach g x ] 
 where
  gfix dom
    | dom == dom' = dom
    | otherwise   = gfix dom'
   where
    dom' = step dom

  step dom =
    nub . sort $
    [ (v,u)
    | (v,u) <- dom 
    , v == u || and [ (v,w) `elem` dom | (w,u') <- g, u' == u ]
    ]

{-
    [ (v,v)
    | v <- nodes g 
    ] ++
    [ (v,u)
    | v <- nodes g
    , u <- reach g v
    , and [ (v,w) `elem` dom | (w,u') <- g, u' == u ]
    ]
-}

prop_Same (G g) =
  let d1 = dom1 g
      d2 = dom2 g
   in whenFail (do putStrLn ("Dom1: " ++ show (d1 \\ d2))
                   putStrLn ("Dom2: " ++ show (d2 \\ d1))) $
        d1 == d2
  
data G = G Graph deriving ( Eq, Ord, Show )

instance Arbitrary G where
  arbitrary =
    do n <- choose (1,25)
       es <- listOf (do i <- choose (1,n)
                        j <- choose (1,n)
                        return (i, j))
       return (G es)
  
  shrink (G es) =
    [ G es' | es' <- shrink es ]

main = quickCheck prop_Same
