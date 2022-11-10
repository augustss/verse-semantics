module Graph where

import qualified Data.Map as M
import Data.Map( Map, (!) )
import qualified Data.Set as S
import Data.Set( Set )
import Data.List( sort, (\\) )

-- this module is heavily inspired by
-- King & Launchbury, "Structuring Depth-First Search Algorithms in Haskell", 1994.
-- https://www.researchgate.net/publication/2252048_Structuring_Depth-First_Search_Algorithms_in_Haskell

-- changed from this paper:
-- - use Data.Map instead of ST-monad
-- - add Cut to Trees to detect back-arrows
-- - mapGraph, leaves, removeLoop
-- - dag (turn a graph into a DAG of its SCCs)

--------------------------------------------------------------------------------
-- depth-first trees

data Tree a
  = Node a [Tree a]
  | Cut a
 deriving ( Eq, Show )

type Forest a
  = [Tree a]

top :: Tree a -> a
top (Node x _) = x
top (Cut x)    = x

-- pruning a possibly infinite forest
prune :: Ord a => Forest a -> Forest a
prune ts = go S.empty ts
 where
  go seen []             = []
  go seen (Cut x    :ts) = Cut x : go seen ts
  go seen (Node x vs:ts)
    | x `S.member` seen  = Cut x : go seen ts
    | otherwise          = Node x (take n ws) : drop n ws
   where
    n  = length vs
    ws = go (S.insert x seen) (vs ++ ts)

-- pre- and post-order traversals
preorder :: Tree a -> [a]
preorder t = preorderF [t]

preorderF :: Forest a -> [a]
preorderF ts = go ts []
 where
  go []               xs = xs
  go (Cut x     : ts) xs = go ts xs
  go (Node x vs : ts) xs = x : go vs (go ts xs)

postorder :: Tree a -> [a]
postorder t = postorderF [t]

postorderF :: Forest a -> [a]
postorderF ts = go ts []
 where
  go []               xs = xs
  go (Cut x     : ts) xs = go ts xs
  go (Node x vs : ts) xs = go vs (x : go ts xs)

-- computing back-arrows
backs :: Ord a => Tree a -> Set a
backs t = S.fromList (go S.empty t)
 where
  go ups (Node x ts) = concatMap (go (S.insert x ups)) ts
  go ups (Cut x)     = [x | x `S.member` ups ]

--------------------------------------------------------------------------------
-- graphs

type Graph a
  = Map a [a]

mapG :: (Ord a, Ord b) => (a -> b) -> Graph a -> Graph b
mapG f g =
  M.map S.toList $
  M.fromListWith S.union
  [ (f x, S.fromList (map f ys))
  | (x,ys) <- M.toList g
  ]

vertices :: Graph a -> [a]
vertices g = [ x | (x,_) <- M.toList g ]

transposeG :: Ord a => Graph a -> Graph a
transposeG g =
  M.fromListWith (++) $
  [ (y,[x]) | (x,ys) <- M.toList g, y <- ys ] ++
  [ (x,[])  | x <- vertices g ]

removeLoops :: Ord a => Graph a -> Graph a
removeLoops g = M.mapWithKey (\x ys -> filter (x/=) ys) g

leaves :: Graph a -> [a]
leaves g = [ x | (x,[]) <- M.toList g ]

--------------------------------------------------------------------------------
-- graphs and trees

generate :: Ord a => Graph a -> a -> Tree a
generate g x = Node x (map (generate g) (g!x))

dfs :: Ord a => Graph a -> [a] -> Forest a
dfs g xs = prune (map (generate g) xs)

reach :: Ord a => Graph a -> [a] -> Graph a
reach g xs = M.fromList [ (x,g!x) | x <- preorderF (dfs g xs) ]

dff :: Ord a => Graph a -> Forest a
dff g = dfs g (vertices g)

preOrd :: Ord a => Graph a -> [a]
preOrd g = preorderF (dff g)

postOrd :: Ord a => Graph a -> [a]
postOrd g = postorderF (dff g)

scc1 :: Ord a => Graph a -> Forest a
scc1 g = reverse (dfs (transposeG g) (reverse (postOrd g)))

scc2 :: Ord a => Graph a -> Forest a
scc2 g = dfs g (reverse (postOrd (transposeG g)))

scc :: Ord a => Graph a -> Forest a
scc g = scc2 g

sccs :: Ord a => Graph a -> [[a]]
sccs = map preorder . scc

--------------------------------------------------------------------------------
-- turn a graph into a DAG of strongly connected components
-- replacing each scc with its representative (smallest value)

dag :: Ord a => Graph a -> Graph a
dag g = removeLoops (mapG rep g) 
 where
  reps  = M.fromList [ (x,r) | xs <- sccs g, let r = minimum xs, x <- xs ]
  rep x = case M.lookup x reps of
            Nothing -> x
            Just r  -> r

--------------------------------------------------------------------------------

