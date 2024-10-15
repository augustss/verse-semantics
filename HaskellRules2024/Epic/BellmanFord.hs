-- Implementation of the BellmanFord Algorithm
-- https://en.wikipedia.org/wiki/Bellman%E2%80%93Ford_algorithm
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Epic.BellmanFord (negativeCycle) where

import qualified Data.Map as M
import qualified Data.List as L
import qualified Epic.List as EL

-- | Find a negative cycle in a graph, given a source vertex and a list of weighted edges.
negativeCycle :: Ord v => v -> [(v, v, Int)] -> Maybe [v]
negativeCycle src0 es = graphNegativeCycle (mkGraph src0 es)

-- | Graph data structure ---------------------------------------------------------

data Graph v = MkGraph
  { src      :: v                     -- ^ source vertex
  , vertices :: [v]                   -- ^ [v1,v2,...,vn]
  , edges    :: M.Map v [(v, Int)]    -- ^ vi -> [(vj, wj), ...]
  }
  deriving (Show)

-- | Make the `Graph` data structure from a source vertex and a list of weighted edges.
mkGraph :: (Ord v) => v -> [(v, v, Int)] -> Graph v
mkGraph src0 es = MkGraph src0 vs es''
  where
    vs   = EL.nub (src0 : concatMap (\(u, v, _) -> [u, v]) es)
    es'  = M.fromListWith (++) [(u, [(v, w)]) | (u, v, w) <- es]
    es'' = M.insert src0 [(v, 0) | v <- vs, v /= src0] es'


type Dist v = M.Map v (Int, v)

-- | Find a negative cycle in a Graph ----------------------------------------------
graphNegativeCycle :: (Ord v) => Graph v -> Maybe [v]
graphNegativeCycle g = cyc
  where
    n  = length (vertices g)
    -- step 1. initialize dist
    d0 = initDist g
    -- step 2. relax distances |V| times
    d  = iter (n - 1) (relax g) d0
    -- step 3. check for negative cycle
    cyc = checkCycle g d

iter :: Int -> (a -> a) -> a -> a
iter n f x
  | n <= 0    = x
  | otherwise = iter (n - 1) f (f x)

initDist :: (Ord v) => Graph v -> Dist v
initDist g = M.fromList [(v0, (0, v0))] where v0 = src g

relax :: (Ord v) => Graph v -> Dist v -> Dist v
relax g d = L.foldl' relaxEdge d (allEdges g)

relaxEdge :: (Ord v) => Dist v -> (v, v, Int) -> Dist v
relaxEdge d (u, v, w) =
  case shrinkEdge d (u, v, w) of
    Just dv' -> M.insert v (dv', u) d
    Nothing  -> d

checkCycle :: (Ord v) => Graph v -> Dist v -> Maybe [v]
checkCycle g d = do
  (u, v) <- hasCycle g d
  let d' = setPred d v u
  Just   $ findCycle d' [v] u

findCycle :: (Ord v) => Dist v -> [v] -> v -> [v]
findCycle d vs u
  | u `elem` vs = reverse (u : takeWhile (/= u) vs)
  | otherwise   = findCycle d (u : vs) u'
  where
    u'          = getPred d u

hasCycle :: (Ord v) => Graph v -> Dist v -> Maybe (v, v)
hasCycle g d = EL.firstJust [cycleEdge d e | e <- allEdges g]

cycleEdge :: Ord v => Dist v -> (v, v, Int) -> Maybe (v, v)
cycleEdge d (u, v, w) =
  case shrinkEdge d (u, v, w) of
    Just _  -> Just (u, v)
    Nothing -> Nothing

shrinkEdge :: Ord v => Dist v -> (v, v, Int) -> Maybe Int
shrinkEdge d (u , v, w)
  | du + w < dv = Just (du + w)
  | otherwise   = Nothing
  where
    du = getDist d u
    dv = getDist d v

allEdges :: Graph v -> [(v, v, Int)]
allEdges g = [ (u, v, w) | (u, vws) <- M.toList (edges g), (v, w) <- vws ]

getDist :: (Ord v) => Dist v -> v -> Int
getDist d v = maybe maxBound fst (M.lookup v d)

setPred :: (Ord v) => Dist v -> v -> v -> Dist v
setPred d v u = M.insert v (getDist d v, u) d

getPred :: (Ord v) => Dist v -> v -> v
getPred d v =
  case M.lookup v d of
    Just (_, u) -> u
    Nothing     -> error "panic: getPred: vertex not found"

----------------------------------------------------------------------
-- | Examples
----------------------------------------------------------------------

es0 :: [(String, String, Int)]
es0 = [ ("x", "y", 0)
      , ("x", "a", 0)
      , ("y", "z", 0)
      , ("z", "x", -1) ]

es1 :: [(String, String, Int)]
es1 = [ ("x", "y", 0)
      , ("x", "a", 0)
      , ("y", "z", 0)
      , ("z", "x", 0) ]


-- >>> negativeCycle "o" es0
-- Just ["z","y","x"]

-- >>> negativeCycle "o" es1
-- Nothing
