module Epic.UnionFind (UF, new, union, find, equal) where

import qualified Data.Map as M

-- | A simple union find graph
newtype UF a = MkUF { parent :: M.Map a a } deriving (Show)

new :: UF a
new = MkUF M.empty

union :: (Ord a) => UF a -> a -> a -> UF a
-- Add (p=q) to the UF
union uf x y = link uf xr yr
  where
    xr = find uf x
    yr = find uf y

link :: (Ord a) => UF a -> a -> a -> UF a
-- Internal, not exported
link (MkUF m) xr yr = MkUF m'
  where
    m' | xr == yr   = m
       | otherwise  = M.insert xr yr m

find :: (Ord a) => UF a -> a -> a
-- Internal function
-- (find uf x) returns the canonical member of x's equivalence class
find (MkUF m) = go
  where
    go x = maybe x go (M.lookup x m)

equal :: (Ord a) => UF a -> a -> a -> Bool
-- (equal uf x y) tells if x,y are in the same equivalence class
equal uf x y = find uf x == find uf y

-- >>> let uf0 = new
-- >>> let uf1 = union uf0 "x" "y"
-- >>> let uf2 = union uf1 "x" "z"
-- >>> equal uf2 "y" "z"
-- True

---
