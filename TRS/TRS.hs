{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches #-}
module TRS where

import qualified Data.Set as S
--import Data.Set( Set )

--------------------------------------------------------------------------------

type Rule a = a -> [(String, a)]

(+++) :: Rule a -> Rule a -> Rule a
r1 +++ r2 = \x -> r1 x ++ r2 x

-- This is used to give rules names.
-- At the moment this is just documentation,
--  but could be incorporated into the Rule.
infix 6 `name`   -- must bind tighter than ++
name :: String -> [a] -> [(String, a)]
name s as = [(s,a) | a <- as]

--------------------------------------------------------------------------------

class Rec t where
  rec :: Rule t -> Rule t

step1 :: Rec a => Rule a -> a -> Maybe a
step1 rule t =
  case apply t of
    (_,t') : _ -> Just t'
    _           -> Nothing
 where
  apply t = rule t ++ rec apply t

steps :: Rec a => Rule a -> a -> [a]
steps rule t = t : case step1 rule t of
                     Nothing -> []
                     Just t' -> steps rule t'

step :: (Ord a, Rec a) => Rule a -> Rule a
step rule t = nub (apply t)
 where
  apply t = rule t ++ rec apply t

normalForms :: (Ord a, Rec a) => Rule a -> a -> [(String, a)]
normalForms rule t = normalFormsFuel (-1) rule t

normalFormsFuel :: (Ord a, Rec a) => Int -> Rule a -> a -> [(String,a)]
normalFormsFuel n rule t =
    case step rule t of
      [] -> [("refl", t)]
      ts -> go n S.empty ts
 where
  go 0 _    _           = []
  go n seen []          = []
  go n seen ((name,t):ts)
    | t `S.member` seen = go n seen ts
    | null ts'          = (name,t) : go n seen' ts
    | otherwise         = go (n-1) seen' (ts' ++ ts)
   where
    seen' = S.insert t seen
    ts'   = map tag $ step rule t
    tag (x,y) = (name ++ ";" ++ x, y)

--

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

--------------------------------------------------------------------------------

