module TRS where

import qualified Data.Set as S
import Data.Set( Set )

--------------------------------------------------------------------------------

type Rule a = a -> [a]

(+++) :: Rule a -> Rule a -> Rule a
r1 +++ r2 = \x -> r1 x ++ r2 x

--------------------------------------------------------------------------------

class Rec t where
  rec :: (t -> [t]) -> t -> [t]

step1 :: Rec a => Rule a -> a -> Maybe a
step1 rule t =
  case apply t of
    t' : _ -> Just t'
    _      -> Nothing
 where
  apply t = rule t ++ rec apply t

steps :: Rec a => Rule a -> a -> [a]
steps rule t = t : case step1 rule t of
                     Nothing -> []
                     Just t' -> steps rule t'

step :: (Ord a, Rec a) => Rule a -> a -> [a]
step rule t = nub (apply t)
 where
  apply t = rule t ++ rec apply t

normalForms :: (Ord a, Rec a) => Rule a -> a -> [a]
normalForms rule t = normalFormsFuel (-1) rule t

normalFormsFuel :: (Ord a, Rec a) => Int -> Rule a -> a -> [a]
normalFormsFuel n rule t = go n S.empty [t]
 where
  go 0 _    _           = []
  go n seen []          = []
  go n seen (t:ts)
    | t `S.member` seen = go n seen ts
    | null ts'          = t : go n seen' ts
    | otherwise         = go (n-1) seen' (ts' ++ ts)
   where
    seen' = S.insert t seen
    ts'   = step rule t

--

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

--------------------------------------------------------------------------------

