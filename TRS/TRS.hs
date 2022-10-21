{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches #-}
module TRS where

import qualified Data.Set as S
import Data.List ( intercalate )
import Control.Monad( when, unless )
import Debug.Trace (trace)
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

normalForms :: (Show a, Ord a, Rec a) => Rule a -> a -> [(String, a)]
normalForms rule t = normalFormsFuel (-1) rule t

normalFormsFuel :: (Show a, Ord a, Rec a) => Int -> Rule a -> a -> [(String,a)]
normalFormsFuel n rule t =
  [ (sequ (filter (not . null) (map fst tr)), snd (head tr))
  | tr <- normalFormsFuelTrace n rule t
  ]
 where
  sequ [] = "refl"
  sequ as = intercalate ";" as

-- traces are produced in reverse order, i.e. final result first
normalFormsTrace :: (Show a, Ord a, Rec a) => Rule a -> a -> [[(String, a)]]
normalFormsTrace rule t = normalFormsFuelTrace (-1) rule t

normalFormsFuelTrace :: (Show a, Ord a, Rec a) => Int -> Rule a -> a -> [[(String,a)]]
normalFormsFuelTrace n rule t = go n S.empty [[("",t)]]
 where
  go 0 _    (tr:_)      = traceShow "fuel 0" []
  go n seen []          = traceShow "stuck" []
  go n seen (tr@((_,t):_):trs)
    | t `S.member` seen = go n seen trs
    | null ts'          = tr : go n seen' trs
    | otherwise         = go (n-1) seen' (map (:tr) ts' ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule t
  go _ _ _ = error "impossible"

type Trace a = [(String, a)]

type Path a = (Result, Trace a)
data Result = NoFuel | Stuck | NormalForm deriving (Show)

dfs :: (Show a, Ord a, Rec a) => Int -> Rule a -> a -> Path a
dfs n rule t = go n S.empty [("",t)]
 where
  go 0 _    tr   = (NoFuel, tr)
  go n seen tr@((_,t):_)
    | null ts'   = (NormalForm, tr)
    | null ts''  = (Stuck, tr)
    | otherwise  = go (n-1) seen' (head ts'' : tr)
    where
      seen' = S.insert t seen
      ts'   = [t' | t' <- step rule t]
      ts''  = filter ((`S.notMember` seen) . snd) ts'
  go _ _ _ = error "impossible"

normalFormsFuelTrace' :: (Show a, Ord a, Rec a) => Int -> Rule a -> a -> Either (Trace a) [[(String,a)]]
normalFormsFuelTrace' n rule t = go n S.empty [[("",t)]]
 where
  go 0 _    trs         = Left (head trs)
  go n seen []          = Right []
  go n seen (tr@((_,t):_):trs)
    | t `S.member` seen = go n seen trs
    | null ts'          = (tr :) <$> go n seen' trs
    | otherwise         = go (n-1) seen' (map (:tr) ts' ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule t
  go _ _ _ = error "impossible"


traceShow :: Show a => String -> a -> a
traceShow msg x = trace ("TRACE: " ++ msg ++ " : " ++ show x ++ "\n") x

printTrace :: (Show a) => [(String,a)] -> IO ()
printTrace tr =
  sequence_
  [ do print t
       unless (null n) $
         putStrLn ("  <--" ++ n ++ "--")
  | (n,t) <- tr
  ]

printTrace' :: (Show a) => [(String,a)] -> IO ()
printTrace' tr =
  sequence_
  [ do unless (null n) $ putStrLn ("  ---" ++ n ++ "-->")
       print t
  | (n,t) <- reverse tr
  ]

--

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

--------------------------------------------------------------------------------
