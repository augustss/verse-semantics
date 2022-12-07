{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TRS.TRS(
  Rule,
  name,
  Rec(..),
  step,
  normalFormsFuelTracePlain,
  normalFormFuelTracePlain,
  Trace,
  -- XXX
  nub,
  ) where

import TRS.Traced
import qualified Data.Set as S
--import Control.Monad( unless )
--import qualified Debug.Trace
--import Data.Set( Set )

--------------------------------------------------------------------------------

type Rule a = RuleEnv a -> a -> [(String, a)]


instance Show (Rule t) where
  show _ = "<<Rule>>"

-- This is used to give rules names.
infix 7 `name`   -- must bind tighter than ++
name :: String -> [a] -> [(String, a)]
name s as = [(s,a) | a <- as]

--------------------------------------------------------------------------------

class Rec t where
  -- RuleEnv can contain some environment (like flags) needed during reduction
  data RuleEnv t
  rec :: Rule t -> Rule t

{-
step1 :: Rec a => RuleEnv a -> Rule a -> a -> Maybe a
step1 env rule t =
  case rec rule env t of
    (_,t') : _ -> Just t'
    _          -> Nothing

steps :: Rec a => RuleEnv a -> Rule a -> a -> [a]
steps env rule t =
  t : case step1 env rule t of
        Nothing -> []
        Just t' -> steps env rule t'
-}

step :: forall a . (Ord a, Rec a) => Rule a -> Rule a
step rule env tt = nub $ rec rule env tt

-- traces are produced in reverse order, i.e. final result first
normalFormsFuelTracePlain :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> [Traced a]
normalFormsFuelTracePlain env an rule at = go an S.empty [start at]
 where
  go 0 _    (_tr:_)      = []
  go _n _seen []          = []
  go n seen (ttr@(t:<--tr):trs)
--x    | Debug.Trace.trace ("go: " ++ show t) False = undefined
    | t `S.member` seen = go n seen trs
    | null ts'          = ttr : go n seen' trs
    | otherwise         = go (n-1) seen' ([t':<--((s,t):tr) | (s,t') <- ts'] ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule env t

type Trace a = [(String, a)]
type Path a = (Result, Trace a)
data Result = NoFuel | Stuck | NormalForm deriving (Show)

-- Like normalFormsFuelTrace, but only does a depth first search
normalFormFuelTracePlain :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> [[(String,a)]]
normalFormFuelTracePlain env n rule t = [snd (dfs env n rule t)]

dfs :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Path a
dfs env an rule at = go an S.empty [("",at)]
 where
  go 0 _    tr   = (NoFuel, tr)
  go n seen tr@((_,t):_)
    | null ts'   = (NormalForm, tr)
    | null ts''  = (Stuck, tr)
    | otherwise  = go (n-1) seen' (head ts'' : tr)
    where
      seen' = S.insert t seen
      ts'   = step rule env t
      ts''  = filter ((`S.notMember` seen) . snd) ts'
  go _ _ _ = error "impossible"

{-
normalFormsFuelTrace' :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Either (Trace a) [[(String,a)]]
normalFormsFuelTrace' env an rule at = go an S.empty [[("",at)]]
 where
  go 0 _    trs         = Left (head trs)
  go _n _seen []          = Right []
  go n seen (tr@((_,t):_):trs)
    | t `S.member` seen = go n seen trs
    | null ts'          = (tr :) <$> go n seen' trs
    | otherwise         = go (n-1) seen' (map (:tr) ts' ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule env t
  go _ _ _ = error "impossible"
-}

{-
traceShow :: Show a => String -> a -> a
--traceShow msg x = trace ("\nTRACE: " ++ msg ++ " : " ++ show x) x
traceShow _msg x = x

printTrace :: (Show a) => [(String,a)] -> IO ()
printTrace tr =
  sequence_
  [ do unless (null n) $ putStrLn ("  --" ++ n ++ "-->")
       print t
  | (n,t) <- reverse tr
  ]
-}

--------------------------------------------------------------------------------
