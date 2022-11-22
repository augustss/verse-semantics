{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TRS.TRS where

import qualified Data.Set as S
import Data.List ( intercalate )
import Control.Monad( unless )
--import Debug.Trace (trace)
--import Data.Set( Set )

--------------------------------------------------------------------------------

type Rule a = RuleEnv a -> a -> [(String, a)]

-- This is used to give rules names.
infix 7 `name`   -- must bind tighter than ++
name :: String -> [a] -> [(String, a)]
name s as = [(s,a) | a <- as]

--------------------------------------------------------------------------------

class Rec t where
  -- RuleEnv can contain some environment (like flags) needed during reduction
  data RuleEnv t
  rec :: Rule t -> Rule t

  norm :: (RuleEnv t) -> t -> t
  norm _ t = t

step1 :: Rec a => RuleEnv a -> Rule a -> a -> Maybe a
step1 env rule t =
  case rec rule env t of
    (_,t') : _ -> Just (norm env t')
    _          -> Nothing

steps :: Rec a => RuleEnv a -> Rule a -> a -> [a]
steps env rule t =
  t : case step1 env rule t of
        Nothing -> []
        Just t' -> steps env rule t'

step :: forall a . (Ord a, Rec a) => Rule a -> Rule a
step rule env tt = nub [ (n,norm env t) | (n,t) <- rec rule env tt ]

normalForms :: (Show a, Ord a, Rec a) => RuleEnv a -> Rule a -> a -> [(String, a)]
normalForms env rule t = normalFormsFuel env (-1) rule t

normalFormsFuel :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> [(String,a)]
normalFormsFuel env n rule t =
  [ (sequ (filter (not . null) (map fst tr)), snd (head tr))
  | tr <- normalFormsFuelTrace env n rule t
  ]
 where
  sequ [] = "refl"
  sequ as = intercalate ";" as

-- traces are produced in reverse order, i.e. final result first
normalFormsTrace :: (Show a, Ord a, Rec a) => RuleEnv a -> Rule a -> a -> [[(String, a)]]
normalFormsTrace env rule t = normalFormsFuelTrace env (-1) rule t

normalFormsFuelTrace :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> [[(String,a)]]
--normalFormsFuelTrace n _ _ | trace ("normalFormsFuelTrace: " ++ show n) False = undefined
normalFormsFuelTrace env n rule t = go n S.empty [[("",t)]]
 where
  go 0 _    (tr:_)      = traceShow "fuel 0" []
                          --trace ("fuel 0\n" ++ showReductionTrace show tr) []
  go n seen []          = [] -- traceShow "stuck" []
  go n seen (tr@((_,t):_):trs)
    | t `S.member` seen = go n seen trs
    | null ts'          = tr : go n seen' trs
    | otherwise         = go (n-1) seen' (map (:tr) ts' ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule env t
  go _ _ _ = error "impossible"

showReductionTrace :: (a -> String) -> Trace a -> String
showReductionTrace sh xs = msg
  where
    msg = "***** Reduction trace\n" ++ (unlines $ map pr $ reverse xs) ++ "*****\n"
    pr (s, a) = s ++ ":\n" ++ sh a ++ "\n----------\n"

type Trace a = [(String, a)]

type Path a = (Result, Trace a)
data Result = NoFuel | Stuck | NormalForm deriving (Show)

-- Like normalFormsFuelTrace, but only does a depth first search
normalFormFuelTrace :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> [[(String,a)]]
normalFormFuelTrace env n rule t = [snd (dfs env n rule t)]

dfs :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Path a
dfs env n rule t = go n S.empty [("",t)]
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

normalFormsFuelTrace' :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Either (Trace a) [[(String,a)]]
normalFormsFuelTrace' env n rule t = go n S.empty [[("",t)]]
 where
  go 0 _    trs         = Left (head trs)
  go n seen []          = Right []
  go n seen (tr@((_,t):_):trs)
    | t `S.member` seen = go n seen trs
    | null ts'          = (tr :) <$> go n seen' trs
    | otherwise         = go (n-1) seen' (map (:tr) ts' ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule env t
  go _ _ _ = error "impossible"


traceShow :: Show a => String -> a -> a
--traceShow msg x = trace ("\nTRACE: " ++ msg ++ " : " ++ show x) x
traceShow msg x = x

printTrace :: (Show a) => [(String,a)] -> IO ()
printTrace tr =
  sequence_
  [ do unless (null n) $ putStrLn ("  --" ++ n ++ "-->")
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
