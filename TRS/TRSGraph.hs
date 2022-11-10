module TRSGraph where

import qualified Data.Set as S
import qualified Data.Map as M

import TRS
import Traced
import Graph

trsGraphFuelTrace :: (Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Graph (Maybe (Traced a))
trsGraphFuelTrace env fuel rule x = M.fromListWith (++) (go S.empty fuel [start x])
 where
  go seen _ [] =
    []

  go seen 0 txs =
    [ (Just tx, map Just (filter (`S.member` nodes) tys)
             ++ [ Nothing | any (not . (`S.member` nodes)) tys ])
    | (tx,tys) <- txys
    ] ++
    [ (Nothing, [])
    | any (not . (`S.member` nodes)) (concatMap snd txys)
    ]
   where
    nodes = seen `S.union` S.fromList txs
    txys  = [ (tx, children tx) | tx <- txs ]

  go seen fuel (tx:txs) =
    (Just tx, map Just tys) :
    go (S.insert tx seen) ((fuel-1) `max` 0)
       ([ ty | ty <- tys, not (ty `S.member` seen) ] ++ txs)
   where
    tys = children tx
    new = any (not . (`S.member` seen)) tys

  children tx = [ y :<-- ((n,term tx):trace tx) | (n,y) <- step rule env (term tx) ]

normalFormsFuelTraceWithGraph :: (Show a, Ord a, Rec a)
                              => RuleEnv a -> Int -> Rule a -> a -> [[(String,a)]]
normalFormsFuelTraceWithGraph env fuel rule t =
  [ toList tx
  | Just tx <- leaves (dag (trsGraphFuelTrace env fuel rule t))
  ]

