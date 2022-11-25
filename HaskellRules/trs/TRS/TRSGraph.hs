module TRS.TRSGraph where

import qualified Data.Set as S
import qualified Data.Map as M

import TRS.TRS
import TRS.Traced
import TRS.Graph

-- depth first graph building 
trsGraphFuelTrace :: (Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Graph (Maybe (Traced a))
trsGraphFuelTrace env afuel rule x = M.fromListWith (++) (go S.empty afuel [start x])
 where
  go _seen _ [] =
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
--    new = any (not . (`S.member` seen)) tys

  children tx = [ y :<-- ((n,term tx):trace tx) | (n,y) <- step rule env (term tx) ]

-- breadth-first graph building
trsGraphFuelTrace' :: (Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> Graph (Maybe (Traced a))
trsGraphFuelTrace' env afuel rule x = M.fromListWith (++) (go S.empty afuel [] [start x])
 where
  go _seen _ [] [] =
    []

  go seen fuel q [] =
    go seen fuel [] (reverse q)

  go seen 0 txs1 txs2 =
    [ (Just tx, map Just (filter (`S.member` nodes) tys)
             ++ [ Nothing | any (not . (`S.member` nodes)) tys ])
    | (tx,tys) <- txys
    ] ++
    [ (Nothing, [])
    | any (not . (`S.member` nodes)) (concatMap snd txys)
    ]
   where
    txs   = txs2 ++ reverse txs1
    nodes = seen `S.union` S.fromList txs
    txys  = [ (tx, children tx) | tx <- txs ]

  go seen fuel q (tx:txs) =
    (Just tx, map Just tys) :
    go (S.insert tx seen) ((fuel-1) `max` 0)
       ([ ty | ty <- tys, not (ty `S.member` seen) ] ++ q) txs
   where
    tys = children tx
--    new = any (not . (`S.member` seen)) tys

  children tx = [ y :<-- ((n,term tx):trace tx) | (n,y) <- step rule env (term tx) ]

normalFormsFuelTraceWithGraph :: (Show a, Ord a, Rec a)
                              => RuleEnv a -> Int -> Rule a -> a -> [Traced a]
normalFormsFuelTraceWithGraph env fuel rule t =
  [ tx
  | Just tx <- leaves (dag (trsGraphFuelTrace env fuel rule t))
  ]

