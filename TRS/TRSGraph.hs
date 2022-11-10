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

  go seen fuel (tx:txs) =
    -- what does x point to?
    [ (Just tx,
        -- all terms it can reach in one step...
        [ Just ty
        | ty <- tys
        , fuel /= 0 || ty `S.member` seen
        ] ++
        -- ...plus Nothing if we're out of fuel and still new terms are generated
        [ Nothing
        | fuel == 0 && new
        ])
    ] ++
    -- add Nothing as a node if we're out of fuel and we still get new terms
    [ (Nothing, [])
    | fuel == 0 && new
    ] ++
    -- take care of the rest of the terms in the stack
    go (S.insert tx seen) ((fuel-1) `max` 0)
       ( -- add the new terms only if we have fuel left
         [ ty
         | fuel /= 0
         , ty <- tys
         , not (ty `S.member` seen)
         ]
      ++ txs
       )
   where
    tys = [ y :<-- ((n,term tx):trace tx) | (n,y) <- step rule env (term tx) ]
    new = any (not . (`S.member` seen)) tys

normalFormsFuelTraceWithGraph :: (Show a, Ord a, Rec a)
                              => RuleEnv a -> Int -> Rule a -> a -> [[(String,a)]]
normalFormsFuelTraceWithGraph env fuel rule t =
  [ toList tx
  | Just tx <- leaves (dag (trsGraphFuelTrace env fuel rule t))
  ]

