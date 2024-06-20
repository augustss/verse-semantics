module Confluence.Main where

import Rules.Core
import Rules.TRS2024 as TRS2024
import TRS.Traced
import Test.QuickCheck

--------------------------------------------------------------------------------

trs2024 :: Rule
trs2024 = everywhere TRS2024.rules

prop_Valid t0 =
  valid (prep t0)

prop_Confluent =
  forAllShrinkBlind arbFork shrinkFork $ \(p, q :<-- tr) ->
    let tr1@(np :<-- _)  = normalize trs2024 p
        tr2@(nq :<-- qs) = normalize trs2024 q
     in whenFail (do putStrLn "== TRACE #1 =="
                     printTrace tr1
                     putStrLn "== TRACE #2 =="
                     printTrace (nq :<-- (qs ++ tr))) $
          norm np == norm nq
 where
  arbFork =
    do p <- prep `fmap` arbitrary
       permf <- liftArbitrary arbPermutation
       let tr = normalize (\e -> permf e (trs2024 e)) p
       return (p,tr)
 
  shrinkFork (p, _ :<-- tr) = 
    [ (p', q' :<-- [(s,p')])
    | p' <- case tr of
              _:_:_ -> [ r | (_,r) <- tr ]
              _     -> shrink p ++ map snd (trs2024 p)
    , valid p'
    , (s,q') <- trs2024 p'
    ]

--------------------------------------------------------------------------------

main :: IO ()
--main = quickCheck prop_Valid
main = quickCheck prop_Confluent

--------------------------------------------------------------------------------

-- helper function
arbPermutation :: Gen ([a] -> [a])
arbPermutation =
  do is <- infiniteListOf (choose (0,maxBound::Int))
     return (\xs -> perm is (length xs) xs)
 where
  perm _is     0 _xs = []
  perm ~(i:is) n  xs = (xs!!j) : perm is (n-1) (take j xs ++ drop (j+1) xs)
   where
    j = i `mod` n

--------------------------------------------------------------------------------

