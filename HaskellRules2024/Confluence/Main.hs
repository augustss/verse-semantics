module Main where

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
  forAllShrinkBlind arbFork shrinkFork $ \(p, q :<-- qs1) ->
    let np :<-- ps  = normalize trs2024 p
        nq :<-- qs2 = normalize trs2024 q
     in whenFail (do putStrLn "== TRACE #1 =="
                     printTrace (np :<-- ps)
                     putStrLn "== TRACE #2 =="
                     printTrace (nq :<-- (qs2 ++ qs1))) $
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
main = quickCheckWith stdArgs{ maxSuccess = 9999 } prop_Confluent

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

