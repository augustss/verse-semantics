module Main where

import Rules.Core
import Rules.TRS2024 as TRS2024
import TRS.Traced
import Test.QuickCheck

--------------------------------------------------------------------------------

trs2024 :: Rule
trs2024 = TRS2024.evalRules

prop_Valid t0 =
  valid (prep t0)

prop_Confluent =
  forAllShrinkBlind arbFork shrinkFork $ \(p, q :<-- qs1) ->
    let np :<-- ps  = normalize lotsOfSteps trs2024 p
        nq :<-- qs2 = normalize lotsOfSteps trs2024 q
     in whenFail (do putStrLn "== TRACE #1 =="
                     displayTrace (np :<-- ps)
                     putStrLn "== TRACE #2 =="
                     displayTrace (nq :<-- (qs2 ++ qs1))) $
          norm np == norm nq
 where
  arbFork =
    do p <- prep `fmap` arbitrary
       permf <- liftArbitrary arbPermutation
       let tr = normalize lotsOfSteps (\e -> permf e (trs2024 e)) p
       return (p,tr)

  shrinkFork (p, q :<-- tr) =
    [ (p', q' :<-- [(s,p')])
    | (p',s,q') <-
        case tr of
          _:_:_ -> [ (p',s,q')
                   | ((s,p'),q') <- tr `zip` (q : map snd tr)
                   ]
          _     -> [ (p',s,q')
                   | p' <- shrink p ++ map snd (trs2024 p)
                   , valid p'
                   , (s,q') <- trs2024 p'
                   ]
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

