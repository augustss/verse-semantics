{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Rules.Core
import Rules.TRS2024 as TRS2024
import TRS.Traced
import Test.QuickCheck

--------------------------------------------------------------------------------

trs2024_noREC :: Rule
trs2024_noREC = TRS2024.runtimeRules `removeRule` "REC" -- no confluence with REC

prop_Valid :: Expr -> Bool
prop_Valid t0 =
  valid (prep t0)

prop_Confluent :: Property
prop_Confluent =
  forAllShrinkBlind arbFork shrinkFork $ \(p, q :<-- qs1) ->
    let (resp, np :<-- ps)  = normalize lotsOfSteps trs2024_noREC p
        (resq, nq :<-- qs2) = normalize lotsOfSteps trs2024_noREC q
     in whenFail (do putStrLn "== TRACE #1 =="
                     displayTrace (np :<-- ps)
                     putStrLn "== TRACE #2 =="
                     displayTrace (nq :<-- (qs2 ++ qs1))) $
          resp /= NormInvalid && resq /= NormInvalid ==>
            norm np == norm nq
 where
  arbFork :: Gen (Expr, Traced Expr)
  arbFork =
    do p <- prep `fmap` arbitrary
       permf <- liftArbitrary arbPermutation
       let perm_rules :: Rule
           perm_rules = \env e -> permf e (trs2024_noREC env e)
           (_res, tr) = normalize lotsOfSteps perm_rules p
       return (p,tr)

  shrinkFork :: (Expr, Traced Expr) -> [(Expr,Traced Expr)]
  shrinkFork (p, q :<-- tr) =
    [ (p', q' :<-- [(s,p')])
    | (p',s,q') <-
        case tr of
          _:_:_ -> [ (p',s,q')
                   | ((s,p'),q') <- tr `zip` (q : map snd tr)
                   ]
          _     -> [ (p',s,q')
                   | p' <- shrink p ++ map snd (stepRule trs2024_noREC p)
                   , valid p'
                   , (s,q') <- stepRule trs2024_noREC p'
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

