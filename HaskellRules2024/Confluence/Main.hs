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
  forAllShrink (prep `fmap` arbitrary) shrinkExpr $ \t ->
  forAllBlind (liftArbitrary arbPermutation) $ \permf ->
    let tr1 = normalize trs2024 t
        tr2 = normalize (\e -> permf e (trs2024 e)) t
     in whenFail (do putStrLn "== TRACE #1 =="
                     printTrace tr1
                     putStrLn "== TRACE #2 =="
                     printTrace tr2) $
          fmap norm tr1 == fmap norm tr2
 where
  shrinkExpr e = filter valid (shrink e ++ map snd (trs2024 e))

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

