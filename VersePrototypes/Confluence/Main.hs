{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main where

import Core.Expr
import Core.Bind
import Core.Rule
import Core.Rules
import Core.Traced

import qualified Data.Set as S
import Test.QuickCheck

--------------------------------------------------------------------------------

trs2024_noREC :: Rule Expr
trs2024_noREC = runtimeRules `without` "REC" -- no confluence with REC

prop_Valid :: Expr -> Bool
prop_Valid t0 =
  valid (prep t0)

prop_ValidTrace :: Property
prop_ValidTrace =
  forAllShrink arbExpr shrinkExpr $ \p ->
    let (resp, np :<-- ps)  = normalizeExpr trs2024_noREC lotsOfSteps p
     in whenFail (do putStrLn "== TRACE =="
                     displayTrace (np :<-- ps)) $
          resp == NormOK ==>
            valid np && all (valid . tsPayload) ps
 where
  arbExpr :: Gen Expr
  arbExpr =
    do prep `fmap` arbitrary

  shrinkExpr :: Expr -> [Expr]
  shrinkExpr p = [ p' | p' <- shrink p, valid p' ]

prop_Confluent :: Property
prop_Confluent =
  forAllShrinkBlind arbFork shrinkFork $ \(p, q :<-- qs1) ->
    let (resp, np :<-- ps)  = normalizeExpr trs2024_noREC lotsOfSteps p
        (resq, nq :<-- qs2) = normalizeExpr trs2024_noREC lotsOfSteps q
     in whenFail' (writeFile "counterexample.txt" (show p)) $
        whenFail (do putStrLn "== TRACE #1 =="
                     displayTrace (np :<-- ps)
                     putStrLn "== TRACE #2 =="
                     displayTrace (nq :<-- (qs2 ++ qs1))) $
          resp == NormOK && resq == NormOK ==>
            norm np == norm nq
 where
  arbFork :: Gen (Expr, Traced Expr)
  arbFork =
    do p <- prep `fmap` arbitrary
       permf <- liftArbitrary arbPermutation
       let perm_rules :: Rule Expr
           perm_rules = permute permf trs2024_noREC
           (_res, tr) = normalizeExpr perm_rules lotsOfSteps p
       return (p,tr)

  shrinkFork :: (Expr, Traced Expr) -> [(Expr,Traced Expr)]
  shrinkFork (p, q :<-- tr@(_:_:_)) =
    [ (p, r :<-- tr1)
    , (r, q :<-- tr2)
    ]
   where
    k     = length tr `div` 2 -- 1<=k<length tr
    tr1   = drop k tr
    tr2   = take k tr
    r     = tsPayload (last tr2)

  shrinkFork (p, _) =
    [ (p', tsPayload step :<-- [step `setTsPayload` p'])
    | p' <- shrink p ++ map tsPayload (stepRule trs2024_noREC p)
    , valid p'
    , step <- stepRule trs2024_noREC p'
    ]

  stepRule rule e =
    [ TS { ts_str = lab, ts_verb = v, ts_payload = e' }
    | (e',lab,v) <- run rule (S.fromList (occurs e)) [] [] e
    ]

--------------------------------------------------------------------------------

main :: IO ()
main = quickCheckWith stdArgs{ maxSuccess = 1000 } prop_Confluent

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

