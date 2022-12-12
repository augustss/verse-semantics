module TRS.NormalForm(
  normalFormsFuelTrace,
--  normalFormsFuel,
--  normalFormsTrace,
--  normalForms,
  normalFormFuelTrace,
  NormResult(..),
  ) where
import TRS.System(TRSystem(..))
import TRS.TRS (Rec, normalFormsFuelTracePlain, normalFormFuelTracePlain, NormResult(..))
import TRS.TRSGraph(normalFormsFuelTraceWithGraph)

normalFormsFuelTrace :: (Show a, Ord a, Rec a)
                     => TRSystem a -> Int -> a -> NormResult a
normalFormsFuelTrace sys n | rulesHaveStructural sys = normalFormsFuelTraceWithGraph env n rls
                           | otherwise               = normalFormsFuelTracePlain     env n rls
  where env = ruleEnv sys
        rls = rules sys

{-
normalForms :: (Show a, Ord a, Rec a) => TRSystem a -> a -> NormResult a
normalForms sys t = normalFormsFuel sys (-1) t

normalFormsFuel :: (Show a, Ord a, Rec a) => TRSystem a -> Int -> a -> NormResult a
normalFormsFuel sys n t =
  [ (sequ (map fst tr), x)
  | (x :<-- tr) <- normalFormsFuelTrace sys n t
  ]
 where
  sequ [] = "refl"
  sequ as = intercalate ";" as

normalFormsTrace :: (Show a, Ord a, Rec a) => TRSystem a -> a -> [Traced a]
normalFormsTrace sys t = normalFormsFuelTrace sys (-1) t
-}
normalFormFuelTrace :: (Show a, Ord a, Rec a)
                     => TRSystem a -> Int -> a -> NormResult a
normalFormFuelTrace sys n | rulesHaveStructural sys = error "normalFormFuelTraceWithGraph not implemented"
                          | otherwise               = normalFormFuelTracePlain env n rls
  where env = ruleEnv sys
        rls = rules sys

