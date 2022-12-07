module TRS.NormalForm(
  normalFormsFuelTrace,
  normalFormsFuel,
  normalFormsTrace,
  normalForms,
  normalFormFuelTrace,
  ) where
import Data.List(intercalate)
import TRS.System(TRSystem(..))
import TRS.TRS (Rec, normalFormsFuelTracePlain, normalFormFuelTracePlain, Trace)
import TRS.TRSGraph(normalFormsFuelTraceWithGraph)
import TRS.Traced(Traced(..))

normalFormsFuelTrace :: (Show a, Ord a, Rec a)
                     => TRSystem a -> Int -> a -> [Traced a]
normalFormsFuelTrace sys n | rulesHaveStructural sys = normalFormsFuelTraceWithGraph env n rls
                           | otherwise               = normalFormsFuelTracePlain     env n rls
  where env = ruleEnv sys
        rls = rules sys

normalForms :: (Show a, Ord a, Rec a) => TRSystem a -> a -> Trace a
normalForms sys t = normalFormsFuel sys (-1) t

normalFormsFuel :: (Show a, Ord a, Rec a) => TRSystem a -> Int -> a -> Trace a
normalFormsFuel sys n t =
  [ (sequ (map fst tr), x)
  | (x :<-- tr) <- normalFormsFuelTrace sys n t
  ]
 where
  sequ [] = "refl"
  sequ as = intercalate ";" as

normalFormsTrace :: (Show a, Ord a, Rec a) => TRSystem a -> a -> [Traced a]
normalFormsTrace sys t = normalFormsFuelTrace sys (-1) t

normalFormFuelTrace :: (Show a, Ord a, Rec a)
                     => TRSystem a -> Int -> a -> [Trace a]
normalFormFuelTrace sys n | rulesHaveStructural sys = error "normalFormFuelTraceWithGraph not implemented"
                          | otherwise               = normalFormFuelTracePlain     env n rls
  where env = ruleEnv sys
        rls = rules sys

