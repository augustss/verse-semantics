module TRS.NormalForm(
  normalFormsFuelTrace,
--  normalFormsFuel,
--  normalFormsTrace,
--  normalForms,
  normalFormFuelTrace,
  NormResult(..),
  ) where
import Epic.Print(Pretty)
import TRS.System(TRSystem(..))
import TRS.TRS (Rec, normalFormsFuelTracePlain, normalFormFuelTracePlain, NormResult(..))
import TRS.TRSGraph(normalFormsFuelTraceWithGraph)

normalFormsFuelTrace :: (Ord a, Rec a, Pretty a)
                     => TRSystem a -> Int -> a -> NormResult a
normalFormsFuelTrace sys | rulesHaveStructural sys = normalFormsFuelTraceWithGraph sys
                         | otherwise               = normalFormsFuelTracePlain     sys

normalFormFuelTrace :: (Ord a, Rec a, Pretty a)
                     => TRSystem a -> Int -> a -> NormResult a
normalFormFuelTrace sys -- | rulesHaveStructural sys = error "normalFormFuelTraceWithGraph not implemented"
                        | otherwise               = normalFormFuelTracePlain sys
