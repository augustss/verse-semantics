module Data.Function.Extras
  ( (>.>)
  ) where

(>.>) :: (a -> b) -> (b -> c) -> a -> c
(>.>) = flip (.)
infixr 1 >.>
