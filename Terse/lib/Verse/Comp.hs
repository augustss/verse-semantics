{-# LANGUAGE TemplateHaskell #-}
module Verse.Comp
  ( comp
  ) where

import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH

import Verse.Comp.Internal
import Verse.Exp
import Verse.Monad
import Verse.Run
import Verse.Run.Val qualified as Val

comp :: LExp -> Q TH.Exp
comp e = [| runVerseT $ do
  s1 <- newS
  s2 <- freshS
  Val.freeze =<< $(runCompT $ comp' 's1 's2 e) |]
