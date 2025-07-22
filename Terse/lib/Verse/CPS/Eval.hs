module Verse.CPS.Eval
  ( eval
  ) where

import Fix

import Verse.CPS.Exp (LExp)
import Verse.CPS.Val (Val)

eval :: LExp -> IO (Maybe [Fix Val])
eval = undefined
