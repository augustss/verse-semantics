-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.Utils
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This is a compat module for the testsuite. It aggregates all the stuff that
-- TestSuite needs to write tests into a single module.
--
-----------------------------------------------------------------------------

module TestSuite.Utils
  ( Set
  , ENV
  , module Epic.Print
  , module FrontEnd.Expr
  , exprTrace
  ) where

import Set(Set)
import ENVP (ENV)
import Epic.Print
import FrontEnd.Expr

import Debug.Trace (trace)

-- | convience function for printing generated ASTs. A good place to put it is in
-- the call to assert
exprTrace :: SrcExpr -> a -> a
exprTrace e = trace (show $ pPrint e)
