-----------------------------------------------------------------------------
-- |
-- Module    : Test
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- Test suite runner for verse-libs.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Test.Tasty
import System.Environment (setEnv)

import qualified TestSuite.DenSem.PomPom


-- the test runner entry point
main :: IO ()
main = do
  setEnv "TASTY_COLOR" "ALWAYS"
  setEnv "TASTY_FALSIFY_TESTS" "100"  -- number of prop-generated tests
  defaultMain tests

{- Note [Reading the property-based test output]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is some example output:

  lhs: operator'|||'[a, a] | b /= rhs: operator'|||'[a | a, a | b]
  lhs: operator'|||'[a, a] | b    : {[{{u=a,v=a}},{{u=b,v=b}}]}
  rhs: operator'|||'[a | a, a | b]: {[{{u=a,v=a}},{{u=a,v=a}}],
  [{{u=a,v=a}},{{u=b,v=b}}]}

This is pretty printed output of the AST. `operator'|||'[a,a]` is the prettified
output of `a ||| a`. `a` is a Variable. So this output is saying:

  The generated expression was: (a ||| a) | b
  The property tested was: (a ||| a) | b == (a | a) ||| (a | b)

  But this was found not to be true.
  The lhs was `(a ||| a) | b` and resulted in `{[{{u=a,v=a}},{{u=b,v=b}}]}`
  But the rhs was `(a | a) ||| (a | b)` and resulted in `{[{{u=a,v=a}},{{u=a,v=a}}]`
  and these are not equal.
-}


tests :: TestTree
tests = testGroup "Libs"
  [ TestSuite.DenSem.PomPom.properties
  ]
