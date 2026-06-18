-----------------------------------------------------------------------------
-- |
-- Module    : Test
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- Test suite runner for verse-parser.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Main where

import Test.Tasty
import System.Environment (setEnv)

import qualified TestSuite.All
import qualified TestSuite.Syntax
import qualified TestSuite.VerseTest
import qualified TestSuite.Compat


-- the test runner entry point
main :: IO ()
main = do
  setEnv "TASTY_COLOR" "ALWAYS"
  defaultMain tests

tests :: TestTree
tests = testGroup "Parser"
  [ TestSuite.Syntax.unitTests
  , TestSuite.All.unitTests
  , TestSuite.VerseTest.unitTests
  , TestSuite.Compat.unitTests
  ]
