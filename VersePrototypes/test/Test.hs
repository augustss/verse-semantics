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

import qualified TestSuite.DenSem.Pom


-- the test runner entry point
main :: IO ()
main = do
  setEnv "TASTY_COLOR" "ALWAYS"
  setEnv "TASTY_FALSIFY_TESTS" "200"
  defaultMain tests

tests :: TestTree
tests = testGroup "Libs"
  [ TestSuite.DenSem.Pom.properties
  ]
