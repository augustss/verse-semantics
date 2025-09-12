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

import qualified TestSuite.Epic.PomSet


-- the test runner entry point
main :: IO ()
main = do
  setEnv "TASTY_COLOR" "ALWAYS"
  defaultMain tests

tests :: TestTree
tests = testGroup "Libs"
  [ TestSuite.Epic.PomSet.properties
  ]
