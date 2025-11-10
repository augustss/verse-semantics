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
import Test.Tasty.Hedgehog
import System.Environment (setEnv)

import qualified TestSuite.DenSem.PomPom


-- the test runner entry point
main :: IO ()
main = do
  setEnv "TASTY_COLOR" "ALWAYS"
  defaultMain $
    localOption (HedgehogTestLimit $ Just 50) tests

tests :: TestTree
tests = testGroup "Libs"
  [ TestSuite.DenSem.PomPom.properties
  ]
