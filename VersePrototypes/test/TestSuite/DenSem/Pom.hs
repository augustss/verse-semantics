-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.DenSem.Pom
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module defines semantic equivalences as property-based tests. We expect
-- that these equivalences hold in our denotational semantics. Right now
-- (Nov. 6th 2025) we only test the PomPom semantic function.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wall          #-}

module TestSuite.DenSem.Pom
  ( properties
  ) where

import qualified PomPom (den)
import FrontEnd.Expr
import Epic.Print

import TestSuite.FrontEnd.Expr

import Test.Tasty
import Test.Tasty.Falsify (testProperty, assert)
import qualified Test.Falsify.Predicate as P

import Test.Falsify.Property            (Property, gen)

--------------------------------------------------------------------------------
--
--
--------------------------------------------------------------------------------

-----------------------------------------------
--
--              Properties
--
-----------------------------------------------

properties :: TestTree
properties = testGroup "Semantic Identities"
  [ testProperty "k;e === e" literals_always_succeed
  , testProperty "e | fail === e" anything_and_fail_is_fail
  , testProperty "e ||| e  === e" unordered_idempotency
  , testProperty "(e1 ||| e2) | e3 === (e1 | e3) ||| (e2 | e3)"
    unordered_distributes_over_choice
  , testProperty "(e1;e2);e3 === e1;(e2;e3)" sequence_is_associative
  ]

literals_always_succeed :: Property ()
literals_always_succeed = do
  n  <- gen $ genSize 5
  k  <- gen genLiteral
  e  <- gen $ genExpr n
  let lhs = k `Seq` e
      rhs = e
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), PomPom.den $ k `Seq` e)
    P..$ ("rhs: " ++ show (pPrint rhs), PomPom.den e)

anything_and_fail_is_fail :: Property ()
anything_and_fail_is_fail = do
  n <- gen $ genSize 5
  e <- gen $ genExpr n
  f <- gen $ genFail
  let lhs = e `Choice` f
      rhs = e
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), PomPom.den $ e `Choice` f)
    P..$ ("rhs: " ++ show (pPrint rhs), PomPom.den $ e)

unordered_distributes_over_choice :: Property ()
unordered_distributes_over_choice = do
  n  <- gen $ genSize 2 -- notice shallower
  e1 <- gen $ genExpr n
  e2 <- gen $ genExpr n
  e3 <- gen $ genExpr n
  let lhs = mkUChoice e1 e2 `Choice` e3
      rhs = mkUChoice (e1 `Choice` e2) (e2 `Choice` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), PomPom.den lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), PomPom.den rhs)

unordered_idempotency :: Property ()
unordered_idempotency = do
  n  <- gen $ genSize 5
  e1 <- gen $ genExpr n
  let lhs = mkUChoice e1 e1
      rhs = e1
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), PomPom.den lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), PomPom.den rhs)

sequence_is_associative :: Property ()
sequence_is_associative = do
  n  <- gen $ genSize 3 -- shallower
  e1 <- gen $ genExpr n
  e2 <- gen $ genExpr n
  e3 <- gen $ genExpr n
  let lhs = (e1 `Seq` e2) `Seq` e3
      rhs = e1 `Seq` (e2 `Seq` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), PomPom.den lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), PomPom.den rhs)
