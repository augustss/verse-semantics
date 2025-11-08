-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.DenSem.Properties
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module defines functions that when given a semantic function, generate a
-- property based test that tests a property we expect the semantic function to
-- uphold.
--
-----------------------------------------------------------------------------

module TestSuite.DenSem.Properties where

import TestSuite.Utils
import TestSuite.FrontEnd.Expr

import Test.Tasty.Falsify (assert)
import qualified Test.Falsify.Predicate as P
import Test.Falsify.Property            (gen, Property)


type DSProperty a = (SrcExpr -> Set [ENV]) -> Property a

ast_size :: Int
ast_size = 4

-- (e1 ||| e2) | e3 === (e1 | e3) ||| (e2 | e3)
uChoiceDistributesOverChoice :: DSProperty ()
uChoiceDistributesOverChoice f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  e2 <- gen $ genExpr n []
  e3 <- gen $ genExpr n []
  let lhs = mkUChoice e1 e2 `Choice` e3
      rhs = mkUChoice (e1 `Choice` e2) (e2 `Choice` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)

-- e | fail == e
failIsChoiceUnit :: DSProperty ()
failIsChoiceUnit fun = do
  n <- gen $ genSize ast_size
  e <- gen $ genExpr n []
  f <- gen $ genFail
  let lhs = e `Choice` f
      rhs = e
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), fun lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), fun rhs)

-- (e1 | e2) | e3 === e1 | (e2 | e3)
choiceIsAssociative :: DSProperty ()
choiceIsAssociative f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  e2 <- gen $ genExpr n []
  e3 <- gen $ genExpr n []
  let lhs = (e1 `Choice` e2) `Choice` e3
      rhs = e1 `Choice` (e2 `Choice` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)

-- (e1 ||| e2) ||| e3 === e1 ||| (e2 ||| e3)
uChoiceIsAssociative :: DSProperty ()
uChoiceIsAssociative f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  e2 <- gen $ genExpr n []
  e3 <- gen $ genExpr n []
  let lhs = mkUChoice (mkUChoice e1 e2) e3
      rhs = mkUChoice e1 (mkUChoice e2 e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)

--- e1 ||| e1 === e1
uChoiceIsIdempotent :: DSProperty ()
uChoiceIsIdempotent f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  let lhs = mkUChoice e1 e1
      rhs = e1
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)

-- k;e === e
literalsAreSeqUnit :: DSProperty ()
literalsAreSeqUnit f = do
  n  <- gen $ genSize ast_size
  k  <- gen genLiteral
  e  <- gen $ genExpr n []
  let lhs = k `Seq` e
      rhs = e
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f $ k `Seq` e)
    P..$ ("rhs: " ++ show (pPrint rhs), f e)

-- (e1;e2);e3 === e1;(e2;e3)
seqIsAssociative :: DSProperty ()
seqIsAssociative f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  e2 <- gen $ genExpr n []
  e3 <- gen $ genExpr n []
  let lhs = (e1 `Seq` e2) `Seq` e3
      rhs = e1 `Seq` (e2 `Seq` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)

-- (e1 | e2); e3 === (e1; e3) | (e2; e3)
choiceDistributesOverSequence :: DSProperty ()
choiceDistributesOverSequence f = do
  n  <- gen $ genSize ast_size
  e1 <- gen $ genExpr n []
  e2 <- gen $ genExpr n []
  e3 <- gen $ genExpr n []
  let lhs = (e1 `Choice` e2) `Seq` e3
      rhs = (e1 `Seq` e3) `Choice` (e2 `Seq` e3)
  assert $
    P.eq
    P..$ ("lhs: " ++ show (pPrint lhs), f lhs)
    P..$ ("rhs: " ++ show (pPrint rhs), f rhs)
