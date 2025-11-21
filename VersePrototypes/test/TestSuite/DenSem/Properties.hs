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

import Hedgehog hiding (annotate)

type DSProperty a = (SrcExpr -> PropertyT IO a) -> Property

ast_size :: Int
ast_size = 1

-- (e1 ||| e2) | e3 === (e1 | e3) ||| (e2 | e3)
uChoiceDistributesOverChoice :: DSProperty String
uChoiceDistributesOverChoice f = property $ do
  (e1,e2,e3) <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    e2 <- genExpr n
    e3 <- genExpr n
    return (e1,e2,e3)
  let lhs = mkUChoice e1 e2 `Choice` e3
      rhs = mkUChoice (e1 `Choice` e2) (e2 `Choice` e3)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- e | fail == e
failIsChoiceUnit :: DSProperty String
failIsChoiceUnit fun = property $ do
  (e, f) <- gen $ do
    n <- genSize ast_size
    e <- genExpr n
    f <- genFail
    return (e,f)
  let !lhs = e `Choice` f
      !rhs = e
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- fun lhs
  r' <- fun rhs
  l' === r'

-- (e1 | e2) | e3 === e1 | (e2 | e3)
choiceIsAssociative :: DSProperty String
choiceIsAssociative f = property $ do
  (e1,e2,e3) <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    e2 <- genExpr n
    e3 <- genExpr n
    return (e1,e2,e3)
  let !lhs = (e1 `Choice` e2) `Choice` e3
      !rhs = e1 `Choice` (e2 `Choice` e3)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- (e1 ||| e2) ||| e3 === e1 ||| (e2 ||| e3)
uChoiceIsAssociative :: DSProperty String
uChoiceIsAssociative f = property $ do
  (e1,e2,e3) <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    e2 <- genExpr n
    e3 <- genExpr n
    return (e1,e2,e3)
  let !lhs = mkUChoice (mkUChoice e1 e2) e3
      !rhs = mkUChoice e1 (mkUChoice e2 e3)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

--- e1 ||| e1 === e1
uChoiceIsIdempotent :: DSProperty String
uChoiceIsIdempotent f = property $ do
  e <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    return e1
  let !lhs = mkUChoice e e
      !rhs = e
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- k;e === e
literalsAreSeqUnit :: DSProperty String
literalsAreSeqUnit f = property $ do
  (k,e) <- gen $ do
    n  <- genSize ast_size
    k  <- genLiteral
    e  <- genExpr n
    return (k,e)
  let !lhs = k `Seq` e
      !rhs = e
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- (e1;e2);e3 === e1;(e2;e3)
seqIsAssociative :: DSProperty String
seqIsAssociative f = property $ do
  (e1,e2,e3) <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    e2 <- genExpr n
    e3 <- genExpr n
    return (e1,e2,e3)
  let !lhs = (e1 `Seq` e2) `Seq` e3
      !rhs = e1 `Seq` (e2 `Seq` e3)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- (e1 | e2); e3 === (e1; e3) | (e2; e3)
choiceDistributesOverSequence :: DSProperty String
choiceDistributesOverSequence f = property $ do
  (e1,e2,e3) <- gen $ do
    n  <- genSize ast_size
    e1 <- genExpr n
    e2 <- genExpr n
    e3 <- genExpr n
    return (e1,e2,e3)
  let !lhs = (e1 `Choice` e2) `Seq` e3
      !rhs = (e1 `Seq` e3) `Choice` (e2 `Seq` e3)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f lhs
  r' <- f rhs
  l' === r'

-- fun( e1a | e1b ){e2}[a3] === fun(e1a){e2}[a3] | fun(e1b){e2}[a3]
funDomainsDistributeOverChoice :: DSProperty String
funDomainsDistributeOverChoice f = property $ do
  (e1a,e1b,e2,a3) <- gen $ do
    n   <- genSize ast_size
    e1a <- genExpr n
    e1b <- genExpr n
    e2  <- genExpr n
    a3  <- genANFAtom n
    return (e1a,e1b,e2,a3)
  let !lhs = Function Closed (e1a `Choice` e1b) effSucceeds e2
      !rhs = Function Closed e1a effSucceeds e2
            `Choice` Function Closed e1b effSucceeds e2
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f (ApplyD lhs a3)
  r' <- f (ApplyD rhs a3)
  l' === r'

-- fun( e1a | e1b ){e2}[a3] === fun(e1a){e2}[a3] | fun(e1b){e2}[a3]
funDomainsDistributeOverUChoice :: DSProperty String
funDomainsDistributeOverUChoice f = property $ do
  (e1a,e1b,e2,a3) <- gen $ do
    n   <- genSize ast_size
    e1a <- genExpr n
    e1b <- genExpr n
    e2  <- genExpr n
    a3  <- genANFAtom n
    return (e1a,e1b,e2,a3)
  let !lhs = Function Closed (mkUChoice e1a e1b) effSucceeds e2
      !rhs = mkUChoice (Function Closed e1a effSucceeds e2)
             (Function Closed e1b effSucceeds e2)
  annotate "lhs: " lhs
  annotate "rhs: " rhs
  l' <- f (ApplyD lhs a3)
  r' <- f (ApplyD rhs a3)
  l' === r'
