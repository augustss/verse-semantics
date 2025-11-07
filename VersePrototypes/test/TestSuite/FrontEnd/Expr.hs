-----------------------------------------------------------------------------
-- |
-- Module    : TestSuite.FrontEnd.Expr
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- Property-based testing harness for FrontEnd.Expr. This module defines
-- convience generators for FrontEnd.Expr as intended to used in other testsuite
-- modules to write property-based.
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wall -Werror  #-}

module TestSuite.FrontEnd.Expr where

import FrontEnd.Expr

import Test.Tasty.Falsify (Gen)

import qualified Test.Falsify.Generator as Gen
import qualified Test.Falsify.Range     as Range

import Control.Monad (replicateM)
import Data.Scientific (Scientific, scientific)
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
--
--              Generators
--
--------------------------------------------------------------------------------

-----------------------------------------------
--
--              Core Verse
--
-----------------------------------------------

genNat :: Gen Int
genNat = Gen.inRange (Range.between (0,2))

genInteger :: Gen Integer
genInteger = fromIntegral <$> Gen.inRange (Range.between (-1024 :: Int,1024 :: Int))

genAlpha :: Gen Char
genAlpha = Gen.elem . NE.fromList $ [ 'a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genString :: Gen String
genString = Gen.list (Range.between (1, 10)) genAlpha -- only gen string up to 10 chars

genScientific :: Gen Scientific
genScientific = scientific <$> coeff <*> expnt
  where
    -- avoid type defaults
    ten :: Int
    ten = 10
    twelve :: Int
    twelve = 12
    ten_to_twelve :: Int
    ten_to_twelve = ten ^ twelve

    coeff :: Gen Integer
    coeff = fromIntegral <$> Gen.inRange (Range.between (ten_to_twelve,ten_to_twelve))

    expnt :: Gen Int
    expnt = Gen.inRange (Range.between (-12,12))

genLit :: Gen Lit
genLit = Gen.oneof $
  NE.fromList [ LInt  <$> genInteger
              -- , LPath . Path  <$> genString
              -- , LPtr  <$> genNat
              -- , LChar <$> genAlpha
              -- , LStr  <$> genString
              -- , LRat  <$> genScientific <*> genString -- unsure what the string is for
              ]

genLiteral :: Gen SrcExpr
genLiteral = Lit <$> genLit

genFail :: Gen SrcExpr
genFail = pure Fail

-----------------------------------------------
--              DenSem Compat
--
-- For things that are not yet in the FrontEnd
-- but are being tested in the den sem work
-----------------------------------------------

genUChoice :: Int -> Gen SrcExpr
genUChoice n = do
  l <- genExpr n
  r <- genExpr n
  return $ mkUChoice l r

mkUChoice :: SrcExpr -> SrcExpr -> SrcExpr
mkUChoice l r = ApplyD (Variable (Ident noLoc "operator'|||'")) (Array [l, r])

-----------------------------------------------
--
--              BigCore Verse
--
-----------------------------------------------

genSize :: Int -> Gen Int
genSize n = Gen.inRange $ Range.between (0,n)

genIdent :: Gen Ident
genIdent = Ident noLoc <$> genString

genVariable :: Gen SrcExpr
genVariable = Variable <$> genIdent

genDone :: Gen SrcExpr
genDone = pure $ Lit (LInt 0)

-- i := e
genDefineE :: Int -> Gen SrcExpr
genDefineE n = DefineE <$> genIdent <*> genExpr n

-- f[e]
genApplyD :: Int -> Gen SrcExpr
genApplyD n = ApplyD <$> genExpr n <*> genExpr n

-- array{e1;e2...}
genArray :: Int -> Gen SrcExpr
genArray n = do
  l <- Gen.inRange (Range.between (0,2)) -- only gen arrays up to 2 elements
  Array <$> replicateM l (genExpr n)

-- e1;e2
genSeq :: Int -> Gen SrcExpr
genSeq n = Seq <$> genExpr n <*> genExpr n

-- e1|e2
genChoice :: Int -> Gen SrcExpr
genChoice n = Choice <$> genExpr n <*> genExpr n

-- e1 = e2
genUnify :: Int -> Gen SrcExpr
genUnify n = Unify <$> genExpr n <*> genExpr n

genPrimOp :: Gen SrcExpr
genPrimOp = EPrim <$> gen_prim
  where
    gen_prim :: Gen PrimOp
    -- gen_prim = Gen.inRange (Range.enum (Add, IsAny))
    gen_prim = Gen.oneof
      $ NE.fromList
      $ pure <$>
      [ Add
      , Sub
      , Mul
      , Div
      , Neg
      , IsInt
      , Gt
      , Lt
      ]

genIf3 :: Int ->  Gen SrcExpr
genIf3 n = let n3 = n `div` 3 -- we always gen a smaller condition than a branch
               n2 = n `div` 2 -- to keep generated ASTs reasonable
           in If3 <$> genExpr n3 <*> genExpr n2 <*> genExpr n2

genEff :: Gen Eff
genEff = Eff <$> c_eff <*> pure SComputes -- only gen pure functions for now
  where
    c_eff = Gen.inRange $ Range.enum (CFails, CIterates)

genFun :: Int -> Gen SrcExpr
genFun n = Function <$> pure Closed <*> genExpr n <*> genEff <*> genExpr n

genExpr :: Int -> Gen SrcExpr
genExpr 0 = genDone
genExpr n' =
  let n = n' - 1
   in
    Gen.oneof
    $ NE.fromList
    [ genVariable
    , genDefineE n
    , genApplyD  n
    , genArray   n
    , genSeq     n
    , genChoice  n
    , genUnify   n
    , genPrimOp
    , genIf3     n
    , genFun     n
    ]

-- we ANF means generate variables, literals, and lambdas
-- genANFAtom :: Int -> Gen SrcExpr
-- genANFAtom 0 = genDone
