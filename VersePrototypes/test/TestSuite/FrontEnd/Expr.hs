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

genAlphaNum :: Gen Char
genAlphaNum = Gen.elem . NE.fromList $ [ 'a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genAlpha :: Gen Char
genAlpha = Gen.elem . NE.fromList $ [ 'a'..'z'] ++ ['A'..'Z']

genString :: Gen String
genString = Gen.list (Range.between (1, 4)) genAlpha -- only gen string up to 4 chars

genScientific :: Gen Scientific
genScientific = scientific <$> coeff <*> expnt
  where
    -- avoid type defaults
    ten, twelve, ten_to_twelve :: Int
    ten           = 10
    twelve        = 12
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

genUChoice :: Int -> [Ident] -> Gen SrcExpr
genUChoice n env = do
  l <- genExpr n env
  r <- genExpr n env
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

genLitOrVar :: [Ident] -> Gen SrcExpr
genLitOrVar env = Gen.choose genLiteral (genVariable env)

-- | Gen a variable, 'is' is a list of bound idents. When 'is' is empty,
-- randomly gen a variable, when 'is' is not empty preferentially pick a bound
-- Ident to create a reference. There is no handling for collisions
-- (accidentally generating the same Ident twice). But this is not important,
-- its the non-empty case!
genVariable :: [Ident] -> Gen SrcExpr
genVariable [] = Variable <$> genIdent
genVariable (fmap pure . NE.fromList -> is) = -- [Ident] -> [Gen Ident]
  Variable
  <$> Gen.frequency [ (1, genIdent)      -- 1/3rd of the time gen random
                    , (2, Gen.oneof is)  -- 2/3rd of the time use a bound var
                    ]

genDone :: Gen SrcExpr
genDone = pure $ Lit (LInt 0)

-- i := e
genDefineE :: Int -> [Ident] -> Gen SrcExpr
genDefineE n env = DefineE <$> genIdent <*> genExpr n env

-- f[e]
genApplyD :: Int -> [Ident] -> Gen SrcExpr
genApplyD n env = ApplyD <$> genExpr n [] <*> genExpr n env

-- array{e1;e2...}
genArray :: [Ident] -> Gen SrcExpr
genArray env = do
  l <- Gen.inRange (Range.between (0,2)) -- only gen arrays up to 2 elements
  Array <$> replicateM l (genLitOrVar env)

-- e1;e2
genSeq :: Int -> [Ident] -> Gen SrcExpr
genSeq n env = Seq <$> genExpr n env <*> genExpr n env

-- e1|e2
genChoice :: Int -> [Ident] -> Gen SrcExpr
genChoice n env = Choice <$> genExpr n env <*> genExpr n env

-- e1 = e2
genUnify :: Int -> [Ident] -> Gen SrcExpr
genUnify n env = Unify <$> genExpr n env <*> genExpr n env

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

genIf3 :: Int -> [Ident] -> Gen SrcExpr
genIf3 n env =
  let n2 = n `div` 2 -- to keep generated ASTs reasonable
   in If3 <$> genExpr n2 env <*> genExpr n2 env <*> genExpr n2 env

genEff :: Gen Eff
genEff = Eff <$> c_eff <*> pure SComputes -- only gen pure functions for now
  where
    c_eff = Gen.inRange $ Range.enum (CFails, CIterates)

genFun :: Int -> [Ident] -> Gen SrcExpr
genFun n env = do
  aperture <- pure Closed
  eff      <- genEff
  domain   <- genExpr n env
  let !bndrs = getVisibleBinders domain
  range    <- genExpr n (bndrs ++ env)
  return $ Function aperture domain eff range

genExpr :: Int -> [Ident] -> Gen SrcExpr
genExpr 0 env = genVariable env
genExpr n' env =
  let n = n' - 1
   in
    Gen.oneof
    $ NE.fromList
    [ genVariable  env
    , genDefineE n env
    , genApplyD  n env
    , genArray     env
    , genSeq     n env
    , genChoice  n env
    , genUnify   n env
    , genPrimOp
    , genIf3     n env
    , genFun     n env
    ]

-- we ANF means generate variables, literals, and lambdas
-- genANFAtom :: Int -> Gen SrcExpr
-- genANFAtom 0 = TODO:
