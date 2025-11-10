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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-} -- only for MonadGen
{-# OPTIONS_GHC -Wall -Werror  #-}

module TestSuite.FrontEnd.Expr where

import FrontEnd.Expr
import TestSuite.Utils

import Hedgehog hiding (Gen)
import qualified Hedgehog       as Gen (Gen)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import Control.Monad (replicateM)
import Data.Scientific (Scientific, scientific)
import Data.Char (toLower)
import Control.Monad.State.Strict

-----------------------------------------------
--
--              Utilities
--
-----------------------------------------------

sample :: IO SrcExpr
sample = Gen.sample . runGen $
  do
  s <- genSize 4
  genExpr s

samplePretty :: IO String
samplePretty = (show . pPrint) <$> sample

gen :: (Monad m, Show a) => Gen a -> PropertyT m a
gen = forAll . runGen

-----------------------------------------------
--
--              Types
--
-----------------------------------------------

newtype Gen a = Gen { runGen' :: StateT Env Gen.Gen a }
  deriving (Functor, Applicative, Monad, MonadState Env)

deriving newtype instance MonadGen Gen

runGen :: Gen a -> Gen.Gen a
runGen = (flip evalStateT) emptyEnv . runGen'

data Env = Env { vars :: [Ident] }
  deriving (Eq, Ord, Show)

emptyEnv :: Env
emptyEnv = Env { vars = [] }

remember :: Ident -> Gen ()
remember i = modify' (\s -> s { vars = i : (vars s) })

bulkRemember :: [Ident] -> Gen ()
bulkRemember i = modify' (\s -> s { vars = i ++ (vars s) })


-----------------------------------------------
--
--              Core Verse
--
-----------------------------------------------

genNat :: Gen Int
genNat = Gen.enum 0 2 -- only gen three ints

genInteger :: Gen Integer
genInteger = Gen.enum 0 1024 -- TODO: only gen positives for now

genString :: Gen String
genString = map toLower <$> Gen.string (Range.singleton 2) Gen.alpha -- always gen strings length 2

genScientific :: Gen Scientific
genScientific = scientific <$> coeff <*> expnt
  where
    coeff :: Gen Integer
    coeff = genInteger

    expnt :: Gen Int
    expnt = Gen.enum 0 10

genLit :: Gen Lit
genLit = Gen.choice $
   [ LInt  <$> genInteger
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
genSize = Gen.constant -- limit the size to 4 for gen'd ASTs

genIdent :: Gen Ident
genIdent = do
  s <- Ident noLoc <$> genString
  remember s
  return s

genLitOrVar :: Gen SrcExpr
genLitOrVar = Gen.choice [genLiteral, genVariable]

-- | Gen a variable, 'is' is a list of bound idents. When 'is' is empty,
-- randomly gen a variable, when 'is' is not empty preferentially pick a bound
-- Ident to create a reference. There is no handling for collisions
-- (accidentally generating the same Ident twice). But this is not important,
-- its the non-empty case!
genVariable :: Gen SrcExpr
genVariable = do
  env <- gets vars
  if null env
    then Variable <$> genIdent
    else Variable
    <$> Gen.frequency [ (2, genIdent)        -- 1/2th of the time gen random
                      , (2, Gen.element env) -- 1/2th of the time use a bound var
                      ]

genDone :: Gen SrcExpr
genDone = pure $ Lit (LInt 0)

-- i := e
genDefineE :: Int -> Gen SrcExpr
genDefineE n = DefineE <$> genIdent <*> genExpr n

-- f[e]
genApplyD :: Int -> Gen SrcExpr
genApplyD n = ApplyD <$> genExpr n <*> genExpr n

-- array{e1;e2...}
genArray :: Gen SrcExpr
genArray = do
  l <- Gen.integral $ Range.linearFrom 0 0 2 -- only gen arrays up to 2 elements
  Array <$> replicateM l genLitOrVar

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
    -- gen_prim = Gen.enum (Add, IsAny)
    gen_prim = Gen.element
      [ Add
      , Sub
      , Mul
      , Div
      , Neg
      , IsInt
      , Gt
      , Lt
      ]

genIf3 :: Int -> Gen SrcExpr
genIf3 n =
  let n2 = n `div` 2 -- to keep generated ASTs reasonable
   in If3 <$> genExpr n2 <*> genExpr n2 <*> genExpr n2

genEff :: Gen Eff
genEff = Eff <$> c_eff <*> pure SComputes -- only gen pure functions for now
  where
    c_eff = Gen.enum CFails CIterates

genFun :: Int -> Gen SrcExpr
genFun n = do
  domain   <- genExpr n
  range    <- genExpr n
  genFun' domain range

genFun' :: SrcExpr -> SrcExpr -> Gen SrcExpr
genFun' domain range = do
  aperture <- pure Closed
  eff      <- genEff
  let !bndrs = getVisibleBinders domain
  bulkRemember bndrs
  return $ Function aperture domain eff range

genExpr :: Int -> Gen SrcExpr
genExpr 0  = genLitOrVar
genExpr n' =
  let n = n' - 1
   in
    Gen.choice
    [ genVariable
    , genDefineE n
    , genApplyD  n
    , genArray
    , genSeq     n
    , genChoice  n
    , genUnify   n
    , genPrimOp
    , genIf3     n
    , genFun     n
    ]

genLam :: Int -> Gen SrcExpr
genLam n = Lam <$> genIdent <*> genExpr n

-- we ANF means generate variables, literals, and lambdas
genANFAtom :: Int -> Gen SrcExpr
genANFAtom _n = Gen.choice [ -- genLam n : TODO: PomPom does not implement yet
                            genLitOrVar
                           ]
