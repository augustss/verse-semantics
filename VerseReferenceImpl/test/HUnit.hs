{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Main
  ( main
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Supply
import Control.Monad.Verse

import Data.Functor
import Data.Match

import Prelude hiding (all)

import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList
  [ test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  ]

data Val a
  = Int {-# UNPACK #-} !Int
  | Tuple [a] deriving (Show, Eq, Functor, Foldable, Traversable)

instance RowMatchable Val

instance ZipMatchable Val where
  zipMatch = curry $ \ case
    (Int x, Int y) -> guard (x == y) $> []
    (Tuple x, Tuple y) -> zipMatch x y
    _ -> Nothing

instance Freezable a b m => Freezable (Val a) (Val b) m where
  freeze = traverse freeze

test1 :: Test
test1 = TestCase $ do
  z <- runSupplyT $ runVerseT do
    x <- freshVar
    y <- all $ unify x =<< newVar (Int 1)
    unify x =<< newVar =<< pure (Int 1) <|> pure (Int 2)
    _ <- readIVar y
    freeze' x
  z @?= Just [Known (Int 1), Known (Int 2)]

test2 :: Test
test2 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    y <- freshIVar
    fork $ do
      xs <- readIVar =<< one (readIVar x)
      writeIVar y xs
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]

test3 :: Test
test3 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    y <- freshIVar
    fork $ writeIVar y =<< readIVar x
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]

test4 :: Test
test4 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    fork $ do
      fork $ void $ readIVar x
      void $ readIVar x
    writeIVar x =<< pure (1 :: Int) <|> pure 2
  z @?= Just [(), ()]

test5 :: Test
test5 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVarRef =<< newVar (Const (0 :: Int))
    (<|>)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar y
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar y
    freeze' x
  z @?= Just [Known (Const 0), Known (Const 0)]

test6 :: Test
test6 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVarRef =<< newVar (Const (0 :: Int))
    (<|>)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar (Const $ getConst y + 1)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar (Const $ getConst y + 2)
    freeze' x
  z @?= Just [Known (Const 1), Known (Const 3)]

test7 :: Test
test7 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- readIVar =<< all do
      pure () <|> pure ()
      readIVar =<< all do
        pure () <|> pure ()
        freshVar
    unify (head $ head x) =<< newVar (Int 0)
    freeze' x
  z @?= Just [[[Known (Int 0), Unknown], [Unknown, Unknown]]]

test8 :: Test
test8 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- readIVar =<< all do
      readIVar =<< all do
        freshVar
    y <- readIVar =<< all do
      pure $ head x
    unify (head $ head y) =<< newVar (Int 0)
    freeze' x
  z @?= Just [[[Known (Int 0)]]]

data Sub a
  = Any
  | AnyInt
  | AnInt !Integer deriving (Show, Eq, Functor, Foldable, Traversable)

instance RowMatchable Sub where
  rowMatch = curry $ \ case
    (Any, Any) -> LE []
    (Any, AnyInt) -> GE []
    (Any, AnInt _) -> GE []
    (AnyInt, Any) -> LE []
    (AnyInt, AnyInt) -> LE []
    (AnyInt, AnInt _) -> GE []
    (AnInt _, Any) -> LE []
    (AnInt _, AnyInt) -> LE []
    (AnInt x, AnInt y) -> Zip $ guard (x == y) $> []

instance Freezable a b m => Freezable (Sub a) (Sub b) m where
  freeze = traverse freeze

test9 :: Test
test9 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar AnyInt
    unify x y
    freeze' x
  z @?= Just [Known AnyInt]

test10 :: Test
test10 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar AnyInt
    unify x y
    freeze' x
  z @?= Just [Known AnyInt]

test11 :: Test
test11 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar $ AnInt 1
    unify x y
    freeze' x
  z @?= Just [Known $ AnInt 1]

test12 :: Test
test12 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- freshVar
    unify x y
    unify y =<< newVar (AnInt 1)
    freeze' x
  z @?= Just [Known $ AnInt 1]

test13 :: Test
test13 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshVar
    y <- freshVar
    fork $ unify y =<< newVar . Tuple =<< readIVar =<< for
      do
        t <- freshVar
        unify t x
        (unify x =<< newVar (Int 1)) <|> (unify t =<< newVar (Int 2))
        pure t
      do
        \ _ ->
          newVar . Int =<< pure 1 <|> pure 2
    unify x =<< newVar (Int 1)
    freeze' y
  z @?= Just [Known (Tuple [Known (Int 1)]), Known (Tuple [Known (Int 2)])]

test14 :: Test
test14 = TestCase do
  void $ runSupplyT $ runVerseT $ void $ readIVar =<< all do
    void $ readIVar =<< for (void $ pure ()) do
      \ _ -> pure () <|> pure ()
