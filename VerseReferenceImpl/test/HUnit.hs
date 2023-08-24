{-# LANGUAGE LambdaCase #-}
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
  ]

data Val a
  = Int {-# UNPACK #-} !Int
  | Tuple [a] deriving (Show, Eq, Functor, Foldable, Traversable)

instance RowMatchable Val

instance ZipMatchable Val where
  zipMatch = curry $ \ case
    (Int x, Int y) -> guard (x == y) $> Int x
    (Tuple x, Tuple y) -> Tuple <$> zipMatch x y
    _ -> Nothing

instance Freezable a b m => Freezable (Val a) (Val b) m where
  freeze = traverse freeze

test1 :: Test
test1 = TestCase $ do
  z <- runSupplyT $ runVerseT $ do
    x <- freshVar
    y <- all $ unify x =<< newVar (Int 1)
    unify x =<< newVar =<< pure (Int 1) <|> pure (Int 2)
    _ <- readIVar y
    freeze' x
  z @?= Just [Known (Int 1), Known (Int 2)]

test2 :: Test
test2 = TestCase $ do
  z <- runSupplyT $ runVerseT $ do
    x <- freshIVar
    y <- freshIVar
    fork $ do
      xs <- readIVar =<< one (readIVar x)
      writeIVar y xs
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]

test3 :: Test
test3 = TestCase $ do
  z <- runSupplyT $ runVerseT $ do
    x <- freshIVar
    y <- freshIVar
    fork $ writeIVar y =<< readIVar x
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]
