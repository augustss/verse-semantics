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
main = runTestTTAndExit test1

data Val a
  = Int {-# UNPACK #-} !Int deriving (Show, Eq, Functor, Foldable, Traversable)

instance RowMatchable Val

instance ZipMatchable Val where
  zipMatch = curry $ \ case
    (Int x, Int y) -> guard (x == y) $> Int x

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
