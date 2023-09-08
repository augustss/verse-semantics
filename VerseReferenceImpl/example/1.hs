{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Supply
import Control.Monad.Verse

import Data.Functor
import Data.Match

import Prelude hiding (all)

data Val a
  = Int !Integer deriving (Show, Functor, Foldable, Traversable)

instance RowMatchable Val

instance ZipMatchable Val where
  zipMatch = curry $ \ case
    (Int x, Int y) -> guard (x == y) $> Int x

instance Freezable a b m => Freezable (Val a) (Val b) m where
  freeze = traverse freeze

instance Monad m => Freshenable (Val a) m where
  freshen = pure

-- Unification
example1 = runVerseT' do
  x <- freshVar
  y <- freshVar
  unify x y
  unify y =<< newVar (Int 0)
  freeze' x

-- Leniency
example2 = runVerseT' do
  x <- freshVar
  y <- freshVar
  fork $ unify y =<< newVar . plus1 =<< readVar x
  unify x =<< newVar (Int 1)
  freeze' y

-- Choice
example3 = runVerseT' do
  x <- freshVar
  y <- freshVar
  unify x =<< newVar . Int =<< pure 1 <|> pure 2 <|> pure 3
  freeze' x

-- all
example4 = runVerseT' do
  x <- freshVar
  y <- all do
    unify x =<< newVar . Int =<< pure 1 <|> pure 2 <|> pure 3
    readVar x
  unify x =<< newVar . Int =<< pure 1 <|> pure 2 <|> pure 3
  runFreezeT . traverse freeze =<< readIVar y

runVerseT' = runSupplyT . runVerseT

plus1 = \ case
  Int x -> Int $ x + 1
