{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative

import Control.Monad ((<=<))

import Data.Functor

import Prelude (($), (=<<))

import Verse4
import Supply

test1 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  fork $ (writeVar y =<< readVar x) <|> writeVar y 1
  writeVar x 1 <|> writeVar y 2
  readVar y

test2 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  fork $ writeVar y 1 <|> writeVar y 2
  writeVar x 1 <|> pure ()
  readVar y

test3 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  (
    do
      fork $ writeVar y =<< readVar x
      empty
    ) <|> (
    do
      writeVar x 1
      writeVar y 2
    )
  readVar y

test4 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ readVar x <|> readVar y
  writeVar x 1
  writeVar y 2
  readVar z

test5 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ readVar x <|> pure 5 <|> readVar y <|> pure 6
  writeVar x =<< pure 1 <|> pure 2
  writeVar y =<< pure 3 <|> pure 4
  readVar z

test6 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ pure 5 <|> readVar y <|> pure 6
  writeVar x =<< pure 1 <|> pure 2
  writeVar y =<< pure 3 <|> pure 4
  readVar z

test7 = runSupplyT $ runVerseT $ do
  x <- freshVar
  fork $ do
    fork $ void $ readVar x
    void $ readVar x
  writeVar x =<< pure 1 <|> pure 2
