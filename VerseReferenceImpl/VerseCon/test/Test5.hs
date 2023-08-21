{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative

import Control.Monad ((<=<))

import Prelude (Num (..), ($), (=<<))

import Verse5
import Ref
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

test7 = runSupplyT $ runVerseT do
  x <- newRef 1
  all $ modifyRef' x (+ 1) <|> modifyRef' x (+ 2)
  readRef x

test8 = runSupplyT $ runVerseT do
  x <- newRef 1
  all $ (modifyRef' x (+ 1) *> empty) <|> modifyRef' x (+ 2)
  readRef x

test9 = runSupplyT $ runVerseT do
  x <- newRef 1
  all do
    modifyRef' x (+ 3)
    (modifyRef' x (+ 1) *> empty) <|> modifyRef' x (+ 2)
  readRef x

test10 = runSupplyT $ runVerseT do
  x <- newRef 1
  all do
    modifyRef' x (+ 3)
    modifyRef' x (+ 1) <|> (modifyRef' x (+ 2) *> empty)
  readRef x

test11 = runSupplyT $ runVerseT do
  x <- newRef 1
  all do
    modifyRef' x (+ 4)
    all do
      modifyRef' x (+ 3)
      modifyRef' x (+ 1) <|> (modifyRef' x (+ 2) *> empty)
    empty
  readRef x
