{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative

import Verse3

test1 = runVerseT do
  x <- freshVar
  y <- freshVar
  fork $ (writeVar y =<< readVar x) <|> writeVar y 1
  writeVar x 1 <|> writeVar y 2
  readVar y

test2 = runVerseT do
  x <- freshVar
  y <- freshVar
  fork $ writeVar y 1 <|> writeVar y 2
  writeVar x 1 <|> pure ()
  readVar y

test3 = runVerseT do
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
