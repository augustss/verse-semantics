{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main
  ) where

import Data.Functor

import Verse.TH

main :: IO ()
main = void [verseFile|examples/1a.verse|]
