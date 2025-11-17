{-# LANGUAGE QuasiQuotes #-}
module Main
  ( main
  ) where

import Prettyprinter

import Verse.TH

main :: IO ()
main = print . pretty =<< [verseFile|examples/1a.verse|]
