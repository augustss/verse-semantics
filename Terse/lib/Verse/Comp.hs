{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Comp
  ( comp
  ) where

import Control.Monad

import Language.Haskell.TH (Q)
import Language.Haskell.TH qualified as TH

import Verse.Comp.Internal
import Verse.Exp
import Verse.Monad
import Verse.Run qualified as Run
import Verse.Run.S
import Verse.Run.Val qualified as Val

comp :: LExp -> Q TH.Exp
comp e = [| runVerseT $ do
  alloc <- Val.newLam () $ const Run.alloc
  read <- Val.newLam () $ const Run.read
  write <- Val.newLam () $ const Run.write
  getLine <- Val.newLam () $ const Run.getLine
  readInt <- Val.newLam () $ const Run.readInt
  print <- Val.newLam () $ const Run.print
  minus <- Val.newLam () $ const Run.minus
  s1 <- newS
  s2 <- freshS
  x <- Val.freeze =<< $(runCompT (comp' 's1 's2 e)
    [ ("Alloc", 'alloc)
    , ("Read", 'read)
    , ("Write", 'write)
    , ("GetLine", 'getLine)
    , ("ReadInt", 'readInt)
    , ("Print", 'print)
    , ("operator'-'", 'minus)
    ])
  readChoiceFree s2
  readStoreFree s2
  pure x |]
