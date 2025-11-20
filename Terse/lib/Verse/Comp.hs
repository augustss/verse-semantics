{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Verse.Comp
  ( comp
  ) where

import Language.Haskell.TH (Quote)
import Language.Haskell.TH qualified as TH

import Verse.Comp.Internal
import Verse.Exp
import Verse.Monad
import Verse.Run qualified as Run
import Verse.Run.Val qualified as Val

comp :: Quote m => LExp -> m TH.Exp
comp e = [| runVerseT $ do
  alloc <- Val.newLam () $ const Run.alloc
  read <- Val.newLam () $ const Run.read
  write <- Val.newLam () $ const Run.write
  getLine <- Val.newLam () $ const Run.getLine
  readInt <- Val.newLam () $ const Run.readInt
  print <- Val.newLam () $ const Run.print
  minus <- Val.newLam () $ const Run.minus
  s1 <- newVar ()
  s2 <- newVar ()
  (s1, s2, var) <- $(runCompT (comp' 's1 's2 e)
    [ ("Alloc", 'alloc)
    , ("Read", 'read)
    , ("Write", 'write)
    , ("GetLine", 'getLine)
    , ("ReadInt", 'readInt)
    , ("Print", 'print)
    , ("operator'-'", 'minus)
    ])
  readVar s1
  readVar s2
  Val.freeze var |]
