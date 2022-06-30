module Main where

import TRSCore
import RulesPOPL
import TRS
import Bind
import Test.QuickCheck

--------------------------------------------------------------------------------

x = ident "x"
y = ident "y"
z = ident "z"

ex1 = ARR [] :=: ((GRT :@: VARR [VINT 2, Var x]) :=: INT 3)
ex2 = ARR [] :=: (VAR x :=: INT 3)

--------------------------------------------------------------------------------

main = quickCheck prop_NormalForms

prop_NormalForms p =
  let qs = normalFormsFuel 999 rules p in
    case nub (map norm qs) of
      []          -> whenFail (print "DOES NOT TERMINATE") False
      q1 : q2 : _ -> whenFail (print (qs!!0) >> print (qs!!1)) False
      _           -> property True

--------------------------------------------------------------------------------


