module Main where

import Core
import Rules
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
  case normalFormsFuel 999 rules p of
    []          -> whenFail (print "DOES NOT TERMINATE") False
    q1 : q2 : _ -> whenFail (print q1 >> print q2) False
    _           -> property True

--------------------------------------------------------------------------------


