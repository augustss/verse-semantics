module Main where

import Core
import Rules
import TRS
import Bind

--------------------------------------------------------------------------------

x = ident "x"
y = ident "y"
z = ident "z"

ex1 = ARR [] :=: ((GRT :@: ARR [VINT 2, Var x]) :=: INT 3)
ex2 = ARR [] :=: (VAR x :=: INT 3)

--------------------------------------------------------------------------------

main = sequence_ [ print t | t <- normalForms rules ex2 ]

--------------------------------------------------------------------------------


