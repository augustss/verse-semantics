module Main where

import TRSCore
import RulesPOPL
import TRS
import Bind
import Test.QuickCheck
import qualified Data.Map as M

--------------------------------------------------------------------------------

x = ident "x"
y = ident "y"
z = ident "z"

ex1 :: Expr
ex1 = ARR [] :=: ((GRT :@: VARR [VINT 2, Var x]) :=: INT 3)
ex2 = ARR [] :=: (VAR x :=: INT 3)

--------------------------------------------------------------------------------

main = quickCheck prop_NormalForms

prop_NormalForms p =
  let trs = normalFormsFuelTrace 99 rulesPOPL p in
    case M.toList (M.fromList [ (norm q,tr) | tr@((_,q):_) <- trs ]) of
      (_,tr1):(_,tr2):_ ->
        whenFail (do printTrace tr1
                     putStrLn "----"
                     printTrace tr2) False

      [] -> whenFail (print "DOES NOT TERMINATE") True
      _  -> property True

rules :: Bool -> ERule
rules True  = rulesPOPL
rules False = rulesFRESH
--------------------------------------------------------------------------------
