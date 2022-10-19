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

--------------------------------------------------------------------

runFresh :: Expr -> [(String, Expr)]
runFresh = normalFormsFuel 99 rulesFRESH . dsFresh

dumpCtx :: (Show a, Show b) => (t -> [(Value -> a, b)]) -> t -> IO ()
dumpCtx c e = mapM_ print [ (ctx (iVar "#") , v) | (ctx, v) <- c e]

eFail :: Expr
eFail = lam Fail

def :: String -> Expr -> Expr
def = DEF . ident

lam :: Expr -> Expr
lam = LAM (ident "_")

iLAM :: String -> Expr -> Expr
iLAM = LAM . ident

iLam :: String -> Expr -> Value
iLam x e = HNF (Lam (Bind (ident x) e))

iVAR :: String -> Expr
iVAR = VAR . ident

iVar :: String -> Value
iVar = Var . ident

iDEF :: String -> Expr -> Expr
iDEF = DEF . ident

iDEFs :: [String] -> Expr -> Expr
iDEFs = defs . map ident

e0 :: Expr
e0 = iDEFs ["f", "f1", "f2"]
        ( (iVAR "f"  :=: iLAM "x" (iLAM "y" (ADD :@: VARR [iVar "x", iVar "y"]))) :>:
            ((iVAR "f1" :=: (iVar "f"  :@: VINT 2)) :>:
              ((iVAR "f2" :=: (iVar "f1" :@: VINT 3)) :>:
                iVAR "f2" )) )

e0' = iDEFs ["f", "f1", "f2"]
        ( (iVAR "f"  :=: iLAM "x" (iLAM "y" (ADD :@: VARR [iVar "x", iVar "y"]))) :>:
          (iVAR "f1" :=: (iVar "f"  :@: VINT 2)) :>:
          (iVAR "f2" :=: (iVar "f1" :@: VINT 3)) :>:
          iVAR "f2" )

e1 =
  iDEFs ["x", "$r1"]
    (
      (INT 5 :=:
        (iVAR "x" :=:
          (
            (iVAR "$r1" :>:
              ( (iLam "x" ((IsINT :@: (iVar "x")) :>: iVAR "x")) :@: iVar "$r1" )
            )
          )
        )
      )
    )

e1' =
  iDEFs ["x", "y"]
    (
      (INT 5 :=: (iVAR "x" :=: iVAR "y") )
      :>:
      iVAR "x"
    )
{-
-- def x in {def $r1 in {5 = (x = ($r1; (\x.isInt(x); x)($r1)))}}

ex x, r1.
  5 = (x = (r1; (\x.isInt(x); x)(r1)))
-}