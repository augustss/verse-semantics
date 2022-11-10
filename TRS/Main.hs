{-# LANGUAGE CPP #-}
module Main where

import TRSCore
import RulesPLDI
import RulesPOPL
import TRS
import TRSGraph
import Graph
import Bind
import Test.QuickCheck
import qualified Data.Map as M
import Data.List
import Data.Function
import System.Environment
--import Debug.Trace

--------------------------------------------------------------------------------

x = ident "x"
y = ident "y"
z = ident "z"

ex1 :: Expr
ex1 = ARR [] :=: ((GRT :@: VARR [VINT 2, Var x]) :=: INT 3)
ex2 = ARR [] :=: (VAR x :=: INT 3)

--------------------------------------------------------------------------------

main = do
  args <- getArgs
  let qcargs = stdArgs{ maxSuccess = 10000 }
      rules = if null args then rulesPLDI else rulesPOPL
  quickCheckWith qcargs (prop_NormalForms2 rules)

#if NO_STRUCT_RULES
-- After reduction, canonicalize results.
-- This makes many (not all) things that are should be equal by the structural rules
-- actually be the same.
final = nubBy ((==) `on` (snd . head)) . map (\ ((s, a) : sas) -> (s, canon a) : sas)
#else
final = id
#endif

prop_NormalForms rules p =
  --trace (show p) $
  let trs = final $ normalFormsFuelTrace defaultTRSFlags 99 rules p in
    case M.toList (M.fromList [ (q,tr) | tr@((_,q):_) <- trs ]) of
      (_,tr1):(_,tr2):_ ->
        whenFail (do putStrLn "===trace:1==="
                     printTrace tr1
                     putStrLn "===trace:2==="
                     printTrace tr2) False

      [] -> whenFail (print "DOES NOT TERMINATE") True
      _  -> property True

prop_NormalForms2 rules p =
  --trace (show p) $
  let trs = normalFormsFuelTraceWithGraph defaultTRSFlags 99 rules' p in
    case M.toList (M.fromList [ (q,tr) | tr@((_,q):_) <- trs ]) of
      (_,tr1):(_,tr2):_ ->
        whenFail (do putStrLn "===trace:1==="
                     printTrace tr1
                     putStrLn "===trace:2==="
                     printTrace tr2) False

      _ -> property True
 where
  rules' env t = rules env t ++ rulesStructural env t

--------------------------------------------------------------------
-- Stuff to help debug rewrite rules in GHCi
--------------------------------------------------------------------

{-
freshTrace :: Expr -> IO ()
freshTrace e = print status >> printTrace' tr
  where
    (status, tr) = dfs 99 rulesPOPL e

freshTraces :: Expr -> IO ()
freshTraces e = case normalFormsFuelTrace' 99 rulesFRESH e of
  Left tr -> print NoFuel >> printTrace' tr
  Right x0 -> print NormalForm >> mapM_ (\tr -> printTrace' tr >> putStrLn "--------") x0

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

-------------------------------------------------------------------------------------
-- examples
-------------------------------------------------------------------------------------

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
  iDEFs ["a", "$r1"]
    (
      (INT 5 :=:
        (iVAR "a" :=:
          (
            (iVAR "$r1" :>:
              ( (iLam "x" ((IsINT :@: (iVar "x")) :>: iVAR "x")) :@: iVar "$r1" )
            )
          )
        )
      )
    )

e1''' =
  iDEFs ["$r1", "x"]
    (
      (INT 5 :=: ((iVAR "$r1" :=: iVAR "x") :>: ((IsINT :@: iVar "x") :>: iVAR "x")))
      :>:
      (iVAR "$r1" :>: INT 5)
    )

e1_4 =
  iDEFs ["$r1", "x"]
    ((iVAR "x" :=: INT 5) :>: (((iVAR "$r1" :=: INT 5) :>: INT 5) :>: INT 5))


e1' =
  iDEFs ["x", "y"]
    (
      (INT 5 :=: (iVAR "x" :=: iVAR "y") )
      :>:
      iVAR "x"
    )

e1'' =
  iDEFs ["x", "y"]
    (
      INT 5 :=: ((iVAR "x" :=: iVAR "y") :>: iVAR "x")
    )
-}
