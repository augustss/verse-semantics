{-# LANGUAGE PatternSynonyms #-}
module Cleanup(cleanup) where
import Data.Generics.Uniplate.Data
import CoreExpr

--import Debug.Trace
--import Text.PrettyPrint.HughesPJClass

cleanup :: Expr -> Expr
cleanup = flattenSeqs . undoCall

pattern Define :: Ident -> Expr -> Expr
pattern Define x e <- DefIn [x] (Unify _ e)
  where Define x e = DefIn [x] (Unify (Var x) e)

undoCall :: Expr -> Expr
undoCall = transform undo
  where
    undo xxx@(Seq [fd@(Unify f fe),
               ad@(Unify a ae),
               If (Define x (Apply f' a')) thn@(Var x') els@(Var (Ident "wrong"))
              ])
      | f == f', a == a', x == x' =
          let (fd', fe') | cannotFail fe = ([], fe)
                         | otherwise = ([fd], f)
              (ad', ae') | cannotFail ae = ([], ae)
                         | otherwise = ([ad], a)
          in Seq $ fd' ++ ad' ++ [If (Define x (Apply fe ae)) thn els]
    undo e = e

cannotFail :: Expr -> Bool
cannotFail Var{} = True
cannotFail Int{} = True
cannotFail (Array es) = all cannotFail es
cannotFail _ = False
