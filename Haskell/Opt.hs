{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Opt(optimize) where
import Data.Generics.Uniplate.Data

import CoreExpr

pattern VType :: Expr -> Expr
pattern VType t <- Apply t@(isType -> True) (DefIn [Ident "_"] (Var (Ident "_")))

isType :: Expr -> Bool
isType (Var (Ident s)) = s `elem` ["false", "any", "int"]
isType (Apply (Var (Ident "tuple")) (Array es)) = all isType es
isType (Apply (Var (Ident "array")) e) = isType e
isType (Apply (Var (Ident "arrow")) (Array [t, u])) = isType t && isType u
isType _ = False

optimize :: Expr -> Expr
optimize = transform opt2 . transform opt1
  where
    opt1 (Unify e (VType t)) = Apply t e
    opt1 e = e
    opt2 (Apply (Var (Ident "any")) e) = e
    opt2 e = e