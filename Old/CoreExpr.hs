{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module CoreExpr(
  Ident(..),
  Expr(..),
  pattern DefInX,
  toParseExpr,
  flattenSeqs,
  freeVars,
  ) where
import Data.Data(Data)
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Uniplate.Data
import Data.List

import qualified ParseExpr as P
import ParseExpr(Ident(..))

-- After desugaring
data Expr
  = Var Ident                      -- x
  | Int Integer                      -- i
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Lambda Ident Expr              -- x => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  -- after extrusion
  | DefIn [Ident] Expr             -- def{x,...}in e
  -- These could be desugared
  | Call Expr Expr                   -- e1(e2)
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  deriving (Eq, Ord, Show, Data)

pattern DefInX :: [Ident] -> Expr -> Expr
pattern DefInX d e <- (getDefIn -> (d, e))
  where DefInX [] e = e
        DefInX d e = DefIn d e

getDefIn :: Expr -> ([Ident], Expr)
getDefIn (DefIn d e) = (d, e)
getDefIn e = ([], e)

instance Pretty Expr where
  pPrintPrec l p e = pPrintPrec l p . toParseExpr $ e

toParseExpr :: Expr -> P.Expr
toParseExpr (Var x) = P.Var x
toParseExpr (Int x) = P.Int x
toParseExpr (Unify e1 e2) = P.Unify (toParseExpr e1) (toParseExpr e2)
toParseExpr (Apply e1 e2) = P.Apply (toParseExpr e1) (toParseExpr e2)
toParseExpr (Array es) = P.Array (map toParseExpr es)
toParseExpr (Lambda x e) = P.Lambda (P.Var x) (toParseExpr e)
toParseExpr (Alt e1 e2) = P.Alt (toParseExpr e1) (toParseExpr e2)
toParseExpr (If e1 e2 e3) = P.If (toParseExpr e1) (toParseExpr e2) (toParseExpr e3)
toParseExpr (For e1 e2) = P.For (toParseExpr e1) (toParseExpr e2)
toParseExpr (DefIn is e) = P.DefIn is (toParseExpr e)
toParseExpr (Call e1 e2) = P.Call (toParseExpr e1) (toParseExpr e2)
toParseExpr (Seq es) = P.Seq (map toParseExpr es)

-- Flatten all sequences and drop simple expressions that can have no effect.
flattenSeqs :: Expr -> Expr
flattenSeqs = transform flatten
  where
    flatten (Seq es) = sSeq $ dropVar $ concatMap getSeq es
    flatten e = e
    getSeq (Seq es) = es
    getSeq e = [e]
    dropVar es =
      reverse $ case reverse es of
                [] -> []
                e:es -> e: filter eff es
    eff Var{} = False
    eff Int{} = False
    eff _ = True
    sSeq [] = error "sSeq []"
    sSeq [e] = e
    sSeq es = Seq es

freeVars :: Expr -> [Ident]
freeVars (Var x) = [x]
freeVars (Int _) = []
freeVars (Unify e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Apply e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Array es) = foldl' union [] $ map freeVars es
freeVars (Lambda x e) = freeVars e \\ [x]
freeVars (Alt e1 e2) = freeVars e1 `union` freeVars e2
freeVars (If (DefInX xs e1) e2 e3) = ((freeVars e1 `union` freeVars e2) \\ xs) `union` freeVars e3
freeVars (For (DefInX xs e1) e2) = (freeVars e1 `union` freeVars e2) \\ xs
freeVars (DefIn xs e) = freeVars e \\ xs
freeVars (Call e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Seq es) = foldl' union [] $ map freeVars es
freeVars e = error $ "freeVars: " ++ show e
