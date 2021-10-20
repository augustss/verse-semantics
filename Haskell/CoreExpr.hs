{-# LANGUAGE DeriveDataTypeable #-}
module CoreExpr(
  P.Ident(..),
  Expr(..),
  toParseExpr,
  flattenSeqs,
  ) where
import Data.Data(Data)
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Uniplate.Data

import qualified ParseExpr as P

-- After desugaring
data Expr
  = Var P.Ident                      -- x
  | Int Integer                      -- i
  | Def P.Ident                      -- def{x}
  | DefIn [P.Ident] Expr             -- def{x,...}in e
  | Unify Expr Expr                  -- e1 = e2
  | Apply Expr Expr                  -- e1[e2]
  | Call Expr Expr                   -- e1(e2)
  | Lambda P.Ident Expr              -- x => e
  | Alt Expr Expr                    -- e1 | e2
  | Array [Expr]                     -- e1, ..., en
  | If Expr Expr Expr                -- if(e1) then e2 else e3
  | For Expr Expr                    -- for(e1) e2
  | Let Expr Expr                    -- let (e1) in e2
  | Do Expr                          -- do e
  | Seq [Expr]  -- non-empty list    -- { e1; ...; en }
  deriving (Eq, Ord, Show, Data)

instance Pretty Expr where
  pPrintPrec l p e = pPrintPrec l p . toParseExpr $ e

toParseExpr :: Expr -> P.Expr
toParseExpr (Def x) = P.Def x
toParseExpr (DefIn is e) = P.DefIn is (toParseExpr e)
toParseExpr (Var x) = P.Var x
toParseExpr (Int x) = P.Int x
toParseExpr (Unify e1 e2) = P.Unify (toParseExpr e1) (toParseExpr e2)
toParseExpr (Apply e1 e2) = P.Apply (toParseExpr e1) (toParseExpr e2)
toParseExpr (Call e1 e2) = P.Call (toParseExpr e1) (toParseExpr e2)
toParseExpr (Array es) = P.Array (map toParseExpr es)
toParseExpr (Lambda x e) = P.Lambda (P.Var x) (toParseExpr e)
toParseExpr (Alt e1 e2) = P.Alt (toParseExpr e1) (toParseExpr e2)
toParseExpr (If e1 e2 e3) = P.If (toParseExpr e1) (toParseExpr e2) (toParseExpr e3)
toParseExpr (For e1 e2) = P.For (toParseExpr e1) (toParseExpr e2)
toParseExpr (Let e1 e2) = P.Let (toParseExpr e1) (toParseExpr e2)
toParseExpr (Do e) = P.Do (toParseExpr e)
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
