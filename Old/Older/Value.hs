{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Value where
import Text.PrettyPrint.HughesPJClass
import Test.SmallCheck.Series

import CoreExpr

type Env = [(Ident, Value)]

data Value
  = VInt Integer
  | VArray [Value]
  | VLambda Env Ident Expr
  | VPrim String
  | VWrong String
  deriving (Eq, Ord, Show)

instance Pretty Value where
  pPrintPrec l p = pPrintPrec l p . valueToExpr

valueToExpr :: Value -> Expr
valueToExpr (VInt i) = Int i
valueToExpr (VArray vs) = Array $ map valueToExpr vs
valueToExpr (VLambda r x e) = toLet r $ Lambda x e
valueToExpr (VPrim s) = Var $ Ident s
valueToExpr (VWrong s) = Var $ Ident $ "WRONG{" ++ s ++ "}"

toLet :: Env -> Expr -> Expr
toLet [] e = e
toLet xvs e = --Let (Seq [ Define x (valueToExpr v) | (x, v) <- xvs ]) e
  DefIn (map fst xvs) $ Seq [ Unify (Var x) (valueToExpr v) | (x, v) <- xvs ]

allValues :: [Value]
allValues = --map VInt [0..63]
  listSeries 6

instance (Monad m) => Serial m Value where
  series = cons1 VInt \/ cons1 VArray
  
