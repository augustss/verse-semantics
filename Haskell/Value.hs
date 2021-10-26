module Value where
import Text.PrettyPrint.HughesPJClass

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
valueToExpr (VWrong _) = Var $ Ident "WRONG"

toLet :: Env -> Expr -> Expr
toLet [] e = e
toLet xvs e = Let (Seq [ Define x (valueToExpr v) | (x, v) <- xvs ]) e

allValues :: [Value]
allValues = map VInt [0..63]
