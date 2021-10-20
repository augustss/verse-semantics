module Scope(extrude, scopeCheck) where

import CoreExpr

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame (x:xs) = elem x xs || anySame xs

-----

extrude :: Expr -> Expr
extrude = extDef

-- Extract defs
ext :: Expr -> (Expr, [Ident])
ext e@Var{} = (e, [])
ext e@Int{} = (e, [])
ext (Def i) = (Var i, [i])
ext (DefIn _ _) = error "ext: DefIn"
ext (Unify e1 e2) = (Unify e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Apply e1 e2) = (Apply e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Call e1 e2) = (Call e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Lambda i e) = (Lambda i $ extDef e, [])
ext (Alt e1 e2) = (Alt (extDef e1) (extDef e2), [])
ext (Array es) = (Array es', concat ds) where (es', ds) = unzip $ map ext es
ext (If e1 e2 e3) = (If (extDef e1) (extDef e2) (extDef e3), [])
ext (For e1 e2) = (defIn (For e1' (extDef e2), d1), []) where (e1', d1) = ext e1
ext (Let e1 e2) = (defIn (Seq [e1', e2'], d1), d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Do e) = (extDef e, [])
ext (Seq es) = (Seq es', concat ds) where (es', ds) = unzip $ map ext es


-- Extract defs and insert a DefIn
extDef :: Expr -> Expr
extDef = defIn . ext

defIn :: (Expr, [Ident]) -> Expr
defIn (e, []) = e
defIn (e, is)
        | anySame is = error $ "defIn" ++ show is
        | otherwise = DefIn is e

-----

scopeCheck :: Expr -> Expr
scopeCheck = id

