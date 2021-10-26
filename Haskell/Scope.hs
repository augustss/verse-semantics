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
ext (Define x e) = (Unify (Var x) e', x:d) where (e', d) = ext e
ext (Range e) = (Apply e' (DefIn [x] (Var x)), d) where (e', d) = ext e; x = Ident "_"
ext (Unify e1 e2) = (Unify e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Apply e1 e2) = (Apply e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Call e1 e2) = (Call e1' e2', d1 ++ d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Lambda i e) = (Lambda i $ extDef e, [])
ext (Alt e1 e2) = (Alt (extDef e1) (extDef e2), [])
ext (Array es) = (Array es', concat ds) where (es', ds) = unzip $ map ext es
ext (If e1 e2 e3) = (If (extDef e1) (extDef e2) (extDef e3), [])
ext (For e1 e2) = (For (extDef e1) (extDef e2), [])
ext (Let e1 e2) = (defIn (Seq [e1', e2'], d1), d2) where (e1', d1) = ext e1; (e2', d2) = ext e2
ext (Do e) = (extDef e, [])
ext (Seq es) = (Seq es', concat ds) where (es', ds) = unzip $ map ext es
ext (DefIn _ _) = error "ext: DefIn"


-- Extract defs and insert a DefIn
extDef :: Expr -> Expr
extDef = defIn . ext

defIn :: (Expr, [Ident]) -> Expr
defIn (e, is)
        | anySame is = error $ "defIn" ++ show is
        | otherwise = DefInX is e

-----

type IdentSet = [Ident]

primops :: IdentSet
primops = map Ident $ words $
  "any array arrow false float int tuple type void wrong " ++
  "operator'+' operator'-' operator'*' operator'/' " ++
  "operator'<' operator'<=' operator'>' operator'>=' " ++
  "print "  -- just to have a fake consumer

scopeCheck :: Expr -> Expr
scopeCheck = scope primops

scope :: IdentSet -> Expr -> Expr
scope r e@(Var x) | x `elem` r = e
                  | otherwise = error $ "scope: undefined " ++ show (x, r)
scope _ e@(Int _) = e
scope r (DefIn d e) = DefIn d (scope (d ++ r) e)
scope r (Unify e1 e2) = Unify (scope r e1) (scope r e2)
scope r (Apply e1 e2) = Apply (scope r e1) (scope r e2)
scope r (Call e1 e2) = Call (scope r e1) (scope r e2)
scope r (Lambda x e) = Lambda x (scope (x:r) e)
scope r (Alt e1 e2) = Alt (scope r e1) (scope r e2)
scope r (Array es) = Array (map (scope r) es)
scope r (If (DefInX d e1) e2 e3) = If (DefInX d $ scope r' e1) (scope r' e2) (scope r e3)
  where r' = d ++ r
scope r (For (DefInX d e1) e2) = For (DefInX d $ scope r' e1) (scope r' e2)
  where r' = d ++ r
scope r (Seq es) = Seq (map (scope r) es)
scope _ e = error $ "scope: unexpected " ++ show e
