{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module FrontEnd.DesugarNew where
import Control.Monad
import Control.Monad.State.Strict
import Epic.List
import Epic.Print
import FrontEnd.Desugar
import FrontEnd.Expr

desugarNew :: Expr -> Expr
desugarNew = eval . (addScope <=< dsTop <=< dropParens)
  where eval = flip evalState 1

dsTop :: Expr -> D Expr
dsTop e = do
  x <- newIdent (getLoc e) "i"
  Exists [x] <$> dsMatch e x

dsMatch :: Expr -> Ident -> D Expr
dsMatch e v | isLiteral e = pure $ unifyV v e
dsMatch e@Variable{} v = pure $ unifyV v e
dsMatch (Array es) v = do
  let f e | isValue e = pure (id, e)    -- Easy special case.  Not really needed
          | otherwise = do
              x <- newIdent (getLoc e) "x"
              d <- dsMatch e x
              pure (\a -> existsV [x] (Seq [d, a]), Variable x)
  (fs, xs) <- unzip <$> mapM f es
  pure $ foldr ($) (unifyV v (Array xs)) fs
dsMatch (ApplyS f a) v = apply Succeeds f a v
dsMatch (ApplyD f a) v = apply id f a v
dsMatch (PrefixOp (Op ":") e) v = dsColon e v
dsMatch (PrefixOp (Ident l op) e) v = dsMatch (call "pre" l op e) v
dsMatch (PostfixOp e (Ident l op)) v = dsMatch (call "post" l op e) v
dsMatch (InfixOp e1 (Op ":=") e2) v = dsDef e1 e2 v
dsMatch (InfixOp e1 o@(Op ":") e2) v = dsMatch (InfixOp e1 (Op ":=") (PrefixOp o e2)) v
dsMatch (InfixOp e1 (Op "where") e2) v = do
  x <- newIdent (getLoc e2) "x"
  d1 <- dsMatch e1 v
  d2 <- dsMatch e2 x
  pure $ existsV [x] $ Seq [d1, d2]
dsMatch (InfixOp e1 (Op "=") e2) v = do
  d1 <- dsMatch e1 v
  d2 <- dsMatch e2 v
  pure $ Seq [d1, d2]
dsMatch (InfixOp e1 (Op "|") e2) v = do
  d1 <- dsMatch e1 v
  d2 <- dsMatch e2 v
  pure $ Choice d1 d2
dsMatch (InfixOp e1 (Ident l op) e2) v = dsMatch (call "in" l op (Array [e1, e2])) v
dsMatch (Seq []) v = dsMatch (Array []) v
dsMatch (Seq (Snoc es r)) v = do
  xs <- mapM (\ e -> newIdent (getLoc e) "x") es
  ds <- zipWithM dsMatch es xs  
  dr <- dsMatch r v
  pure $ existsV xs $ Seq $ ds ++ [dr]
dsMatch (Block es) v = dsMatch (Seq es) v
dsMatch (Function [(e1,rs)] e2) v = do
  x <- newIdent (getLoc e1) "x"
  y <- newIdent (getLoc e1) "y"
  q <- newIdent (getLoc e1) "q"
  d1 <- dsMatch e1 x
  d2 <- dsMatch e2 q
  pure $ Lambda x rs (Define y d1) $ Seq [Define q (ApplyD (Variable v) (Variable y)), d2]
dsMatch e _ = error $ "dsMatch: " ++ prettyShow e

apply :: (Expr -> Expr) -> Expr -> Expr -> Ident -> D Expr
apply con f e v | isValue f = apply1 con f e v   -- Easy special case.  Not really needed
apply con f e v = do
  g <- newIdent (getLoc f) "g"
  h <- newIdent (getLoc f) "h"
  df <- dsMatch f g
  a <- apply1 con (Variable h) e v
  pure $ existsV [g, h] $ Seq [unifyV h df, a]

apply1 :: (Expr -> Expr) -> Expr -> Expr -> Ident -> D Expr
apply1 con h e v | isValue e = apply2 con h e v   -- Easy special case.  Not really needed
apply1 con h e v = do
  x <- newIdent (getLoc e) "x"
  y <- newIdent (getLoc e) "y"
  de <- dsMatch e x
  a <- apply2 con h (Variable y) v
  pure $ existsV [x, y] $ Seq [unifyV y de, a]

apply2 :: (Expr -> Expr) -> Expr -> Expr -> Ident -> D Expr
apply2 con h y v = pure $ unifyV v (con (ApplyD h y))

dsColon :: Expr -> Ident -> D Expr
dsColon e@(Variable _) v = pure $ ApplyD e (Variable v)   -- Easy special case.  Not really needed
dsColon e v = do
  h <- newIdent (getLoc e) "h"
  f <- newIdent (getLoc e) "f"
  de <- dsMatch e h
  pure $ existsV [h, f] $ Seq [unifyV f de, ApplyD (Variable f) (Variable v)]


dsDef :: Expr -> Expr -> Ident -> D Expr
dsDef (Variable i) e v = Define i <$> dsMatch e v
dsDef (ApplyS f a) e v = dsDef f (Function [(a,[])] e) v
dsDef (InfixOp e1 (Op ":") e2) e v = dsDef e1 (ApplyS e2 e) v
dsDef p _ _ = error $ "dsDef: " ++ prettyShow p

isValue :: Expr -> Bool
isValue Variable{} = True
isValue (Array es) = all isValue es
isValue e = isLiteral e

unifyV :: Ident -> Expr -> Expr
unifyV i e = Unify (Variable i) e

existsV :: [Ident] -> Expr -> Expr
existsV is e = seqE $ map (\ i -> Define i AnyT) is ++ [e]
--existsV = exists

{-
unifyV :: Ident -> Expr -> Expr
unifyV = Define

existsV :: [Ident] -> Expr -> Expr
existsV _is e = e
-}

-- Hackily move exists in a lambda domain to the top
existsHack :: Expr -> D Expr
existsHack = pure . f
  where f (Lambda i rs d r) = Lambda i rs (pullExists d) (f r)
        f e = composOp f e
        pullExists (Exists is e) = Exists (is ++ is') e' where (is', e') = pull e
        pullExists _ = undefined
        pull (Exists is e) = (is ++ is', e') where (is', e') = pull e
        pull (Seq []) = ([], Seq [])
        pull (Seq (e:es)) = (is ++ is', Seq (e' : es'))
          where (is, e') = pull e
                (is', Seq es') = pull (Seq es)
        pull (Unify v e) = (is, Unify v e') where (is, e') = pull e
        pull e = ([], e)
        
