{-# LANGUAGE PatternSynonyms #-}
module AST(
  AST(..),
  parseTreeToAST
  ) where
import GHC.Stack
import ParseVerse

data AST
  = AIdent String
  | APath String
  | ANum String
  | AChar String
  | AString String
  | AOp String [AST]
  deriving (Show, Eq)

pattern AList :: [AST] -> AST
pattern AList as = AOp "list" as

pattern ACommas :: [AST] -> AST
pattern ACommas as = AOp "commas" as

pattern AOp1 :: String -> AST -> AST
pattern AOp1 op a1 = AOp op [a1]

pattern AOp2 :: String -> AST -> AST -> AST
pattern AOp2 op a1 a2 = AOp op [a1, a2]

err :: String -> ParseTree -> a
err s x = error $ "unexpeced: " ++ s ++ "\n" ++ show x

parseTreeToAST :: ParseTree -> AST
parseTreeToAST (SName "File" (SSeq [_, SUnit, SUnit, SUnit, x, _])) = toExpr x
parseTreeToAST x = err "parseTreeToAST" x

toExpr :: HasCallStack => ParseTree -> AST
toExpr (SName "List" (SSeq [SUnit, SUnit, _, SOpt Nothing, SUnit])) = AList []
toExpr (SName "List" (SSeq [SUnit, SUnit, _, SOpt (Just (SSeq [cs, SMany scs, _])), SUnit])) = AList $ map toExpr (cs : map f scs)
  where f (SSeq [_, x]) = x
        f x = err "toList-1" x
toExpr (SName "Commas" (SSeq [e, SMany []])) = toExpr e
toExpr (SName "Commas" (SSeq [e, SMany ses])) = ACommas $ map toExpr (e : map f ses)
  where f (SSeq [SChar ',', _, x]) = x
        f x = err "toCommas-1" x
toExpr (SName "Expr" (SAlt 0 (SSeq [t, SMany ts, _]))) = foldl f (toExpr t) ts
  where f e (SSeq [_, _, _, x]) = AOp "post-@" [e, toExpr x]
        f _ x = err "toExpr-1" x
toExpr (SName "Expr" (SAlt 1 (SSeq [_, _, c, _, e]))) = AOp "pre-@" [toExpr c, toExpr e]
toExpr (SName "Fun" (SSeq [t, SMany ts, _])) = foldl f (toExpr t) ts
  where f _e _x = error "unimplemented"
toExpr (SName "Def" (SAlt 0 (SSeq [o, SMany xs, _]))) = foldl f (toDef1 o) xs
  where f _e _x = error "unimplemented"
        toDef1 (SAlt 0 e) = toExpr e
        toDef1 (SAlt 1 (SSeq [_iv, _, SAlt 0 (SSeq [_op, _, _def])])) = error "unimplemented"
        toDef1 (SAlt 1 (SSeq [_iv, _, SAlt 1 (SSeq [_, _])])) = error "unimplemented"
        toDef1 x = err "toDef1" x
toExpr (SName "Def" (SAlt 1 _)) = error "unimplemented"
toExpr (SName "Def" (SAlt 2 _)) = error "unimplemented"
toExpr (SName "Or" (SSeq [e, SMany xs])) = foldr1 (AOp2 "or") (toExpr e : map f xs)
  where f (SSeq [_, SSeq [SStr "or", _], _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "And" (SSeq [e, SMany xs])) = foldr1 (AOp2 "and") (toExpr e : map f xs)
  where f (SSeq [_, SSeq [SStr "and", _], _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "Not" (SAlt 0 c)) = toExpr c
toExpr (SName "Not" (SAlt 1 (SSeq [SSeq [SStr "not", _], _, x]))) = AOp1 "not" (toExpr x)
toExpr (SName "Eq" (SSeq [e, SMany xs])) = foldr1 (AOp2 "=") (toExpr e : map f xs)
  where f (SSeq [_, SStr "=", _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "NotEq" (SSeq [e, SMany xs])) = foldr1 (AOp2 "<>") (toExpr e : map f xs)
  where f (SSeq [_, SStr "<>", _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "Less" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Less" (SSeq [e, SOpt (Just (SSeq [_, op, _, _, f]))])) = AOp2 (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Greater" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Greater" (SSeq [e, SOpt (Just (SSeq [_, op, _, f]))])) = AOp2 (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Choose" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Choose" (SSeq [e, SOpt (Just (SSeq [_, SStr "|", _, f]))])) = AOp2 "|" (toExpr e) (toExpr f)
toExpr (SName "To" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "To" (SSeq [e, SOpt (Just (SSeq [_, op, _, f]))])) = AOp2 (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Add" (SSeq [e, SMany xs])) = foldl f (toExpr e) xs
  where f r (SSeq [_, op, _, x]) = AOp2 (flattenParseTree op) r (toExpr x)
        f _ x = err "toExpr" x
toExpr (SName "Mul" (SSeq [e, SMany xs])) = foldl f (toExpr e) xs
  where f r (SSeq [_, op, _, x]) = AOp2 (flattenParseTree op) r (toExpr x)
        f _ x = err "toExpr" x
toExpr (SName "Prefix" (SAlt 0 x)) = toExpr x
toExpr (SName "Prefix" (SAlt 1 (SSeq [op, _, (SAlt _ x)]))) = AOp1 ("prefix-" ++ flattenParseTree op) (toExpr x)
toExpr (SName "Call" (SSeq [x, SMany xs])) = foldl f (toExpr x) xs
  where f _r _x = error "unimplemented"
toExpr (SName "Base" (SAlt 0 (SSeq [_, x, _]))) = toExpr x
toExpr (SName "Base" (SAlt 1 (SName "Num" x))) = ANum $ flattenParseTree x
toExpr (SName "Base" (SAlt 2 (SName "Char" x))) = AChar $ flattenParseTree x
toExpr (SName "Base" (SAlt 3 (SName "Path" x))) = APath $ flattenParseTree x
toExpr (SName "Base" (SAlt 4 (SName "String" x))) = AString $ flattenParseTree x
toExpr (SName "Base" (SAlt 5 _)) = error "unimplemented"
toExpr (SName "Base" (SAlt 6 _)) = error "unimplemented"
toExpr (SName "Base" (SAlt 7 (SSeq [_, x]))) = toExpr x
toExpr (SName "QualIdent" (SSeq [SOpt Nothing, i])) = toExpr i
toExpr (SName "QualIdent" (SSeq [SOpt (Just x), i])) = AOp2 "qual" (toExpr x) (toExpr i)
toExpr (SName "Ident" x) = AIdent $ flattenParseTree x
toExpr x = error $ "toExpr: unimplemented\n" ++ show x
