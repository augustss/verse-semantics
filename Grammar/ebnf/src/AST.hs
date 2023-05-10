{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
module AST(
  AST(..),
  parseTreeToAST
  ) where
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import GHC.Stack
import ParseVerse
import Text.PrettyPrint.HughesPJClass(Pretty(..), text, nest, sep, parens)

data AST
  = AIdent String
  | APath String
  | ANum String
  | AChar String
  | AString String
  | AOp String [AST]
  deriving (Show, Eq, Data)

pattern AList :: [AST] -> AST
pattern AList as = AOp "list" as

pattern ACommas :: [AST] -> AST
pattern ACommas as = AOp "commas" as

--pattern AOp1 :: String -> AST -> AST
--pattern AOp1 op a1 = AOp op [a1]

--pattern AOp2 :: String -> AST -> AST -> AST
--pattern AOp2 op a1 a2 = AOp op [a1, a2]

pattern AInfix :: String -> AST -> AST -> AST
pattern AInfix op a1 a2 = AOp "infix" [AString op, a1, a2]

pattern APrefix :: String -> AST -> AST
pattern APrefix op a1 = AOp "prefix" [AString op, a1]

--pattern APostfix :: String -> AST -> AST
--pattern APostfix op a1 = AOp "postfix" [AString op, a1]

pattern SNameT :: String -> ParseTree -> ParseTree
pattern SNameT s x <- SName s (SSeq [SName _ x])

instance Pretty AST where
  pPrintPrec _ _ (AIdent s) = text s
  pPrintPrec l _ (APath s) = text "P" <> pPrintPrec l 0 s
  pPrintPrec _ _ (ANum s) = text s
  pPrintPrec _ _ (AChar s) = text s
  pPrintPrec _ _ (AString s) = text s
--  pPrintPrec l p (AOp "list" [a]) = pPrintPrec l p a
  pPrintPrec l _ (AOp s as) = parens $ sep $ [text s] ++ map (nest 2 . pPrintPrec l 0) as

err :: HasCallStack => String -> ParseTree -> a
err s x = error $ "unexpeced: " ++ s ++ "\n" ++ show x

parseTreeToAST :: ParseTree -> AST
parseTreeToAST (SName "File" (SSeq [_, SUnit, SUnit, SUnit, x, _, _])) = toExpr $ dropUnit $ dropSpace x
--parseTreeToAST (SName "File" x) = toExpr $ dropUnit $ dropSpace x
parseTreeToAST x = err "parseTreeToAST" x

toExpr :: HasCallStack => ParseTree -> AST
toExpr (SName "List" (SSeq [SOpt Nothing, _])) = AList []
toExpr (SName "List" (SSeq [SOpt (Just (SSeq [cs, SMany scs, _]))])) = AList $ map toExpr (cs : map f scs)
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
toExpr (SName "Or" (SSeq [e, SMany xs])) = foldr1 (AInfix "or") (toExpr e : map f xs)
  where f (SSeq [_, SSeq [SStr "or", _], _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "And" (SSeq [e, SMany xs])) = foldr1 (AInfix "and") (toExpr e : map f xs)
  where f (SSeq [_, SSeq [SStr "and", _], _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "Not" (SAlt 0 c)) = toExpr c
toExpr (SName "Not" (SAlt 1 (SSeq [SSeq [SStr "not", _], _, x]))) = APrefix "not" (toExpr x)
toExpr (SName "Eq" (SSeq [e, SMany xs])) = foldr1 (AInfix "=") (toExpr e : map f xs)
  where f (SSeq [_, SStr "=", _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "NotEq" (SSeq [e, SMany xs])) = foldr1 (AInfix "<>") (toExpr e : map f xs)
  where f (SSeq [_, SStr "<>", _, x]) = toExpr x
        f x = err "toExpr" x
toExpr (SName "Less" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Less" (SSeq [e, SOpt (Just (SSeq [_, op, _, _, f]))])) = AInfix (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Greater" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Greater" (SSeq [e, SOpt (Just (SSeq [_, op, _, f]))])) = AInfix (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Choose" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "Choose" (SSeq [e, SOpt (Just (SSeq [_, SStr "|", _, f]))])) = AInfix "|" (toExpr e) (toExpr f)
toExpr (SName "To" (SSeq [e, SOpt Nothing])) = toExpr e
toExpr (SName "To" (SSeq [e, SOpt (Just (SSeq [_, op, _, f]))])) = AInfix (flattenParseTree op) (toExpr e) (toExpr f)
toExpr (SName "Add" (SSeq [e, SMany xs])) = foldl f (toExpr e) xs
  where f r (SSeq [_, op, _, x]) = AInfix (flattenParseTree op) r (toExpr x)
        f _ x = err "toExpr" x
toExpr (SName "Mul" (SSeq [e, SMany xs])) = foldl f (toExpr e) xs
  where f r (SSeq [_, op, _, x]) = AInfix (flattenParseTree op) r (toExpr x)
        f _ x = err "toExpr" x
toExpr (SName "Prefix" (SAlt 0 x)) = toExpr x
toExpr (SName "Prefix" (SAlt 1 (SSeq [op, _, (SAlt _ x)]))) = APrefix (flattenParseTree op) (toExpr x)
toExpr (SName "Call" (SSeq [x, SMany xs])) = foldl f (toExpr x) xs
  where f _r _x = error "unimplemented"
toExpr (SName "Base" (SAlt 0 (SSeq [_, x, _]))) = toExpr x
toExpr (SName "Base" (SAlt 1 (SNameT "Num" x))) = ANum $ flattenParseTree x
toExpr (SName "Base" (SAlt 2 (SNameT "Char" x))) = AChar $ flattenParseTree x
toExpr (SName "Base" (SAlt 3 (SNameT "Path" x))) = APath $ flattenParseTree x
toExpr (SName "Base" (SAlt 4 (SNameT "String" x))) = AString $ flattenParseTree x
toExpr (SName "Base" (SAlt 5 _)) = error "unimplemented"
toExpr (SName "Base" (SAlt 6 x)) = toExpr x
toExpr (SName "Base" (SAlt 7 (SSeq [x]))) = toExpr x
toExpr (SName "QualIdent" (SSeq [SOpt Nothing, i])) = toExpr i
toExpr (SName "QualIdent" (SSeq [SOpt (Just (SSeq [SChar '(', x, SStr ":)"])), i])) = AOp "qual" [toExpr x, toExpr i]
toExpr (SNameT "Ident" x) = AIdent $ flattenParseTree x
toExpr (SName "If" (SSeq [SStr "if", _, sp, ct, el])) = AOp "if" [toOptSpecs sp, cnd, thn, els]
  where
    els =
      case el of
        SAlt 0 (SName "Else" (SSeq [SStr "else", _, xe])) ->
          case xe of
            SAlt 0 (SSeq [x]) -> toExpr x
            SAlt 1 (SSeq [SAlt 0 x]) -> toExpr x
            SAlt 1 (SSeq [SAlt 1 x]) -> toExpr x
            _ -> err "toExpr" xe
        SAlt 1 _ -> aNil
        _ -> err "toExpr" el
    (cnd, thn) =
      case ct of
        SAlt 0 (SSeq [c, SAlt 0 t]) -> (toExpr c, toExpr t)
        SAlt 0 (SSeq [c, SAlt 1 t]) -> (toExpr c, toThen t)
        SAlt 1 (SSeq [c, SOpt Nothing]) -> (toExpr c, aNil)
        SAlt 1 (SSeq [c, SOpt (Just t)]) -> (toExpr c, toThen t)
        _ -> err "toExpr" ct
    toThen (SName "Then" (SSeq [SStr "then", _, SAlt 0 b])) = toExpr b
    toThen (SName "Then" (SSeq [SStr "then", _, SAlt 1 b])) = toExpr b
    toThen x = err "toThen" x
toExpr (SName "Paren" (SSeq [SChar '(', x, SChar ')'])) = toExpr x
toExpr (SName "Block" (SAlt 0 x)) = toExpr x
toExpr (SName "Block" (SAlt 1 (SSeq [_, x]))) = toExpr x
toExpr (SName "Block" (SAlt 2 (SSeq [SChar ':', _, x, _]))) = toExpr x
toExpr (SName "Brace" (SSeq [SChar '{', x, SChar '}'])) = toExpr x
toExpr (SName "KeyBlock" x) = toExpr x
toExpr (SName "SDef" (SSeq [_, x])) = toExpr x
toExpr x = error $ "toExpr: unimplemented\n" ++ show x

toOptSpecs :: ParseTree -> AST
toOptSpecs (SOpt Nothing) = aNil
toOptSpecs (SOpt (Just x)) = toSpecs x
toOptSpecs x = err "toOptSpecs" x

toSpecs :: ParseTree -> AST
toSpecs = error "unimplemented"

aNil :: AST
aNil = AIdent "#nil"

-- Remove all SUnit in sequences.
dropUnit :: ParseTree -> ParseTree
dropUnit = transformBi f
  where f (SSeq xs) = SSeq $ filter (/= SUnit) xs
        f x = x

-- Turn all whitespace only productions into SUnit
dropSpace :: ParseTree -> ParseTree
dropSpace = transformBi f
  where f (SName s _) | s `elem` spaces = SUnit
        f x = x
        spaces = ["Space", "NewLine", "Ending", "Scan", "ScanNS", "ScanKey", "ScanKeyNS"]
