{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
module AST(
  AST(..),
  parseTreeToAST
  ) where
import Data.Data (Data)
import Data.Generics.Uniplate.Data
import Data.Maybe
import GHC.Stack
import ParseVerse
import Text.PrettyPrint.HughesPJClass(Pretty(..), text, nest, sep, parens)

data AST
  = AIdent String
  | APath String
  | ANum String
  | AChar Char
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

pattern APostfix :: String -> AST -> AST
pattern APostfix op a1 = AOp "postfix" [AString op, a1]

pattern SNameT :: String -> ParseTree -> ParseTree
pattern SNameT s x <- SName s (SSeq [SName _ x])

aBlock :: [AST] -> AST
aBlock as = AOp "block" as

unList :: HasCallStack => AST -> [AST]
unList (AOp "list" as) = as
unList x = error $ "unList: " ++ show x

instance Pretty AST where
  pPrintPrec _ _ (AIdent s) = text s
  pPrintPrec l _ (APath s) = text "P" <> pPrintPrec l 0 s
  pPrintPrec _ _ (ANum s) = text s
  pPrintPrec _ _ (AChar c) = text $ show c
  pPrintPrec _ _ (AString s) = text $ show s
--  pPrintPrec l p (AOp "list" [a]) = pPrintPrec l p a
  pPrintPrec l _ (AOp s as) = parens $ sep $ [text s] ++ map (nest 2 . pPrintPrec l 0) as

err :: HasCallStack => String -> ParseTree -> a
err s x = error $ "unexpeced: " ++ s ++ "\n" ++ show x

parseTreeToAST :: ParseTree -> AST
parseTreeToAST (SName "File" (SSeq [_, SUnit, SUnit, SUnit, x, _, _])) = toExpr $ dropUnit $ dropSpace x
parseTreeToAST (SName "File" (SSeq [_, SUnit, SUnit, SUnit, x, _])) = toExpr $ dropUnit $ dropSpace x
--parseTreeToAST (SName "File" x) = toExpr $ dropUnit $ dropSpace x
parseTreeToAST x = err "parseTreeToAST" x

toExpr :: HasCallStack => ParseTree -> AST
toExpr (SName "List" (SSeq [SOpt Nothing])) = AList []
toExpr (SName "List" (SSeq [SOpt (Just (SSeq [cs, SMany scs, _]))])) = AList $ map toExpr (cs : map f scs)
  where f (SSeq [_, x]) = x
        f x = err "toList-1" x
toExpr (SName "Commas" (SSeq [e, SMany []])) = toExpr e
toExpr (SName "Commas" (SSeq [e, SMany ses])) = ACommas $ map toExpr (e : map f ses)
  where f (SSeq [SChar ',', x]) = x
        f x = err "toCommas-1" x
toExpr (SName "Expr" (SAlt 0 (SSeq [t, SMany ts, _]))) = foldl f (toExpr t) ts
  where f r (SSeq [SChar '@', x]) = AOp "post@" [r, toExpr x]
        f _ x = err "toExpr-1" x
toExpr (SName "Expr" (SAlt 1 (SSeq [SChar '@', c, e]))) = AOp "pre@" [toExpr c, toExpr e]
toExpr (SName "Fun" (SSeq [t, SMany ts, _])) = foldl f (toExpr t) ts
  where f r (SAlt 0 (SSeq [kw, _, x])) = AOp (flattenParseTree kw) [r, toAlt2 toExpr toDefs x]
        f r (SAlt 1 (SSeq [kw, x])) = AOp (flattenParseTree kw) [r, toAlt2 toExpr toExpr x]
        f _ x = err "toExpr-Def" x
toExpr (SName "Def" (SAlt 0 (SSeq [o, SMany xs, _]))) = foldl f (toDef1 o) xs
  where
    f r (SAlt 0 (SSeq [x])) =
      case toExpr x of
        AOp "In" (AOp "V:" [t, s] : as) | [] <- as -> c
                                        | [AString op, e] <- as -> AOp op [c, e]
          where c = AOp "V:" [r, t, s]
        e -> AOp "def-in" [r, e]
    f r (SAlt 1 (SSeq [SStr ":=", x])) = AOp ":=" [r, toAlt2 toExpr toExpr x]
    f r (SAlt 2 (SSeq [SStr "where", _, x])) = AOp "where" [r, toAlt2 toExpr toDefs x]
    f r (SAlt 3 (SSeq [SStr "is", _, x])) = AOp "is" [r, toAlt2 toExpr toExpr x]
    f _ x = err "toExpr-Def" x
    toDef1 (SAlt 0 e) = toExpr e
    toDef1 (SAlt 1 (SSeq [iv, SAlt 0 (SSeq [op, d])])) = toInVar [AString $ 'V':flattenParseTree op, toAlt2 toExpr toExpr d] iv
    toDef1 (SAlt 1 (SSeq [iv, SAlt 1 (SSeq [])]))  = toInVar [] iv
    toDef1 x = err "toDef1" x
    toInVar as (SAlt 0 (SName "In" x)) = AOp "In" (toIn x : as)
    toInVar as (SAlt 1 (SName "Var" (SSeq [op, _, x]))) = AOp (flattenParseTree op) (toExpr x : as)  -- Tim's version
    toInVar as (SAlt 1 (SName "Var" (SSeq [op, _, ospec, x]))) = AOp (flattenParseTree op) (toOptSpecs ospec : toExpr x : as)  -- ShipVerse
    toInVar _ x = err "toInVar" x
    toIn (SSeq [op, SAlt 0 (SName "In" x)]) = AOp ('V':flattenParseTree op) [toIn x]
    toIn (SSeq [op, SAlt 1 (SSeq [x, os])]) = AOp ('V':flattenParseTree op) [toExpr x, toOptSpecs os]
    toIn x = err "toIn" x
toExpr (SName "Def" (SAlt 1 (SSeq [op, x]))) = APrefix (flattenParseTree op) (toExpr x)
toExpr (SName "Def" (SAlt 2 (SSeq [t, SOpt m, _]))) = AOp (flattenParseTree t) (maybe [] f m)
  where f (SAlt 0 b) = [toExpr b]
        f (SAlt 1 d) = [toExpr d]
        f x = err "toExpr" x
toExpr (SName "Or" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "And" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "Not" (SAlt 0 c)) = toExpr c
toExpr (SName "Not" (SAlt 1 (SSeq [SSeq [SStr "not", _], x]))) = APrefix "not" (toExpr x)
toExpr (SName "Eq" (SSeq [e, xs])) = leftAssoc e xs
toExpr (SName "NotEq" (SSeq [e, xs])) = leftAssoc e xs
toExpr (SName "Less" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "Greater" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "Choose" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "To" (SSeq [e, xs])) = rightAssoc e xs
toExpr (SName "Add" (SSeq [e, xs])) = leftAssoc e xs
toExpr (SName "Mul" (SSeq [e, xs])) = leftAssoc e xs
toExpr (SName "Prefix" (SAlt 0 x)) = toExpr x
toExpr (SName "Prefix" (SAlt 1 (SSeq [op, x]))) =
  let a = case x of
            SAlt 0 b -> AOp "brace" [toExpr b]
            SAlt 1 p -> toExpr p
            _ -> error "toExpr-Prefix" x
  in  case op of
        SAlt 2 (SSeq [SChar '[', i, SChar ']']) -> AOp "pre[]" [toExpr i, a]
        _ -> APrefix (flattenParseTree op) a
toExpr (SName call (SSeq [ax, SMany xs])) | call == "Call" || call == "CallL" = foldl f (toExpr ax) xs
  where
    f r (SName "Postfix" px) =
      case px of
        SAlt 0 x -> toInvoke r x
        SAlt 1 (SSeq [SAlt 0 x]) -> AOp "call" [r, toExpr x]
        SAlt 1 (SSeq [SAlt 1 x]) -> AOp "specs" [r, toSpecs x]
        SAlt 2 (SSeq [SAlt 0 _, _, x]) -> AOp "at" [r, to x]
        SAlt 2 (SSeq [SAlt 1 _, _, x]) -> AOp "of" [r, to x]
        SAlt 3 (SSeq [sx]) ->
          case sx of
            SAlt 0 (SChar '^') -> APostfix "^" r
            SAlt 1 (SChar '?') -> APostfix "?" r
            SAlt 2 (SSeq [SChar '[', x, SChar ']']) -> AInfix "post[]" r (toExpr x)
            _ -> err "Call-1" sx
        SAlt 4 (SSeq [SChar '.', x]) -> AInfix "." r (toExpr x)
        _ -> err "Call-2" px
    f _ x = err "Call-3" x
    to (SAlt 0 x) = toExpr x
    to (SAlt 1 x) = toExpr x
    to x = err "Call-to" x
toExpr (SName "Base" (SAlt 0 (SSeq [_, x, _]))) = toExpr x
toExpr (SName "Base" (SAlt 1 (SNameT "Num" x))) = ANum $ flattenParseTree x
toExpr (SName "Base" (SAlt 2 (SNameT "Char" x))) = AChar $ toChar x
toExpr (SName "Base" (SAlt 3 (SNameT "Path" x))) = APath $ flattenParseTree x
toExpr (SName "Base" (SAlt 4 (SNameT "String" x))) = toString x
toExpr (SName "Base" (SAlt 5 x)) = toMarkup x
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
toExpr (SName "Block" (SAlt 1 (SSeq [_, x]))) = aBlock [toExpr x]
toExpr (SName "Block" (SAlt 2 (SSeq [SChar ':', _, x, _]))) = aBlock $ unList $ toExpr x
toExpr (SName "BlockMT" (SAlt 0 x)) = toExpr x
toExpr (SName "BlockMT" (SAlt 1 (SSeq [_, x]))) = aBlock [toExpr x]
toExpr (SName "BlockMT" (SAlt 2 (SSeq [SChar ':', mx])))
  | SOpt (Just (SSeq [_, x, _])) <- mx = aBlock $ unList $ toExpr x
  | SOpt Nothing                 <- mx = AList []
toExpr (SName "Brace" (SSeq [SChar '{', x, SChar '}'])) = aBlock $ unList $ toExpr x
toExpr (SName "BraceInd" (SAlt 0 x)) = toExpr x
toExpr (SName "BraceInd" (SAlt 1 (SSeq [_,x,_]))) = aBlock $ unList $ toExpr x
toExpr (SName "KeyBlock" x) = toExpr x

toExpr x = error $ "toExpr: unimplemented\n" ++ show x

leftAssoc :: HasCallStack => ParseTree -> ParseTree -> AST
leftAssoc e (SMany xs) = foldl f (toExpr e) xs
  where f r (SSeq [op, x]) = AInfix (flattenParseTree op) r (toExpr x)
        f _ x = err "leftAssoc-1" x
leftAssoc _ x = err "leftAssoc-2" x

rightAssoc :: HasCallStack => ParseTree -> ParseTree -> AST
rightAssoc e (SOpt Nothing) = toExpr e
rightAssoc e (SOpt (Just (SSeq [op, f]))) = AInfix (flattenParseTree op) (toExpr e) (toExpr f)
rightAssoc e x = err "rightAssoc" (SSeq [e, x])

toAlt2 :: HasCallStack => (ParseTree -> AST) -> (ParseTree -> AST) -> ParseTree -> AST
toAlt2 f _ (SAlt 0 x) = f x
toAlt2 _ f (SAlt 1 x) = f x
toAlt2 _ _ x = error "toAlt2" x

toDefs :: HasCallStack => ParseTree -> AST
toDefs (SName "Defs" (SSeq [d, SMany ds])) = AOp "defs" $ map toExpr $ d : map f ds
  where f (SSeq [SChar ',', x]) = x
        f x = err "toDefs-1" x
toDefs x = err "toDefs-2" x

toOptSpecs :: ParseTree -> AST
toOptSpecs (SOpt Nothing) = aNil
toOptSpecs (SOpt (Just x)) = toSpecs x
toOptSpecs x = err "toOptSpecs" x

toSpecs :: ParseTree -> AST
toSpecs (SName "Specs" (SSeq [SOpt ow, SChar '<', xx, SChar '>', ss])) =
  let e = case ow of Nothing -> toExpr xx; Just _ -> AOp "with" [toExpr xx]
  in  case ss of
        SAlt 0 x -> AOp "spec" [e, toSpecs x]
        SAlt 1 _ -> AOp "spec" [e, aNil]
        _ -> err "toSpecs-1" ss
toSpecs x = err "toSpecs" x

toInvoke :: AST -> ParseTree -> AST
toInvoke r (SName "Invoke" (SSeq [xos1, xx, xu])) =
  let os1 = toOptSpecs xos1
      mu  = case xu of SAlt 0 u -> toUntil u; SAlt 1 _ -> aNil; _ -> err "postfix" xu
      toDo (SName "Do" (SSeq [SStr "do", _, SAlt 0 b])) = toExpr b
      toDo (SName "Do" (SSeq [SStr "do", _, SAlt 1 b])) = toExpr b
      toDo xxx = err "toDo" xxx
      toUntil (SName "Until" (SAlt 0 (SSeq [SStr "until", _, SAlt 0 b]))) = AOp "until" [toExpr b]
      toUntil (SName "Until" (SAlt 0 (SSeq [SStr "until", _, SAlt 1 b]))) = AOp "until" [toExpr b]
      toUntil (SName "Until" (SAlt 1 (SSeq [SStr "catch", _, x]))) = toInvoke (AIdent "catch") x
      toUntil x = err "toUntil" x
  in  case xx of
        SAlt 0 (SSeq [p, os2, SAlt 0 b]) -> AOp "invoke1" [r, os1, toExpr p, toOptSpecs os2, toExpr b, mu]
        SAlt 0 (SSeq [p, os2, SAlt 1 d]) -> AOp "invoke1" [r, os1, toExpr p, toOptSpecs os2, toDo   d, mu]
        SAlt 1 (SSeq [b, SOpt Nothing])  -> AOp "invoke2" [r, os1, toExpr b, mu]
        SAlt 1 (SSeq [b, SOpt (Just (SSeq [os2, d]))]) -> AOp "invoke3" [r, os1, toExpr b, toOptSpecs os2, toDo d]
        _ -> err "Invoke" xx            
toInvoke _ x = err "toInvoke" x

toString :: ParseTree -> AST
toString (SSeq [SChar '"', SMany xs, SChar '"']) = AOp "string" $ merge' $ merge [] $ map f xs
  where f (SAlt 0 (SName "Interp" (SSeq [SChar '{', x, SChar '}']))) = toExpr x
        f (SAlt 1 x) = AString [toCharEsc x]
        f (SAlt 2 (SSeq [x])) = AString $ flattenParseTree x
        f x = err "toString" x
        merge [] [] = []
        merge r [] = [AString $ concat $ reverse r]
        merge r (AString s : as) = merge (s : r) as
        merge r (a : as) = [AString $ concat $ reverse r, a] ++ merge [] as
        merge' [] = [AString ""]
        merge' ss = ss
toString x = err "toString" x

toChar :: ParseTree -> Char
toChar (SAlt 0 (SName "CharLit" (SAlt 0 (SSeq [SChar '\'', x, SChar '\''])))) = c where [c] = flattenParseTree x
toChar (SAlt 0 (SName "CharLit" (SAlt 1 (SSeq [SChar '\'', x, SChar '\''])))) = toCharEsc x
toChar (SAlt 1 (SName "Char8"  (SSeq (SStr "0o" : xs)))) = toEnum $ read $ "0x" ++ flattenParseTree (SSeq xs)
toChar (SAlt 2 (SName "Char32" (SSeq (SStr "0u" : xs)))) = toEnum $ read $ "0x" ++ flattenParseTree (SSeq xs)
toChar x = err "toChar" x

toCharEsc :: ParseTree -> Char
toCharEsc (SName "CharEsc" (SSeq [SChar '\\', x])) = fromMaybe c $ lookup c escs
  where [c] = flattenParseTree x
        escs = [ ('r', '\r'), ('n', '\n'), ('t', '\t')]
toCharEsc x = err "toCharEsc" x

toMarkup :: ParseTree -> AST
toMarkup (SName "Markup" (SSeq [SName "MarkupT" ax])) =
  case ax of
    SAlt 0 (SSeq [SChar '<', tags, SStr ":>", _, cont, _]) -> AOp "markup1" [toTags tags, toContents cont]
    SAlt 1 (SSeq [SChar '<', tags, SChar ';',    cont, SChar '>']) -> AOp "markup2" [toTags tags, toContents cont]
    SAlt 2 (SSeq [SChar '<', tags, SChar '>',    cont, SStr "</", i, SMany is, SChar '>']) ->
      AOp "markup3" $ toTags tags : toContents cont : toExpr i : map f is
      where f (SSeq [SChar '/', x]) = toExpr x
            f x = err "toMarkup-1" x
    _ -> err "toMarkup-2" ax
toMarkup x = err "toMarkup" x

toTags :: ParseTree -> AST
toTags (SName "Tags" (SSeq [mc, qi, SMany invs, otags])) = AOp "tags" [amc, foldl toInvoke (toExpr qi) invs, aotags]
  where amc =
          case mc of
            SAlt 0 (SSeq [x, SChar '.']) -> toExpr x
            SAlt 1 _ -> aNil
            _ -> err "toTags-1" mc
        aotags =
          case otags of
            SOpt Nothing -> aNil
            SOpt (Just (SSeq [SChar ',', x])) -> toTags x
            _ -> err "toTags-2" otags
toTags x = err "toTags" x

toContents :: ParseTree -> AST
toContents (SName "Contents" (SAlt 0 x)) = AOp "contents1" [toContent x]
toContents (SName "Contents" (SAlt 1 (SSeq [SChar '~', c, SMany cs]))) = AOp "contents2" (toContent c : map f cs)
  where f (SSeq [SChar '~', x]) = toContent x
        f x = err "toContents-1" x
toContents x = err "toContents" x

toContent :: ParseTree -> AST
toContent (SName "Content" (SMany axs)) = AOp "content" $ cont [] axs
  where
    cont ss (SAlt 6 (SSeq [x]) : xs) = cont (flattenParseTree x : ss) xs
    cont ss (SAlt 1 x          : xs) = cont ([toCharEsc x] : ss)      xs
    cont ss (SAlt 4 (SName "Comment" _) : xs) = cont ss xs
    cont ss (SAlt 5 (SName "Line" x) : xs) = cont (flattenParseTree x : ss) xs  -- ??? emit \n
    cont ss@(_:_) xs = AString (concat $ reverse ss) : cont [] xs

    cont [] (SAlt 0 (SName "Interp" (SSeq [SChar '{', x, SChar '}'])) : xs) = AOp "interp" [toExpr x] : cont [] xs
    cont [] (SAlt 2 x : xs) = toMarkup x : cont [] xs
    cont [] (SAlt 3 (SName "Ampersand" (SSeq [_, x, _])) : xs) = AOp "ampersand" [toExpr x] : cont [] xs
    cont _  (x : _) = err "toContent-1" x
    cont [] [] = []
toContent x = err "toContent" x

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
        spaces = ["Space", "Ending", "Scan", "ScanNS", "ScanKey", "ScanKeyNS"]
