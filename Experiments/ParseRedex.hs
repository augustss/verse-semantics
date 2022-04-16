module Main where
import Prelude hiding ((<>))
import Data.Char
import System.IO
import Text.ParserCombinators.ReadP
import Text.PrettyPrint.HughesPJClass hiding (char)
import Text.Printf

readUTF8File :: FilePath -> IO String
readUTF8File fn = do
  h <- openFile fn ReadMode
  hSetEncoding h utf8
  hGetContents h

main :: IO ()
main = do
{-
  hSetEncoding stdout utf8
  file <- readUTF8File "example.rkt"
  print file
-}
  file <- readFile "example.rkt"
  let v = runReadP (pSExp <* eof) file
  print v

dropGuillemets :: String -> String
dropGuillemets = filter (`notElem` "\xab\xbb")

------------------

runReadP :: (Show a) => ReadP a -> String -> a
runReadP p s =
  case readP_to_S p s of
    [(a, "")] -> a
    x -> error $ "runReadP: " ++ show x

------------------

data SExp
  = SInt Integer
  | SStr String
  | SSym String
  | SList [SExp]
  deriving (Show)

pSExp :: ReadP SExp
pSExp = skipSpaces *> (pInt +++ pStr +++ pSym +++ pList +++ pQuote) <* skipSpaces

pInt :: ReadP SExp
pInt = SInt . read <$> ((:) <$> satisfy dig <*> munch isDigit)
  where dig c = c == '-' || isDigit c

pStr :: ReadP SExp
pStr = SStr <$> (char '"' *> munch (/= '"') <* char '"')

pSym :: ReadP SExp
pSym = SSym <$> ((:) <$> satisfy syms <*> munch sym)
  where sym c = c `notElem` ("\"'() \t\n\r" :: String)
        syms c = not (isDigit c) && sym c

pList :: ReadP SExp
pList = SList <$> (char '(' *> many pSExp <* char ')')

pQuote :: ReadP SExp
pQuote = char '\'' *> pSExp

------------------

type Sym = String

data Exp
  = EVar Sym | EInt Integer | EArray [Val] | ELam Sym Exp | ERec Sym Exp
  | EUnify Exp Exp | ESeq [Exp] | EOp Sym Exp | EAlt [Exp]
  | EIf Exp Exp Exp | EFor Exp Exp | EDef Heap Exp | EWrong
  deriving (Show)

type Val = Exp

data Heap = HVar Sym | HVal Sym Val | HMany [Heap]
  deriving (Show)

conv :: SExp -> [(String, Exp)]
conv (SList xs) = map f xs
  where f (SList [SStr s, e]) = (s, sExpToExp e)
        f x = error $ "conv elem: " ++ show x
conv s = error $ "conv: " ++ show s

ops :: [String]
ops = ["add", "sub", "mul", "gt", "apply", "int"]

sExpToExp :: SExp -> Exp
sExpToExp (SSym "wrong") = EWrong
sExpToExp (SSym s) = EVar s
sExpToExp (SInt i) = EInt i
sExpToExp (SList (SSym "arr" : es)) = EArray (map sExpToExp es)
sExpToExp (SList [SSym "=>", SSym x, e]) = ELam x $ sExpToExp e
sExpToExp (SList [SSym "rec", SSym x, e]) = ERec x $ sExpToExp e
sExpToExp (SList [SSym "=", e1, e2]) = EUnify (sExpToExp e1) (sExpToExp e2)
sExpToExp (SList (SSym "seq" : es)) = ESeq (map sExpToExp es)
sExpToExp (SList [SSym op, e]) | op `elem` ops = EOp op $ sExpToExp e
sExpToExp (SList (SSym "bar" : es)) = EAlt (map sExpToExp es)
sExpToExp (SList [SSym "if", e1, e2, e3]) = EIf (sExpToExp e1) (sExpToExp e2) (sExpToExp e3)
sExpToExp (SList [SSym "for", e1, e2]) = EFor (sExpToExp e1) (sExpToExp e2)
sExpToExp (SList [SSym "def", SSym _, h, e]) = EDef (sExpToHeap h) (sExpToExp e)
sExpToExp s = error $ "sExpToExp: " ++ show s

sExpToHeap :: SExp -> Heap
sExpToHeap (SSym s) = HVar s
sExpToHeap (SList [SSym ":=", SSym x, e]) = HVal x $ sExpToExp e
sExpToHeap (SList hs) = HMany $ map sExpToHeap hs

instance Pretty Exp where
  pPrintPrec _ _ (EVar s) = text s
  pPrintPrec _ _ (EInt i) = text (show i)
  pPrintPrec l _ (EArray [e]) = text "(" <> pPrintPrec l 0 e <> text ",)"
  pPrintPrec l _ (EArray es) = text "(" <> fsep (punctuate (text ",") (map (pPrintPrec l 0) es)) <> text ")"
  pPrintPrec l p (ELam x e) = maybeParens (p > 1) $ text x <+> text "=>" <+> pPrintPrec l 1 e
  pPrintPrec l p (ERec x e) = maybeParens (p > 1) $ text "rec" <+> text x <+> pPrintPrec l 2 e
  pPrintPrec l p (EUnify e1 e2) = maybeParens (p > 3) $ pPrintPrec l 3 e1 <+> text eq <+> pPrintPrec l 3 e2
    where eq | l == prettyNormal = "="
             | otherwise = "=="
  pPrintPrec l p (ESeq es) = maybeParens (p > 0) $ fsep (punctuate (text ";") (map (pPrintPrec l 0) es))
  pPrintPrec l p (EOp op es@EArray{}) = text op' <> pPrintPrec l 0 es
    where op' | l == prettyNormal = op
              | otherwise = op ++ "^"
  pPrintPrec l p (EOp op e) = text op' <> text "(" <> pPrintPrec l 0 e <> text ")"
    where op' | l == prettyNormal = op
              | otherwise = op ++ "^"
  pPrintPrec l p (EAlt []) = text "fail"
  pPrintPrec l p (EAlt [e]) = pPrintPrec l p e
  pPrintPrec l p (EAlt es) = maybeParens (p > 2) $ fsep (punctuate (text bar) (map (pPrintPrec l 2) es))
    where bar | l == prettyNormal = "|"
              | otherwise = " choice "
  pPrintPrec l p (EIf e1 e2 e3) = maybeParens (p > 0) $ text "if" <+> pPrintPrec l 0 e1 <+> text "then" <+> pPrintPrec l 0 e2 <+> text "else" <+> pPrintPrec l 0 e3
  pPrintPrec l p (EFor e1 e2) = maybeParens (p > 0) $ text "for" <+> pPrintPrec l 0 e1 <+> text "in" <+> pPrintPrec l 0 e2
  pPrintPrec l p (EDef h e) = maybeParens (p > 0) $ text "def" <+> pPrintPrec l 0 h <+> text "in" <+> pPrintPrec l 0 e
  pPrintPrec l _ EWrong = text "wrong"

instance Pretty Heap where
  pPrintPrec _ _ (HVar x) = text x
  pPrintPrec l _ (HVal x e) = text x <> text ":=" <> pPrintPrec l 0 e
  pPrintPrec l _ (HMany hs) = fsep $ punctuate (text ",") (map (pPrintPrec l 0) hs)

ltx :: (Pretty a) => a -> String
ltx a = renderStyle stl (pPrintPrec (PrettyLevel 1) 0 a)
  where stl = Style { mode = OneLineMode, lineLength = 10000, ribbonsPerLine = 0 }

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

strReds :: [(String, Exp)] -> String
strReds = concatMap (\ (s, e) -> printf "%10s:  %s\n" s (prettyShow e))

ppReds :: [(String, Exp)] -> IO ()
ppReds = putStr . strReds

latexReds :: [(String, Exp)] -> String
latexReds ((_, e) : rs) =
  "\\begin{example} $$\n\
  \  \\begin{array}{rcl}\n" ++

  "    \\multicolumn{3}{l}{|" ++ ltx e ++ "|} \\\\\n" ++
  concatMap red rs ++
  "  \\end{array}$$\n\
  \\\end{example}\n"
  where
    red (n, x) =
      "    \\rulename{" ++ n' ++ "} & \\movesto & |" ++ ltx (normSyms x) ++ "| \\\\\n"
      where n' = map toLower n
      
latexFile :: FilePath -> IO ()
latexFile fn = do
  file <- readUTF8File fn
  putStrLn $ latexReds $ conv $ runReadP pSExp $ dropGuillemets file

-- Shorten symbol names
-- XXX not properly implemented.
normSyms :: Exp -> Exp
normSyms (EVar s) = EVar $ truncSym s
normSyms e@(EInt _) = e
normSyms (EArray es) = EArray (map normSyms es)
normSyms (ELam s e) = ELam (truncSym s) (normSyms e)
normSyms (ERec s e) = ERec (truncSym s) (normSyms e)
normSyms (EUnify e1 e2) = EUnify (normSyms e1) (normSyms e2)
normSyms (ESeq es) = ESeq (map normSyms es)
normSyms (EOp s e) = EOp s (normSyms e)
normSyms (EAlt es) = EAlt (map normSyms es)
normSyms (EIf e1 e2 e3) = EIf (normSyms e1) (normSyms e2) (normSyms e3)
normSyms (EFor e1 e2) = EFor (normSyms e1) (normSyms e2)
normSyms (EDef h e) = EDef (heap h) (normSyms e)
  where heap (HVar s) = HVar $ truncSym s
        heap (HVal s e) = HVal (truncSym s) (normSyms e)
        heap (HMany hs) = HMany (map heap hs)
normSyms EWrong = EWrong

truncSym :: String -> String
truncSym = reverse . dropWhile isDigit . reverse
