module SExp(
  SExp(..),
  pSExp,
  ) where
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data SExp
  = SInt Integer
  | SStr String
  | SSym String
  | SList [SExp]
  deriving (Eq, Ord)

instance Show SExp where
  showsPrec _ (SInt i) = showsPrec 0 i
  showsPrec _ (SStr s) = showsPrec 0 s
  showsPrec _ (SSym s) = showString s
  showsPrec _ (SList es) = showParen True $ compose $ intersperse (showChar ' ') (map (showsPrec 10) es)
    where compose = foldr (.) id

instance Read SExp where
  readsPrec _ = readP_to_S pSExp

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
        syms c = not (isDigit c || c == '-') && sym c

pList :: ReadP SExp
pList = SList <$> (char '(' *> many pSExp <* char ')')

pQuote :: ReadP SExp
pQuote = char '\'' *> pSExp

