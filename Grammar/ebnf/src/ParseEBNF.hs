module ParseEBNF(
  Rule(..), Elem(..), Code(..), Expr(..),
  parseEBNF) where
import Control.Monad
--import qualified Control.Monad.State.Strict as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type P = Parsec Void String

parseEBNF :: FilePath -> String -> [Rule]
parseEBNF fn file =
  case runParser pFile fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

---------------------------------------------------------

data Rule = Rule
  { name :: String
  , rhs  :: Elem
  }
  deriving (Show, Eq)

data Elem
  = Seq [Elem]
  | Alt [Elem]
  | Chr Char
  | ChrRange Char Char
  | Str String
  | Not Elem
  | EMany Elem
  | Many Elem
  | Look Elem
  | Opt Elem
  | NonTerm String
  | Code Code
  | Deref String
  deriving (Show, Eq)

-- Whar the difference between 'xy' and "xy"?

data Code
  = Push
  | Pop
  | Set String Expr
  | CSeq [Code]
  | Parse String Elem
  | If Expr Code (Maybe Code)
  | Error
  deriving (Show, Eq)

data Expr
  = EVar String
  | EStr String
  | EGT Expr Expr
  | ELT Expr Expr
  | EGE Expr Expr
  | ELE Expr Expr
  | EEQ Expr Expr
  | Enot Expr
  | Eand Expr Expr
  | Eor Expr Expr
  deriving (Show, Eq)

skip :: P ()
skip = void $ many spaceChar

lexeme :: P a -> P a
lexeme = L.lexeme skip

symbol :: String -> P String
symbol = L.symbol skip

keyword :: String -> P ()
keyword s = void $ lexeme (string s <* notFollowedBy alphaNumChar)

-----------------------------------------

sep :: P ()
sep = void $ symbol ";;"

pFile :: P [Rule]
pFile = skip *> sep *> sepBy pRule sep <* eof

pRule :: P Rule
pRule = Rule <$> (pIdent <* symbol ":=") <*> pAlts

pIdent :: P String
pIdent = try $ do
  s <- lexeme (some alphaNumChar <?> "identifier")
  guard $ s `notElem` ["if", "error", "parse", "pop", "push", "set"]
  pure s

pAlts :: P Elem
pAlts = alts <$> sepBy1 pSeq (symbol "|")
  where alts [x] = x
        alts xs = Alt xs

pSeq :: P Elem
pSeq = seqs <$> some pElem
  where seqs [x] = x
        seqs xs = Seq xs

pChar :: P Char
pChar = ((char '\'' *> printChar <* char '\'')
     <|> char '0' *> (char 'o' <|> char 'u') *> (rd <$> some hexDigitChar)
        ) <* skip
  where rd cs = toEnum $ read $ "0x" ++ cs

pCharRange :: P Elem
pCharRange = do
  c <- pChar
  (ChrRange c <$> (symbol ".." *> pChar)) <|> pure (Chr c)

pStr :: P String
pStr = do
  void $ char '"'
  cs <- takeWhileP (Just "string") (/= '"')
  void $ char '"'
  skip
  pure cs

pElem :: P Elem
pElem = choice
  [ pCharRange
  , NonTerm <$> pIdent
  , EMany <$> between (symbol "{:") (symbol ":}") pAlts
  , Many <$> between (symbol "{") (symbol "}") pAlts
  , Opt  <$> between (symbol "[") (symbol "]") pAlts
  ,          between (symbol "(") (symbol ")") pAlts
  , Not  <$> (symbol "!" *> pElem)
  , Look <$> (symbol "&" *> pElem)
  , Str  <$> pStr
  , Code <$> (optional semi *> pCode <* optional semi)
  , Deref <$> (symbol "^" *> pIdent)
  ]
  where semi = try (char ';' <* notFollowedBy (char ';')) <* skip

pCode :: P Code
pCode = choice
  [ Pop   <$  keyword "pop"
  , Push  <$  keyword "push"
  , Set   <$> (keyword "set" *> (pIdent <* symbol "=")) <*> pExpr
  , Parse <$> (keyword "parse" *> (pIdent <* symbol ":=")) <*> pElem
  , If    <$> (keyword "if" *> between (symbol "(") (symbol ")") pExpr)
                      <*> (keyword "then" *> pCode)
                      <*> (optional (keyword "else" *> pCode))
  , Error <$  keyword "error"
  ]

pExpr :: P Expr
pExpr = pOr

pOr :: P Expr
pOr = do
  e <- pAnd
  (Eor e <$> (keyword "or" *> pOr)) <|> pure e

pAnd :: P Expr
pAnd = do
  e <- pNot
  (Eand e <$> (keyword "and" *> pAnd)) <|> pure e

pNot :: P Expr
pNot = (Enot <$> (keyword "not" *> pCmp)) <|> pCmp

pCmp :: P Expr
pCmp = do
  e <- pAtom
  choice [ ELE e <$> (symbol "<=" *> pAtom)
         , EGE e <$> (symbol ">=" *> pAtom)
         , EGT e <$> (symbol ">"  *> pAtom)
         , ELT e <$> (symbol "<"  *> pAtom)
         , EEQ e <$> (symbol "="  *> pAtom)
         , pure e]

pAtom :: P Expr
pAtom = choice
  [ EVar <$> pIdent
  , EStr <$> pStr
  ]
