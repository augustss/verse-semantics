--module Main where
import Control.Monad
import qualified Control.Monad.State.Strict as S
import Data.Void
import System.Environment
import Text.Megaparsec hiding(try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


main :: IO ()
main = do
  [n] <- getArgs
  f <- readFile n
  let r = parseDie pFile n f
  print r

data LexState = LexState
  { nest :: Bool
  , blockInd :: String
  , lineInd :: String
  }
  deriving (Show)

initLexState :: LexState
initLexState = LexState { nest = True, blockInd = "", lineInd = "" }


type P = ParsecT Void String (S.State [LexState])

runP :: P a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
runP pa fn s = S.evalState (runParserT pa fn s) [initLexState]

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

-- The regular try combinator does not backtrack the LexState.
-- This version has an error handler that resets the LexState.
try :: P a -> P a
try p = do
  ls <- S.get -- Get initial state.
  let err e = do
        S.put ls -- Reset state,
        parseError e -- and signal error.
  M.try (withRecovery err p) -- Use 'try' with special error handler.

---------------------------------------------------------

data Rule = Rule
  { name :: String
  , rhs  :: Elem
  }
  deriving (Show)

data Elem
  = Seq [Elem]
  | Alt [Elem]
  | Chr Char
  | ChrRange Char Char
  | Str String
  | Not Elem
  | Many Elem
  | Look Elem
  | Opt Elem
  | NonTerm String
  | Code Code
  deriving (Show)

-- Whar the difference between 'xy' and "xy"?

data Code
  = Push
  | Pop
  | Set String Expr
  | CSeq [Code]
  | Parse String Elem
  | If Expr Code (Maybe Code)
  | Error
  deriving (Show)

data Expr
  = EVar String
  | EStr String
  | EGT Expr Expr
  | ELE Expr Expr
  | EEQ Expr Expr
  | Enot Expr
  | Eand Expr Expr
  | Eor Expr Expr
  deriving (Show)

skip :: P ()
skip = S.void $ many spaceChar

lexeme :: P a -> P a
lexeme = L.lexeme skip

symbol :: String -> P String
symbol = L.symbol skip

keyword :: String -> P ()
keyword s = void $ lexeme (string s <* notFollowedBy alphaNumChar)

-----------------------------------------

sep :: P ()
sep = S.void $ symbol ";;"

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
  char '"'
  cs <- takeWhileP (Just "string") (/= '"')
  char '"'
  skip
  pure cs

pElem :: P Elem
pElem = choice
  [ pCharRange
  , NonTerm <$> pIdent
  , Many <$> between (symbol "{") (symbol "}") pAlts
  , Opt  <$> between (symbol "[") (symbol "]") pAlts
  ,          between (symbol "(") (symbol ")") pAlts
  , Not  <$> (symbol "!" *> pElem)
  , Look <$> (symbol "&" *> pElem)
  , Str  <$> pStr
  , Code <$> (semi *> pCode)
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
  choice [ EGT e <$> (symbol ">"  *> pAtom)
         , ELE e <$> (symbol "<=" *> pAtom)
         , EEQ e <$> (symbol "="  *> pAtom)
         , pure e]

pAtom :: P Expr
pAtom = choice
  [ EVar <$> pIdent
  , EStr <$> pStr
  ]
