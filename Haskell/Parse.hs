module Parse where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
--import Epic.Print hiding (char)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Read (readMaybe)

import ParseExpr

type P = Parsec Void String

-- Skip space and comments
skip :: P ()
skip = L.space space1 (L.skipLineComment "#") (L.skipBlockCommentNested "<#" "#>")

lexeme :: P a -> P a
lexeme = L.lexeme skip

symbol :: String -> P String
symbol = L.symbol skip

pWord :: P String
pWord = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_') <?> "identifier")

pIdent :: P Ident
pIdent = try $ do
  w <- pWord
  guard $ w `notElem` keywords
  pure $ Ident w

keywords :: [String]
keywords = ["case", "def", "do", "else", "for", "if", "in", "let", "of", "then", "typedef", "where"]

pKeyword :: String -> P ()
pKeyword s = try $ do
  w <- pWord
  guard (w == s)

pKeywordOpt :: String -> P ()
pKeywordOpt s = pKeyword s <|> pure ()

pParens :: P a -> P a
pParens = between (symbol "(") (symbol ")")

pBraces :: P a -> P a
pBraces = between (symbol "{") (symbol "}")

pBrackets :: P a -> P a
pBrackets = between (symbol "[") (symbol "]")

pAngles :: P a -> P a
pAngles = between (pOp "<") (pOp ">")

pDecimal :: P Integer
pDecimal = L.decimal <* skip

-- XXX Needs works
pString :: P String
pString = lexeme $ char '"' *> many (satisfy (/= '"')) <* char '"'

pOp :: String -> P String
pOp ":" = pOp' ":" "="
pOp "=" = pOp' "=" ">"
pOp "<" = pOp' "<" ">="
pOp ">" = pOp' ">" "="
pOp "^" = pOp' "^" ":"
pOp "-" = pOp' "-" ">"
pOp "|" = pOp' "|" "|"
pOp "&" = pOp' "&" "&"
pOp s = symbol s

-- Parse the string s, but not if it's followed by one of the characters in ex
pOp' :: String -> [Char] -> P String
pOp' s ex = (lexeme . try) (string s <* notFollowedBy (choice $ map char ex))

pAtom :: P Expr
pAtom = choice [Var <$> pIdent, Int <$> pDecimal, pParens pExprET, pDef, pSeq, pTypedef]

pDef :: P Expr
pDef = Def <$> (pKeyword "def" *> pBraces pIdent)

pTypedef :: P Expr
pTypedef = TypeDef <$> (pKeyword "typedef" *> pBraces pExpr)

pTerm :: P Expr
pTerm = do
  fn <- pAtom
  let pArg = (Left <$> pBrackets pExprET) <|> (Right <$> pParens pExprET)
      apply f (Left a) = Apply f a
      apply f (Right a) = Call f a
  foldl apply fn <$> many pArg

pSeq :: P Expr
pSeq = Seq <$> pBraces (sepBy1 pExpr (pOp ";"))

pExpr1 :: P Expr
pExpr1 =
  choice
    [ pKeyword "if" *> (If <$> pParens pExpr <*> (pKeywordOpt "then" *> pBlock) <*> (pKeyword "else" *> pBlock)),
      pKeyword "for" *> (For <$> pParens pExpr <*> (pKeywordOpt "in" *> pBlock)),
      pKeyword "let" *> (Let <$> pParens pExpr <*> (pKeywordOpt "in" *> pBlock)),
      pKeyword "case" *> (Case <$> pParens pExpr <*> (pKeywordOpt "of" *> pBraces (sepBy1 pExprT (pOp ";")))),
      pKeyword "do" *> (Do <$> pBlock),
      pTerm
    ]
  where
    -- Could parse : block here
    pBlock :: P Expr
    pBlock = pExpr

pExpr2 :: P Expr
pExpr2 = makeExprParser pExpr1 operatorTable

operatorTable :: [[Operator P Expr]]
operatorTable =
  [ [pre Range ":"],
    [postOp "^"],
    [op InfixL "*", opI InfixL "/"],
    [op InfixL "+", op InfixL "-"],
    [fn Alt InfixR "|", op InfixR "->"],
    [fn HasType InfixL ":", fn Unify InfixR "="] ++ map (opI InfixR) [">=", "<=", "<", ">", "<>"],
    [opI InfixR "&&"],
    [opI InfixR "||"],
    [fn Define InfixL ":=", kw Where InfixN "where"],
    [fn Lambda InfixR "=>"]
  ]
  where

    pre :: (Expr -> Expr) -> String -> Operator P Expr
    pre f s = Prefix (f <$ pOp s)

    post :: (Expr -> Expr) -> String -> Operator P Expr
    post f s = Postfix (f <$ pOp s)

    postOp :: String -> Operator P Expr
    postOp s = post app s
      where
        app x = Call (Var (Ident ("postfix'" ++ s ++ "'"))) x

    fn :: (Expr -> Expr -> Expr) -> (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    fn f fx s = fx (f <$ pOp s)

    kw :: (Expr -> Expr -> Expr) -> (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    kw f fx s = fx (f <$ pKeyword s)

    op :: (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    op = opA Call

    opI :: (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    opI = opA Apply

    opA :: (Expr -> Expr -> Expr) -> (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    opA c fx s = fn app2 fx s
      where
        app2 x y = c (Var (Ident ("operator'" ++ s ++ "'"))) (Array [x, y])

pExprT :: P Expr
pExprT = arrayS <$> sepBy1 pExpr2 (pOp ",")

-- Empty tuple is allowed
pExprET :: P Expr
pExprET = pExpr <|> pure (Array [])

pExpr :: P Expr
pExpr = pExprT

pFile :: P Expr
pFile = skip *> pExpr <* eof

------

-- Do not construct 1-tuples
arrayS :: [Expr] -> Expr
arrayS [e] = e
arrayS es = Array es

------

runP :: P a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
runP = runParser

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

testp :: P a -> String -> a
testp p = parseDie p "<string>"

parseString :: String -> Expr
parseString = parseDie pFile "<string>"
