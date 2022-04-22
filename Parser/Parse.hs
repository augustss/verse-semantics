module Parse where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Char
import Data.Void
--import Epic.Print hiding (char)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Read (readMaybe)

import Error
import Expr

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
  l <- getSourcePos
  w <- pWord
  guard $ w `notElem` keywords
  pure $ Ident l w

keywords :: [String]
keywords = ["array", "do", "else", "for", "fn", "function", "if", "in", "let", "of", "then", "where"]

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
pOp "." = pOp' "." "."
pOp s = symbol s

-- Parse the string s, but not if it's followed by one of the characters in ex
pOp' :: String -> [Char] -> P String
pOp' s ex = (lexeme . try) (string s <* notFollowedBy (choice $ map char ex))

pAtom :: P Expr
pAtom = choice [Variable <$> pIdent, LitInt <$> pDecimal, pEmpty, pParens pExprSeq, pArray]
  where pEmpty = try $ pParens (pure (Array (BExprs [])))

pArray :: P Expr
pArray = pKeyword "array" *> (Array <$> pBlockM)

pTerm :: P Expr
pTerm = do
  fn <- pAtom
  let pArg :: P (Expr -> Expr)
      pArg = (flip Index <$> pBrackets pExprSeq) <|>
             (flip Call <$> pParens pExprSeq) <|>
             (flip EffAttr <$> try (pAngles pIdent))
      apply a f = f a
  foldl apply fn <$> many pArg

pFunction :: P Expr
pFunction = Function <$> ((pKeyword "fn" <|> pKeyword "function") *> pParens pExprSeq) <*> many pEff <*> pBlock
  where
    pEff :: P Eff
    pEff = pAngles pIdent

pBlock :: P Block
pBlock = pBlockM <|> (BExpr <$> pExprT)

pBlockM :: P Block
pBlockM = BExprs <$> pBraces (sepEndBy pExprT (pOp ";"))

pExprSeq :: P Expr
pExprSeq = seqS <$> sepEndBy pExprT (pOp ";")

pExprSeq1 :: P Expr
pExprSeq1 = seqS <$> sepEndBy1 pExprT (pOp ";")

seqS :: [Expr] -> Expr
seqS [] = Array (BExprs [])
seqS [e] = e
seqS es = Seq es

pIf :: P Expr
pIf = pKeyword "if" *> (
  (mkIf <$> pParens pExprSeq1 <*> optional (pKeywordOpt "then" *> pBlock) <*> optional (pKeyword "else" *> pBlock))
  <|>
  (If1 <$> pBlockM)
  )
  where
    mkIf _ Nothing Nothing = syntaxError "if(e) must have a then and/or else"
    mkIf e1 (Just e2) Nothing   = If2  e1 e2
    mkIf e1 Nothing   (Just e3) = If2E e1 e3
    mkIf e1 (Just e2) (Just e3) = If3  e1 e2 e3

pFor :: P Expr
pFor = pKeyword "for" *> (
  (For2 <$> pParens pExprSeq1 <*> (pKeywordOpt "in" *> pBlock))
  <|>
  (For1 <$> pBlockM)
  )

pLet :: P Expr
pLet = pKeyword "let" *> (Let <$> pParens pExprSeq <*> (pKeywordOpt "in" *> pBlock))

pCase :: P Expr
pCase = pKeyword "case" *> (mkCase <$> optional (pParens pExprSeq1) <*> (pKeywordOpt "of" *> pBlockM))
  where mkCase Nothing e2 = Case1 e2
        mkCase (Just e1) e2 = Case2 e1 e2

pDo :: P Expr
pDo = pKeyword "do" *> (Do <$> pBlockM)

pExpr1 :: P Expr
pExpr1 = choice [ pIf, pFor, pLet, pCase, pDo, pFunction, pTerm ]

pExpr2 :: P Expr
pExpr2 = makeExprParser pExpr1 operatorTable

-- XXX Add more operators
operatorTable :: [[Operator P Expr]]
operatorTable =
  [ [preOp ":"],
    [postOp "^"],
    [op InfixL "*", op InfixL "/"],
    [op InfixL "+", op InfixL "-"],
    [op InfixR "|", op InfixR "->", op InfixN ".."],
    [op InfixL ":"] ++ map (op InfixR) ["=", ">=", "<=", "<", ">", "<>"],
    [op InfixR "&&"],
    [op InfixR "||"],
    [op InfixL ":="],
    [op InfixL "where"],  -- XXX precedence
    [op InfixR "=>"]
  ]
  where
    preOp :: String -> Operator P Expr
    preOp s = Prefix (app <$> pOpL s)
      where app l x = PrefixOp (Ident l s) x

    postOp :: String -> Operator P Expr
    postOp s = Postfix (app <$> pOpL s)
      where app l x = PostfixOp x (Ident l s)

    op :: (P (Expr -> Expr -> Expr) -> Operator P Expr) -> String -> Operator P Expr
    op fx s = fx (app <$> oper)
      where app l x y = InfixOp x (Ident l s) y
            oper | isAlpha (head s) = getSourcePos <* pKeyword s
                 | otherwise = pOpL s

    pOpL s = getSourcePos <* pOp s

pExprT :: P Expr
pExprT = arrayS <$> sepBy1 pExpr2 (pOp ",")

pFile :: P Expr
pFile = skip *> pExprSeq <* eof

-----
{-
data Test
  = TestParse {testName :: String, testExpr :: Expr}
  | TestEval {testName :: String, testExpr :: Expr, testResult :: [String]}
  | TestEvalMany {testName :: String, testExpr :: Expr, testResult :: [String]}
  | TestEvalError {testName :: String, testExpr :: Expr, testError :: String}
  deriving (Show)

pTest :: P Test
pTest =
  skip
    *> choice
      [ TestParse <$> (pKeyword "testp" *> pParens pWord) <*> pBraces pExpr,
        TestEval <$> (pKeyword "tests" *> pParens pWord) <*> pBraces pExpr <*> pBraces (sepBy1 pString (pOp ",")),
        TestEvalMany <$> (pKeyword "testm" *> pParens pWord) <*> pBraces pExpr <*> pBraces (sepBy1 pString (pOp ",")),
        TestEvalError <$> (pKeyword "teste" *> pParens pWord) <*> pBraces pExpr <*> pBraces pString
      ]

pTests :: P [Test]
pTests = many pTest <* eof
-}

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
