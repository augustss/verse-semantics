module Parse(
  parseDie, pFile,
  -- Exports for further parsing
  pKeyword, skip, eof, many, pParens, pBraces,
  pIdent, pExprSeq,
  P) where

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
  if w `elem` ["operator", "prefix", "postfix"] then do
    _ <- char '\''
    op <- takeWhile1P Nothing (`elem` opChars)
    _ <- char '\''
    let w' = pre ++ "'" ++ op ++ "'"
        pre = if w == "operator" then "in" else if w == "prefix" then "pre" else "post"
    pure $ Ident l w'
   else do
    pure $ Ident l w

opChars :: [Char]
opChars = "!@#$%^&*-+=:<>?/"

keywords :: [String]
keywords = ["array", "do", "else", "for", "fn", "function", "if", "in", "let", "of", "option", "then", "type", "where"]

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
--pString :: P String
--pString = lexeme $ char '"' *> many (satisfy (/= '"')) <* char '"'

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
pOp "~" = pOp' "~" ">"
pOp s = symbol s

-- Parse the string s, but not if it's followed by one of the characters in ex
pOp' :: String -> [Char] -> P String
pOp' s ex = (lexeme . try) (string s <* notFollowedBy (choice $ map char ex))

pAtom :: P Expr
pAtom = choice [ Variable <$> pIdent, LitInt <$> pDecimal, pEmpty
               , Parens <$> pParens pExprSeq, pArray, pTypedef ]
  where pEmpty = try $ pParens (pure (Array []))

pArray :: P Expr
pArray = pKeyword "array" *> (Array <$> pBlockEs)

pTypedef :: P Expr
pTypedef = pKeyword "type" *> (Typedef <$> pBlockM)

pTerm :: P Expr
pTerm = do
  fn <- pAtom
  let pArg :: P (Expr -> Expr)
      pArg = (flip ApplyD <$> pBrackets pExprSeq) <|>
             (flip ApplyS <$> pParens pExprSeq) <|>
             (flip EffAttr <$> try (pAngles pIdent))
      apply a f = f a
  foldl apply fn <$> many pArg

pFunction :: P Expr
pFunction = Function <$> ((pKeyword "fn" <|> pKeyword "function") *> some pArg) <*> pBlock
  where
    pArg :: P (Expr, [Eff])
    pArg = (,) <$> pParens pExprSeq <*> many (pAngles pIdent)

pBlockEs :: P [Expr]
pBlockEs = pBraces (sepEndBy pExprT (pOp ";"))

pBlock :: P Block
pBlock = pBlockM <|> pExprT

pBlockM :: P Block
pBlockM = Block <$> pBlockEs

pExprSeq :: P Expr
pExprSeq = seqS <$> sepEndBy pExprT (pOp ";")

pExprSeq1 :: P Expr
pExprSeq1 = seqS <$> sepEndBy1 pExprT (pOp ";")

seqS :: [Expr] -> Expr
seqS [] = Array []
seqS [e] = e
seqS es = Seq es

pIf :: P Expr
pIf = pKeyword "if" *> (
  (mkIf <$> pParens pExprSeq1 <*> optional (pKeywordOpt "then" *> pBlock) <*> optional (pKeyword "else" *> pBlock))
   <|>
  (If1 <$> pBlockM)
  )
  where
    mkIf _ Nothing Nothing = syntaxError noLoc "if(e) must have a 'then' and/or 'else'"
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

pOption :: P Expr
pOption = pKeyword "option" *> (Option <$> optional pExprSeq1)

pExpr1 :: P Expr
pExpr1 = choice [ pIf, pFor, pLet, pCase, pDo, pOption, pFunction, pTerm ]

pExpr2 :: P Expr
pExpr2 = makeExprParser pExpr1 operatorTable

-- XXX Add more operators
operatorTable :: [[Operator P Expr]]
operatorTable =
  [ [preOp ":", preOp "!"],
    [postOp "^", postOp "?"],
    [op InfixL "*", op InfixL "/", op InfixL "&"],
    [op InfixL "+", op InfixL "-"],
    [op InfixR "|", op InfixR "~>", op InfixN ".."],
    [op InfixR ":"] ++ map (op InfixR) [">=", "<=", "<", ">", "<>"],
    [op InfixR "&&"],
    [op InfixR "||"],
    [op InfixN ":=", op InfixL "="],
    [op InfixL "where"],  -- XXX precedence
    [preOp ".."],
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
      where app l x y = hackDef l x s y
            oper | isAlpha (head s) = getSourcePos <* pKeyword s
                 | otherwise = pOpL s

    pOpL s = getSourcePos <* pOp s

    -- x:t=e  is the same as x:t:=e
    hackDef l x@(InfixOp _ (Ident _ ":") _) "=" y = InfixOp x (Ident l ":=") y
    hackDef l x s y = InfixOp x (Ident l s) y

pExprT :: P Expr
pExprT = arrayS <$> sepBy1 pExpr2 (pOp ",")

pFile :: P Expr
pFile = skip *> pExprSeq <* eof

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

_testp :: P a -> String -> a
_testp p = parseDie p "<string>"

_parseString :: String -> Expr
_parseString = parseDie pFile "<string>"
