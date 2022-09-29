module Parse(
  parseDie, pFile,
  -- Exports for further parsing
  pKeyword, skip, eof, many, pParens, pBraces,
  pIdent, pExprSeq,
  P) where

import Control.Monad
import OpParser
import Data.Char
import Data.Maybe
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
  w0 <- pWord
  suf <- optional (char '$')
  let w = w0 ++ maybeToList suf
  guard $ w `notElem` keywords
  if w `elem` ["operator", "prefix", "postfix", "infix"] then do
    _ <- char '\''
    op <- takeWhile1P Nothing (`elem` opChars)
    _ <- char '\''
    let w' = pre ++ "'" ++ op ++ "'"
        pre = if w == "postfix" then "post" else if w == "prefix" then "pre" else "in"
    skip
    pure $ Ident l w'
   else do
    pure $ Ident l w

opChars :: [Char]
opChars = "!@#$%^&*-+=:<>?/[]."

keywords :: [String]
keywords = ["and", "array", "block", "do", "else", "effects", "for", "fn", "function", "if"
           , "in", "let", "not", "of", "or", "option", "set", "then", "var", "where"] ++
           macros

macros :: [String]
macros = ["all", "one", "type"]

pKeyword :: String -> P ()
pKeyword s = try $ do
  w <- pWord
  guard (w == s)

pMacroName :: P Ident
pMacroName = try $ do
  l <- getSourcePos
  w <- pWord
  guard (w `elem` macros)
  pure $ Ident l w

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

pLiteral :: P Expr
pLiteral = choice
  [ LitInt <$> pDecimal
  , LitChar <$> pChar
  , LitStr <$> pString
  ]

pDecimal :: P Integer
pDecimal = L.decimal <* skip

pChar :: P Char
pChar = pQuotedChar <|> pCharCode

pQuotedChar :: P Char
pQuotedChar = char '\'' *> (pPrintableChar <|> pBackslashChar) <* char '\''
  where pPrintableChar = satisfy isPrint
        pBackslashChar :: P Char
        pBackslashChar = do
          _ <- char '\\'
          c <- satisfy (const True)
          case lookup c escs of
            Nothing -> fail "pQuotedChar"
            Just c' -> pure c'
        escs = [('r', '\r'), ('n', '\n'), ('t', '\t')] ++
               map (\ c -> (c, c)) "'\\{}#<>&~"

pCharCode :: P Char
pCharCode = fail "unimplemented"

pString :: P String
pString = fail "pString unimplemented"

-- XXX Needs works
--pString :: P String
--pString = lexeme $ char '"' *> many (satisfy (/= '"')) <* char '"'

pOp :: String -> P String
pOp ":" = pOp' ":" "=-"
pOp "=" = pOp' "=" ">"
pOp "<" = pOp' "<" ">="
pOp ">" = pOp' ">" ">="
--pOp "^" = pOp' "^" ":"
pOp "-" = pOp' "-" "=>"
pOp "|" = pOp' "|" "|"
pOp "&" = pOp' "&" "&"
pOp "." = pOp' "." "=."
pOp "~" = pOp' "~" ">"
pOp "+" = pOp' "+" "="
pOp "*" = pOp' "*" "="
pOp "/" = pOp' "/" "="
pOp s = symbol s

-- Parse the string s, but not if it's followed by one of the characters in ex
pOp' :: String -> [Char] -> P String
pOp' s ex = (lexeme . try) (string s <* notFollowedBy (choice $ map char ex))

pAtom :: P Expr
pAtom = choice [ Variable <$> pIdent, pLiteral, pEmpty
               , Parens <$> pParens pExprSeq, pArray, pMacro1
               , pOption, pFunction, pBlockM, pEffects ]
  where pEmpty = try $ pParens (pure (Array []))

-- XXX This does not behave like TimVerse.  Without ';' the ',' is use as the delimiter.
-- A trailing ';' can be used, but not a trailing ','.
pArray :: P Expr
pArray = pKeyword "array" *> (Array <$> pBlockEs)

--pTypedef :: P Expr
--pTypedef = pKeyword "type" *> (Typedef <$> pBlockM)
pMacro1 :: P Expr
pMacro1 = Macro1 <$> pMacroName <*> pBlockM

pEffects :: P Expr
pEffects = pKeyword "effects" *> (ApplyEff <$> pParens (many pIdent) <*> pBlockM)

pTerm :: P Expr
pTerm = do
  fn <- pTermPost
  let pArg :: P (Expr -> Expr)
      pArg = (flip ApplyD <$> pBrackets pExprSeq) <|>
             (flip ApplyS <$> pParens pExprSeq) <|>
             (flip EffAttr <$> try (pAngles pIdent))
      apply a f = f a
  foldl apply fn <$> many pArg

pFunction :: P Expr
pFunction = Function <$> ((pKeyword "fn" <|> pKeyword "function") *> some pArg) <*> pBlockM
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
    mkIf _  Nothing   Nothing   = syntaxError noLoc "if(e) must have a 'then' and/or 'else'"
    mkIf e1 (Just e2) Nothing   = If2  e1 e2
    mkIf e1 Nothing   (Just e3) = If2E e1 e3
    mkIf e1 (Just e2) (Just e3) = If3  e1 e2 e3

pFor :: P Expr
pFor = pKeyword "for" *> (
  (For2 <$> pParens pExprSeq1 <*> (pKeywordOpt "do" *> pBlock))
  <|>
  (For1 <$> pBlockM)
  )

pLet :: P Expr
pLet = pKeyword "let" *> (Let <$> pParens pExprSeq <*> (pKeywordOpt "do" *> pBlock))

pCase :: P Expr
pCase = pKeyword "case" *> (mkCase <$> optional (pParens pExprSeq1) <*> (pKeywordOpt "of" *> pBlockM))
  where mkCase Nothing e2 = Case1 e2
        mkCase (Just e1) e2 = Case2 e1 e2

pDo :: P Expr
pDo = pKeyword "block" *> (Do <$> pBlockM)

pOption :: P Expr
pOption = pKeyword "option" *> (Option <$> optional pExprSeq1)

pSet :: P Expr
pSet = pKeyword "set" *> do
  e <- pExprT
  case e of
    InfixOp e1 op@(Ident _ sop) e2
      | sop `elem` ["=", "+=", "-=", "*=", "/="] ->
        pure $ Set e1 op e2
    _ -> fail "set not followed by assignment operator"

pVar :: P Expr
pVar = pKeyword "var" *> do
  e <- pExprT
  case e of
    InfixOp (InfixOp (Variable i1) (Ident _ ":") e2) (Ident _ "=") e3 -> pure $ MVar i1 e2 e3
    _ -> fail "var not followed by x : t = e"

pExpr1 :: P Expr
pExpr1 = choice [ pIf, pFor, pLet, pCase, pDo, pSet, pVar, pTerm ]

pExpr2 :: P Expr
pExpr2 = makeExprParser pExpr1 operatorTable

pTermPost :: P Expr
pTermPost = do
  let pPost = do
        l <- getSourcePos
        let op s = pOp s *> pure (\ x -> PostfixOp x (Ident l s))
            dot = (\ i x -> InfixOp x (Ident l ".") (Variable i)) <$> (pOp "." *> pIdent)
        choice [op "^", op "?", dot]
  a <- pAtom
  ops <- many pPost
  pure $ foldl (flip ($)) a ops

{-
pTermPost :: P Expr
pTermPost = makeExprParser pAtom operatorTablePost

operatorTablePost :: [[Operator P Expr]]
operatorTablePost =
  [ [postOp "^", postOp "?"]
  ]
  where
    postOp :: String -> Operator P Expr
    postOp s = Postfix (app <$> pOpL s)
      where app l x = PostfixOp x (Ident l s)

    pOpL s = getSourcePos <* pOp s
-}

-- XXX Add more operators
operatorTable :: [[Operator P Expr]]
operatorTable =
  [ [preOp ":", preOp "not", preOp "?", preOp "[]"],
    [op InfixL "*", op InfixL "/", op InfixL "&"],
    [op InfixL "+", op InfixL "-"],
    [op InfixR "~>", op InfixN ".."],
    [op InfixR "|", op InfixN ":"],
    [op InfixR ">=", op InfixR "<=", op InfixR "<", op InfixR ">", op InfixL "<>", op InfixL "="],
    [op InfixR "and"],
    [op InfixR "or"],
    [op InfixR ":=", op InfixR ">>"
    ,op InfixN "+=", op InfixN "-=", op InfixN "*=", op InfixN "/=", op InfixN ".="
    ,InfixR defOp
    ],
    [op InfixL "where"],  -- XXX precedence
    [preOp ".."],
    [op InfixR "=>"]
--  , [op InfixN ":-"]
  ]
  where
    preOp :: String -> Operator P Expr
    preOp s = Prefix app
      where app = do
              l <- oper s
              pure $ \ x -> PrefixOp (Ident l s) x

    op :: ((Expr -> P (Expr -> Expr -> Expr)) -> Operator P Expr) -> String -> Operator P Expr
    op fx s = fx app
      where
        app (InfixOp _ (Ident _ ":") _) | s == "=" = fail ":e="
        app _ = do
          l <- oper s
          pure $ \ x y -> InfixOp x (Ident l s) y

    oper s | isAlpha (head s) = getSourcePos <* pKeyword s
           | otherwise = pOpL s

    pOpL s = getSourcePos <* pOp s

    defOp :: Expr -> P (Expr -> Expr -> Expr)
    defOp (InfixOp _ (Ident _ ":") _) = do
      l <- pOpL "="
      traceM "defOp"
      pure $ \ x y -> InfixOp x (Ident l ":=") y
    defOp _ = fail "defOp"

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
