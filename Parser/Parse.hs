module Parse(
  parseDie, pFile,
  -- Exports for further parsing
  pKeyword, skip, eof, many, pParens, pBraces, symbol, optional,
  pIdent, pExprSeq,
  P,
  testp, parseString) where

import Control.Monad
import qualified Control.Monad.State.Strict as S
import OpParser
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
--import Data.Ratio(numerator)
--import Data.Scientific(isInteger)
import Data.Void
--import Epic.Print hiding (char)
import Text.Megaparsec hiding(try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Read (readMaybe)

import Error
import Expr
import Print(prettyShow)

-- import Debug.Trace

data LexState = LexState
  { lastInd   :: !(Maybe String)
  , blkIndent :: ![String]        -- current block indentation
  }
  deriving (Show)

initLexState :: LexState
initLexState = LexState { blkIndent = [], lastInd = Nothing }

type P = ParsecT Void String (S.State LexState)

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

isNL :: Char -> Bool
isNL c = c == '\n' || c == '\r'

isHSpace :: Char -> Bool
isHSpace c = isSpace c && not (isNL c)

---------------------------------------------------------

-- Skip space and comments
skip :: P ()
skip = do
  S.modify $ \ ls -> ls { lastInd = Nothing }
  L.space aspace (L.skipLineComment "#") (L.skipBlockCommentNested "<#" "#>")
  where aspace = do
          c <- spaceChar
          when (isNL c) $ do
            s <- takeWhileP (Just "white space") isHSpace
            S.modify $ \ ls -> ls { lastInd = Just s }

pNLSpace :: P String
pNLSpace = do
  _ <- takeWhile1P (Just "white space") isNL
  takeWhileP (Just "white space") isHSpace

skipH :: P ()
skipH = L.space hspace1 (L.skipLineComment "#") (L.skipBlockCommentNested "<#" "#>")

lexeme :: P a -> P a
lexeme = L.lexeme skip

symbol :: String -> P String
symbol = L.symbol skip

pWord :: P String
pWord = lexeme ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_') <?> "identifier")

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
keywords = (["alias", "and", "array", "block", "do", "else", "effects", "for", "fn", "function", "if"
           , "in", "let", "not", "of", "or", "option", "ref", "return", "set", "then", "var", "where"]
           ++ macros)
           \\ ["logic"] -- Allowed both as a type and a macro

macros :: [String]
macros = ["all", "allow", "assert", "assume", "expect", "first", "logic", "one", "type", "unify"]
         ++ effects

effects :: [String]
effects = [ "decides", "diverges", "fails", "succeeds" ]

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

pEffectName :: P Ident
pEffectName = try $ do
  l <- getSourcePos
  w <- pWord
  guard (w `elem` effects)
  pure $ Ident l w

pKeywordOpt :: String -> P ()
pKeywordOpt s = pKeyword s <|> pure ()

pKeywordOptDot :: String -> P ()
pKeywordOptDot s = pKeyword s <|> void (pOp ".") <|> pure ()

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
  , (LitRat <$> L.scientific <*> many letterChar) <* skip
  , pString
  ]

pDecimal :: P Integer
pDecimal = choice
  [ try (char '0' *> char' 'x' *> L.hexadecimal)
  , try (char '0' *> char' 'o' *> L.octal)
  , try (char '0' *> char' 'b' *> L.binary)
  , try (L.decimal <* notFollowedBy (char '.'))
  ] <* skip

pChar :: P Char
pChar = (pQuotedChar {- <|> pCharCode -}) <* skip

-- Char inside '
pQuotedChar :: P Char
pQuotedChar = char '\'' *> (pPrintableChar "'\\" <|> pBackslashChar) <* char '\''

-- Any printable Char, except the quote and \
pPrintableChar :: String -> P Char
pPrintableChar spec = satisfy $ \ c -> isPrint c && c `notElem` spec

-- A \x sequence
pBackslashChar :: P Char
pBackslashChar = do
  _ <- char '\\'
  ch <- satisfy (const True)
  let escs = [('r', '\r'), ('n', '\n'), ('t', '\t')] ++
             map (\ c -> (c, c)) "'\"\\{}#<>&~"
  case lookup ch escs of
    Nothing -> fail "pQuotedChar"
    Just c' -> pure c'

-- A character without quotes
--pCharCode :: P Char
--pCharCode = fail "unimplemented pCharCode"

pString :: P Expr
pString = do
  let pStr = some (pPrintableChar "\"\\{" <|> pBackslashChar)
      pInterp = pBraces pExprSeq
      conc [] = LitStr ""
      conc [e] = e
      conc es = ApplyD (Variable (Ident noLoc "strConc$")) (Array es)
      toStr e = Macro1 (Ident noLoc "toStr$") [] e
  _ <- char '"'
  cs <- many ((LitStr <$> pStr) <|> (toStr <$> pInterp))
  _ <- char '"'
  skip
  pure $ conc cs

-- XXX Needs works
--pString :: P String
--pString = lexeme $ char '"' *> many (satisfy (/= '"')) <* char '"'

pOp :: String -> P String
pOp ":" = pOp' ":" "=-)"
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
pAtom = choice [ pMacro, Variable <$> pIdent, pQualVariable, pLiteral, pEmpty
               , Parens <$> pParens pExprSeq, pArray
               , pOption, pFunction, pBlockM, pEffects ]
  where pEmpty = try $ pParens (pure (Array []))

pQualVariable :: P Expr
pQualVariable = try (QualVariable <$> pParens (pExprT <* char ':') <*> pIdent)

-- XXX This does not behave like TimVerse.  Without ';' the ',' is use as the delimiter.
-- A trailing ';' can be used, but not a trailing ','.
pArray :: P Expr
pArray = pKeyword "array" *> (Array <$> pBlockEs)

--pTypedef :: P Expr
--pTypedef = pKeyword "type" *> (Typedef <$> pBlockM)
-- XXX remove try by combining with Variable
pMacro :: P Expr
pMacro = try $ do
  n <- pMacroName
  (Macro1 n <$> many pAttr <*> pBlockM) <|>
   (Macro2 n <$> pParens pExprSeq <*> pBlockM)

pAttr :: P Ident
pAttr = pAngles pEffectId

pEffectId :: P Ident
pEffectId = pIdent <|> pEffectName

pEffects :: P Expr
pEffects = pKeyword "effects" *> (ApplyEff <$> pParens (many pEffectId) <*> pBlockM)

pTerm :: P Expr
pTerm = do
  fn <- pAtom
  let pArg :: P (Expr -> Expr)
      pArg = (flip ApplyD <$> pBrackets pExprSeq) <|>
             (flip ApplyS <$> try (pParens pExprSeq)) <|>
             (flip EffAttr <$> try pAttr) <|>
             pPost
      pPost = do
        l <- getSourcePos
        let op s = pOp s *> pure (\ x -> PostfixOp x (Ident l s))
            dot = (\ i x -> InfixOp x (Ident l ".") (Variable i)) <$> (pOp "." *> pIdent)
        choice [op "^", op "?", dot]
      apply a f = f a
  foldl apply fn <$> many pArg

pFunction :: P Expr
pFunction = Function <$> ((pKeyword "fn" <|> pKeyword "function") *> some pArg) <*> pBlockM
  where
    pArg :: P (Expr, [Eff])
    pArg = (,) <$> pParens pExprSeq <*> many pAttr

pBlockEs :: P [Expr]
pBlockEs = pBraces (sepEndBy pExprT (pOp ";"))

pBlock :: P Block
pBlock = pBlockM <|> pExprT

pBlockM :: P Block
pBlockM = Block <$> (pBlockEs  <|> pIndBlock)

pIndBlock :: P [Expr]
pIndBlock = do
  s <- try (char ':' *> skipH *> pNLSpace)
  S.modify $ \ ls -> ls { blkIndent = s : blkIndent ls }
  pIndBlock'

pIndBlock' :: P [Expr]
pIndBlock' = do
  es <- sepEndBy pExprT (pSemi <|> pSameInd)
  pLessInd <|> eof
  pure es
  where
    pSemi = void (pOp ";")
    pSameInd = do
      ls <- S.get
--      traceM ("pSameInd " ++ show ls)
      case ls of
        LexState { lastInd = Just s, blkIndent = s' : _ } | s == s' ->
          S.put ls{ lastInd = Nothing }
        _ -> fail "indentation"
    pLessInd = do
      ls <- S.get
      case ls of
        LexState { lastInd = Just s, blkIndent = s' : bs } | s `isPrefixOf` s' ->
          S.put ls{ blkIndent = bs }  -- exit this block
        _ -> fail "indentation"

pExprSeq :: P Expr
pExprSeq = seqS <$> sepEndBy pExprT (pOp ";")

--pExprSeq1 :: P Expr
--pExprSeq1 = seqS <$> sepEndBy1 pExprT (pOp ";")

seqS :: [Expr] -> Expr
seqS [] = Array []
seqS [e] = e
seqS es = Seq es

pIf :: P Expr
pIf = pKeyword "if" *> (
  (mkIf <$> getSourcePos <*> pParenBlock <*> optional (pKeywordOptDot "then" *> pBlock) <*> optional (pKeyword "else" *> pBlock))
   <|>
  (mkIfC <$> pBlockM <*> optional (pKeyword "else" *> pBlock))
  )
  where
    mkIf l _  Nothing   Nothing   = syntaxError l "if(e) must have a 'then' and/or 'else'"
    mkIf _ e1 (Just e2) Nothing   = If2  e1 e2
    mkIf _ _e1 Nothing   (Just _e3) = unimplemented "if()else" -- If2E e1 e3 -- XXX is this correct?
    mkIf _ e1 (Just e2) (Just e3) = If3  e1 e2 e3
    mkIfC e1 Nothing            = If1  e1
    mkIfC e1 (Just e2)          = If2E e1 e2

pFor :: P Expr
pFor = pKeyword "for" *> (
  (For2 <$> pParenBlock <*> (pKeywordOptDot "do" *> pBlock))
  <|>
  (For1 <$> pBlockM)
  )

pLet :: P Expr
pLet = pKeyword "let" *> (Let <$> pParenBlock <*> (pKeywordOptDot "do" *> pBlock))

pParenBlock :: P Expr
pParenBlock = pParens pExprSeq <|> (seqS <$> pIndBlock)

pCase :: P Expr
pCase = pKeyword "case" *> (mkCase <$> optional (pParens pExprSeq) <*> (pKeywordOpt "of" *> pBlockM))
  where mkCase Nothing e2 = Case1 e2
        mkCase (Just e1) e2 = Case2 e1 e2

pDo :: P Expr
pDo = pKeyword "block" *> (Do <$> pBlockM)

pOption :: P Expr
pOption = pKeyword "option" *> (Option <$> optional pExprSeq)

pSet :: P Expr
pSet = pKeyword "set" *> do
  e <- pExprT
  case e of
    InfixOp e1 op@(Ident _ sop) e2
      | sop `elem` ["=", "+=", "-=", "*=", "/="] ->
        pure $ Set e1 op e2
    _ -> fail "set not followed by assignment operator"

pVar :: P Expr
pVar = pVRA >>= \ con -> do
  e <- pExprT
  case e of
    -- XXX using := here isn't quite right.  It will allow 'var x:t:=e' to work.
    InfixOp (InfixOp (Variable i1) (Ident _ ":") e2) (Ident _ ":=") e3 -> pure $ con i1 (Just e2) (Just e3)
    InfixOp (Variable i1) (Ident _ ":") e2 -> pure $ con i1 (Just e2) Nothing
    InfixOp (Variable i1) (Ident _ "=") e2 -> pure $ con i1 Nothing (Just e2)
    _ -> fail $ "var/ref not followed by x : t [= e]\n" ++ prettyShow e
 where
   pVRA = choice [pKeyword "var" $> MVar, pKeyword "ref" $> MRef, pKeyword "alias" $> MAlias]

pReturn :: P Expr
pReturn = pKeyword "return" *> (Return <$> pExpr2)

pExpr1 :: P Expr
pExpr1 = choice [ pIf, pFor, pLet, pCase, pDo, pSet, pVar, pTerm, pReturn ]

pExpr2 :: P Expr
pExpr2 = makeExprParser pExpr1 operatorTable

{-
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
  [ [preOp ":", preOp "not", preOp "?", preOp "[]", preOp "-", preOp "^"],
    [op InfixL "*", op InfixL "/", op InfixL "&"],
    [op InfixL "+", op InfixL "-"],
    [op InfixR "->", op InfixR ".."],
    [op InfixR "|", op InfixR ":"],
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
      pure $ \ x y -> InfixOp x (Ident l ":=") y
    defOp _ = fail "defOp"

pExprT :: P Expr
pExprT = arrayS <$> sepBy1 pExpr2 (pOp ",")

pFile :: P Expr
--pFile = skip *> pExprSeq <* eof
pFile = skip *> p <* eof
  where
    p = do
      S.modify $ \ ls -> ls{ blkIndent = [""] }
      seqS <$> pIndBlock'

arrayS :: [Expr] -> Expr
arrayS [e] = e
arrayS es = Array es

------

runP :: P a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
runP pa fn s = S.evalState (runParserT pa fn s) initLexState

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

testp :: P a -> String -> a
testp p = parseDie p "<string>"

parseString :: String -> Expr
parseString = parseDie pFile "<string>"
