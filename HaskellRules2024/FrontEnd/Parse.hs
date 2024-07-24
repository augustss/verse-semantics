module FrontEnd.Parse(
    P,   -- The parser monad
    parseDie, parseTry, pFile,

    -- Exports for further parsing
    pKeyword, skip, eof, many, pParens, pBraces, symbol, optional,
    pIdent, pExprSeq, pOp, pLiteral, pMacroName, try,
    pString, pBlockM, pAny,

    lexeme, string, testp, parseString
  ) where

import FrontEnd.Error
import FrontEnd.Expr

import Epic.OpParser
import Epic.Print(prettyShow)

import Control.Monad
import qualified Control.Monad.State.Strict as S
import Data.Char ( isSpace, isPrint, isAlpha, isDigit )
import Data.Functor
import Data.List
import Data.Maybe
--import Data.Ratio(numerator)
--import Data.Scientific(isInteger)
import Data.Void

import Text.Megaparsec hiding(try)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
--import Text.Read (readMaybe)

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

pAny :: P String
pAny = many anySingle

{-
pWordOp :: P String
pWordOp = do
  w0 <- pWord
  suf <- optional (char '$')
  let w = w0 ++ maybeToList suf
  if w `elem` ["operator", "prefix", "postfix", "infix"] then do
    _ <- char '\''
    op <- takeWhile1P Nothing (`elem` opChars)
    _ <- char '\''
    let w' = pre ++ "'" ++ op ++ "'"
        pre = if w == "postfix" then "post" else if w == "prefix" then "pre" else "in"
    skip
    pure w'
   else do
    pure w
-}

pWordOp :: P String
--   Parses        as
--   -----------------------
--   wombat        "wombat"
--   operator'+'   "+"
--   prefix'+'     "+"
--
pWordOp = do
  w0 <- pWord
  suf <- optional (char '$')
  let w = w0 ++ maybeToList suf
  if w `elem` ["operator", "prefix"]
   then do { _ <- char '\''
           ; op <- takeWhile1P Nothing (`elem` opChars)
           ; _ <- char '\''
           ; skip
           ; pure op }
   else pure w

pIdent :: P Ident
pIdent = try $ do
  l <- getSourcePos
  w <- pWordOp
  guard $ w `notElem` keywords
  pure $ Ident l w

opChars :: [Char]
opChars = "!@#$%^&*-+=:<>?/[]."

keywords :: [String]
keywords = (["alias", "and", "array", "block", "do", "else", "effects", "for", "fn", "function", "if"
           , "in", "let", "map", "not", "of", "or", "option", "ref", "return", "set", "then"
           , "truth",  "var", "where"
           , "lambda", "lam", "exi", "exists" ]
           ++ macros)
           \\ ["logic"] -- Allowed both as a type and a macro

macros :: [String]
macros =
  [ "all"
  , "one"
  , "some"
  , "guard"
  , "verify"
  , "check"
  -- other macros
  , "allow"
  , "assert"
  , "assume"
  , "expect"
  , "first"
  , "last"
  , "logic"
  , "lowered"
  , "reject"
  , "type"
  , "unify"
  -- , "Err"
  ]



macrosOp :: [String]
macrosOp = ["in'='"]
           ++ macros

effects :: [String]
effects = [ "decides", "diverges", "fails", "succeeds", "iterates", "closed", "open" ]

pKeyword :: String -> P ()
pKeyword s = try $ do
  w <- pWord
  guard (w == s)

pMacroName :: P Ident
pMacroName = try $ do
  l <- getSourcePos
  w <- pWordOp
  guard (w `elem` macrosOp)
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

pLiteral :: P SrcExpr
pLiteral = choice
  [ Lit . LInt <$> pDecimal
  , Lit . LChar <$> pChar
  -- Handle 1..2 incorrectly
  , (Lit <$> (LRat <$> L.scientific <*> ((:) <$> letterChar <*> many alphaNumChar)) <* skip)
  , Lit . LPath <$> pPath
  , pString
  ]

pDecimal :: P Integer
pDecimal = choice
  [ try (char '0' *> char' 'x' *> L.hexadecimal)
  , try (char '0' *> char' 'o' *> L.octal)
  , try (char '0' *> char' 'b' *> L.binary)
  , try (L.decimal <* notFollowedBy (char '.' <* notFollowedBy (char '.')))
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

-- Simplified paths
pPath :: P Path
pPath = try $ do
  c1 <- char '/'
  c2 <- satisfy isAlpha
  cs <- many (satisfy (\ c -> isAlpha c || isDigit c || c == '_' || c == '/'))
  skip
  pure $ Path $ c1 : c2 : cs

pString :: P SrcExpr
pString = do
  let pStr = some (pPrintableChar "\"\\{" <|> pBackslashChar)
      pInterp = pBraces pExprSeq
      conc [] = Lit (LStr "")
      conc [e] = e
      conc es = ApplyD (Variable (Ident noLoc "strConc$")) (Array es)
      toStr e = Macro1 (Ident noLoc "toStr$") [] e
  _ <- char '"'
  cs <- many ((Lit . LStr <$> pStr) <|> (toStr <$> pInterp))
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

pAtom :: P SrcExpr
pAtom = choice [ pMacro, Variable <$> pIdent, pQualVariable, pLiteral, pEmpty
               , Parens <$> pParens pExprSeq, pArray, pMap, pTruth
               , pOption, pFunction, pBlockM ]
  where pEmpty = try $ pParens (pure (Array []))

pQualVariable :: P SrcExpr
pQualVariable = try (QualVariable <$> pParens (pExprT <* char ':') <*> pIdent)

pMap :: P SrcExpr
pMap = pKeyword "map" *> (Map <$> pBlockEs)

pTruth :: P SrcExpr
pTruth = pKeyword "truth" *> (Truth <$> pBraces pExpr1)

-- Try to mimic TimVerse by turning a tuple into an array
-- A trailing ';' can be used, but not a trailing ','.
pArray :: P SrcExpr
pArray = (pKeyword "array" *> (tArray <$> pBlockEs))
         <|> (pAngles pExprT)
  where
    tArray [Tuple es] = Array es
    tArray es = Array es

pMacro :: P SrcExpr
pMacro = try $ do
  n <- pMacroName
  (Macro1 n <$> many pAttr <*> pBlockM) <|>
   (Macro2 n <$> pParens pExprSeq <*> pBlockM)

pAttr :: P Ident
pAttr = pAngles pEffectId

pEffectId :: P Ident
pEffectId = pIdent <|> pEffectName

pTerm :: P SrcExpr
pTerm = do
  fn <- pAtom
  let pArg :: P (SrcExpr -> SrcExpr)
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

pFunction :: P SrcExpr
pFunction = Function <$> ((pKeyword "fn" <|> pKeyword "function") *> some pArg) <*> pBlockM
  where
    pArg :: P (SrcExpr, [Eff])
    pArg = (,) <$> pParens pExprSeq <*> many pAttr

pBlockEs :: P [SrcExpr]
pBlockEs = pBraces (sepEndBy pExprT (pOp ";"))

pBlock :: P SrcBlk
pBlock = pBlockM <|> pExprT

pBlockM :: P SrcBlk
pBlockM = Blk <$> (pBlockEs  <|> pIndBlock)

pIndBlock :: P [SrcExpr]
pIndBlock = do
  s <- try (char ':' *> skipH *> pNLSpace)
  S.modify $ \ ls -> ls { blkIndent = s : blkIndent ls }
  pIndBlock'

pIndBlock' :: P [SrcExpr]
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

pExprSeq :: P SrcExpr
pExprSeq = seqS <$> sepEndBy pExprT (pOp ";")

--pExprSeq1 :: P SrcExpr
--pExprSeq1 = seqS <$> sepEndBy1 pExprT (pOp ";")

seqS :: [SrcExpr] -> SrcExpr
seqS [] = Array []
seqS [e] = e
seqS es = Seq es

pIf :: P SrcExpr
pIf = pKeyword "if" *> (
  (mkIf <$> getSourcePos <*> pParenBlock <*> optional (pKeywordOptDot "then" *> pBlock) <*>
            optional (pKeyword "else" *> optional (pOp ".") *> pBlock))
   <|>
  (mkIfC <$> pBlockM <*> optional (pKeyword "else" *> pBlock))
  )
  where
    mkIf l _  Nothing   Nothing   = syntaxError l "if(e) must have a 'then' and/or 'else'"
    mkIf _ e1 (Just e2) Nothing   = If2  e1 e2
    mkIf _ e1 Nothing   (Just e3) = If2E e1 e3
    mkIf _ e1 (Just e2) (Just e3) = If3  e1 e2 e3
    mkIfC e1 Nothing            = If1  e1
    mkIfC e1 (Just e2)          = If2E e1 e2

pFor :: P SrcExpr
pFor = pKeyword "for" *> (
  (For2 <$> pParenBlock <*> (pKeywordOptDot "do" *> pBlock))
  <|>
  (For1 <$> pBlockM)
  )

pLet :: P SrcExpr
pLet = pKeyword "let" *> (Let <$> pParenBlock <*> (pKeywordOptDot "do" *> pBlock))

pParenBlock :: P SrcExpr
pParenBlock = pParens pExprSeq <|> (seqS <$> pIndBlock)

pCase :: P SrcExpr
pCase = pKeyword "case" *> (mkCase <$> optional (pParens pExprSeq) <*> (pKeywordOpt "of" *> pBlockM))
  where mkCase Nothing e2 = Case1 e2
        mkCase (Just e1) e2 = Case2 e1 e2

pDo :: P SrcExpr
pDo = pKeyword "block" *> (Block <$> pBlockM)

pOption :: P SrcExpr
pOption = pKeyword "option" *> (Option <$> pBraces (optional pExpr1))

pSet :: P SrcExpr
pSet = pKeyword "set" *> do
  e <- pExprT
  case e of
    InfixOp e1 op@(Ident _ sop) e2
      | sop `elem` ["=", "+=", "-=", "*=", "/="] ->
        pure $ Set e1 op e2
    _ -> fail "set not followed by assignment operator"

pVar :: P SrcExpr
pVar = pVRA >>= \ con -> do
  e <- pExprT
  case e of
    -- XXX using := here isn't quite right.  It will allow 'var x:t:=e' to work.
    InfixOp (InfixOp (Variable i1) (Ident _ ":") e2) (Ident _ ":=") e3 -> pure $ con i1 (Just e2) (Just e3)
    InfixOp (Variable i1) (Ident _ ":") e2 -> pure $ con i1 (Just e2) Nothing
    InfixOp (Variable i1) (Ident _ "=") e2 -> pure $ con i1 Nothing (Just e2)
    Variable i1 | eref@(MRef _ _ _) <- con i1 Nothing Nothing -> pure eref
    _ -> fail $ "var/ref not followed by x : t [= e]\n" ++ prettyShow e
 where
   pVRA = choice [pKeyword "var" $> MVar, pKeyword "ref" $> MRef, pKeyword "alias" $> MAlias]

pReturn :: P SrcExpr
pReturn = pKeyword "return" *> (Return <$> pExpr2)

pExpr1 :: P SrcExpr
pExpr1 = choice [ pIf, pFor, pLet, pCase, pDo, pSet, pVar, pTerm, pReturn
                , pLambda, pExists, pGuard ]

pExpr2 :: P SrcExpr
pExpr2 = makeExprParser pExpr1 operatorTable

-- Lambda and exists (not strictly part of source at all)
pGuard :: P SrcExpr
pGuard = Guard <$> pAtom <*> ((pOp ">>") *> pExpr1)

pExists :: P SrcExpr
pExists = p_exi *> (Exists <$> some pIdent <*> pBlockM)
  where
    p_exi = pKeyword "exi" <|> pKeyword "exists"

pLambda :: P SrcExpr
pLambda = p_lam *> (Lam <$> pIdent <*> pBlockM)
   where
     p_lam = pKeyword "lambda" <|> pKeyword "lam" <|> void (pOp "\\")

{-
pTermPost :: P SrcExpr
pTermPost = do
  let pPost = do
        l <- getSourcePos
        let op s = pOp s *> pure (\ x -> PostfixOp x (Ident l s))
            dot = (\ i x -> InfixOp x (Ident l ".") (Variable i)) <$> (pOp "." *> pIdent)
        choice [op "^", op "?", dot]
  a <- pAtom
  ops <- many pPost
  pure $ foldl (flip ($)) a ops

pTermPost :: P SrcExpr
pTermPost = makeExprParser pAtom operatorTablePost

operatorTablePost :: [[Operator P SrcExpr]]
operatorTablePost =
  [ [postOp "^", postOp "?"]
  ]
  where
    postOp :: String -> Operator P SrcExpr
    postOp s = Postfix (app <$> pOpL s)
      where app l x = PostfixOp x (Ident l s)

    pOpL s = getSourcePos <* pOp s
-}

-- XXX Add more operators
operatorTable :: [[Operator P SrcExpr]]
operatorTable =
  [
{-13-}   [preOp ":", preOp "?", preOp "[]", preOp "-", preOp "^", preOp "+"],
{-12-}   [op InfixL "*", op InfixL "/", op InfixL "&"],
{-11-}   [op InfixL "+", op InfixL "-"],
{-10-}   [op InfixR "->", op InfixR ".."],
{-9-}    [op InfixR "|", op InfixL ":"],
{-8-}    [op InfixR ">=", op InfixR "<=", op InfixR "<", op InfixR ">", op InfixL "<>", op InfixL "="],
{-7-}    [preOp "not"],
{-6-}    [op InfixR "and"],
{-5-}    [op InfixR "or"],
{-4-}    [op InfixR ":=", op InfixR ">>"
         ,op InfixN "+=", op InfixN "-=", op InfixN "*=", op InfixN "/=", op InfixN ".="
         ,InfixR defOp
         ],
{-3-}    [op InfixL "where"],  -- XXX precedence
{-2-}    [preOp ".."],
{-1-}    [op InfixR "=>"]
--  , [op InfixN ":-"]
  ]
  where
    preOp :: String -> Operator P SrcExpr
    preOp s = Prefix app
      where app = do
              l <- oper s
              pure $ \ x -> PrefixOp (Ident l s) x

    op :: ((SrcExpr -> P (SrcExpr -> SrcExpr -> SrcExpr)) -> Operator P SrcExpr) -> String -> Operator P SrcExpr
    op fx s = fx app
      where
        app (InfixOp _ (Ident _ ":") _) | s == "=" = fail ":e="
        app _ = do
          l <- oper s
          pure $ \ x y -> InfixOp x (Ident l s) y

    oper s | isAlpha (head s) = getSourcePos <* pKeyword s
           | otherwise = pOpL s

    pOpL s = getSourcePos <* pOp s

    defOp :: SrcExpr -> P (SrcExpr -> SrcExpr -> SrcExpr)
    defOp (InfixOp _ (Ident _ ":") _) = do
      l <- pOpL "="
      pure $ \ x y -> InfixOp x (Ident l ":=") y
    defOp _ = fail "defOp"

pExprT :: P SrcExpr  -- A tuple e1,e2,e3
pExprT = arrayS <$> sepBy1 pExpr2 (pOp ",")
  where
    arrayS :: [SrcExpr] -> SrcExpr
    arrayS [e] = e
    arrayS es = Tuple es

pFile :: P SrcExpr
--pFile = skip *> pExprSeq <* eof
pFile = skip *> p <* eof
  where
    p = do
      S.modify $ \ ls -> ls{ blkIndent = [""] }
      seqS <$> pIndBlock'

------

runP :: P a -> FilePath -> String -> Either (ParseErrorBundle String Void) a
runP pa fn s = S.evalState (runParserT pa fn s) initLexState

parseDie :: P a -> FilePath -> String -> a
parseDie p fn file =
  case runP p fn file of
    Left err -> error $ errorBundlePretty err
    Right x -> x

parseTry :: P a -> FilePath -> String -> Either String a
parseTry p fn file = either (Left . errorBundlePretty) Right $ runP p fn file

testp :: P a -> String -> a
testp p = parseDie p "<string>"

parseString :: String -> SrcExpr
parseString = parseDie pFile "<string>"
