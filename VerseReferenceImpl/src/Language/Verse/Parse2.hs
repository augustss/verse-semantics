{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Verse.Parse2( parse, parse2, toPos ) where

import Control.Comonad
import Control.Monad(when, void)
import Control.Monad.Identity(runIdentity)

import Data.ByteString qualified as ByteString
import Data.ByteString(ByteString)
import Data.ByteString.Internal(c2w, w2c)
import Data.Char qualified as Char
import Data.Char(isAlpha, isAlphaNum)
import Data.Functor.Apply
import Data.Text qualified as Text
import Data.Text(Text)
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)

import Language.Verse.Error qualified as E
import Language.Verse.Loc (L (..), Loc(..))
import Language.Verse.Name(Name)
import Language.Verse.Parse.Exp (Exp
                                , pattern (:=:)
                                , pattern (:<>:)
                                , pattern (:.:)
                                , pattern (:..:)
                                , pattern (:<:)
                                , pattern (:<=:)
                                , pattern (:>:)
                                , pattern (:>=:)
                                , pattern (:|:)
                                , pattern (:+:)
                                , pattern (:-:)
                                , pattern (:*:)
                                , pattern (:/:)
                                )
import Language.Verse.Parse.Exp qualified as Exp
import Language.Verse.Parse.Exp ( pattern (:->:)
                                )
import Language.Verse.Parse.Exp qualified as Pat
import Language.Verse.Parse.Exp (IdentExp)
import Language.Verse.Parse.Exp qualified as IdentExp
import Language.Verse.Pos qualified as Pos

import Numeric(readHex, readBin)
import Text.Parsec qualified as P
import Text.Parsec((<|>), (<?>), ParseError, tokenPrim, tokens)
import Text.Parsec.Error qualified as PE
import Text.Parsec.Pos qualified as PPos
import Text.Parsec.Prim qualified as PPrim

import Prelude hiding (exp)

-- Nice to have when debugging
{-
import Debug.Trace(trace)

traceChar :: String -> Parser ()
traceChar msg = do
  p <- pos
  c <- P.lookAhead pAny
  trace (show (PPos.sourceLine p) ++ ":" ++ show (PPos.sourceColumn p) ++ ": '" ++ show (w2c c) ++ "' " ++ msg) $ return ()
  where
  pAny :: Parser Word8
  pAny = tokenPrim
    (\c -> showW8s [c])
    (\pos w _ws -> updatePosWord pos w)
    (\w -> Just w)
-}

-- The predefined interface for Parsec on ByteString converts all Word8 to Char one at a time.
-- That doesn't work well with UTF-8

newtype Word8String = WS ByteString

instance (Monad m) => PPrim.Stream Word8String m Word8 where
  uncons (WS byteString) =
    case ByteString.uncons byteString of
      Nothing -> return Nothing
      Just (word8, byteString) -> return $ Just (word8, WS byteString)

type Parser = P.Parsec Word8String ParserState

parse :: String -> ByteString -> Either ParseError (L (Exp Name))
parse path content = runIdentity $ P.runParserT pFile beginPS path (WS content)


-- Wrapper to make it an almost drop in replacement for the old parser
-- The difference is that it needs the file name for error messages
parse2 :: String -> ByteString -> Either E.Error (L (Exp Name))
parse2 path bytestring =
  case parse path bytestring of
    Left err -> Left $ E.OtherError (toPos $ PE.errorPos err) (showWithoutPos err)
    Right x -> Right x
  where
    -- Copied from Parsec.Error, but without position since it's reported separately
    showWithoutPos err =
      PE.showErrorMessages "or" "unknown parse error"  "expecting" "unexpected" "end of input" (PE.errorMessages err)



liftL2 :: (Apply f, Comonad f) => (f a -> f b -> c) -> f a -> f b -> f c
liftL2 f a b = f <$> duplicate a <.> duplicate b

---------------------------- Tim Grammar

-- Alpha     := 'A'..'Z' | 'a'..'z' | '_'
pAlpha :: Parser Word8
pAlpha = satisfy (( \ c -> isAlpha c || c == '_') . w2c)

-- Alnum     := 'A'..'Z' | 'a'..'z' | '_' | '0'..'9'
pAlnum :: Parser Word8
pAlnum = satisfy (( \ c -> isAlphaNum c || c == '_') . w2c)

-- Hex       := 'A'..'F' | 'a'..'f' |       '0'..'9'
pHex :: Parser Word8
pHex =  satisfy (Char.isHexDigit . w2c)

pBin :: Parser Word8
pBin =  satisfy ( \ c -> c == c2w '0' || c == c2w '1')

-- Digits    := '0'..'9' {:'0'..'9':}
pDigits :: Parser [Word8]
pDigits =  P.many1 pDigit

pDigit :: Parser Word8
pDigit =  satisfy (Char.isDigit . w2c)

-- U8        := 0o80..0oBF
pU8 :: Parser Word8
pU8 = bounds 0x80 0xbf

bounds :: Word8 -> Word8 -> Parser Word8
bounds lo hi = satisfy ( \ w -> lo <= w && w <= hi)

-- UTF8      :=                                      0o00..0o7F
--           |                                       0oC2..0oDF U8
--           |  !(0oE0 0o80..0o9F | 0oED 0oA0..0oBF) 0oE0..0oEF U8 U8
--           |  !(0oF0 0o80..0o8F | 0oF4 0o90..0oBF) 0oF0..0oF4 U8 U8 U8
pUTF8 :: Parser [Word8]
pUTF8 =
  (:[]) <$> bounds 0x00 0x7f
  <|>
  (\ a b -> [a,b]) <$> bounds 0xc2 0xdf <*> pU8
  <|>
  (\ a b c -> [a,b,c]) <$> bounds 0xe0 0xef <*> pU8 <*> pU8
  <|>
  (\ a b c d -> [a,b,c,d]) <$> bounds 0xf0 0xf4 <*> pU8 <*> pU8 <*> pU8


-- -- NO UTF32     := !(0uD800..0uDFFF) 0u0..0u10FFFF

-- Printable := 0o09 | !("<#" | "#>" | 0o0..0o1F | 0o7F | 0oC2 0o80..0o9F | 0oE2 0o80 0oA8..0oA9 ) UTF8
pPrintable :: Parser [Word8]
pPrintable = ((:[]) <$> byte 0x09) <|> P.notFollowedBy (void pLessHash <|> void pHashGreater <|> void (bounds 0x00 0x1f) <|> void (byte 0x7f)) *> pUTF8

-- -- NO UTF32          |  0u09 | !("<#" | "#>" | 0u0..0u1F | 0o7F..0o9F             | 0u2028 | 0u2029      ) UTF32

-- Space     := {0o09 | 0o20 | Comment}
pSpace :: Parser (L String)
pSpace = (\ p1 p2 -> L (toLoc p1 p2) "<space>") <$> pos <* P.many (match '\t'  <|> match ' ' <|> pComment) <*> pos

-- NewLine   := 0o0D [0o0A] | 0o0A
pNewline :: Parser (L String)
pNewline =
  match '\n'
  <|>
  match '\r' <* P.option () (void $ match '\n')
--  )
--  <?>
--  "end of line"

-- Ending    := &(NewLine | end)
pEnding :: Parser (L String)
pEnding = wrapLoc <$> pos <*> ("<end of line/file>" <$ P.lookAhead (void pNewline <|> P.eof)) <*> pos

-- Ind       := Ending Line push; set Nest=false; set BlockInd=LineInd; set LinePrefix=""
pInd :: Parser ()
pInd = do
  push
  bInd <- getBlockInd    -- current indentation level
  _ <- pEnding
  setNest False
  qOk <- P.optionMaybe pLine
  lInd <- getLineInd
  let ind =  case qOk of
           Nothing -> bInd        -- next line is indented less or same as this one
           Just _ -> lInd
  setBlockInd ind
  setLinePrefix ByteString.empty

-- Ded       := Ending pop
pDed :: Parser ()
pDed = do
  _ <- pEnding
  pop

-- Line      := NewLine; parse i:={0o09|0o20}; (Ending | !(0o09|0o20) Space
--              if     (i>BlockInd | Nest and i=BlockInd) then set LineInd=ThisInd
--              else if(not i<=BlockInd                 ) then error)
-- The above does not agree with ShipVerse, since empty lines will terminate indentation.
pLine :: Parser ()
pLine = P.try $ do -- This P.try is needed since it will fail if the next line is not indented enough
  _ <- pNewline
  prefix <- P.many ( char ' ' <|> char '\t')
  emptyLine <- P.option False ( True <$ P.lookAhead pNewline)
  if emptyLine then
    pLine
  else do
    let i = ByteString.pack prefix
    nest <- getNest
    ind <- getBlockInd
    _ <- (pEnding <|> pSpace)
    if deeper i ind {- || nest && ind == i-} then do
      setNest True
      setLineInd i
    else if deeper ind i then P.parserZero
    else do
      if nest && i == ind then setLineInd i
      else P.parserZero


deeper :: ByteString -> ByteString -> Bool
deeper lhs rhs = ByteString.isPrefixOf rhs lhs && rhs /= lhs

-- Scan      := Space {Line}
pScan :: Parser ()
pScan = pSpace *> pScanNS

pScanNS :: Parser ()
pScanNS = void $ P.many pLine

-- ScanKey   := Space (&NewLine Scan LinePrefix Space | !NewLine)
pScanKey :: Parser ()
pScanKey = pSpace *> pScanKeyNS

pScanKeyNS :: Parser ()
pScanKeyNS = P.lookAhead pNewline *> pScan *> pLinePrefix *> void pSpace
             <|>
             P.notFollowedBy pNewline

-- LineCmt   :=  "#" !'>' {Text        } Ending
pLineCmt :: Parser ()
pLineCmt = () <$ pHash <* P.manyTill pText (P.lookAhead pEnding)

-- BlockCmt  := "<#" !'>' {Text|NewLine} !'<' "#>"
pBlockCmt :: Parser ()
pBlockCmt = () <$ pLessHash <* P.many (pText <|> void pNewline) <* pHashGreater

-- IndCmt    := "<#>"     {Text        } Ind {Text|Line} Ded
pIndCmt :: Parser ()
pIndCmt = () <$ pLessHashGreater <* P.many pText <* pInd <* P.many (pLine <|> pText) <* pDed

-- Comment   := LineCmt   | BlockCmt | IndCmt
pComment :: Parser (L String)
pComment = wrapLoc <$> pos <*> ("<comment>" <$ (pLineCmt <|> pBlockCmt <|> pIndCmt)) <*> pos

-- Text      := Printable | BlockCmt | "<#>"
pText :: Parser ()
pText = ((void $ string "<#>") <|> void pPrintable <|> void pBlockCmt)

-- Exp       := ['e' ['+'|'-'] Digits] !('e' ('+'|'-'|Digit))
pExp :: Parser (Char, [Word8])
pExp = match 'e' *> ( match '-' *> (('-',) <$> pDigits)
                      <|>
                      P.option () (void $ match '+') *> (('+',) <$> pDigits) )

-- Units     := [Alpha {Alpha}] !Alpha
-- Above does not agree with Shipverse
pUnits :: Parser (L Name)
pUnits = do
  p1 <- pos
  w <- pAlpha
  ws <- P.many pAlnum
  p2 <- pos
  case Text.decodeUtf8' $ ByteString.pack (w:ws) of
    Left err -> fail $ "parsing units failed due to " ++ show err
    Right txt -> return $ L (toLoc p1 p2) txt


-- Num       := ("0x" Hex {Hex} | !(("0b"|"0o"|"0u"|"0x") Hex) Digits ['.' Digits] Exp Units) !'.' !Alnum
-- This is a brutal hack for now, using Haskell's read function for conversion of floats
-- pNumI also handles Char8 and Char32 since they starts with a digit.
pNumT :: Parser (L (Exp Name))
pNumT =
  (pos <* match '0' >>= \ p1 ->
                          (fixHexChar p1 <$ match 'u' <*> fromTo 1 6 pHex <*> pos <* P.notFollowedBy pAlnum -- HACK accept all 6 digits hex nuumbers, not only 0-10ffff
                           <?>
                            ("a hex number in the range 0-10ffff after '0u' for character literal at " ++ toStr p1))
                          <|>
                          (fixHexChar p1 <$ match 'o' <*> fromTo 1 2 pHex <*> pos <* P.notFollowedBy pAlnum
                            <?>
                           ("a hex number in the range 0-ff after '0o' for character literal at " ++ toStr p1))
                         <|>
                          ( mkHex p1 <$ match 'x' <*> P.many1 pHex <*> pos <* P.notFollowedBy pAlnum
                            <?>
                            ("one or more hex digits after '0x' for a hexadecimal literal at " ++ toStr p1))
                          <|>
                          (mkBin p1 <$ match 'b' <*> P.many1 pBin <*> pos <* P.notFollowedBy pAlnum
                            <?>
                            ("one or more binary digits after '0b' for a binary literal at " ++ toStr p1))
                          <|>
                          -- Need a P.try with pDot since this can be an int followed by an extension field
                          ( mkNum p1 <$> ((c2w '0':) <$> P.many pDigit) <*> P.optionMaybe (P.try (pDot *> pDigits)) <*> P.optionMaybe pExp <*> P.optionMaybe pUnits <*> pos)
  )
  <|>
  (mkNum <$> pos <*> pDigits <*> P.optionMaybe (P.try (pDot *> pDigits)) <*> P.optionMaybe pExp <*> P.optionMaybe pUnits <*> pos)
 where
   mkHex p1 xs p2 = L (toLoc p1 p2)  $ Exp.Int . fst . head . readHex . map w2c $ xs
   mkBin p1 xs p2 = L (toLoc p1 p2)  $ Exp.Int . fst . head . readBin . map w2c $ xs

   mkNum :: PPos.SourcePos -> [Word8] -> Maybe [Word8] -> Maybe (Char, [Word8]) -> Maybe (L Name) -> PPos.SourcePos -> L (Exp Name)
   mkNum p1 is Nothing   Nothing units p2 =  addUnits units $ (Exp.Int $ wsToInteger is) <$ unitLoc p1 p2
   mkNum p1 is Nothing   (Just ('+', es)) units p2 =  addUnits units $ (Exp.Int $ wsToInteger is * (10 ^ wsToInteger es)) <$ unitLoc p1 p2
   mkNum p1 is fs es units p2 = addUnits units $ (mkFloat is (qFractionToWords fs) (qExpToWords es) <$ unitLoc p1 p2)

   wsToInteger :: [Word8] -> Integer
   wsToInteger ws = read $ '0': map w2c ws

   qFractionToWords :: Maybe [Word8]-> [Word8]
   qFractionToWords Nothing = [c2w '0']
   qFractionToWords (Just es) = es

   qExpToWords :: Maybe (Char, [Word8])-> [Word8]
   qExpToWords Nothing = []
   qExpToWords (Just (sgn, es)) = map c2w ['e', sgn] ++ es

   fixHexChar :: PPos.SourcePos -> [Word8] -> PPos.SourcePos -> L (Exp Name)
   fixHexChar p1 ws p2 =
     L (toLoc p1 p2) $ Exp.Char $ Char.chr $ fst $ head $ readHex $ map w2c ws

   addUnits Nothing e = e
   addUnits (Just u) e = Exp.Units e <$> duplicate u

   mkFloat :: [Word8] -> [Word8] -> [Word8] -> (Exp Name)
   mkFloat is fs es =
     let s = map w2c (is ++ c2w '.':fs ++ es)
     in case reads s of
       [(f,[])] -> Exp.Float f
       _ -> error ("Failed to parse floating point number " ++ s)


pNum :: Parser (L (Exp Name))
pNum = pNumT <* pSpace

-- Special   := '\'|'{'|'}'|'#'|'<'|'>'|'&'|'~'
special :: [Word8]
special = map c2w ['\'', '"', '\\', '{', '}', '#', '<', '>', '&', '~']

-- CharEsc   := '\' ('r'|'n'|'t'|'''|'"'|Special)
pCharEsc :: Parser Char
pCharEsc = fixChar <$ match '\\' <*> satisfy (`elem` charEsc)
  where
    fixChar c =
      case w2c c of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        x -> x

    charEsc = map c2w ['r', 'n', 't'] ++ special

-- CharLit   := ''' Printable ''' !''' | ''' CharEsc '''
pCharLit :: Parser (L Char)
pCharLit = do
  p1 <- pos
  c <- match '\'' *> (pCharEsc <|> pPrintable') <* match '\''
  p2 <- pos
  return $ L (toLoc p1 p2) c

-- Utility function to get the correct return type
pPrintable' :: Parser Char
pPrintable' = pPrintable >>= fix
  where
    fix :: [Word8] -> Parser Char
    fix ws =
      case Text.decodeUtf8' $ ByteString.pack ws of
        Left err -> fail $ "illegal char " ++ show ws ++ ":" ++ show err
        Right txt ->
          case Text.uncons txt of
            Nothing -> fail $ "utf8 decoder returned empty text for:" ++ show ws
            Just (c,ws) ->
              if ws == Text.empty then
                return c
              else
                fail $ "utf8 decoder returned more than one character for" ++ show ws

-- Char8     := "0o" (       Hex) [Hex]                    !Alnum
-- Embedded in pNum since both starts with a digit

-- Char32    := "0u" ("10" | Hex) [Hex] [Hex] [Hex] [Hex]) !Alnum
-- Embedded in pNum since both starts with a digit

-- Char      := CharLit | Char8 | Char32
pChar :: Parser (L (Exp Name))
pChar = (Exp.Char <$>) <$> pCharLit   -- <|> pChar8 <|> pChar32) included in pNum instead

-- Ident     := Alpha {Alnum} !Alnum ["'" {!('<#'|'#>'|'\'|'{'|'}'|'"'|''') 0o20-0o7E} "'"]
pIdentT :: Parser (L Name)
pIdentT = P.try $ do  -- This P.try is needed for now since it will fail for keywords
  p1 <- pos
  x <- pAlpha
  xs <- P.many pAlnum
  extra <- pSuffix
  p2 <- pos
  case Text.decodeUtf8' (ByteString.pack (x:xs ++ extra)) of
    Left err -> fail $ "identifier with illegal utf8 character " ++ show err
    Right n -> do
      when (n `elem` reserved) P.parserZero
      return $ L (toLoc p1 p2) n
  where
  pSuffix :: Parser [Word8]
  pSuffix = (fix <$ match '\'' <*> P.manyTill ok (match '\''))
                     <|>
                     return []

  special :: Text.Text
  special = "\\{}\"'"

  ok = P.notFollowedBy ( string "<#" <|> string "#>" ) *> satisfy ( \ w8 -> (w8 >= 0x20 && w8 <= 0x7E && not (Text.elem (w2c w8) special)))

  fix w8s = c2w '\'' : w8s ++ [c2w '\'']


pIdent :: Parser (L Name)
pIdent = pIdentT <* pSpace

-- Path      := '/' Label ('@' Label | !'@')] {'/' ['(' Path ":)"] Ident} !'/'
-- Paths are currently represented with a Name, could be changed to something with structure
pPathT :: Parser Name
pPathT = do
  _ <- match '/'
  label <- pLabel
  atLabel <- P.optionMaybe pAtLabel
  idents <- P.many $ (,) <$ pSlash <*> P.optionMaybe (pLParen *> pPathT <* pColonParen) <*> pIdentT
  return $ Text.pack "/" <> label <> fixAt atLabel <> (Text.concat $ map fixIdent idents)
 where
   pAtLabel :: Parser Name
   pAtLabel = pAt *> pLabel

   fixAt :: Maybe Name -> Name
   fixAt Nothing = Text.empty
   fixAt (Just txt) = Text.pack "@" <> txt

   fixIdent :: (Maybe Name, L Name) -> Name
   fixIdent (Nothing, extract -> name) = Text.pack "/" <> name
   fixIdent (Just path, extract -> name) = Text.pack "/(" <> path <> Text.pack ":)" <> name

pPath :: Parser (L (IdentExp Name))
pPath = wrapLoc <$> pos <*> (IdentExp.IdentPath <$> pPathT) <*> pos <* pSpace

-- Label     := Alnum {Alnum|'-'|'.'} !(Alnum|'-'|'.')
pLabel :: Parser Name
pLabel = do
  x <- pAlnum
  xs <- P.many $ satisfy (isLabel . w2c)
  case Text.decodeUtf8' $ ByteString.pack $ x:xs of
    Left err -> fail $ "Label with illegal utf8 character " ++ show err
    Right txt -> return txt
 where
  isLabel '-' = True
  isLabel '.' = True
  isLabel x = Char.isAlphaNum x

-- Interp    := '{' List '}'
pInterp :: Parser (L (Exp Name))
pInterp = mkListPos <$> match '{' <*> pList <*> match '}'

-- Ampersand := push; parse LinePrefix='&'; Space Def (';'|Ending); pop

-- String    := '"' {Interp | CharEsc |                                       !('\'|'{'|'}'|'"') Text} '"'
pString :: Parser (L (Exp Name))
pString = do
  p1 <- pos
  s1 <- match '"' *> pStringText
  xs <- pStringRest <* match '"'
  p2 <- pos
  return $ L (toLoc p1 p2) $ Exp.String (extract s1) xs

pStringText :: Parser (L Text)
pStringText = ( \ p1 xs p2 -> L (toLoc p1 p2) (Text.pack xs)) <$> pos <*> P.many (pCharEsc <|> pStringChar) <*> pos

pStringRest  :: Parser [(L (Exp Name), L Text)]
pStringRest =
  ( \ e s xs -> (e,s):xs) <$> pInterp <*> pStringText <*> pStringRest
  <|>
  return []

pStringChar :: Parser Char
pStringChar = w2c <$> satisfy (not . (`Text.elem` "\\{}\"") . w2c)


-- Content   :=     {Interp | CharEsc | Markup | Ampersand | Comment | Line | !Special           Text}
-- Contents  := Content | '~' Content {'~' Content}
-- Markup    := '<' Scan Tags Scan ":>" Space Ind Contents Ded
--           |  '<' Scan Tags Scan ';'  Scan      Contents '>'
--           |  '<' Scan Tags Scan '>'  Scan      Contents '</' Ident Space {'/' Ident Space} '>'

-- Key       := !Alnum Space !":="

-- Return    := ("return"|"yield"|"break"|"continue") Key
-- Not exactly as the grammar, this parser also consumes any following Block/Def in the case of return
-- I guess break also could take an argument, an maybe yield, but continue?
pReturn :: Parser (L (Exp Name))
pReturn =
  (\ p1 qE p2 -> L (toLoc p1 p2) (Exp.Return qE)) <$> pos <* pKeyword "return" <* pSpace <*> P.optionMaybe (pBlock <|> pSpace *> pDef) <*> pos <* pStopDef
  <|>
  (\ p1 p2 -> L (toLoc p1 p2) Exp.Yield) <$> pos <* pKeyword "yield" <*> pos <* pSpace <* pStopDef
  <|>
  (\ p1 p2 -> L (toLoc p1 p2) Exp.Break) <$> pos <* pKeyword "break" <*> pos <* pSpace <* pStopDef
  <|>
  (\ p1 p2 -> L (toLoc p1 p2) Exp.Continue) <$> pos <* pKeyword "continue" <*> pos <* pSpace <* pStopDef


-- Reserved  := ("catch"|"do"|"else"|"if"|"in"|"is"|"not"|"then"|"until"|"where"|"with"
--              |"alias"|"const"|"live"|"mutable"|"ref"|"set"|"var") Key | Return
reserved :: [Text.Text]
reserved = ["catch", "do", "else", "if", "in", "is", "not", "then", "until", "where", "with", "alias", "const", "live", "mutable", "ref", "set", "var", "return", "yield", "break", "continue"
           , "enum", "not", "block", "all", "one", "forall", "true", "false", "fail"  -- TODO added these to reserved words for refimpl
           , "next", "over", "while", "when" -- TODO shouldn't these also be reserved?
           ]

-- DotSpace  := '.' &(0o09 | 0o20 | Ending)
-- The lookahead in combination with Ending is surprising here. The newline will not be consumed by Space and hence the use case for DotSpace will fail
pDotSpace :: Parser (L String)
pDotSpace = (<.) <$> match '.' <*> P.lookAhead (match '\t' <|> match ' ' <|> pEnding)

-- Brace     := Scan '{' List '}' Space
pBrace :: Parser (L (Exp Name))
pBrace = P.try $ mkListPos <$ pScan <*> pLBrace <*> pList <*> pRBrace <* pSpace

-- Block     := Brace | DotSpace Space Def Space | ':' Space Ind List Ded
pBlock :: Parser (L (Exp Name))
pBlock = pBrace
         <|>
         pDotSpace *> pSpace *> pDef <* pSpace
         <|>
         P.try (mkListPos <$> pColon <* pSpace <* pInd <*> pList <* pDed <*> pPos)  -- After "of" there can be either a "colon pInd ... pDed" or a prefix ":"

-- BraceInd  := Brace | Ind List Ded
pBraceInd :: Parser (L (Exp Name))
pBraceInd =
  pBrace <|> (mkList <$ pInd <*> pList <* pDed)

-- KeyBlock  := Block
pKeyBlock :: Parser (L (Exp Name))
pKeyBlock = pBlock


-- A NameBlock is a block with only identifiers and attributes. It's used for enum.
pNameBlock :: Parser (L [([L (Exp Name)], L Name)])
pNameBlock =
  pLBrace *> pNameList <* pRBrace <* pSpace
  <|>
  pColon *> pSpace *> pInd *> pNameList <* pDed

-- Defs      := Def {Space ',' Scan Def}
pDefs :: Parser (L (Exp Name))
pDefs = P.try $ do
  d <- pDef
  ds <- tryDefs d
  return $ mkList ds
 where
  tryDefs :: L (Exp Name) -> Parser (L [L (Exp Name)])
  tryDefs e = do
    qD <- P.optionMaybe (P.try (pSpace *> pComma *> pScan *> pDef))
    ds <- case qD of
          Nothing -> return $ [] <$ e
          Just d' -> tryDefs d'
    return $ (:) <$> duplicate e <.> ds


-- Paren     :=  '(' List  ')' Space
pParen :: Parser (L (Exp Name))
pParen = mkListPos  <$> pLParen <*> pList <*> pRParen <* pSpace

-- QualIdent := ['(' List ":)" Space] Ident
pQualIdent :: Parser (L (IdentExp Name))
pQualIdent =
  fixQual <$> P.optionMaybe ( char '(' *> pSpace *> pList <* pColonParen) <* pSpace <*> pIdent
  where
  fixQual qQual n =
    case qQual of
      Nothing -> IdentExp.IdentName <$> n
      Just qs -> IdentExp.IdentQualName <$> qs <.> (duplicate n)


-- Use when it can be either a pParen or a pQualIdent. Needed to get rid of a P.try that is otherwise needed.
pParenOrQualIdent :: Parser (L (Exp Name))
pParenOrQualIdent = do
  p1 <- pos
  qParens <- P.optionMaybe ((,) <$ char '(' <* pSpace <*> pList <*> (True <$ char ')' <|> False <$ pColonParen))
  p2 <- pos
  _ <- pSpace
  case qParens of
    Nothing -> ((Exp.Pat <$>) . (Pat.Name <$>) . (IdentExp.IdentName <$>)) <$> pIdent
    Just (es, True) -> return $ wrapLoc p1 (extract $ mkList es) p2
    Just (es, False) -> do
      n <- pIdent
      _ <- pSpace
      return $ (Exp.Pat <$>) $ (Pat.Name <$>) $ wrapLoc p1 (extract $ IdentExp.IdentQualName <$> es <.> duplicate n) p2

-- Specs     := [ScanKey "with" Key] '<' Scan Choose Space '>' Space (Specs | !Specs)
pSpecs :: Parser [L (Exp Name)]
pSpecs =  P.many (pSpec <* pSpace) -- Do not use "P.sepBy pSpec pSpace", since the latter always succeeds

pSpecs1 :: Parser [L (Exp Name)]
pSpecs1 = P.many1 (pSpec <* pSpace)

-- Parsing <attribute> in most cases needs backtracking
pSpec :: Parser (L (Exp Name))
pSpec = P.try $ P.optionMaybe (pKeyword "with" *> pSpace) *> match '<' *> pChoose <* match '>'

addSpecs :: [L (Exp Name)] -> L (Exp Name) -> L (Exp Name)
addSpecs [] base = base
addSpecs sp base = Exp.ExpSpecs <$> duplicate base <.> (sp <$ last sp)

-- Tags      := Space (!'/' Call ScanKey '.' | !Reserved) QualIdent Space {Invoke} [',' Scan Tags]

-- Do        := ScanKey "do"    Key (KeyBlock | Def)
pDo :: L (Exp Name) -> Parser (L (Exp Name))
pDo e1 = liftL2 Exp.Do e1 <$ pScanKey <* pKeyword "do" <* pSpace <*> (pKeyBlock <|> pDef)

-- Until     := ScanKey "until" Key (KeyBlock | Def) | ScanKey "catch" Key Invoke
pUntil :: L (Exp Name) -> Parser (L (Exp Name))
pUntil e1 = P.try (liftL2 Exp.Until e1 <$ pScanKey <* pKeyword "until" <* pSpace <*> (pKeyBlock <|> pDef))
            <|>
            P.try (do
               p <- pScanKey *> pKeyword "catch" *> pSpace *> pParen
               e2 <- pInvoke p
               return $ liftL2 Exp.Catch e1 e2)

-- Then      := ScanKey "then"  Key (KeyBlock | Def)
pThen :: Parser (L (Exp Name))
pThen = P.try $ pScanKey *> pKeyword "then" *> pScanKey *> (pKeyBlock <|> pDef)

-- Else      := ScanKey "else"  Key (ScanKey If | !(ScanKey If) (KeyBlock | Def))
pElse :: Parser (L (Exp Name))
pElse = P.try $ pScanKey *> pKeyword "else" *> pScanKey *> (P.try (pScanKey *> pIf) <|> pKeyBlock <|> pDef)

-- Invoke    :=          [Specs] (Paren [Specs] (Block | Do  ) | Block [[Specs] Do  ]) (Until | !Until)
pInvoke :: L (Exp Name) -> Parser (L (Exp Name))
pInvoke base =
  P.try (( \ s1 p s2 b -> Exp.Inst <$> duplicate (addSpecs s2 $ Exp.ParenInvoke <$> duplicate (addSpecs s1 base) <.> duplicate p) <.> duplicate b) <$> pSpecs <*> pParen <*> pSpecs <*> pBlock)
  <|>
  P.try (( \ s1 e -> Exp.Inst <$> duplicate (addSpecs s1 base) <.> duplicate e) <$> pSpecs <*> pBlock)
  <|>
  P.try (pDo base)
  <|>
  P.try (pUntil base)


-- If        := "if" Key [Specs] (Paren         (Block | Then) | Block [        Then]) (Else  | !Else )
pIf :: Parser (L (Exp Name))
pIf = P.try $ pKeyword "if" *> pSpace *> (fixIf <$> pParen <*> P.optionMaybe (P.try (pBlock <|> pThen)) <*> P.optionMaybe pElse
                                                             <|>
                                                             fixIf <$> pBlock <*> P.optionMaybe pThen <*> P.optionMaybe pElse)
   where
     fixIf c Nothing  Nothing  = Exp.If <$> duplicate c
     fixIf c (Just t) Nothing  = Exp.IfThen <$> duplicate c <.> duplicate t
     fixIf c Nothing  (Just e) = Exp.IfElse  <$> duplicate c <.> duplicate e
     fixIf c (Just t) (Just e) = Exp.IfThenElse <$> duplicate c <.> duplicate t <.> duplicate e


-- In        := ("in" Key | ':') Space (In | &Choose NotEq [Space Specs])
-- Is 'in' other syntax for ':'?
pIn :: Maybe (L (Exp Name)) -> Parser (L (Exp Name))
pIn qE1 = fixIn <$> (pColon <|> pKeyword "in") <* pSpace <*> (pIn Nothing <|> pNotEq) <* pSpace <*> pSpecs
  where
  fixIn p1 e specs = addSpecs specs $
    case qE1 of
      Nothing -> Exp.Pat <$> (Exp.PrefixColon <$ p1 <.> duplicate e)
      (Just e1) -> Exp.ExpInfixColon <$ p1 <.> duplicate e1 <.> duplicate e


-- Var       := ("var"|"set"|"ref"|"alias") Key Space Choose
pVar :: Parser (L (Exp Name))
pVar = (((\ specs e -> addSpecs specs $ Exp.ExpVar <$> duplicate e) <$ pKeyword "var")
        <|>
        ((\ specs e -> addSpecs specs $ Exp.ExpSet <$> duplicate e) <$ pKeyword "set")
        <|>
        ((\ specs e -> addSpecs specs $ Exp.ExpRef <$> duplicate e) <$ pKeyword "ref")
        <|>
        ((\ specs e -> addSpecs specs $ Exp.ExpAlias <$> duplicate e) <$ pKeyword "alias"))
       <*> pSpecs <* pSpace <*> pChoose

-- Base      := '(' List ')' | Num | Char | Path | String | Markup | If | !Reserved QualIdent
-- TODO Markup
pBase :: Parser (L (Exp Name))
pBase =
  (\ n -> Exp.Exists <$> duplicate n) <$ pKeyword "exists" <* pSpace <*> pIdent  -- TODO Here for refimpl
  <|>
  (\ n -> Exp.All <$> duplicate n) <$ pKeyword "all" <* pSpace <*> pBlock -- TODO Here for refimpl
  <|>
  (\ n -> Exp.One <$> duplicate n) <$ pKeyword "one" <* pSpace <*> pBlock -- TODO Here for refimpl
  <|>
  (\ n -> Exp.Block <$> duplicate n) <$ pKeyword "block" <* pSpace <*> pBlock -- TODO Here for refimpl
  <|>
  (Exp.True <$) <$> pKeyword "true"  -- TODO: Is "true" a reserved words?
  <|>
  (Exp.False <$) <$> pKeyword "false" -- TODO: Is "false" a reserved words?
  <|>
  (Exp.Fail <$) <$> pKeyword "fail" -- TODO: Is "fail" a reserved words?
  <|>
  pChar
  <|>
  pNum
  <|>
  pEnum
  <|>
  ((Exp.Pat <$>) . (Pat.Name<$>)) <$> pPath
  <|>
  pString
  <|>
  pIf
  <|>
  pParenOrQualIdent <* pSpace

-- Call      := Base    {Space Postfix}
pCall :: Parser (L (Exp Name))
pCall = do
  call <- pBase
  pPostfix call

-- Postfix   := Invoke  | !Invoke (Paren | Specs) | ("at"|"of") Key (KeyBlock | Fun)
--                      | ('^' | '?' | '[' List ']' | ScanKey '.' QualIdent)
pPostfix :: (L (Exp Name)) -> Parser (L (Exp Name))
pPostfix base = pSpace *> (
  repeatChoice base [ pInvoke
                    , \ a -> liftL2 Exp.ParenInvoke a <$> pParen
                    , \ a -> (\ b -> addSpecs b a) <$> pSpecs1
                    , \ a -> liftL2 Exp.BracketInvoke a <$ pKeyword "of" <* pSpace <*> (pKeyBlock <|> pFun)
                    , \ a -> liftL2 Exp.ParenInvoke a  <$ pKeyword "at" <* pSpace <*> (pKeyBlock <|> pFun)
                    , \ a -> (Exp.PostfixCaret <$> duplicate a) <$ match '^'
                    , \ a -> (Exp.PostfixQuery <$> duplicate a) <$ match '?'
                    , \ a -> (\ p1 b p2 -> Exp.BracketInvoke <$> duplicate a <.> duplicate (mkList b) <. p1 <. p2) <$> pLBracket <*> pList <*> pRBracket
                    , \ a -> (liftL2 (:.:) a <$ pScanKey <* pDot <*> pQualIdent)
                    ])

-- Prefix    := Call    | ('^' | '?' | '[' List ']' | '+' | '-' | '*') Space (Brace | Prefix)
pPrefix :: Parser (L (Exp Name))
pPrefix =
  (\ e -> Exp.PrefixCaret <$> duplicate e) <$ match '^' <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  (\ e -> Exp.PrefixQuery <$> duplicate e) <$ match '?' <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  ( \ e1 e2 -> Exp.PrefixBracket <$> e1 <.> duplicate e2) <$ pLBracket <*> pList <* pRBracket <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  (\ e -> Exp.PrefixPlus <$> duplicate e) <$ pPlus <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  (\ e -> Exp.PrefixMinus <$> duplicate e) <$ pMinus <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  (\ e -> Exp.PrefixMultiply <$> duplicate e) <$ pMultiply <* pSpace <*> (pBrace <|> pPrefix)
  <|>
  pCall

-- Mul       := Prefix  { Space ('*' | '/' | '&'       ) Scan  Prefix  }
pMul :: Parser (L (Exp Name))
pMul =
  doBinary pPrefix pPrefix [(pMultiply, (:*:)), (pDivide, (:/:))]

-- Add       := Mul     { Space ('+' | '-'             ) Scan  Mul     }
pAdd :: Parser (L (Exp Name))
pAdd =
  doBinary pMul pMul [(pPlus, (:+:)), (pMinus, (:-:))]

-- To        := Add     [ Space ("to" Key | ".." | "->") Scan  To      ]
pTo :: Parser (L (Exp Name))
pTo =
  doBinary pAdd pTo [ (pKeyword "to", (:..:))
                    , (string "..", (:..:))
                    , (string "->", (:->:))
                    ]

-- Choose    := To      [ Space ('|'                   ) Scan  Choose  ]
pChoose :: Parser (L (Exp Name))
pChoose =
  doBinary pTo pChoose [(match '|', (:|:))]

-- Greater   := Choose  [ Space ('>'  | ">="           ) Scan  Greater ]
pGreater :: Parser (L (Exp Name))
pGreater =
  doBinary pChoose pGreater [ (pGreaterEqual, (:>=:))
                            , (pGreaterThan, (:>:))
                            ]

-- Less      := Greater [ Space ('<'  | "<="           ) Scan  &(Choose Space !'>' !'>=') Less]
pLess :: Parser (L (Exp Name))
pLess = -- This works since Specs are collected in pPostfix if there are any
  doBinary pGreater pLess  [ (pLessEqual, (:<=:))
                           , (pLessThan, (:<:))
                           ]

-- NotEq     := Less    { Space ("<>"                  ) Scan  Choose  }
pNotEq :: Parser (L (Exp Name))
pNotEq =
  doBinary pLess pChoose [(string "<>", (:<>:))]

-- Eq        := NotEq   { Space ('='                   ) Scan  NotEq   }
pEq :: Parser (L (Exp Name))
pEq =
  doBinary pNotEq pNotEq [(pEqual, (:=:))]

-- Not       := Eq      |       ("not" Key             ) Space Not
pNot :: Parser (L (Exp Name))
pNot =
  (\ e -> Exp.Not <$> duplicate e) <$ pKeyword "not" <* pSpace <*> pNot
  <|>
  pEq

-- And       := Not     { Space ("and" Key             ) Scan  And     }
pAnd :: Parser (L (Exp Name))
pAnd =
  doBinary pNot pAnd [(pKeyword "and", Exp.And)]

-- Or        := And     { Space ("or"  Key             ) Scan  Or      }
pOr :: Parser (L (Exp Name))
pOr =
  doBinary pAnd pOr [(pKeyword "or", Exp.Or)]


-- Def       := (Or | (In | Var) Space (('='|":="|"+="|"*="|"/=") Space (BraceInd | Def) | !'=' !':='))
--              { &In Def
--              | Space   ":="    Space (BraceInd | Def )
--              | Space   "where" Key   (KeyBlock | Defs)
--              | ScanKey "is"    Key   (KeyBlock | Def ) } StopDef
--           |  ('&'|"..") Space Def | Return [Block | Def] StopDef


pDef :: Parser (L (Exp Name))
pDef = pDef' Nothing

pDef' :: Maybe (L (Exp Name)) -> Parser (L (Exp Name))
pDef' qE =
  (((maybeInfix <$> (pIn qE <|> pVar) <* pSpace
     <*> P.optionMaybe ( ( \ op e2 -> (op, e2)) <$>
                         (
                           (:=:) <$ pEqual
                           <|>
                           Exp.InfixColonEqual <$ string ":="
                           <|>
                           Exp.SetInfixPlusEqual <$ string "+="
                           <|>
                           Exp.SetInfixMinusEqual <$ string "-="
                           <|>
                           Exp.SetInfixDivideEqual <$ string "/="
                           <|>
                           Exp.SetInfixMultiplyEqual <$ string "*="
                         ) <* pSpace <*> (pBraceInd <|> pDef)
                       )
   )
    <|>
    pOr) -- No need for P.try, if we are here then it must match
  <* pSpace >>= (\ e -> repeatChoiceNoTry e [ \ e1 -> P.lookAhead (pIn Nothing) *> pDef' (Just e1)
                                            , \ e1 -> liftL2 Exp.InfixColonEqual e1 <$ string ":=" <* pSpace <*> (pBraceInd <|> pDef)
                                            , \ e1 -> liftL2 Exp.Where e1 <$ pKeyword "where" <* pSpace <*> (pKeyBlock <|> pDefs)
                                            , \ e1 -> P.try $ liftL2 Exp.Is e1 <$ pScanKey <* pKeyword "is" <*> (pKeyBlock <|> pDef)
                                            ]
                )
  )

   <|>
  (
    (
      ((\ e -> Exp.PrefixAmpersand <$> duplicate e) <$ string "&")
      <|>
      ((\ e -> Exp.PrefixDotDot <$> duplicate e) <$ string "..")
    ) <* pSpace <*> pDef
  )
  <|>
  pReturn
  <|>
  pForall

maybeInfix :: L (Exp Name) -> Maybe (L (Exp Name) -> L (Exp Name) -> Exp Name, L (Exp Name)) -> L (Exp Name)
maybeInfix e1 (Just (op,e2)) = liftL2 op e1 e2
maybeInfix e1 Nothing = e1



pForall :: Parser (L (Exp Name))
pForall =
  (\ p1 n -> Exp.Forall <$ p1 <.> duplicate n) <$> pKeyword "forall" <* pSpace <*> pIdent

pEnum :: Parser (L (Exp Name))
pEnum =
  (\ e s ns -> Exp.Enum s <$ e <.> ns) <$> pKeyword "enum" <*> pSpecs <* pSpace <*> pNameBlock


-- Fun       := Def { Space ("over" | "when" | "while") Key (KeyBlock | Defs)
--                 | Space ("=>" Space | "next" Key) (BraceInd | Fun) } StopFun
pFun :: Parser (L (Exp Name))
pFun = pDef <* pSpace >>= pFun'

-- TODO Not all implmented
pFun' :: L (Exp Name) -> Parser (L (Exp Name))
pFun' e1 =
  repeatChoiceNoTry e1 [ \e1 -> liftL2 Exp.Fun e1 <$ pFatArrow <* pSpace <*> (pBraceInd <|> pFun) <* pSpace
                       , \e1 -> liftL2 Exp.Next e1 <$ pKeyword "next" <* pSpace <*> (pBraceInd <|> pFun) <* pSpace
                       , \e1 -> liftL2 Exp.Over e1 <$ pKeyword "over" <* pSpace <*> (pKeyBlock <|> pDefs) <* pSpace
                       , \e1 -> liftL2 Exp.When e1 <$ pKeyword "when" <* pSpace <*> (pKeyBlock <|> pDefs) <* pSpace
                       , \e1 -> liftL2 Exp.While e1 <$ pKeyword "while" <* pSpace <*> (pKeyBlock <|> pDefs) <* pSpace
                  ]

-- Expr      := Fun {Space '@' Space Call} StopExpr
--           |  '@' Space Call Scan &('@' | QualIdent) Expr
-- added optional ';' since it's already used in the wild
pExpr :: Parser (L (Exp Name))
pExpr =
  liftL2 Exp.AtSpec <$ pAt <* pSpace <*> pCall <* pScan  <* P.optionMaybe pSemi <*> pExpr
  <|>
  fixAfterAt <$> pFun <*> P.many ((.>) <$ pSpace <*> pAt <* pSpace <*> pCall <* pScan)
  where
    fixAfterAt :: L (Exp Name) -> [L (Exp Name)] ->  L (Exp Name)
    fixAfterAt e [] = e
    fixAfterAt e (x:xs) = fixAfterAt (Exp.SpecAt <$> duplicate e <.> duplicate x) xs

-- StopExpr  := &(Space (":)" | ')' | ']' | '}' | ';' | ',' | Ending !(ScanKey "is" Key)))
pAfterExpr :: Parser (L String)
pAfterExpr  = pColonParen <|> pRParen <|> pRBracket <|> pRBrace <|> pSemi <|> pComma <|> pEnding   --  !(ScanKeyNS "is" KeyEnd)))After

-- StopFun   := &(Space ("@")) | StopExpr
pAfterFun :: Parser (L String)
pAfterFun   = pAt <|> pAfterExpr

-- StopDef   := &(Space ("=>" | ("over"|"when"|"while"|"next") Key | StopFun
pStopDef :: Parser (L String)
pStopDef = P.lookAhead pAfterDef

pAfterDef :: Parser (L String)
pAfterDef = pSpace *> ( pFatArrow <|> pKeyword "over" <|> pKeyword "when" <|> pKeyword "while" <|> pKeyword "next" <|> pKeyword "over" <|> pAfterFun)

-- Commas    := Expr {',' Scan Expr}

pCommas :: Parser (L (Exp Name))
pCommas = do
  p1 <- P.getPosition
  xs <- pExpr `P.sepBy1` (pComma *> pScan)
  p2 <- P.getPosition
  return $ mkTuple (toLoc p1 p2) xs

mkTuple :: Loc -> [L (Exp Name)] -> L (Exp Name)
mkTuple _loc [x] = x
mkTuple loc xs = L loc $ Exp.Tuple xs


-- Separator := (';' | Ending) Scan
pSeparator :: Parser ()
pSeparator = (pSemi <|> pEnding) *> pScan

-- List      := push; set LinePrefix=""; Scan [Commas {Separator Commas} [Separator]]; pop
pList :: Parser (L [L (Exp Name)])
pList = doList pCommas pSeparator

pNameList :: Parser (L [([L (Exp Name)], L Name)])
pNameList = doList pAtName pNameSeparator

pAtName :: Parser ([L (Exp Name)], L Name)
pAtName =
  (,) <$> P.many ( (.>) <$> pAt <* pSpace <*> pCall <* pScan) <*> pIdent

pNameSeparator :: Parser ()
pNameSeparator = (pSemi <|> pComma <|> pEnding) *> pScan


-- The pSeparator and pItem must not consume any characters if they fail
doList :: Parser a -> Parser b -> Parser (L [a])
doList pItem pSeparator = do
  push
  setLinePrefix ByteString.empty
  pScan
  p1 <- pos
  items <- pItems
  p2 <- pos
  pop
  return $ L (toLoc p1 p2) items
 where
  pItems = do
    qItem <- P.optionMaybe pItem
    case qItem of
      Nothing -> return []
      Just item -> do
        qSeparator <- P.optionMaybe pSeparator
        case qSeparator of
          Nothing -> return [item]
          Just _ -> do
            items <- pItems
            return $ item : items

-- File      := [0oEF 0oBB 0oBF] set Nest=true; set BlockInd=""; set LineInd=""; List end
pFile :: Parser (L (Exp Name))  -- no need to set state since it's done in the parse function
pFile = do
  xs <- pList <* P.eof
  return $ mkList xs

mkList :: L[L (Exp Name)] -> L (Exp Name)
mkList (extract -> [x]) = x
mkList xs = Exp.List <$> xs

mkListPos :: L a -> L[L (Exp Name)] -> L b -> L (Exp Name)
mkListPos p1 xs p2 = mkList xs <. p1 <. p2

---------------------------------useful parsers

-- Binary takes parser for lhs and phs and a list with pairs of parsers for operator and function to apply f matching.-
-- Binary     := pLhs Space { pOperator Scan  pRhs Space}

doBinary :: Parser (L a) -> Parser (L b) -> [(Parser (L c), L a -> L b -> a)] -> Parser (L a)
doBinary pLhs pRhs choices = do
  lhs <- pLhs -- No try since doBinary is only called when pLhs must match
  _ <- pSpace
  repeatChoiceNoTry lhs $ map fixBinary choices
 where
  fixBinary (p, f) = \ e1 -> liftL2 f e1 <$ p <* pScan <*> pRhs <* pSpace

repeatChoice :: a -> [a -> Parser a] -> Parser a
repeatChoice e choices = do
  tryChoices e choices
 where
  tryChoices e [] = return e
  tryChoices e (p:ps) = do
    qE <- P.optionMaybe $ P.try $ p e
    case qE of
      Nothing -> tryChoices e ps
      Just e -> tryChoices e choices


repeatChoiceNoTry :: a -> [a -> Parser a] -> Parser a
repeatChoiceNoTry e choices = do
  tryChoices e choices
 where
  tryChoices e [] = return e
  tryChoices e (p:ps) = do
    qE <- P.optionMaybe $ p e
    case qE of
      Nothing -> tryChoices e ps
      Just e -> tryChoices e choices


pLParen :: Parser (L String)
pLParen =  match '('

pRParen :: Parser (L String)
pRParen =  match ')'

pLBracket :: Parser (L String)
pLBracket =  match '['

pRBracket :: Parser (L String)
pRBracket =  match ']'

pLBrace :: Parser (L String)
pLBrace =  match '{'

pRBrace :: Parser (L String)
pRBrace =  match '}'

pComma :: Parser (L String)
pComma =  match ','

pDot :: Parser (L String)
pDot =  P.try (match '.' <* P.notFollowedBy (match '.' <|> match ' ' <|> match '\t' <|> pEnding))

pSemi :: Parser (L String)
pSemi =  match ';'

pFatArrow :: Parser (L String)
pFatArrow =  string "=>"

pHash :: Parser (L String)
pHash =  P.try (match '#' <* P.notFollowedBy (match '>'))

pLessHash :: Parser (L String)
pLessHash =  P.try (string "<#" <* P.notFollowedBy (match '>'))

pLessHashGreater :: Parser (L String)
pLessHashGreater =  string "<#>"

pHashGreater :: Parser (L String)
pHashGreater =  string "#>"


pEqual :: Parser (L String)
pEqual =  P.try (match '=' <* P.notFollowedBy (match '>'))

pColon :: Parser (L String)
pColon =  P.try (match ':' <* P.notFollowedBy (match '=' <|> match ')'))

pGreaterThan :: Parser (L String)
pGreaterThan =  P.try (match '>' <* P.notFollowedBy (match '='))

pLessThan :: Parser (L String)
pLessThan =  P.try (match '<' <* P.notFollowedBy (match '=' <|> match '#' <|> match '>'))

pGreaterEqual :: Parser (L String)
pGreaterEqual =  string ">="

pLessEqual :: Parser (L String)
pLessEqual =  string "<="

pPlus :: Parser (L String)
pPlus =  P.try (match '+' <* P.notFollowedBy (match '='))

pMinus :: Parser (L String)
pMinus =  P.try (match '-' <* P.notFollowedBy (match '=' <|> match '>'))

pMultiply :: Parser (L String)
pMultiply =  P.try (match '*' <* P.notFollowedBy (match '='))

pDivide :: Parser (L String)
pDivide =  P.try (match '/' <* P.notFollowedBy (match '='))

pSlash :: Parser (L String)
pSlash =  match '/'

pAt :: Parser (L String)
pAt =  match '@'

pColonParen :: Parser (L String)
pColonParen = string ":)"

pLinePrefix :: Parser ()
pLinePrefix = do
  prefix <- getLinePrefix
  p $ ByteString.unpack prefix
 where
  p [] = return ()
  p (w:ws) = byte w *> p ws

pKeyword :: String -> Parser (L String)
pKeyword kwd = P.try $ do
  p1 <- pos
  x <- pAlpha
  xs <- P.many pAlnum
  p2 <- pos
  case Text.decodeUtf8' (ByteString.pack $ x:xs) of
    Left err -> fail $ "keyword with illegal utf8 character" ++ show err
    Right txt -> do
      if Text.pack kwd == txt then
        return $ wrapLoc p1 kwd p2
      else
        P.parserZero

match :: Char -> Parser (L String)
match c = wrapLoc <$> pos <*> ([c] <$ byte (c2w c)) <*> pos

char :: Char -> Parser Word8
char c = byte (c2w c) <?> show c

byte :: Word8 -> Parser Word8
byte c = satisfy (==c)

-- Parse at least 'f' and at most 't' times with the given parser
fromTo :: Int -> Int -> Parser a -> Parser [a]
fromTo f t p = do
  x1 <- P.count f p
  x2 <- fromTo' (t-f)
  return $ x1 ++ x2
  where
    fromTo' n | n <=0 = return []
    fromTo' n = do
      qX <- P.optionMaybe p
      case qX of
        Nothing -> return []
        Just x -> do
          xs <- fromTo' (n-1)
          return $ x:xs

---------------------------------- Parser state

data Context =
  Context {
    nest :: Bool,              -- True while inside an indented block, false while starting a new indented block
    blockInd :: ByteString,
    lineInd ::ByteString,
    linePrefix ::ByteString
  } deriving (Show)

beginContext :: Context
beginContext =
  Context {
    nest = True,
    blockInd = ByteString.empty,
    lineInd = ByteString.empty,
    linePrefix = ByteString.empty
  }

data ParserState =
  ParserState {
    active :: Context,
    stack :: [Context]
  }

beginPS :: ParserState
beginPS =
  ParserState {
    active = beginContext,
    stack = []
  }

push :: Parser ()
push = P.modifyState $ \ s -> s { stack = active s : stack s }

pop :: Parser ()
pop = P.modifyState $ \ s -> case stack s of
                               (x:xs) -> s { active = x, stack = xs }
                               _ -> error "Unbalanced push/pop in Parse2"

getActive :: (Context -> a) -> Parser a
getActive f = do
  s <- P.getState
  return $ f $ active s

modifyActive :: (Context -> Context) -> Parser ()
modifyActive f = P.modifyState $ \ s -> s { active = f (active s) }


getNest :: Parser Bool
getNest = getActive nest

getBlockInd :: Parser ByteString
getBlockInd = getActive blockInd

getLineInd :: Parser ByteString
getLineInd = getActive lineInd

getLinePrefix :: Parser ByteString
getLinePrefix = getActive linePrefix

setNest :: Bool -> Parser ()
setNest b = modifyActive $ \ a -> a { nest = b }

setBlockInd :: ByteString -> Parser ()
setBlockInd bs = modifyActive $ \ a -> a { blockInd = bs }

setLineInd :: ByteString -> Parser ()
setLineInd bs = modifyActive $ \ a -> a { lineInd = bs }

setLinePrefix :: ByteString -> Parser ()
setLinePrefix bs = modifyActive $ \ a -> a { linePrefix = bs }

------------------------------------------------ PPos.SourcePos to Verse.Pos

toStr :: PPos.SourcePos -> String
toStr sourcePos = show (PPos.sourceLine sourcePos) ++ ":" ++ show (PPos.sourceColumn sourcePos)

toPos :: PPos.SourcePos -> Pos.Pos
toPos sourcePos = Pos.Pos {
  Pos.line = PPos.sourceLine sourcePos ,
  Pos.column = PPos.sourceColumn sourcePos ,
  Pos.offset = 0 -- No offset in PPos.SourcePos
  }

toLoc :: PPos.SourcePos -> PPos.SourcePos -> Loc
toLoc sourcePos1 sourcePos2 = Loc (toPos sourcePos1) (toPos sourcePos2)

pos :: Parser PPos.SourcePos
pos = P.getPosition

pPos :: Parser (L ())
pPos = unitLoc <$> pos <*> pos

unitLoc :: PPos.SourcePos -> PPos.SourcePos -> L ()
unitLoc p1 p2 = L (toLoc p1 p2) ()

wrapLoc :: PPos.SourcePos -> a -> PPos.SourcePos -> L a
wrapLoc p1 x p2 = L (toLoc p1 p2) x

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f   = tokenPrim (\c -> showW8s [c])
                        (\pos c _cs -> updatePosWord pos c)
                        (\w -> if f w then Just w else Nothing)

string :: String -> Parser (L String)
string s = P.try $ wrapLoc <$> pos <*> (s <$ tokens showW8s updatePosWords (map c2w s)) <*> pos

updatePosWord :: PPos.SourcePos -> Word8 -> PPos.SourcePos
updatePosWord pos w =
  PPos.updatePosChar pos (if w < 127 then w2c w else ' ')

updatePosWords :: PPos.SourcePos -> [Word8] -> PPos.SourcePos
updatePosWords pos ws =
  foldl updatePosWord pos ws


showW8s :: [Word8] -> String
showW8s ws = case Text.decodeUtf8' $ ByteString.pack ws of
  Left _err -> show ws
  Right txt -> show txt
