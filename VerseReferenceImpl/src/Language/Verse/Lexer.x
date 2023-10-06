{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Language.Verse.Lexer
  ( Lexer
  , runLexer
  , getToken
  , throwError
  ) where

import Control.Monad.Except qualified as Except
import Control.Monad.State (MonadState (..), StateT, evalStateT, gets, modify)
import Control.Monad.Trans.Except (Except, runExcept)

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteString (w2c)
import Data.Char (chr, ord)
import Data.List
import Data.Ratio
import Data.Text.Encoding qualified as Text
import Data.Text qualified as Text
import Data.Word

import Language.Verse.Error
import Language.Verse.Indent
import Language.Verse.Loc
import Language.Verse.Pos as Pos
import Language.Verse.Token (Token, StringDelimiter)
import Language.Verse.Token qualified as Token
-- import Debug.Trace(trace)
}

$alpha = [A-Za-z\_]
$alnum = [A-Za-z\_0-9]
$space = [\ \t]
@newline = (\r \n?) | \n
@operator = ([\x20-\x7E] # [\# \\ \{ \} \" \'])*

:-
<0, nested, maybeNesting, indCmt> {
  " " { space }
  \t { tab }
}

<0, nested, maybeNesting> {
  "<#>" { indCmt0 }
}

<0, nested, maybeNesting, indCmt, indented, colon, equal, colonEqual, plusEqual, minusEqual, multiplyEqual, divideEqual, fatArrow> {
  "#" .* ;
}

<indented, colon, equal, colonEqual, plusEqual, minusEqual, multiplyEqual, divideEqual, fatArrow> {
  "<#>" { indCmtIndented }
}

<0, nested, maybeNesting, blockCmt, indCmt> {
  @newline { newline }
  "<#" { leftBlockCmt }
}

<indented, indentedIndCmt> {
  @newline { newlineIndented }
}

<indCmt> {
  "<#>" { indCmtIndCmt }
  "" { emptyIndCmt }
}

<indentedBlockCmt, indentedIndCmt> {
  "<#>" ;
  . ;
}

<blockCmt> {
  \t { tabBlockCmt }
  "<#>" { indBlockCmt }
  "#>" { rightBlockCmt }
  . { textBlockCmt }
}

<indented, colon, equal, colonEqual, plusEqual, minusEqual, multiplyEqual, divideEqual, fatArrow, indentedBlockCmt, indentedIndCmt> {
  "<#" { leftBlockCmtIndented }
}

<indentedBlockCmt> {
  "#>" { rightIndentedBlockCmt }
  @newline ;
}

<0> {
  "" { empty0 }
}

<nested> {
  "" { emptyNested }
}

<maybeNewline> {
  "{" { leftBraceMaybeNewline }
  "do" { doMaybeNewline }
  "then" { thenMaybeNewline }
  "else" { elseMaybeNewline }
  "" { emptyMaybeNewline }
}

<maybeNesting> {
  "{" { leftBraceMaybeNesting }
  "" { emptyNesting }
}

<nesting> "" { emptyNesting }

<indented, colon, equal, colonEqual, plusEqual, minusEqual, multiplyEqual, divideEqual, fatArrow> {
  [\ \t]+ ;
}

<colon> {
  @newline { newlineColon }
  "" { emptyColon }
}

<equal> {
  @newline { newlineEqual }
  "" { emptyEqual }
}

<colonEqual> {
  @newline { newlineColonEqual }
  "" { emptyColonEqual }
}

<plusEqual> {
  @newline { newlinePlusEqual }
  "" { emptyPlusEqual }
}

<minusEqual> {
  @newline { newlineMinusEqual }
  "" { emptyMinusEqual }
}

<multiplyEqual> {
  @newline { newlineMultiplyEqual }
  "" { emptyMultiplyEqual }
}

<divideEqual> {
  @newline { newlineDivideEqual }
  "" { emptyDivideEqual }
}

<fatArrow> {
  @newline { newlineFatArrow }
  "" { emptyFatArrow }
}

<indented> {
  ":" { colonIndented }
  "=" { equalIndented }
  ":=" { colonEqualIndented }
  "+=" { plusEqualIndented }
  "-=" { minusEqualIndented }
  "*=" { multiplyEqualIndented }
  "/=" { divideEqualIndented }
  "=>" { fatArrowIndented }
  "(" { token Token.LeftParen }
  ")" { token Token.RightParen }
  ":)" { token Token.ColonRightParen }
  "{" { tokenDo incBrace Token.LeftBrace }
  "}" { rightBraceOrString }
  "[" { token Token.LeftBracket }
  "]" { token Token.RightBracket }
  ";" { token Token.Semi }
  "," { token Token.Comma }
  "." { token Token.Dot }
  ".." { token Token.DotDot }
  "<>" { token Token.NotEqual }
  "<" { token Token.Less }
  "<=" { token Token.LessEqual }
  ">" { token Token.Greater }
  ">=" { token Token.GreaterEqual }
  "|" { token Token.Pipe }
  "->" { token Token.ThinArrow }
  "?" { token Token.QuestionMark }
  "+" { token Token.Plus }
  "-" { token Token.Minus }
  "*" { token Token.Multiply }
  "/" { token Token.Divide }
  "^" { token Token.Caret }
  "&" { token Token.Ampersand }
  "@" { token Token.AtSign }
  "~" { token Token.Tilde }
  "all" { token Token.All }
  "and" { token Token.And }
  "array" { token Token.Array }
  "at" { token Token.At }
  "block" { token Token.Block }
  "do" { token Token.Do }
  "catch" { token Token.Catch }
  "class" { token Token.Class }
  "do" { token Token.Do }
  "else" { token Token.Else }
  "enum" { token Token.Enum }
  "exists" { token Token.Exists }
  "function" { token Token.Function }
  "fail" { token Token.Fail }
  "false" { token Token.False }
  "for" { token Token.For }
  "if" { token Token.If }
  "module" { token Token.Module }
  "of" { token Token.Of }
  "not" { token Token.Not }
  "one" { token Token.One }
  "until" { token Token.Until }
  "set" { token Token.Set }
  "struct" { token Token.Struct }
  "sync" { token Token.Sync }
  "then" { token Token.Then }
  "true" { token Token.True }
  "truth" { token Token.Truth }
  "option" { token Token.Option }
  "or" { token Token.Or }
  "var" { token Token.Var }
  "where" { token Token.Where }
  [0-9]+ { int }
  [0-9]+"."[0-9]+ { float }
  \" { stringBegin }
  "'"[^'\n]"'" { char }
  "'\"[rnt'\"\\\{\}\#\<\>&\~]"'" { charEscaped }
  "0o"[0-7A-F]+ { charHex }
  "0u"[0-7A-F]+ { charHex }

  $alpha $alnum* ("'" @operator "'")? { name }
}

<insideString> {
  \{  { stringValue }
  \"  { stringEnd }
  \\[rnt'\"\\\{\}\#\<\>&\~] { stringTextEscaped }
  [^"\n] { stringText }
}



{
newtype Lexer a = Lexer
  { getLexer :: StateT S (Except Error) a
  } deriving (Functor, Applicative, Monad)

runLexer :: Lexer a -> ByteString -> Either Error a
runLexer m = runExcept . evalStateT (getLexer m) . mkS
  where
    mkS input = S
      { alexInput = AlexInput { pos = Pos.minBound, input }
      , indent = []
      , indents = []
      , brace = 0
      , braces = []
      , beginString = Token.Quote
      , reverseString = []
      , states = []
      }

data S = S
  { alexInput :: !AlexInput
  , indent :: !Indent
  , indents :: ![Indent]
  , brace :: !Int      -- keep track of open braces, use for "abc{ whatever } def", if a string ends with { then return StringBegin, accepting } as start of string when braces is 0.
  , braces :: ![Int]
  , beginString :: !Token.StringDelimiter
  , reverseString :: ![Char]
  , states :: ![State]
  }

data AlexInput = AlexInput
  { pos :: !Pos
  , input :: !ByteString
  }

type State = Int

getToken :: Lexer (L Token)
getToken = do
  alexInput <- getAlexInput
  alexScan alexInput <$> peekStates >>= \ case
    AlexEOF ->
      pure $ L (Loc alexInput.pos alexInput.pos) Token.EOF
    AlexError AlexInput { pos } ->
      throwError' $ LexError pos
    AlexSkip alexInput _ -> do
      putAlexInput alexInput
      getToken
    AlexToken alexInput' n f -> do
      putAlexInput alexInput'
      f alexInput.pos alexInput'.pos n alexInput.input

type Action = Pos -> Pos -> Int -> ByteString -> Lexer (L Token)

action :: Lexer (L Token) -> Action
action m = \ _ _ _ _ -> m

space :: Action
space = action $ do
  pushIndent Space
  getToken

tab :: Action
tab = action $ do
  pushIndent Tab
  getToken

newline :: Action
newline = action $ do
  putIndent []
  getToken

stringBegin :: Action
stringBegin _i _j n xs = do
  let text = Text.decodeUtf8 $ ByteString.take n xs
  pushStates insideString
  getToken

stringText :: Action
stringText i _j n xs = do
  let text = Text.decodeUtf8 $ ByteString.take n xs
  addReverseString $ extract $ show text
  getToken
  where
    extract ['"', x, '"'] = x
    extract xs = error (show i ++ ": not a string character: " ++ xs)

stringTextEscaped :: Action
stringTextEscaped i _j n xs = do
  let text = Text.decodeUtf8 $ ByteString.take n xs
  addReverseString $ extract $ show text
  getToken
  where
    extract ['"', '\\', '\\', 'r', '"'] = '\r'
    extract ['"', '\\', '\\', 'n', '"'] = '\n'
    extract ['"', '\\', '\\', 't', '"'] = '\t'
    extract ['"', '\\', '\\', '\\', '\\', '"'] = '\\'
    extract ['"', '\\', '\\',  x , '"'] = x
    extract xs = error (show i ++ ":not a string escape character: " ++ xs)


stringValue :: Action
stringValue i j _n _xs = do
  popStates
  (begin, str) <- getString
  pushBrace
  pure $ L (Loc i j) (Token.String begin str Token.Brace)

rightBraceOrString :: Action
rightBraceOrString i j _n _xs = do
    bs <- peekBrace
    if bs > 0 then do
      decBrace
      pure $ L (Loc i j) Token.RightBrace
    else do
      pushStates insideString
      setBeginString Token.Brace
      popBrace
      getToken

stringEnd :: Action
stringEnd i j n xs = do
  let text = Text.decodeUtf8 $ ByteString.take n xs
  popStates
  (begin, str) <- getString
  pure $ L (Loc i j) (Token.String begin str Token.Quote)

empty0 :: Action
empty0 = action $ do
  pushStates maybeNewline
  getToken

indCmt' :: Lexer (L Token)
indCmt' = do
  pushIndents =<< getIndent
  pushStates indCmt
  pushStates indentedIndCmt
  getToken

indCmt0 :: Action
indCmt0 = action indCmt'

indCmtIndented :: Action
indCmtIndented = action $ do
  popStates
  indCmt'

emptyNested :: Action
emptyNested i j _ _ = do
  x <- getIndent
  y <- peekIndents
  if x `isPrefixOf` y then do
    popIndents
    popStates
    pure $ L (Loc i j) Token.Dedent
  else if y `isPrefixOf` x then do
    pushStates maybeNewline
    getToken
  else
    throwError' $ IndentError i x y

indCmtIndCmt :: Action
indCmtIndCmt i _ _ _ = do
  x <- getIndent
  y <- peekIndents
  if x `isPrefixOf` y then do
    popIndents
    pushIndents x
    pushStates indentedIndCmt
    getToken
  else if y `isPrefixOf` x then do
    pushStates indentedIndCmt
    getToken
  else
    throwError' $ IndentError i x y

emptyIndCmt :: Action
emptyIndCmt i _ _ _ = do
  x <- getIndent
  y <- peekIndents
  if x `isPrefixOf` y then do
    popIndents
    popStates
    getToken
  else if y `isPrefixOf` x then do
    pushStates indentedIndCmt
    getToken
  else
    throwError' $ IndentError i x y

leftBlockCmt :: Action
leftBlockCmt = action $ do
  pushStates blockCmt
  pushIndent Space
  pushIndent Space
  getToken

tabBlockCmt :: Action
tabBlockCmt = action $ do
  pushIndent Tab
  getToken

indBlockCmt :: Action
indBlockCmt = action $ do
  pushIndent Space
  pushIndent Space
  pushIndent Space
  getToken

textBlockCmt :: Action
textBlockCmt = action $ do
  pushIndent Space
  getToken

rightBlockCmt :: Action
rightBlockCmt = action $ do
  pushIndent Space
  pushIndent Space
  popStates
  getToken

leftBlockCmtIndented :: Action
leftBlockCmtIndented = action $ do
  pushStates indentedBlockCmt
  getToken

rightIndentedBlockCmt :: Action
rightIndentedBlockCmt = action $ do
  popStates
  getToken

maybeNewlineAction :: Token -> Action
maybeNewlineAction x i j _ _ = do
  popStates
  pushStates indented
  pure $ L (Loc i j) x

leftBraceMaybeNewline :: Action
leftBraceMaybeNewline i j _ _ = do
  popStates
  pushStates indented
  incBrace
  pure $ L (Loc i j) Token.LeftBrace

doMaybeNewline :: Action
doMaybeNewline = maybeNewlineAction Token.Do

thenMaybeNewline :: Action
thenMaybeNewline = maybeNewlineAction Token.Then

elseMaybeNewline :: Action
elseMaybeNewline = maybeNewlineAction Token.Else

emptyMaybeNewline :: Action
emptyMaybeNewline = maybeNewlineAction Token.Newline

leftBraceMaybeNesting :: Action
leftBraceMaybeNesting i j _ _ = do
  popStates
  popIndents
  pushStates indented
  incBrace
  pure $ L (Loc i j) Token.LeftBrace

emptyNesting :: Action
emptyNesting i j _ _ = do
  popStates
  pushStates nested
  pure $ L (Loc i j) Token.Indent

newlineIndented :: Action
newlineIndented = action $ do
  popStates
  putIndent []
  getToken

newlineToken :: Int -> Token -> Action
newlineToken s x i j _ _ = do
  popStates
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates s
  pure $ L (Loc i j) x

emptyToken :: Token -> Action
emptyToken x i j _ _ = do
  popStates
  pure $ L (Loc i j) x

colonIndented :: Action
colonIndented = action $ do
  pushStates colon
  getToken

newlineColon :: Action
newlineColon = newlineToken nesting Token.ColonEOL

emptyColon :: Action
emptyColon = emptyToken Token.Colon

equalIndented :: Action
equalIndented = action $ do
  pushStates equal
  getToken

newlineEqual :: Action
newlineEqual = newlineToken maybeNesting Token.Equal

emptyEqual :: Action
emptyEqual = emptyToken Token.Equal

colonEqualIndented :: Action
colonEqualIndented = action $ do
  pushStates colonEqual
  getToken

plusEqualIndented :: Action
plusEqualIndented = action $ do
  pushStates plusEqual
  getToken

minusEqualIndented :: Action
minusEqualIndented = action $ do
  pushStates minusEqual
  getToken

multiplyEqualIndented :: Action
multiplyEqualIndented = action $ do
  pushStates multiplyEqual
  getToken

divideEqualIndented :: Action
divideEqualIndented = action $ do
  pushStates divideEqual
  getToken

newlineColonEqual :: Action
newlineColonEqual = newlineToken maybeNesting Token.ColonEqual

newlinePlusEqual :: Action
newlinePlusEqual = newlineToken maybeNesting Token.PlusEqual

newlineMinusEqual :: Action
newlineMinusEqual = newlineToken maybeNesting Token.MinusEqual

newlineMultiplyEqual :: Action
newlineMultiplyEqual = newlineToken maybeNesting Token.MultiplyEqual

newlineDivideEqual :: Action
newlineDivideEqual = newlineToken maybeNesting Token.DivideEqual

emptyColonEqual :: Action
emptyColonEqual = emptyToken Token.ColonEqual

emptyPlusEqual :: Action
emptyPlusEqual = emptyToken Token.PlusEqual

emptyMinusEqual :: Action
emptyMinusEqual = emptyToken Token.MinusEqual

emptyMultiplyEqual :: Action
emptyMultiplyEqual = emptyToken Token.MultiplyEqual

emptyDivideEqual :: Action
emptyDivideEqual = emptyToken Token.DivideEqual


fatArrowIndented :: Action
fatArrowIndented = action $ do
  pushStates fatArrow
  getToken

newlineFatArrow :: Action
newlineFatArrow = newlineToken maybeNesting Token.FatArrow

emptyFatArrow :: Action
emptyFatArrow = emptyToken Token.FatArrow

token :: Token -> Action
token x i j _ _ = pure $ L (Loc i j) x

tokenDo :: Lexer () -> Token -> Action
tokenDo todo x i j _ _ =
  do
    todo
    pure $ L (Loc i j) x

int :: Action
int i j n xs =
  pure . L (Loc i j) . Token.Int .
  ByteString.foldl' f 0 $ ByteString.take n xs
  where
    f z x = z * 10 + (toInteger $ x - ord' '0')

float :: Action
float i j n xs =
  pure . L (Loc i j) . Token.Float . toRational .
  ByteString.foldl' f (0, 0, 0) $ ByteString.take n xs
  where
    f (!z0, !z1, !n1) x
      | x == ord' '.' = (z1, z0, 1)
      | otherwise = (z0, z1 * 10 + (toInteger $ x - ord' '0'), n1 * 10)
    toRational (z0, z1, n1) =
      (z0 * n1 + z1) % n1


{-
string :: Action
string i j n xs =
  pure . L (Loc i j) . Token.String .
  extract $ show $ Text.decodeUtf8 $ ByteString.take (n-2) $ ByteString.tail xs
  where
    extract [] = []
    extract ('\\':'r':xs) = '\r' : extract xs
    extract ('\\':'n':xs) = '\n' : extract xs
    extract ('\\':'t':xs) = '\t' : extract xs
    extract ('\\':x:xs) = x : extract xs
    extract (x:xs) = x : extract xs
-}


char :: Action
char i j 3 xs =
  pure . L (Loc i j) . Token.Char .
  extract $ Text.decodeUtf8 $ ByteString.take 3 xs
  where
    extract txt = Text.index txt 1
char i _j _n _xs =
  throwError' $ LexError i

charEscaped :: Action
charEscaped i j 4 xs =
  pure . L (Loc i j) . Token.Char .
  toCharEscaped $ extract $ Text.decodeUtf8 $ ByteString.take 4 xs
  where
    extract txt = Text.index txt 2
    toCharEscaped 'r'  = '\r'
    toCharEscaped 'n'  = '\n'
    toCharEscaped 't'  = '\t'
    toCharEscaped c  = c
charEscaped i _j _n _xs =
  throwError' $ LexError i

charHex :: Action
charHex i j n xs =
  pure . L (Loc i j) . Token.Char . chr . fromInteger .
  ByteString.foldl' f 0 $ ByteString.take (n-2) $ ByteString.drop 2 xs
  where
    toDigit x = if x <= ord' '9' then x - ord' '0'
                else if x <= ord' 'F' then x - ord' 'A'
                else x - ord' 'a'
    f z x = z * 16 + (toInteger $ toDigit x)


ord' :: Char -> Word8
ord' = fromIntegral . ord

name :: Action
name i j n xs =
  pure . L (Loc i j) . Token.Name . Text.decodeUtf8 $ ByteString.take n xs

throwError :: Loc -> Token -> Lexer a
throwError loc = throwError' . ParseError loc

throwError' :: Error -> Lexer a
throwError' = Lexer . Except.throwError

getAlexInput :: Lexer AlexInput
getAlexInput = gets' alexInput

putAlexInput :: AlexInput -> Lexer ()
putAlexInput alexInput = modify' $ \ s -> s { alexInput }

pushStates :: State -> Lexer ()
pushStates x = modify' $ \ s -> s { states = x:s.states }

peekStates :: Lexer State
peekStates = do
  s <- get'
  pure $ case s.states of
    [] -> 0
    x:_ -> x

popStates :: Lexer ()
popStates = do
  s <- get'
  case s.states of
    [] -> pure ()
    _:states -> modify' $ \ s -> s { states }

peekIndents :: Lexer Indent
peekIndents = do
  s <- get'
  pure $ case s.indents of
    [] -> []
    x:_ -> x

pushIndents :: Indent -> Lexer ()
pushIndents x = modify' $ \ s -> s { indents = x:s.indents }

popIndents :: Lexer ()
popIndents = do
  s <- get'
  case s.indents of
    [] -> pure ()
    _:indents -> put' s { indents }

pushIndent :: White -> Lexer ()
pushIndent x = modify' $ \ s -> s { indent = x:s.indent }

getIndent :: Lexer Indent
getIndent = gets' indent

putIndent :: Indent -> Lexer ()
putIndent indent = modify' $ \ s -> s { indent }

pushBrace :: Lexer ()
pushBrace = modify' $ \ s -> s { brace = 0, reverseString = [], braces = s.brace:s.braces }

peekBrace :: Lexer Int
peekBrace = do
  s <- get'
  pure $ s.brace

popBrace :: Lexer ()
popBrace = do
  s <- get'
  case s.braces of
    [] -> pure ()
    brace:braces -> modify' $ \ s -> s { brace, braces }

incBrace :: Lexer ()
incBrace = modify' $ \ s -> s { brace = s.brace + 1 }

decBrace :: Lexer ()
decBrace = modify' $ \ s -> s { brace = s.brace - 1 }

setBeginString :: StringDelimiter -> Lexer ()
setBeginString delimiter = modify' $ \ s -> s { beginString = delimiter }

addReverseString :: Char -> Lexer ()
addReverseString c = modify' $ \ s -> s { reverseString = c:s.reverseString }

getString :: Lexer (Token.StringDelimiter, String)
getString = do
  s <- get'
  modify' $ \ s -> s { beginString = Token.Quote, reverseString = [] }
  pure $ (s.beginString, reverse s.reverseString)

get' :: Lexer S
get' = Lexer get

gets' :: (S -> a) -> Lexer a
gets' = Lexer . gets

put' :: S -> Lexer ()
put' = Lexer . put

modify' :: (S -> S) -> Lexer ()
modify' = Lexer . modify

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} = case ByteString.uncons input of
  Nothing -> Nothing
  Just (x, input) -> Just (x, AlexInput { pos = movePos pos (ByteString.w2c x), .. })

movePos :: Pos -> Char -> Pos
movePos Pos {..} = \ case
  '\n' -> Pos { line = line + 1, column = 1, offset = offset + 1 }
  '\t' -> Pos { column = column + 8 - ((column - 1) `mod` 8), offset = offset + 1, .. }
  _ -> Pos { column = column + 1, offset = offset + 1, .. }
}
