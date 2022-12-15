{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Char (ord)
import Data.List
import Data.Ratio
import Data.Text.Encoding qualified as Text
import Data.Word

import Language.Verse.Error
import Language.Verse.Indent
import Language.Verse.Loc
import Language.Verse.Pos as Pos
import Language.Verse.Token (Token)
import Language.Verse.Token qualified as Token
}

$alpha = [A-Za-z\_]
$alnum = [A-Za-z\_0-9]
$space = [\ \t]
@newline = (\r \n?) | \n

:-
<0, nested, maybeNesting> {
  " " { space }
  \t { tab }
  @newline { newline }
}

<0> "" { empty0 }

<nested> "" { emptyNested }

<dedenting> "" { emptyDedenting }

<maybeNesting> {
  "{" { leftBrace }
  "" { emptyMaybeNesting }
}

<nesting> "" { emptyNesting }

<indented> {
  "#" .* ;
  "<#" [.\n]* "#>" ;
  [\ \t]+ ;
  @newline { newlineIndented }
  ":" $space* @newline { colon }
  "=" $space* @newline { equal }
  ":=" $space* @newline { colonEqual }
  "=>" $space* @newline { fatArrow }
  "(" { token Token.LeftParen }
  ")" { token Token.RightParen }
  "{" { token Token.LeftBrace }
  "}" { token Token.RightBrace }
  ";" { token Token.Semi }
  ":" { token Token.Colon }
  "," { token Token.Comma }
  "." { token Token.Dot }
  "=" { token Token.Equal }
  "<>" { token Token.NotEqual }
  "<" { token Token.Less }
  "<=" { token Token.LessEqual }
  ">" { token Token.Greater }
  ">=" { token Token.GreaterEqual }
  "|" { token Token.Pipe }
  ":=" { token Token.ColonEqual }
  "->" { token Token.ThinArrow }
  "=>" { token Token.FatArrow }
  "?" { token Token.QuestionMark }
  "+" { token Token.Plus }
  "-" { token Token.Minus }
  "*" { token Token.Multiply }
  "/" { token Token.Divide }
  "all" { token Token.All }
  "block" { token Token.Block }
  "class" { token Token.Class }
  "do" { token Token.Do }
  "else" { token Token.Else }
  "exists" { token Token.Exists }
  "fail" { token Token.Fail }
  "false" { token Token.False }
  "for" { token Token.For }
  "if" { token Token.If }
  "isInt" { token Token.IsInt }
  "lambda" { token Token.Lambda }
  "module" { token Token.Module }
  "not" { token Token.Not }
  "one" { token Token.One }
  "struct" { token Token.Struct }
  "then" { token Token.Then }
  "true" { token Token.True }
  "truth" { token Token.Truth }
  [0-9]+ { int }
  [0-9]+"."[0-9]+ { float }
  $alpha $alnum* { name }
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
      , states = []
      }

data S = S
  { alexInput :: !AlexInput
  , indent :: !Indent
  , indents :: ![Indent]
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

empty0 :: Action
empty0 = action $ do
  pushStates indented
  getToken

emptyNested :: Action
emptyNested i j _ _ = do
  x <- getIndent
  y <- peekIndents
  if x `isPrefixOf` y then do
    popIndents
    popStates
    pushStates dedenting
    pure $ L (Loc i j) Token.Dedent
  else if y `isPrefixOf` x then do
    pushStates indented
    getToken
  else
    throwError' $ IndentError i x y

emptyDedenting :: Action
emptyDedenting i j _ _ = do
  popStates
  pure $ L (Loc i j) Token.Newline

leftBrace :: Action
leftBrace i j _ _ = do
  popStates
  popIndents
  pushStates indented
  pure $ L (Loc i j) Token.LeftBrace

emptyMaybeNesting :: Action
emptyMaybeNesting i j _ _ = do
  popStates
  pushStates nested
  pure $ L (Loc i j) Token.Indent

emptyNesting :: Action
emptyNesting i j _ _ = do
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates nested
  pure $ L (Loc i j) Token.Indent

newlineIndented :: Action
newlineIndented i j _ _ = do
  popStates
  putIndent []
  pure $ L (Loc i j) Token.Newline

colon :: Action
colon i j _ _ = do
  popStates
  pushStates nesting
  pure $ L (Loc i j) Token.ColonEOL

equal :: Action
equal i j _ _ = do
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates maybeNesting
  pure $ L (Loc i j) Token.Equal

colonEqual :: Action
colonEqual i j _ _ = do
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates maybeNesting
  pure $ L (Loc i j) Token.ColonEqual

fatArrow :: Action
fatArrow i j _ _ = do
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates maybeNesting
  pure $ L (Loc i j) Token.FatArrow

token :: Token -> Action
token x i j _ _ =
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
