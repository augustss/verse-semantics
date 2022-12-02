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

@newline = \r? \n

:-
  <0, nested> " " { space }
  <0, nested> \t { tab }
  <0, nested> @newline { newline }
  <0> "" { empty0 }
  <nested> "" { emptyNested }
  <indented> "#" .* ;
  <indented> "<#" [.\n]* "#>" ;
  <indented> [\ \t]+ ;
  <indented> @newline { newlineIndented }
  <indented> ":" [\ \t]* @newline { colon }
  <indented> "=" [\ \t]* @newline { equals }
  <indented> ":=" [\ \t]* @newline { colonEquals }
  <indented> "=>" [\ \t]* @newline { fatArrow }
  <nesting> "" { emptyNesting }
  <indented> "(" { token Token.LeftParen }
  <indented> ")" { token Token.RightParen }
  <indented> "{" { token Token.LeftBrace }
  <indented> "}" { token Token.RightBrace }
  <indented> ";" { token Token.Semi }
  <indented> ":" { token Token.Colon }
  <indented> "," { token Token.Comma }
  <indented> "." { token Token.Dot }
  <indented> "=" { token Token.Equals }
  <indented> "|" { token Token.Pipe }
  <indented> ":=" { token Token.ColonEquals }
  <indented> "->" { token Token.ThinArrow }
  <indented> "=>" { token Token.FatArrow }
  <indented> "?" { token Token.QuestionMark }
  <indented> "+" { token Token.Plus }
  <indented> "-" { token Token.Minus }
  <indented> "*" { token Token.Multiply }
  <indented> "/" { token Token.Divide }
  <indented> "if" { token Token.If }
  <indented> "then" { token Token.Then }
  <indented> "else" { token Token.Else }
  <indented> "for" { token Token.For }
  <indented> "do" { token Token.Do }
  <indented> "block" { token Token.Block }
  <indented> "class" { token Token.Class }
  <indented> "struct" { token Token.Struct }
  <indented> "module" { token Token.Module }
  <indented> "exists" { token Token.Exists }
  <indented> "lambda" { token Token.Lambda }
  <indented> "false" { token Token.False }
  <indented> "true" { token Token.True }
  <indented> "truth" { token Token.Truth }
  <indented> [0-9]+ { int }
  <indented> [0-9]+"."[0-9]+ { float }
  <indented> "fail" { token Token.Fail }
  <indented> "all" { token Token.All }
  <indented> "one" { token Token.One }
  <indented> "not" { token Token.Not }
  <indented> $alpha $alnum* { name }

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
newline = action newline'

newline' :: Lexer (L Token)
newline' = do
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
    pure $ L (Loc i j) Token.Dedent
  else if y `isPrefixOf` x then do
    pushStates indented
    getToken
  else
    throwError' $ IndentError i x y

emptyNesting :: Action
emptyNesting i j _ _ = do
  popStates
  pushIndents =<< getIndent
  putIndent []
  pushStates nested
  pure $ L (Loc i j) Token.Indent

newlineIndented :: Action
newlineIndented = action $ do
  popStates
  newline'

colon :: Action
colon i j _ _ = do
  popStates
  pushStates nesting
  pure $ L (Loc i j) Token.Colon

equals :: Action
equals i j _ _ = do
  popStates
  pushStates nesting
  pure $ L (Loc i j) Token.Equals

colonEquals :: Action
colonEquals i j _ _ = do
  popStates
  pushStates nesting
  pure $ L (Loc i j) Token.ColonEquals

fatArrow :: Action
fatArrow i j _ _ = do
  popStates
  pushStates nesting
  pure $ L (Loc i j) Token.FatArrow

token :: Token -> Action
token x i j _ _ = do
  popStates
  pure $ L (Loc i j) x

int :: Action
int i j n xs = do
  popStates
  int' i j n xs

int' :: Action
int' i j n xs =
  pure . L (Loc i j) . Token.Int .
  ByteString.foldl' f 0 $ ByteString.take n xs
  where
    f z x = z * 10 + (toInteger $ x - ord' '0')

float :: Action
float i j n xs = do
  popStates
  float' i j n xs

float' :: Action
float' i j n xs =
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
name i j n xs = do
  popStates
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
