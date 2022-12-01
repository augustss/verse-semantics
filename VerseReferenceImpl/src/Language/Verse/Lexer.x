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
import Control.Monad.State
import Control.Monad.Trans.Except (Except, runExcept)

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Internal qualified as ByteString (w2c)
import Data.Char (ord)
import Data.Function
import Data.Ratio
import Data.Text.Encoding qualified as Text
import Data.Word

import Language.Verse.Error
import Language.Verse.Loc
import Language.Verse.Pos as Pos
import Language.Verse.Token (Token)
import Language.Verse.Token qualified as Token
}

$alpha = [A-Za-z\_]
$alnum = [A-Za-z\_0-9]

:-
  $white+ ;
  "#".* ;
  "<#"[.\n]*"#>" ;
  "(" { token Token.LeftParen }
  ")" { token Token.RightParen }
  "{" { token Token.LeftBrace }
  "}" { token Token.RightBrace }
  ";" { token Token.Semi }
  ":" { token Token.Colon }
  "," { token Token.Comma }
  "." { token Token.Dot }
  "=" { token Token.Equals }
  "|" { token Token.Pipe }
  ":=" { token Token.ColonEquals }
  "->" { token Token.ThinArrow }
  "=>" { token Token.FatArrow }
  "?" { token Token.QuestionMark }
  "+" { token Token.Plus }
  "-" { token Token.Minus }
  "*" { token Token.Multiply }
  "/" { token Token.Divide }
  "if" { token Token.If }
  "then" { token Token.Then }
  "else" { token Token.Else }
  "for" { token Token.For }
  "do" { token Token.Do }
  "block" { token Token.Block }
  "class" { token Token.Class }
  "struct" { token Token.Struct }
  "module" { token Token.Module }
  "exists" { token Token.Exists }
  "lambda" { token Token.Lambda }
  "false" { token Token.False }
  "true" { token Token.True }
  "truth" { token Token.Truth }
  [0-9]+ { int }
  [0-9]+"."[0-9]+ { float }
  "fail" { token Token.Fail }
  "all" { token Token.All }
  "one" { token Token.One }
  "not" { token Token.Not }
  $alpha $alnum* { name }

{
newtype Lexer a = Lexer
  { getLexer :: StateT AlexInput (Except Error) a
  } deriving (Functor, Applicative, Monad)

runLexer :: Lexer a -> ByteString -> Either Error a
runLexer m = runExcept . evalStateT (getLexer m) . AlexInput Pos.minBound

getToken :: Lexer (L Token)
getToken = Lexer $ fix $ \ recur -> do
  s <- get
  case alexScan s 0 of
    AlexEOF ->
      pure $ L (Loc s.pos s.pos) Token.EOF
    AlexError AlexInput { pos } ->
      Except.throwError $ LexError pos
    AlexSkip s _ -> do
      put s
      recur
    AlexToken s' n f -> do
      put s'
      pure . (L $ Loc s.pos s'.pos) $ f n s.input

token :: Token -> Int -> ByteString -> Token
token x _ _ = x

int :: Int -> ByteString -> Token
int n xs =
  Token.Int . ByteString.foldl' f 0 $
  ByteString.take n xs
  where
    f z x = z * 10 + (toInteger $ x - ord' '0')

float :: Int -> ByteString -> Token
float n xs =
  Token.Float . toRational . ByteString.foldl' f (0, 0, 0) $
  ByteString.take n xs
  where
    f (!z0, !z1, !n1) x
      | x == ord' '.' = (z1, z0, 1)
      | otherwise = (z0, z1 * 10 + (toInteger $ x - ord' '0'), n1 * 10)
    toRational (z0, z1, n1) =
      (z0 * n1 + z1) % n1

ord' :: Char -> Word8
ord' = fromIntegral . ord

name :: Int -> ByteString -> Token
name n xs =
  Token.Name . Text.decodeUtf8 $
  ByteString.take n xs

throwError :: Loc -> Token -> Lexer a
throwError loc = Lexer . Except.throwError . ParseError loc

data AlexInput = AlexInput
  { pos :: !Pos
  , input :: !ByteString
  }

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
