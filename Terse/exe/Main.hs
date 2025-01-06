{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Trans.Class

import Data.Functor
import Data.Text.IO qualified as Text

import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Terminal

import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.FilePath
import System.IO

import Pos
import Loc
import Text (Text)
import Text qualified

import Verse qualified
import Verse.Exp qualified as Verse
import Verse.Eval qualified as Verse
import Verse.Parse qualified as Parse
import Verse.Parse qualified as Verse (parse')

main :: IO ()
main = getArgs >>= \ case
  input:_ -> openFile input ReadMode >>= run
  [] -> do
    settings <- getHomeDirectory <&> \ homeDirectory -> defaultSettings
      { historyFile = Just $ homeDirectory </> ".terse_history"
      }
    runInputT settings $ haveTerminalUI >>= \ case
      True -> loop
      False -> lift $ run stdin
  where
    run =
      Text.hGetContents >=>
      Verse.run >=>
      either outputErrLn (print . hcat . punctuate pipe . fmap pretty)
      where
        outputErrLn x = hNowSupportsANSI stderr >>= \ case
          False -> hPrint stderr x
          True -> Terminal.hPutDoc stderr $ x <> hardline
    loop = parseInput >>= \ case
      Nothing -> pure ()
      Just (input, Left (pos, ann)) -> do
        outputErrLn $ prettyParseError input pos ann
        loop
      Just (input, Right e) -> do
        lift (Verse.eval e) >>= outputErrLn . \ case
          Right xs -> hcat . punctuate pipe $ pretty <$> xs
          Left xs -> prettyStuckError input xs
        loop
      where
        outputErrLn x = lift (hNowSupportsANSI stdout) >>= \ case
          False ->
            outputStrLn $ show x
          True ->
            outputStrLn . Text.unpack . Terminal.renderStrict $
            layoutPretty defaultLayoutOptions x

parseInput :: InputT IO (Maybe (Text, Either (Pos, [Text]) Verse.LExp))
parseInput = getInputLine "> " >>= \ case
  Nothing -> pure Nothing
  Just (Text.pack -> xs) -> Just <$> parseWith (getInputLine' ". ") xs
  where
    getInputLine' xs = getInputLine xs <&> \ case
      Nothing -> mempty
      Just [] -> mempty
      Just xs -> Text.pack $ '\n':xs

parseWith
  :: Monad m
  => m Text -> Text -> m (Text, Either (Pos, [Text]) Verse.LExp)
parseWith m = loop . Verse.parse'
  where
    loop = \ case
      Parse.Yield f -> case f mempty of
        Parse.Empty {} -> loop . f =<< m
        Parse.Yield f -> loop . f =<< m
        Parse.Pure x input -> pure (input, Right x)
      Parse.Pure x input -> pure (input, Right x)
      Parse.Empty input pos ann -> pure (input, Left (pos, ann))
