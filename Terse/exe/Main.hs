{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer.CPS

import Data.Functor
import Data.Text.IO qualified as Text

import Prettyprinter
import Prettyprinter.Render.Terminal (bold)
import Prettyprinter.Render.Terminal qualified as Terminal

import System.Console.ANSI
import System.Console.Haskeline
import System.Directory
import System.Environment
import System.FilePath
import System.IO

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
      Just (Left e) -> do
        outputErrLn . annotate bold $ "Parse" <+> "error" <> colon <+> pretty e
        loop
      Just (Right (xs, e)) ->
        let
          prettyStuck' = prettyStuck xs
        in do
          lift (Verse.eval e) >>= outputErrLn . \ case
            Right xs -> hcat . punctuate pipe $ pretty <$> xs
            Left xs -> prettyStuck' xs
          loop
      where
        outputErrLn x = lift (hNowSupportsANSI stdout) >>= \ case
          False ->
            outputStrLn $ show x
          True ->
            outputStrLn . Text.unpack . Terminal.renderStrict $
            layoutPretty defaultLayoutOptions x

parseInput :: InputT IO (Maybe (Either String (Text, Verse.LExp)))
parseInput = getInputLine "> " >>= \ case
  Nothing -> pure Nothing
  Just (Text.pack -> xs) -> do
    (x, xs) <- runWriterT $ do
      tell xs
      parseWith (getInputLine' ". ") xs
    pure . Just $ (xs,) <$> x
  where
    getInputLine' xs = lift (getInputLine xs) >>= \ case
      Nothing -> pure mempty
      Just [] -> pure mempty
      Just xs -> tell' . Text.pack $ '\n':xs
    tell' x =
      tell x $> x

parseWith :: Monad m => m Text -> Text -> m (Either String Verse.LExp)
parseWith m = fmap Parse.eitherResult . loop . Verse.parse'
  where
    loop = \ case
      Parse.Partial k -> case k mempty of
        Parse.Fail {} -> loop . k =<< m
        Parse.Partial k -> loop . k =<< m
        r@Parse.Done {} -> pure r
      r -> pure r
