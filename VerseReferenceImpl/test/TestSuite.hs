{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad.Trans.Except

import Data.ByteString qualified as ByteString
import Data.Functor
import Data.List

import Language.Verse

import Prettyprinter

import System.Directory
import System.FilePath
import System.IO.Error

import Test.HUnit

main :: IO ()
main = runTestTTAndExit =<< getTest

getTest :: IO Test
getTest = do
  filePaths <- listDirectory "test"
  let verseFiles = sort $ filter ((== ".verse") . takeExtension) filePaths
  pure . TestList $ verseFiles <&> \ verseFile -> TestCase $ do
    ByteString.readFile ("test" </> verseFile) >>= runExceptT . eval >>= \ case
      Left e -> do
        let errFile = replaceExtension verseFile "err"
        expected <- (readFile $ "test" </> errFile) `catchIOError` \ e ->
          if isDoesNotExistError e then pure "" else ioError e
        let actual = show $ pretty e <> line
        assertEqual errFile expected actual
      Right xs -> do
        let outFile = replaceExtension verseFile "out"
        expected <- (readFile $ "test" </> outFile) `catchIOError` \ e ->
          if isDoesNotExistError e then pure "" else ioError e
        let actual = show $ foldr (\ x z -> pretty x <> line <> z) mempty xs
        assertEqual outFile expected actual
