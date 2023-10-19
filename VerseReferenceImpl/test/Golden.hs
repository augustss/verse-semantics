{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Except
import Control.Monad.Verse (runVerseT)

import Data.ByteString qualified as ByteString
import Data.List
import Data.Traversable

import Language.Verse
import Language.Verse.Error
import Language.Verse.Mode

import Prettyprinter

import System.Directory
import System.FilePath
import System.IO.Error

import Test.HUnit

main :: IO ()
main = do
  executionTest <- getTest Execution $ "test" </> "execution"
  verificationTest <- getTest Verification $ "test" </> "verification"
  runTestTTAndExit $ TestList [executionTest, verificationTest]

getTest :: Mode -> FilePath -> IO Test
getTest mode directory = do
  filePaths <- listDirectory' directory
  let verseFiles = sort $ filter ((== ".verse") . takeExtension) filePaths
  pure . TestList $ mkTestCase mode . (directory </>) <$> verseFiles

mkTestCase :: Mode -> FilePath -> Test
mkTestCase mode verseFile = TestLabel verseFile . TestCase $
  ByteString.readFile verseFile >>=
  runExceptT . runSupplyT . runVerseT . eval mode >>= \ case
    Left e -> handleError e
    Right Nothing -> handleError StuckError
    Right (Just xs) -> do
      let outFile = replaceExtension verseFile "out"
      expected <- readFile outFile `catchIOError` \ e ->
        if isDoesNotExistError e then pure "" else ioError e
      let actual = show $ foldr (\ x z -> pretty x <> line <> z) mempty xs
      assertEqual outFile expected actual
  where
    handleError e = do
      let errFile = replaceExtension verseFile "err"
      expected <- readFile errFile `catchIOError` \ e ->
        if isDoesNotExistError e then pure "" else ioError e
      let actual = show $ pretty e <> line
      assertEqual errFile expected actual

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' x = do
  xs <- listDirectory x `catchIOError` const (pure [])
  join <$> (for xs $ \ y -> (y:) <$> fmap (y </>) <$> listDirectory' (x </> y))
