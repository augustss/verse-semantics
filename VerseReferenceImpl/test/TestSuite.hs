{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Except
import Control.Monad.Verse

import Data.ByteString qualified as ByteString
import Data.List
import Data.Traversable

import Language.Verse
import Language.Verse.Pretty

import Prettyprinter

import System.Directory
import System.FilePath
import System.IO.Error

import Test.HUnit

main :: IO ()
main = runTestTTAndExit =<< getTest

getTest :: IO Test
getTest = do
  filePaths <- listDirectory' "test"
  let verseFiles = sort $ filter ((== ".verse") . takeExtension) filePaths
  pure . TestList $ mkTestCase <$> verseFiles

mkTestCase :: FilePath -> Test
mkTestCase verseFile = TestLabel verseFile . TestCase $
  ByteString.readFile ("test" </> verseFile) >>= runExceptT . runSupplyT . runVerseT . (prettyM <=< eval) >>= \ case
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
      let actual = show $ foldr (\ x z -> x <> line <> z) mempty xs
      assertEqual outFile expected actual

listDirectory' :: FilePath -> IO [FilePath]
listDirectory' x = do
  xs <- listDirectory x `catchIOError` const (pure [])
  join <$> (for xs $ \ y -> (y:) <$> fmap (y </>) <$> listDirectory' (x </> y))
