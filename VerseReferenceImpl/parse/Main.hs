{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where


import Data.ByteString qualified as ByteString
import Data.ByteString(ByteString)
import Data.List(sort)
import Language.Verse.Lexer
import Language.Verse.Parse
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error

data Options = Options {
  verbose :: Bool,
  useShow :: Bool
  }

noOptions :: Options
noOptions = Options {
  verbose = False,
  useShow = False
  }


doParse :: Options -> FilePath -> ByteString -> IO ()
doParse options path contents =
  do
    case runLexer parse contents of
         Right e | verbose options -> do
           hPutStrLn stdout $ "Succeed parsing " ++ path
           if useShow options then
             hPutStr stdout $ show e
           else
             hPutDoc stdout $ pretty e
           hPutStrLn stdout ""
           return ()
         Right _e ->
           return ()
         Left e -> do
           hPutStrLn stderr $ "Failed parsing " ++ path
           hPutDoc stderr $ pretty e
           hPutStrLn stderr ""


doParseFile :: Options -> FilePath -> IO ()
doParseFile options path =
  if (`elem` [".verse", ".versetest"]) $ takeExtension $ path then do
    contents <- ByteString.readFile path
    doParse options path contents
  else do
    paths <- listDirectory' path
    mapM_ (doParseFile options) paths


listDirectory' :: FilePath -> IO [FilePath]
listDirectory' x = do
  xs <- listDirectory x `catchIOError` const (pure [])
  return $ map (x </>) $ sort xs


extractFlags :: Options -> [String] -> (Options, [String])
extractFlags options ("-v":paths)        = extractFlags options{ verbose = True } paths
extractFlags options ("--verbose":paths) = extractFlags options{ verbose = True } paths
extractFlags options ("--show":paths)    = extractFlags options{ verbose = True, useShow = True } paths
extractFlags options paths = (options, paths)

main :: IO ()
main =
  do
    args <- getArgs
    let (options, paths) = extractFlags noOptions args
    case paths of
      [] -> do
        contents <- ByteString.getContents
        doParse options "stdin" contents
      _ ->
        mapM_ (doParseFile options) paths
