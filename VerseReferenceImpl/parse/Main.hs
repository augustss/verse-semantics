{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad(when, (<=<))
import Control.Monad.Abort
import Control.Monad.Supply
import Data.ByteString qualified as ByteString
import Data.ByteString(ByteString)
import Data.List(sort, find, isSuffixOf)
import Language.Verse.Desugar qualified as D
import Language.Verse.Mode
import Language.Verse.Parse2  qualified as P2
import Language.Verse.Rewrite qualified as R
import Language.Verse.Error
import Language.Verse.Loc(L(..))
import Language.Verse.Name(Name)
import Language.Verse.Parse.Exp(Exp)
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error

data Options = Options {
  getParser :: String -> ByteString -> Either Error (L (Exp Name)),
  getWorker :: Options -> FilePath -> ByteString -> IO (),
  progress :: Bool,
  verbose :: Bool,
  useShow :: Bool,
  skiplist :: [(String, String)],
  mode :: Mode
  }

noOptions :: Options
noOptions = Options {
  getParser = parser2,
  getWorker = parse,
  progress = False,
  verbose = False,
  useShow = False,
  skiplist = [],
  mode = Execution
  }


-- back to one parser
parser2 :: String -> ByteString -> Either Error (L (Exp Name))
parser2 path contents = P2.parse2 path contents

-- how much to do, parse, rewrite, desugar
parse :: Options -> FilePath -> ByteString -> IO ()
parse options path contents = report options path $ getParser options path contents

rewrite :: Options -> FilePath -> ByteString -> IO ()
rewrite options path contents = report options path $ liftEither . (runSupplyT . R.rewrite <=< getParser options path) $ contents

desugar :: Options -> FilePath -> ByteString -> IO ()
desugar options path contents = report options path $ liftEither . (runSupplyT . (D.desugar (mode options) <=< R.rewrite) <=< getParser options path) $ contents


-- generic reporter for the result
report :: (Show a, Pretty a) => Options -> FilePath -> Either Error a -> IO ()
report options path result = do
  case  result of
    Right e -> do
      when (progress options || verbose options) $ hPutStrLn stdout $ "Done " ++ path
      when (verbose options) $ do
        if useShow options then
          hPutStr stdout $ show e
        else
          hPutDoc stdout $ pretty e
        hPutStrLn stdout ""
    Left e -> do
      hPutStrLn stderr $ "Failed " ++ path
      hPutStr stderr $ show e
      hPutStrLn stderr ""


processFile :: Options -> FilePath -> IO ()
processFile options path =
  if (`elem` [".verse", ".versetest"]) $ takeExtension $ path then do
    case find (\ (skip, _) -> isSuffixOf skip path) (skiplist options) of
      Just (_, why) ->
        hPutStrLn stdout $ "Skipping " ++ path ++ " due to: " ++ why
      Nothing -> do
        contents <- ByteString.readFile path
        (getWorker options) options path contents
  else do
    paths <- listDirectory' path
    mapM_ (processFile options) paths


listDirectory' :: FilePath -> IO [FilePath]
listDirectory' x = do
  xs <- listDirectory x `catchIOError` const (pure [])
  return $ map (x </>) $ sort xs


extractFlags :: Options -> [String] -> IO (Options, [String])
extractFlags options ("-v":paths)        = extractFlags options{ verbose = True } paths
extractFlags options ("--verbose":paths) = extractFlags options{ verbose = True } paths
extractFlags options ("--progress":paths) = extractFlags options{ progress = True } paths
extractFlags options ("--parse":paths) = extractFlags options{ getWorker = parse } paths
extractFlags options ("--rewrite":paths) = extractFlags options{ getWorker = rewrite } paths
extractFlags options ("--show":paths)    = extractFlags options{ verbose = True, useShow = True } paths
extractFlags options ("--skiplist":path:paths) = do
  content <- readFile path
  let sl = read content
  if verbose options then hPutStrLn stdout ("Found skiplist: " ++ show sl) else return ()
  extractFlags options{ skiplist = sl } paths
extractFlags options ("--desugar":paths)  = extractFlags options{ getWorker = desugar } paths
extractFlags options ("--desugar-verification":paths) = extractFlags options{ getWorker = desugar, mode = Verification } paths
extractFlags options ("--desugar-execution":paths) = extractFlags options{ getWorker = desugar, mode = Execution } paths
extractFlags options (flag@('-':_):paths)  =  do
  hPutStrLn stderr $ "Ignoring unknown flag '" ++ flag ++ "'"
  extractFlags options paths
extractFlags options paths = return (options, paths)

main :: IO ()
main =
  do
    args <- getArgs
    (options, paths) <- extractFlags noOptions args
    case paths of
      [] -> do
        contents <- ByteString.getContents
        getWorker options options "stdin" contents
      _ ->
        mapM_ (processFile options) paths
