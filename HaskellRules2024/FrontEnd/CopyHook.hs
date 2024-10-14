-- Copy 'hooks/pre-push' into '.git/hooks/pre-push'.
-- Git does check outy files in the .git directory,
-- so we have to get it there by other means.
-- Do it by having 'cabal' trying to copy it on every
-- invokation.

module FrontEnd.CopyHook(copyHook) where
import Control.Monad
import System.Directory

copyHook :: IO ()
copyHook = do
  done1 <- copyHook' ".."          -- most likely place for cabal invokation is HaskellRules2024
  when (not done1) $ do
    done2 <- copyHook' "../.."     -- but maybe we are in a sub-directory    
    when (not done2) $ do
      _ <- copyHook' "."           -- or up one level
      return ()

copyHook' :: FilePath -> IO Bool
copyHook' prefix = do
  let gdir  = prefix ++ "/.git/hooks"
      ghook = gdir ++ "/pre-push"
      hook  = prefix ++ "/hooks/pre-push"
  g <- doesFileExist ghook
  if not g then do
    -- The pre-push hook does not exists
    d <- doesDirectoryExist gdir
    h <- doesFileExist hook
    if d && h then do
      -- We are in the right place in the tree to copy,
      -- and there is a hook to copy.
--      print (hook, ghook)
      copyFile hook ghook
      return True        -- hook copied
     else
      return False       -- could not copy hook
   else
    return True          -- hook already exists
