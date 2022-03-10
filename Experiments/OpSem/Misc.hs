{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- Grab bag of random stuff.
module OpSem.Misc(module OpSem.Misc) where
import Data.List
import qualified Data.Map as M
import GHC.Stack
import Text.PrettyPrint.HughesPJClass

assert :: HasCallStack => String -> Bool -> a -> a
assert s False _ = error $ "assert: " ++ s
assert _ True  a = a

assertM :: (HasCallStack, Monad m) => String -> Bool -> m ()
assertM s False = error $ "assert: " ++ s
assertM _ True  = pure ()

{- XXX This is what I'd like to do, but I can't figure out how.
import Control.Monad.Extra(concatMapM)
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = concat <$> mapM f as
-}

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

showListWith :: (a -> String) -> [a] -> String
showListWith f = ("[" ++) . (++ "]") . intercalate "," . map f

instance (Pretty k, Pretty v) => Pretty (M.Map k v) where
  pPrint = pPrint . M.toList

