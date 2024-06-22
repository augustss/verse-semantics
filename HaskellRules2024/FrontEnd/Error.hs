module FrontEnd.Error where

import Epic.Print
import GHC.Stack

unimplemented :: (HasCallStack) => String -> a
unimplemented s = error $ "unimplemented: " ++ s

impossible :: (HasCallStack, Show a) => a -> b
impossible a = error $ "impossible: " ++ show a

internalError :: (HasCallStack) => a
internalError = error "internalError"

internalErrorMsg :: (HasCallStack) => String -> a
internalErrorMsg s = error $ "internalError: " ++ s

syntaxError :: (HasCallStack, Pretty loc) => loc -> String -> a
syntaxError l s = errorMessage $ "syntax error: " ++ prettyShow l ++ " " ++ s

errorMessage :: String -> a
errorMessage msg = error $ "error: " ++ msg

assert :: (HasCallStack) => Bool -> String -> a -> a
assert True _ a = a
assert False s _ = error $ "assert: " ++ s
