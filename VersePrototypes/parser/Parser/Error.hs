module Parser.Error where

import Epic.Print
import GHC.Stack

-- TODO: Jeff: Remove and exchange for an Epic.Error library or shared util library
-- This is a copy of FrontEnd.Error

unimplemented :: (HasCallStack) => String -> a
unimplemented s = error $ "unimplemented: " ++ s

impossible :: (HasCallStack, Show a) => String -> a -> b
impossible str a = error $ render $
                   sep [ text "impossible:" <+> text str
                       , text (show a) ]

internalError :: (HasCallStack) => a
internalError = error "internalError"

internalErrorMsg :: (HasCallStack) => String -> a
internalErrorMsg s = error $ "internalError: " ++ s

syntaxError :: (HasCallStack, Pretty loc) => loc -> String -> a
syntaxError l s = errorMessage $ "syntax error: " ++ prettyShow l ++ " " ++ s

errorMessage :: (HasCallStack) => String -> a
errorMessage msg = error $ "error: " ++ msg

assert :: (HasCallStack) => Bool -> String -> a -> a
assert True _ a = a
assert False s _ = error $ "assert: " ++ s
