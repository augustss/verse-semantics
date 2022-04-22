module Error where

import GHC.Stack

unimplemented :: (HasCallStack) => a
unimplemented = error "unimplemented"

impossible :: (HasCallStack) => a
impossible = error "impossible"

internalError :: (HasCallStack) => a
internalError = error "internalError"

internalErrorMsg :: (HasCallStack) => String -> a
internalErrorMsg s = error $ "internalError: " ++ s

syntaxError :: (HasCallStack) => String -> a
syntaxError s = error $ "syntax error: " ++ s
