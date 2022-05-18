module Error(module Error) where

import GHC.Stack

unimplemented :: (HasCallStack) => a
unimplemented = error "unimplemented"

impossible :: (HasCallStack, Show a) => a -> b
impossible a = error $ "impossible: " ++ show a

internalError :: (HasCallStack) => a
internalError = error "internalError"

internalErrorMsg :: (HasCallStack) => String -> a
internalErrorMsg s = error $ "internalError: " ++ s

syntaxError :: (HasCallStack, Show loc) => loc -> String -> a
syntaxError l s = error $ show l ++ " syntax error: " ++ s

assert :: (HasCallStack) => Bool -> String -> a -> a
assert True _ a = a
assert False s _ = error $ "assert: " ++ s
