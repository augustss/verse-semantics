module FrontEnd.Error(module FrontEnd.Error) where
import GHC.Stack
import Epic.Print

unimplemented :: (HasCallStack) => String -> a
unimplemented s = error $ "unimplemented: " ++ s

impossible :: (HasCallStack, Show a) => a -> b
impossible a = error $ "impossible: " ++ show a

internalError :: (HasCallStack) => a
internalError = error "internalError"

internalErrorMsg :: (HasCallStack) => String -> a
internalErrorMsg s = error $ "internalError: " ++ s

syntaxError :: (HasCallStack, Pretty loc) => loc -> String -> a
syntaxError l s = error $ prettyShow l ++ " syntax error: " ++ s

assert :: (HasCallStack) => Bool -> String -> a -> a
assert True _ a = a
assert False s _ = error $ "assert: " ++ s
