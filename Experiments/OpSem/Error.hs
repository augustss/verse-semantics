module OpSem.Error(internalError, explicitError, wrong) where
import GHC.Stack

internalError :: HasCallStack => String -> a
internalError s = error $ "internal error: " ++ s

explicitError :: HasCallStack => String -> a
explicitError s = error $ "internal error: " ++ s

wrong :: HasCallStack => String -> a
wrong s = error $ "WRONG error: " ++ s
