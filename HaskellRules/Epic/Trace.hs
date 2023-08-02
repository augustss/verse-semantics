module Epic.Trace(traceLoc) where
import GHC.Stack
import System.IO.Unsafe

traceLoc :: HasCallStack => String -> a -> a
traceLoc msg a = unsafePerformIO $ do
  putStrLn $ "trace: " ++ msg
  putStrLn (prettyCallStack callStack)
  return a
