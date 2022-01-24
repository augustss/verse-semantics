--
-- Very simple test framework.
-- A test is an Ex, which contains a name, a test to compute,
-- and a reference value.  If the reference value is Nothing the test
-- is not expected to terminate gracefully.
-- Both exceptions a timeout map to Nothing.
-- To run a test use testEx.
--
-- Examples:
--   Ex "good" (Just 3) (1+2)
--   Ex "bad" Nothing (error "boo")
--   Ex "worse" Nothing (let loop () = loop () in loop ())
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ex(Ex(..), testEx) where
import Control.DeepSeq
import Control.Exception
import System.Timeout

limit :: Int
limit = 100000  -- us that the test is allowed to take before timeout

data Ex a = Ex
  { name :: String,
    ref :: Maybe a,
    test :: a
  }
  deriving (Show)

testEx :: (Eq a, NFData a, Show a) => Ex a -> IO ()
testEx Ex{..} = do
  let exio = evaluate $ force test
  res <- catch (timeout limit exio) (\ (_ :: SomeException) -> return Nothing)
  putStrLn $ name ++ " " ++ if res == ref then "OK" else "failed: " ++ show res

