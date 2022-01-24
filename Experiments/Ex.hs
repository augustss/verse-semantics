{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Ex(Ex(..), testEx) where
import Control.DeepSeq
import Control.Exception
import System.Timeout

limit :: Int
limit = 100000  -- us that the test is allowed to take before timeout

data Ex where
  Ex :: (Eq a, NFData a, Show a) =>
    { name :: String,
      ref :: Maybe a,
      test :: a
    } -> Ex

deriving instance Show Ex

testEx :: Ex -> IO ()
testEx Ex{name,ref,test} = do
  let exio = evaluate $ force test
  res <- catch (timeout limit exio) (\ (_ :: SomeException) -> return Nothing)
  putStrLn $ name ++ " " ++ if res == ref then "OK" else "failed: " ++ show res

