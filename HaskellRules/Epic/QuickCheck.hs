module Epic.QuickCheck
  ( generateOne
  )
 where

import Test.QuickCheck( Gen, generate, resize )
import System.Random
import System.IO.Unsafe( unsafePerformIO ) -- sigh...

-- this module provides a function that should have been part of QuickCheck but isn't
-- "luckily" we can implement it ourselves using unsafePerformIO

generateOne :: Gen a -> a
generateOne gen = unsafePerformIO $
  do rnd <- getStdGen
     setStdGen (mkStdGen 17)
     x <- generate (resize 0 gen)
     setStdGen rnd
     return x
