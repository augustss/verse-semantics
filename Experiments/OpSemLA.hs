{-# OPTIONS_GHC -Wall #-}
module Main(
  module Ex,
  module OpSem.Comp,
  module OpSem.Eval,
  module OpSem.Exp,
  module OpSem.Misc,
  module OpSem.Op,
  module OpSem.Tests,
  main,
  ) where
import Ex
import OpSem.Comp
import OpSem.Eval
import OpSem.Exp
import OpSem.Misc
import OpSem.Op
import OpSem.Tests

main :: IO ()
main = testAll
