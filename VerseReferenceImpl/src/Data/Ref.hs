module Data.Ref
  ( EqRef (..)
  ) where

import Data.IORef
import Data.STRef

class EqRef r where
  eqRef :: r a -> r a -> Bool

instance EqRef IORef where
  eqRef = (==)

instance EqRef (STRef s) where
  eqRef = (==)
