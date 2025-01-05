module Pos
  ( Pos (..)
  , emptyPos
  ) where

data Pos = Pos
  { indexWord8 :: {-# UNPACK #-} !Int
  , rowIndexWord8 :: {-# UNPACK #-} !Int
  , row :: {-# UNPACK #-} !Int
  , column :: {-# UNPACK #-} !Int
  } deriving Show

emptyPos :: Pos
emptyPos = Pos
  { indexWord8 = 0
  , rowIndexWord8 = 0
  , row = 0
  , column = 0
  }
