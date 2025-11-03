module ByteString
  ( module Data.ByteString
  , slice
  , prettyByteString
  , newline
  ) where

import Data.ByteString
import Data.ByteString.Internal (c2w)
import Data.Text.Encoding qualified as Text
import Data.Word

import Prettyprinter

import Prelude (Int, (.))

slice :: Int -> Int -> ByteString -> ByteString
slice i j = drop i . take j

prettyByteString :: ByteString -> Doc ann
prettyByteString = pretty . Text.decodeUtf8Lenient

newline :: Word8
newline = c2w '\n'
