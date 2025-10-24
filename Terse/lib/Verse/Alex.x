{
module Verse.Core.Alex
  ( AlexInput
  , AlexReturn (..)
  , alexScan
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Word

import Verse.Token
}

%encoding "utf-8"

:-
<0> $white ;
<0> "fail" { Fail }

{
type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = ByteString.uncons
}
