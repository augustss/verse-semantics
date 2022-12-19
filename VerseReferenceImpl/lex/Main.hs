{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Comonad

import Data.ByteString qualified as ByteString
import Data.Functor

import Language.Verse.Lexer
import Language.Verse.Token

main :: IO ()
main = print . runLexer (reverse <$> loop []) =<< ByteString.getContents
  where
    loop xs = getToken <&> extract >>= \ case
      EOF -> pure xs
      x -> loop $ x:xs
