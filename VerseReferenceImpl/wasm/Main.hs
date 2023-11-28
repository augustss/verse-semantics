{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Exception
import Control.Monad.Supply
import Control.Monad.Trans.Except

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Functor
import Data.Text (Text)
import Data.Text.Foreign qualified as Text

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Language.Verse qualified as Verse
import Language.Verse.Error
import Language.Verse.Val (FrozenVal)

import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = pure ()

foreign export ccall "verse_eval" eval
  :: Ptr CChar
  -> Int
  -> Ptr (Ptr CChar)
  -> IO Int

eval :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
eval inPtr n outPtrPtr =
  unsafePackCStringLen (inPtr, n) >>= eval' >>= \ xs ->
  Text.withCStringLen xs $ \ (ptr, n) -> do
    outPtr <- flip reallocBytes n =<< peek outPtrPtr
    poke outPtrPtr outPtr
    copyBytes outPtr ptr n
    pure n

eval' :: ByteString -> IO Text
eval' xs = catch' $ eval'' xs <&> renderStrict . layoutSmart layoutOptions . \ case
  Left e -> pretty e
  Right xs -> vsep $ pretty <$> xs

eval'' :: ByteString -> IO (Either Error [FrozenVal])
eval'' = runExceptT . runSupplyT . Verse.eval2 "<web page>"

catch' :: IO Text -> IO Text
catch' m = m `catch` \ (_ :: SomeException) ->
  pure . renderStrict . layoutSmart layoutOptions $ "internal" <+> "error"

layoutOptions :: LayoutOptions
layoutOptions = defaultLayoutOptions
  { layoutPageWidth = AvailablePerLine 60 1.0
  }

foreign export ccall "calloc_ptr" callocPtr :: IO (Ptr (Ptr a))

callocPtr :: IO (Ptr (Ptr a))
callocPtr = calloc
