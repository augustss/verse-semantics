{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Supply
import Control.Monad.Trans.Except
import Control.Monad.Verse

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringLen)
import Data.Functor
import Data.Text (Text)
import Data.Text.Foreign as Text

import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Language.Verse qualified as Verse
import Language.Verse.Error

import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = pure ()

foreign export ccall "verse_eval" eval :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int

eval :: Ptr CChar -> Int -> Ptr (Ptr CChar) -> IO Int
eval inPtr n outPtrPtr =
  unsafePackCStringLen (inPtr, n) >>= eval' >>= \ xs ->
  Text.withCStringLen xs $ \ (ptr, n) -> do
    outPtr <- flip reallocBytes n =<< peek outPtrPtr
    poke outPtrPtr outPtr
    copyBytes outPtr ptr n
    pure n

eval' :: ByteString -> IO Text
eval' xs = eval'' xs <&> \ case
  Left e -> renderStrict . layoutSmart layoutOptions $ pretty e
  Right xs -> renderStrict . layoutSmart layoutOptions $ vsep xs

eval'' :: ByteString -> IO (Either Error [Doc ann])
eval'' = runExceptT . runSupplyT . runVerseT . Verse.eval >=> \ case
  Right (Just xs) -> pure . Right $ pretty <$> xs
  Right Nothing -> pure $ Left StuckError
  Left e -> pure $ Left e

layoutOptions :: LayoutOptions
layoutOptions = defaultLayoutOptions
  { layoutPageWidth = AvailablePerLine 60 1.0
  }

foreign export ccall "calloc_ptr" callocPtr :: IO (Ptr (Ptr a))

callocPtr :: IO (Ptr (Ptr a))
callocPtr = calloc
