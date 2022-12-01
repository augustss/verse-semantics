{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Debug.Traceable
  ( trace
  , traceId
  , trace1
  , traceA
  , traceA1
  , Traceable (..)
  , Traceable1 (..)
  , debug1
  , debugs1
  , unsafeDebugsRef
  , debugsStableName
  ) where

import Control.Monad.Ref

import Data.IORef

import Debug.Trace qualified as Debug

import System.IO.Unsafe
import System.Mem.StableName

import Unsafe.Coerce

trace :: Traceable a => a -> b -> b
-- trace = Debug.trace . debug
trace = flip const

traceId :: Traceable a => a -> a
traceId x = trace x x

trace1 :: (Traceable1 f, Traceable a) => f a -> b -> b
trace1 = trace . debug1

traceA :: (Traceable a, Applicative m) => a -> m ()
traceA = flip trace $ pure ()

traceA1 :: (Traceable1 f, Traceable a, Applicative m) => f a -> m ()
traceA1 = flip trace1 $ pure ()

class Traceable a where
  debug :: a -> String
  debug = flip debugs ""
  debugs :: a -> ShowS
  debugs = (++) . debug
  debugList :: [a] -> ShowS
  debugList [] z = "[]" ++ z
  debugList (x:xs) z = '[' : debugs x (loop xs)
    where
      loop = \ case
        [] -> ']' : z
        y:ys -> ',' : debugs y (loop ys)

instance Traceable a => Traceable [a] where
  debugs = debugList

instance Traceable Char where
  debugs = shows
  debugList = (++)

instance Traceable Int where
  debugs = shows

instance Traceable Word where
  debugs = shows

class Traceable1 f where
  liftDebugs :: (a -> ShowS) -> f a -> ShowS

debug1 :: (Traceable1 f, Traceable a) => f a -> String
debug1 = flip debugs1 ""

debugs1 :: (Traceable1 f, Traceable a) => f a -> ShowS
debugs1 = liftDebugs debugs

unsafeDebugsRef :: forall f m a . Traceable a => f m -> Ref m a -> ShowS
unsafeDebugsRef _ x =
  debugs "(Ref " .
  debugsStableName x .
  debugs " " .
  debugs (unsafePerformIO $ readRef (unsafeCoerce x :: IORef a)) .
  debugs ")"

debugsStableName :: a -> ShowS
debugsStableName x =
  debugs "#" .
  debugs (hashStableName . unsafePerformIO $ makeStableName x)
