{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Verse.CPS.Convert
  ( convert
  ) where

import Loc

import Verse.CPS.Exp (Label)
import Verse.CPS.Exp qualified as CPS
import Verse.Exp

data Result = Result
  { exp :: CPS.LExp
  , env :: {-# UNPACK #-} !CPS.Label
  , state :: {-# UNPACK #-} !CPS.Label
  , yield :: {-# UNPACK #-} !CPS.Label
  , succeed :: {-# UNPACK #-} !CPS.Label
  , fail :: {-# UNPACK #-} !CPS.Label
  }

data Arg = Arg
  { env :: {-# UNPACK #-} !CPS.Label
  , state :: {-# UNPACK #-} !CPS.Label
  , yield :: {-# UNPACK #-} !CPS.Label
  , succeed :: {-# UNPACK #-} !CPS.Label
  , fail :: {-# UNPACK #-} !CPS.Label
  , empty :: {-# UNPACK #-} !CPS.Label
  }

newtype Convert a = Convert
  { unConvert :: In -> Out a
  } deriving (Functor)

type In = Label

data Out a = Out !a {-# UNPACK #-} !Label deriving (Functor)

instance Applicative Convert where
  pure = Convert . Out
  f <*> x = Convert $ \ s -> case unConvert f s of
    Out f s -> case unConvert x s of
      Out x s -> Out (f x) s

instance Monad Convert where
  x >>= f = Convert $ \ s -> case unConvert x s of
    Out x s -> unConvert (f x) s

runConvert :: Convert a -> a
runConvert m = case unConvert m 0 of
  Out x _ -> x

newLabel :: Convert Label
newLabel = Convert $ \ s -> Out s $ s + 1

convert :: LExp -> Result
convert x = runConvert $ do
  env <- newLabel
  state <- newLabel
  yield <- newLabel
  succeed <- newLabel
  fail <- newLabel
  let empty = fail
  exp <- convert' x Arg {..}
  pure Result {..}

convert' :: LExp -> Arg -> Convert CPS.LExp
convert' (L loc e) arg = case e of
  Var x ->
    pure . L loc $
    CPS.AppSuccess arg.succeed (CPS.Var x) arg.state arg.fail arg.empty
