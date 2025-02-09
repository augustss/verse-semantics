{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Verse.CPS.Convert
  ( Result (..)
  , convert
  ) where

import Loc

import Verse.CPS.Exp (Label)
import Verse.CPS.Exp qualified as CPS
import Verse.Exp

data Result = Result
  { exp :: CPS.LExp
  , state :: {-# UNPACK #-} !CPS.Label
  , succeed :: {-# UNPACK #-} !CPS.Label
  , fail :: {-# UNPACK #-} !CPS.Label
  }

data Arg = Arg
  { state :: {-# UNPACK #-} !CPS.Label
  , succeed :: {-# UNPACK #-} !CPS.Label
  , fail :: {-# UNPACK #-} !CPS.Label
  , empty :: {-# UNPACK #-} !CPS.Label
  }

newtype Convert a = Convert
  { unConvert :: In -> Out a
  }

type In = Label

data Out a = Out !a {-# UNPACK #-} !Label deriving (Functor)

instance Functor Convert where
  fmap f x = Convert $ \ s -> case unConvert x s of
    Out x s -> Out (f x) s

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
  state <- newLabel
  succeed <- newLabel
  fail <- newLabel
  let empty = fail
  exp <- convert' x Arg {..}
  pure Result {..}

convert' :: LExp -> Arg -> Convert CPS.LExp
convert' (L loc e) arg = case e of
  Var x ->
    pure . L loc $
    CPS.AppSucceed arg.succeed (CPS.Var x) arg.state arg.fail
  Abs x e -> do
    f <- newLabel
    state <- newLabel
    succeed <- newLabel
    fail <- newLabel
    empty <- newLabel
    e <- convert' e Arg {..}
    pure . L loc $
      CPS.Let f x state succeed fail empty e . L loc $
      CPS.AppSucceed arg.succeed (CPS.Label f) arg.state arg.fail
  App f x -> seq' f x arg $ \ f x arg ->
    pure . L loc $ CPS.App f (CPS.Label x)
      arg.state
      arg.succeed
      arg.fail
      arg.empty
  Exi x e ->
    L loc . CPS.Exi x <$> convert' e arg
  Int x ->
    pure . L loc $
    CPS.AppSucceed arg.succeed (CPS.Int x) arg.state arg.fail
  e1 :& e2 ->
    seq_ e1 e2 arg
  e1 := e2 -> seq' e1 e2 arg $ \ x1 x2 arg ->
    pure . L loc $ CPS.Eq (CPS.Label x1) (CPS.Label x2)
      arg.state
      arg.succeed
      arg.fail
      arg.empty
  e1 :< e2 -> seq' e1 e2 arg $ \ x1 x2 arg ->
    pure . L loc $ CPS.Less (CPS.Label x1) (CPS.Label x2)
      arg.state
      arg.succeed
      arg.fail
      arg.empty
  e1 :| e2 -> do
    fail <- newLabel
    empty <- newLabel
    e1 <- convert' e1 arg { fail, empty }
    e2 <- convert' e2 arg
    pure . L loc . CPS.LetFail fail e2 . L loc $ CPS.LetFail empty e2 e1
  e1 :+ e2 -> seq' e1 e2 arg $ \ x1 x2 arg ->
    pure . L loc $ CPS.Plus (CPS.Label x1) (CPS.Label x2)
      arg.state
      arg.succeed
      arg.fail
  e1 :- e2 -> seq' e1 e2 arg $ \ x1 x2 arg ->
    pure . L loc $ CPS.Minus (CPS.Label x1) (CPS.Label x2)
      arg.state
      arg.succeed
      arg.fail
  Fail ->
    pure . L loc $
    CPS.AppFail arg.fail

seq'
  :: LExp -> LExp -> Arg
  -> (Label -> Label -> Arg -> Convert CPS.LExp)
  -> Convert CPS.LExp
seq' e1 e2 arg f = do
  succeed1 <- newLabel
  x1 <- newLabel
  state1 <- newLabel
  fail1 <- newLabel
  succeed2 <- newLabel
  x2 <- newLabel
  state2 <- newLabel
  fail2 <- newLabel
  e1 <- convert' e1 arg
    { succeed = succeed1
    , empty = arg.empty
    }
  e2 <- convert' e2 arg
    { succeed = succeed2
    , state = state1
    , fail = fail1
    , empty = arg.empty
    }
  e3 <- f x1 x2 arg
    { state = state2
    , fail = fail2
    , empty = arg.empty
    }
  pure . L (extract e1) $ CPS.LetSucceed succeed1 x1
    state1
    fail1
    (L (extract e2) $ CPS.LetSucceed succeed2 x2 state2 fail2 e3 e2)
    e1

seq_
  :: LExp -> LExp -> Arg
  -> Convert CPS.LExp
seq_ e1 e2 arg = do
  succeed1 <- newLabel
  x1 <- newLabel
  state1 <- newLabel
  fail1 <- newLabel
  e1 <- convert' e1 arg
    { succeed = succeed1
    , empty = arg.empty
    }
  e2 <- convert' e2 arg
    { state = state1
    , fail = fail1
    , empty = arg.empty
    }
  pure . L (extract e1) $ CPS.LetSucceed succeed1 x1
    state1
    fail1
    e2
    e1
