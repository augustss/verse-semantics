module Verse.CPS.Exp
  ( ExpF (..)
  , Exp
  , LExp
  , Val (..)
  , Label
  ) where

import Fix
import Loc

import Verse.Name

data ExpF a
  = Let
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
    a
  | App
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | LetYield
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !Label -- Level
    {-# UNPACK #-} !Label -- Handler
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
  | LetSuccess
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
  | AppSuccess
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Failure continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | LetFailure a
  | AppFailure {-# UNPACK #-} !Label
  | LetEmpty a
  | AppEmpty {-# UNPACK #-} !Label
  | LetEnv {-# UNPACK #-} !Label {-# UNPACK #-} !Label a
  | LetState {-# UNPACK #-} !Label a
  | Exi {-# UNPACK #-} !Name a
  | Tup
    [Val]
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
  | Eq
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Empty continuation
    !Label !Label
  | Less
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | Plus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
  | Minus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Success continuation
    {-# UNPACK #-} !Label -- Failure continuation
    deriving Show

type Exp = Fix ExpF

type LExp = L ExpF

data Val
  = Var {-# UNPACK #-} !Name
  | Label {-# UNPACK #-} !Label
  | Int !Integer deriving Show

type Label = Int
