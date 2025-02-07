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
    {-# UNPACK #-} !Name -- Parameter
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
    a
  | App
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | LetSucceed
    {-# UNPACK #-} !Label
    {-# UNPACK #-} !Label -- Parameter
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
    a
    a
  | AppSucceed
    {-# UNPACK #-} !Label -- Callee
    {-# UNPACK #-} !Val -- Argument
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | LetFail
    {-# UNPACK #-} !Label
    a
    a
  | AppFail {-# UNPACK #-} !Label
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
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | Less
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- Env
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
    {-# UNPACK #-} !Label -- Empty continuation
  | Plus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
  | Minus
    !Val -- Left
    !Val -- Right
    {-# UNPACK #-} !Label -- State
    {-# UNPACK #-} !Label -- Yield continuation
    {-# UNPACK #-} !Label -- Succeed continuation
    {-# UNPACK #-} !Label -- Fail continuation
  deriving Show

type Exp = Fix ExpF

type LExp = L ExpF

data Val
  = Var {-# UNPACK #-} !Name
  | Label {-# UNPACK #-} !Label
  | Int !Integer deriving Show

type Label = Int
