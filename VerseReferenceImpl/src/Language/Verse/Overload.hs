{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Overload
  ( Overload (..)
  ) where

import Language.Verse.Desugar.Exp qualified as Desugar
import Language.Verse.Ident
import Language.Verse.Intrinsic
import Language.Verse.Label
import Language.Verse.Loc
import {-# SOURCE #-} Language.Verse.Named (Named)

import Prettyprinter

data Overload m a
  = Function {-# UNPACK #-} !Label !(IdentMap (Named m a)) !(IdentMap Bool) Exp Exp
  | Struct {-# UNPACK #-} !Label !(IdentMap (Named m a)) !(IdentMap Bool) Exp
  | Class {-# UNPACK #-} !Label !(IdentMap (Named m a)) (Maybe a) !(IdentMap Bool) Exp
  | Intrinsic !Intrinsic deriving (Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L Ident)

instance Eq (Overload f a) where
  (==) = curry $ \ case
    (Function x _ _ _ _, Function y _ _ _ _) -> x == y
    (Struct x _ _ _, Struct y _ _ _) -> x == y
    (Class x _ _ _ _, Class y _ _ _ _) -> x == y
    (Intrinsic x, Intrinsic y) -> x == y
    _ -> False

instance Pretty (Overload f a) where
  pretty = \ case
    Function x _ _ _ _ -> "function#" <> prettyLabel x
    Struct x _ _ _ -> "struct#" <> prettyLabel x
    Class x _ _ _ _ -> "class#" <> prettyLabel x
    Intrinsic x -> pretty x
