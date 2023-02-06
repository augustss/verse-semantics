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
import Language.Verse.Name
import {-# SOURCE #-} Language.Verse.Named (Named)

import Prettyprinter

data Overload m a
  = Function !Label !(IdentMap Name (Named m a)) !(IdentMap Name Bool) Exp Exp
  | Struct !Label !(IdentMap Name (Named m a)) !(IdentMap Name Bool) Exp
  | Class !Label !(IdentMap Name (Named m a)) (Maybe a) !(IdentMap Name Bool) Exp
  | Intrinsic Intrinsic deriving (Functor, Foldable, Traversable)

type Exp = L (Desugar.Exp L (Ident Name))

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
