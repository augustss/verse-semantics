module Language.Verse.Function
  ( Function (..)
  ) where

import Language.Verse.Ident
import Language.Verse.Label
import Language.Verse.Name

data Function f a = Function
  !Label
  !(IdentMap Name (f a))
  !(IdentMap Name Bool)
  Exp
  Exp deriving (Show, Functor, Foldable, Traversable)

instance Eq (Function f a) where
  Function x _ _ _ _ == Function y _ _ _ _ = x == y

type Exp = L (Desugar.Exp L (Ident Name))
