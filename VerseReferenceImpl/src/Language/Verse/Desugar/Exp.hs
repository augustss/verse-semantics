{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  ) where

import Data.HashMap.Strict (HashMap)

import Language.Verse.Label
import Language.Verse.Name

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: !Name
  | f (Exp f a) :..: f (Exp f a)
  | f (Exp f a) :<: f (Exp f a)
  | f (Exp f a) :<=: f (Exp f a)
  | f (Exp f a) :>: f (Exp f a)
  | f (Exp f a) :>=: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | f (Exp f a) :+: f (Exp f a)
  | f (Exp f a) :-: f (Exp f a)
  | f (Exp f a) :*: f (Exp f a)
  | f (Exp f a) :/: f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Query (f (Exp f a))
  | Module !Label !(HashMap a Bool) (f (Exp f a))
  | Struct !Label !(HashMap a Bool) (f (Exp f a))
  | Class !Label (Maybe (f (Exp f a))) !(HashMap a Bool) (f (Exp f a))
  | Inst (f (Exp f a)) !(HashMap a Bool) (f (Exp f a))
  | IfThenElse !(HashMap a Bool) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(HashMap a Bool) (f (Exp f a)) (f (Exp f a))
  | Exists (f a) (f (Exp f a))
  | Var (f a) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | Function !(HashMap a Bool) (f (Exp f a)) (f (Exp f a))
  | Invoke (f (Exp f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int Integer
  | Float Double
  | Name a
  | Default (f a) (f (Exp f a)) (f (Exp f a))
  | IsInt (f (Exp f a))

deriving instance (Show (f (Exp f a)), Show (f a), Show a) => Show (Exp f a)
