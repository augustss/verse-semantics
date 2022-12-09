{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  ) where

import Data.HashSet (HashSet)

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :<>: f (Exp f a)
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
  | IfThenElse !(HashSet a) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(HashSet a) (f (Exp f a)) (f (Exp f a))
  | Exists (f a) (f (Exp f a))
  | Invoke (f (Exp f a)) (f (Exp f a))
  | Lambda (f a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Int Integer
  | Float Double
  | Name a
  | Colon (f (Exp f a))

deriving instance (Show (f (Exp f a)), Show (f a), Show a) => Show (Exp f a)
