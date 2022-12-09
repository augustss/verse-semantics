{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Parse.Exp
  ( Exp (..)
  ) where

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
  | If (f (Exp f a))
  | IfThen (f (Exp f a)) (f (Exp f a))
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | For (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Block (f (Exp f a))
  | Exists (f a) (f (Exp f a))
  | Invoke (f (Exp f a)) (f (Exp f a))
  | Lambda (f a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | True
  | False
  | Int Integer
  | Float Double
  | Name a
  | PrefixColon (f (Exp f a))
  | InfixColon (f a) (f (Exp f a))
  | InfixColonEqual (f a) (f (Exp f a))
  | IsInt (f (Exp f a))  -- LA: temporary: test if it's an integer

deriving instance (Show (f (Exp f a)), Show (f a), Show a) => Show (Exp f a)
