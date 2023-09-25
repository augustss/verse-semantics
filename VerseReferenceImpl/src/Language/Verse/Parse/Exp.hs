{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Parse.Exp
  ( Exp (..)
  , Pat (..)
  ) where

import Language.Verse.Name

data Exp f a
  = f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :<>: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | f (Exp f a) :.: {-# UNPACK #-} !Name
  | f (Exp f a) :..: f (Exp f a)
  | f (Exp f a) :<: f (Exp f a)
  | f (Exp f a) :<=: f (Exp f a)
  | f (Exp f a) :>: f (Exp f a)
  | f (Exp f a) :>=: f (Exp f a)
  | PrefixPlus (f (Exp f a))
  | f (Exp f a) :+: f (Exp f a)
  | PrefixMinus (f (Exp f a))
  | f (Exp f a) :-: f (Exp f a)
  | f (Exp f a) :*: f (Exp f a)
  | f (Exp f a) :/: f (Exp f a)
  | f (Exp f a) :->: f (Exp f a)
  | List [f (Exp f a)]
  | f (Exp f a) `Where` f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | PrefixBracket (f (Exp f a))
  | PrefixQuery (f (Exp f a))
  | Query (f (Exp f a))
  | Module (f (Exp f a))
  | Struct (f (Exp f a))
  | Class (Maybe (f (Exp f a))) (f (Exp f a))
  | Enum [a]
  | Inst (f (Exp f a)) (f (Exp f a))
  | If (f (Exp f a))
  | IfThen (f (Exp f a)) (f (Exp f a))
  | IfElse (f (Exp f a)) (f (Exp f a))
  | IfThenElse (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | For (f (Exp f a))
  | ForDo (f (Exp f a)) (f (Exp f a))
  | Block (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Exists (f a)
  | Set (f a) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Option (f (Exp f a))
  | True
  | False
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Fun (f (Exp f a)) (f (Exp f a))
  | InfixColonEqual (f (Pat f a)) (f (Exp f a))
  | Pat (Pat f a)

deriving instance ( Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)

data Pat f a
  = Name a
  | Var (f a) (f (Exp f a))
  | PrefixColon (f (Exp f a))
  | InfixColon (f (Pat f a)) (f (Exp f a))
  | InfixArrow (f (Pat f a)) (f (Pat f a))
  | Invoke (f (Pat f a)) (f (Exp f a))

deriving instance ( Show (f (Exp f a))
                  , Show (f (Pat f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Pat f a)
