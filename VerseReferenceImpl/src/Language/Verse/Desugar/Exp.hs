{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Desugar.Exp
  ( Exp (..)
  ) where

import Data.HashMap.Strict (HashMap)

import Language.Verse.Label
import Language.Verse.Name
import Language.Verse.Ident (Ident)

data Exp f a
  = f (Exp f a) :*>: f (Exp f a)
  | f (Exp f a) :=: f (Exp f a)
  | f (Exp f a) :.: !Name
  | f (Exp f a) :..: f (Exp f a)
  | f (Exp f a) :|: f (Exp f a)
  | Fail
  | One (f (Exp f a))
  | All (f (Exp f a))
  | Not (f (Exp f a))
  | Query (f (Exp f a))
  | Module {-# UNPACK #-} !Label !(HashMap a Bool) (f (Exp f a))
  | Struct {-# UNPACK #-} !Label !(HashMap a Bool) (f (Exp f a))
  | Class {-# UNPACK #-} !Label (Maybe (f (Exp f a))) !(HashMap a Bool) (f (Exp f a))
  | Inst (f (Exp f a)) !(HashMap a Bool) (f (Exp f a))
  | Enum {-# UNPACK #-} !Label !(HashMap a Bool) [Ident] [(f (Exp f a))] -- [Ident] and [(f (Exp f a))] are in "definition order" this is important for pretty printing and :enum
  | EnumValue {-# UNPACK #-} !Label !Integer
  | IfThenElse !(HashMap a Bool) (f (Exp f a)) (f (Exp f a)) (f (Exp f a))
  | ForDo !(HashMap a Bool) (f (Exp f a)) (f (Exp f a))
  | Exists (f a) (f (Exp f a))
  | Var (f a) (f (Exp f a))
  | Set (f a) (f (Exp f a))
  | Fun !(HashMap a Bool) (f (Exp f a)) (f (Exp f a))
  | ParenInvoke (f (Exp f a)) (f (Exp f a))
  | BracketInvoke (f (Exp f a)) (f (Exp f a))
  | Tuple [f (Exp f a)]
  | Truth (f (Exp f a))
  | Option (f (Exp f a))
  | Int !Integer
  | Float {-# UNPACK #-} !Double
  | Name a
  | IfArchetypeName a a (f (Exp f a)) (f (Exp f a))
  | ArchetypeName a

deriving instance ( Show (f (Exp f a))
                  , Show (f a)
                  , Show a
                  ) => Show (Exp f a)
