{-# LANGUAGE LambdaCase #-}
module Machine.CESK
  ( Exp (..)
  , Val (..)
  , Name
  , State
  , Addr
  , Env
  , Store
  , Storable (..)
  , Cont (..)
  , step
  , eval
  ) where

import Control.Monad.Except
import Control.Monad.Supply

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Env
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as Store
import Data.Text (Text)

data Exp
  = Var {-# UNPACK #-} !Name
  | App Exp Exp
  | Val Val deriving Show

data Val
  = Fun Name Exp deriving Show

type Name = Text

type State = (Exp, Env, Store, Cont)

type Env = HashMap Name Addr

type Store = IntMap Storable

data Storable = Storable !Val !Env

type Addr = Int

data Cont
  = Mt
  | Ar Exp Env Cont
  | Fn Val Env Cont deriving Show

data Result
  = NameError {-# UNPACK #-} !Name
  | AddrError {-# UNPACK #-} !Addr
  | Halt Val deriving Show

step :: ( MonadError Result m
        , MonadSupply Addr m
        ) => State -> m State
step = \ case
  (Var x, env, store, k) -> case Env.lookup x env of
    Nothing -> throwError $ NameError x
    Just addr -> case Store.lookup addr store of
      Nothing -> throwError $ AddrError addr
      Just (Storable v env') -> pure (Val v, env', store, k)
  (App e0 e1, env, store, k) ->
    pure (e0, env, store, Ar e1 env k)
  (Val v, env, store, Ar e env' k) ->
    pure (e, env', store, Fn v env k)
  (Val v, env, store, Fn (Fun x e) env' k) -> do
    addr <- supply
    pure (e, Env.insert x addr env', Store.insert addr (Storable v env) store, k)
  (Val v, _, _, Mt) ->
    throwError $ Halt v

eval :: Exp -> Result
eval = runSupply . loop . (, mempty, mempty, Mt)
  where
    loop s = runExceptT (step s) >>= \ case
      Left x -> pure x
      Right s -> loop s
