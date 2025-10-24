{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Verse.Comp.Internal
  ( runCompT
  , comp'
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Env

import Language.Haskell.TH (Q, Quote)
import Language.Haskell.TH qualified as TH

import Loc

import Verse.Exp
import Verse.Monad
import Verse.Name
import Verse.Run
import Verse.Run.Val qualified as Val

newtype Comp a = Comp
  { unComp :: ReaderT R Q a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader R
             )

instance Quote Comp where
  newName = Comp . lift . TH.newName

data R = R
  { env :: !Env
  , stack :: [Loc]
  }

type Env = HashMap Name TH.Exp

runCompT :: Comp a -> Q a
runCompT = flip runReaderT R {..} . unComp
  where
    env = mempty
    stack = mempty

comp' :: TH.Exp -> TH.Exp -> LExp -> Comp TH.Exp
comp' s1 s2 = wrap $ \ case
  Var x -> asks (Env.lookup x . (.env)) >>= \ case
    Just y -> [| unifyS $(pure s1) $(pure s2) $> $(pure y) |]
    Nothing -> [| fork stuck *> freshVar' |]
  Exi x e -> [| do
    var <- Val.freshVar
    $(localEnv (Env.insert x (TH.VarE 'var)) $ comp' s1 s1 e) |]
  Int x -> [| do
    unifyS $(pure s1) $(pure s2)
    Val.newVar $ Val.Int $(TH.litE . TH.integerL $ fromIntegral x) |]
  e1 :& e2 -> [| do
    s3 <- freshS
    $(comp' s1 (TH.VarE 's3) e1)
    $(comp' (TH.VarE 's3) s2 e2) |]
  e1 := e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 (TH.VarE 's3) e1)
    var2 <- $(comp' (TH.VarE 's3) s2 e2)
    Val.unifyVar var1 var2
    pure var1 |]
  e1 :| e2 -> [| do
    var <- Val.freshVar
    fork $ do
      readChoiceFree $(pure s1)
      Val.unifyVar var =<< $(comp' s1 s2 e1) <|> $(comp' s1 s2 e2)
    pure var |]
  Fail -> [| empty |]
  All e -> [| do
    var <- Val.freshVar
    heap <- newHeap $(pure s1)
    fork $ do
      Val.unifyVar var <=< Val.newVar . Val.Tup <=< all' $ do
        s1 <- newS
        s2 <- freshS
        local (const heap) $(comp' (TH.VarE 's1) (TH.VarE 's2) e)
      unifyChoiceFree $(pure s1) $(pure s2)
      unifyStoreFree $(pure s1) $(pure s2)
    pure var |]
  One e -> [| do
    var <- Val.freshVar
    heap <- newHeap $(pure s1)
    fork $ do
      Val.unifyVar var <=< one $ do
        s1 <- newS
        s2 <- freshS
        local (const heap) $(comp' (TH.VarE 's1) (TH.VarE 's2) e)
      unifyChoiceFree $(pure s1) $(pure s2)
      unifyStoreFree $(pure s1) $(pure s2)
    pure var |]

localEnv :: (Env -> Env) -> Comp a -> Comp a
localEnv f = local (\ r -> r { env = f r.env })

wrap :: (ExpF LExp -> Comp a) -> LExp -> Comp a
wrap f (L i x) = local (\ r -> r { stack = i:r.stack }) $ f x
