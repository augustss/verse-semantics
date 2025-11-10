{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Verse.Comp.Internal
  ( CompT
  , Env
  , runCompT
  , comp'
  ) where

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Writer.CPS (runWriterT)
import Control.Monad.Writer.CPS

import Data.Foldable
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as Env
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Vars
import Data.Traversable

import Language.Haskell.TH
  ( Quote
  , integerL
  , listE
  , litE
  , tupE
  , tupP
  , varE
  , varP
  )
import Language.Haskell.TH qualified as TH

import Loc

import Verse.Exp
import Verse.Monad (Stream (..), all', if', one, fork, split, stuck)
import Verse.Name
import Verse.Run
import Verse.Run.Val qualified as Val

newtype CompT m a = CompT
  { unCompT :: ReaderT R (WriterT Vars m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader R
             , MonadWriter Vars
             )

instance MonadTrans CompT where
  lift = CompT . lift . lift

instance Quote m => Quote (CompT m) where
  newName = lift . TH.newName

data R = R
  { env :: !Env
  , stack :: [Loc]
  }

type Env = HashMap Name TH.Name

type Vars = Set TH.Name

runCompT :: Functor m => CompT m a -> Env -> m a
runCompT m env = evalRWT (unCompT m) R {..}
  where
    stack = mempty

evalRWT :: (Monoid w, Functor m) => ReaderT r (WriterT w m) a -> r -> m a
evalRWT m = fmap fst . runWriterT . runReaderT m

comp' :: Quote m => TH.Name -> TH.Name -> LExp -> CompT m TH.Exp
comp' s1 s2 = wrap $ \ case
  Var x -> asks (Env.lookup x . (.env)) >>= \ case
    Nothing -> [| fork stuck *> Val.freshVar |]
    Just y -> do
      tell $ Vars.singleton y
      [| unifyS $(varE s1) $(varE s2) $> $(varE y) |]
  Abs x e -> do
    s3 <- TH.newName "s1"
    s4 <- TH.newName "s2"
    var <- TH.newName "var"
    (e, xs) <- freeVars . localEnv (Env.insert x var) $ comp' s3 s4 e
    let
      (freeVarsP, freeVarsE)
        = tupP *** tupE
        $ unzip
        $ fmap (varP *** varE)
        $ Map.toList xs
    [| do
      unifyS $(varE s1) $(varE s2)
      Val.newLam $freeVarsE $ \ $freeVarsP $(varP s3) $(varP s4) $(varP var) ->
        $(pure e) |]
  App e1 e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 's3 e1)
    s4 <- freshS
    var2 <- $(comp' 's3 's4 e2)
    var <- Val.freshVar
    fork $ Val.unifyVar var =<< app var1 s4 $(varE s2) var2
    pure var |]
  Exi x e -> [| do
    var <- Val.freshVar
    $(localEnv (Env.insert x 'var) $ comp' s1 s2 e) |]
  Int x -> [| do
    unifyS $(varE s1) $(varE s2)
    Val.newVar $ Val.Int $(litE . integerL $ fromIntegral x) |]
  e1 :& e2 -> [| do
    s3 <- freshS
    _ <- $(comp' s1 's3 e1)
    $(comp' 's3 s2 e2) |]
  Tup es ->
    let
      loop s1 vars = \ case
        [] -> [| do
          unifyS $(varE s1) $(varE s2)
          Val.newTup $(listE $ varE <$> reverse vars) |]
        e:es -> [| do
          s2 <- freshS
          var <- $(comp' s1 's2 e)
          $(loop 's2 ('var:vars) es) |]
    in loop s1 [] es
  e1 := e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 's3 e1)
    var2 <- $(comp' 's3 s2 e2)
    Val.unifyVar var1 var2
    pure var1 |]
  e1 :| e2 -> [| do
    var <- Val.freshVar
    fork $ do
      readChoiceFree $(varE s1)
      Val.unifyVar var =<< $(comp' s1 s2 e1) <|> $(comp' s1 s2 e2)
    pure var |]
  e1 :.. e2 -> [| do
    var <- Val.freshVar
    fork $ do
      readChoiceFree $(varE s1)
      s3 <- freshS
      var1 <- $(comp' s1 's3 e1)
      var2 <- $(comp' 's3 s2 e2)
      (,) <$> Val.readVar var1 <*> Val.readVar var2 >>= \ case
        (Val.Int x1, Val.Int x2) ->
          Val.unifyVar var <=< asum $ Val.newVar . Val.Int <$> [x1 .. x2]
        _ -> stuck
    pure var |]
  e1 :+ e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 's3 e1)
    s4 <- freshS
    var2 <- $(comp' 's3 's4 e2)
    var <- Val.freshVar
    fork $ Val.unifyVar var =<< plus' s4 $(varE s2) var1 var2
    pure var |]
  e1 :- e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 's3 e1)
    s4 <- freshS
    var2 <- $(comp' 's3 's4 e2)
    var <- Val.freshVar
    fork $ Val.unifyVar var =<< minus' s4 $(varE s2) var1 var2
    pure var |]
  e1 :< e2 -> [| do
    s3 <- freshS
    var1 <- $(comp' s1 's3 e1)
    s4 <- freshS
    var2 <- $(comp' 's3 's4 e2)
    var <- Val.freshVar
    fork $ Val.unifyVar var =<< less' s4 $(varE s2) var1 var2
    pure var |]
  Fail -> [| empty |]
  All e -> [| do
    var <- Val.freshVar
    heap <- newHeap $(varE s1)
    fork $ do
      Val.unifyVar var <=< Val.newTup <=< all' $ do
        s1 <- newS
        s2 <- freshS
        local (const heap) $(comp' 's1 's2 e) <* readS s2
      unifyS $(varE s1) $(varE s2)
    pure var |]
  For e1 x e2 -> [| do
    var <- Val.freshVar
    let
      loop s1 vars = \ case
        Done -> do
          unifyS s1 $(varE s2)
          Val.unifyVar var <=< Val.newTup $ reverse vars
        Step var m -> do
          s2 <- freshS
          var <- $(localEnv (Env.insert x 'var) $ comp' 's1 's2 e2)
          heap <- newHeap s2
          loop s2 (var:vars) =<< local (const heap) m
    fork $ loop $(varE s1) [] =<< do
      heap <- newHeap $(varE s1)
      split $ do
        s1 <- newS
        s2 <- freshS
        local (const heap) $(comp' 's1 's2 e1) <* readS s2
    pure var |]
  One e -> [| do
    var <- Val.freshVar
    heap <- newHeap $(varE s1)
    fork $ do
      Val.unifyVar var <=< one $ do
        s1 <- newS
        s2 <- freshS
        local (const heap) $(comp' 's1 's2 e) <* readS s2
      unifyS $(varE s1) $(varE s2)
    pure var |]
  If e1 x e2 e3 -> [| do
    var <- Val.freshVar
    heap <- newHeap $(varE s1)
    fork $ Val.unifyVar var =<< if'
      (do s1 <- newS
          s2 <- freshS
          local (const heap) $(comp' 's1 's2 e1) <* readS s2)
      (\ var -> $(localEnv (Env.insert x 'var) $ comp' s1 s2 e2))
      $(comp' s1 s2 e3)
    pure var |]

freeVars :: Quote m => CompT m a -> CompT m (a, Map TH.Name TH.Name)
freeVars m = do
  r <- ask
  (xs, env) <- mapAccumM (\ xs x -> do
    y <- TH.newName "var"
    pure (Map.insert y x xs, y)) mempty r.env
  (x, w) <- lift $ runRWT (unCompT m) r { env }
  let ys = Map.restrictKeys xs w
  for_ ys $ tell . Vars.singleton
  pure (x, ys)

localEnv :: Monad m => (Env -> Env) -> CompT m a -> CompT m a
localEnv f = local (\ r -> r { env = f r.env })

runRWT :: Monoid w => ReaderT r (WriterT w m) a -> r -> m (a, w)
runRWT m = runWriterT . runReaderT m

wrap :: Monad m => (ExpF LExp -> CompT m a) -> LExp -> CompT m a
wrap f (L i x) = local (\ r -> r { stack = i:r.stack }) $ f x
