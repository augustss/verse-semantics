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
import Data.Functor
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

import GHC.Exts (fromList)

import Loc

import Verse.Exp
import Verse.Monad
  ( Stream (..)
  , all'
  , if'
  , newVar
  , one
  , fork
  , readVar
  , split
  , stuck
  )
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
    Nothing -> [| fork stuck *> (($(varE s1), $(varE s2), ) <$> Val.freshVar) |]
    Just y -> do
      tell $ Vars.singleton y
      [| pure ($(varE s1), $(varE s2), $(varE y)) |]
  Abs x e -> do
    s1' <- TH.newName "s1"
    s2' <- TH.newName "s2"
    var <- TH.newName "var"
    (e, xs) <- freeVars . localEnv (Env.insert x var) $ comp' s1' s2' e
    let (fvsP, fvsE) = tupP *** tupE $ unzip $ (varP *** varE) <$> Map.toList xs
    [| fmap ($(varE s1), $(varE s2), ) .
      Val.newLam $fvsE $ \ $fvsP $(varP s1') $(varP s2') $(varP var) ->
        $(pure e) |]
  App e1 e2 -> [| do
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    Val.fork3 $ app var1 s1 s2 var2 |]
  Exi x e -> [| do
    var <- Val.freshVar
    $(localEnv (Env.insert x 'var) $ comp' s1 s2 e) |]
  Int x -> [|
    ($(varE s1), $(varE s2), ) <$>
    Val.newInt $(litE . integerL $ fromIntegral x) |]
  e1 :& e2 -> [| do
    (s1, s2, _) <- $(comp' s1 s2 e1)
    $(comp' 's1 's2 e2) |]
  Tup es ->
    let
      loop s1 s2 vars = \ case
        [] -> [|
          fmap ($(varE s1), $(varE s2), ) .
          Val.newTup $
          fromList $(listE $ varE <$> reverse vars) |]
        e:es -> [| do
          (s1, s2, var) <- $(comp' s1 s2 e)
          $(loop 's1 's2 ('var:vars) es) |]
    in
      loop s1 s2 [] es
  e1 := e2 -> [| do
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    Val.unifyVar var1 var2
    pure (s1, s2, var2) |]
  e1 :| e2 -> [| Val.fork3 $ do
    readVar $(varE s1)
    $(comp' s1 s2 e1) <|> $(comp' s1 s2 e2) |]
  e1 :.. e2 -> [| Val.fork3 $ do
    readVar $(varE s1)
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    (,) <$> Val.readVar var1 <*> Val.readVar var2 >>= \ case
      (Val.Int x1, Val.Int x2) ->
        fmap (s1, s2, ) . asum $ Val.newVar . Val.Int <$> [x1 .. x2]
      _ -> stuck |]
  e1 :+ e2 -> [| do
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    Val.fork3 $ plus' s1 s2 var1 var2 |]
  e1 :- e2 -> [| do
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    Val.fork3 $ minus' s1 s2 var1 var2 |]
  e1 :< e2 -> [| do
    (s1, s2, var1) <- $(comp' s1 s2 e1)
    (s1, s2, var2) <- $(comp' 's1 's2 e2)
    Val.fork3 $ less' s1 s2 var1 var2 |]
  Fail -> [| empty |]
  All e -> [| do
    heap <- newHeap $(varE s2)
    Val.fork3 . fmap ($(varE s1), $(varE s2), ) . Val.newTup . fromList <=< all' $ do
      s1 <- newVar (); s2 <- newVar ()
      (s1, s2, var) <- local (const heap) $(comp' 's1 's2 e)
      readVar s1 *> readVar s2 $> var |]
  For e1 x e2 -> [|
    let
      loop s1 s2 vars = \ case
        Done -> fmap (s1, s2, ) . Val.newTup . fromList $ reverse vars
        Step var m -> do
          (s1, s2, var) <- $(localEnv (Env.insert x 'var) $ comp' 's1 's2 e2)
          heap <- newHeap s2
          loop s1 s2 (var:vars) =<< local (const heap) m
    in
      Val.fork3 $ loop $(varE s1) $(varE s2) [] =<< do
        heap <- newHeap $(varE s2)
        split $ do
          s1 <- newVar (); s2 <- newVar ()
          (s1, s2, var) <- local (const heap) $(comp' 's1 's2 e1)
          readVar s1 *> readVar s2 $> var |]
  One e -> [| do
    heap <- newHeap $(varE s2)
    Val.fork3 . fmap ($(varE s1), $(varE s2), ) . one $ do
      s1 <- newVar (); s2 <- newVar ()
      (s1, s2, var) <- local (const heap) $(comp' 's1 's2 e)
      readVar s1 *> readVar s2 $> var |]
  If e1 x e2 e3 -> [| do
    heap <- newHeap $(varE s2)
    Val.fork3 $ if'
      (do s1 <- newVar (); s2 <- newVar ()
          (s1, s2, var) <- local (const heap) $(comp' 's1 's2 e1)
          readVar s1 *> readVar s2 $> var)
      (\ var -> $(localEnv (Env.insert x 'var) $ comp' s1 s2 e2))
      $(comp' s1 s2 e3) |]

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
