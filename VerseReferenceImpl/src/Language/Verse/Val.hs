{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Val
  ( Val (..)
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Var
import Control.Monad.Writer.CPS

import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Ratio
import Data.Unifiable

import Language.Verse.Ident
import Language.Verse.Loc
import Language.Verse.Name
import Language.Verse.Simplify.Exp qualified as Simplify

import Prettyprinter

data Val a
  = Int !Integer
  | Float !Double
  | Rational !Rational
  | Truth a
  | Function !(IdentMap Name a) !(IdentSet Name) !Exp !Exp
  | Cons a a
  | Tuple [a] deriving (Show, Functor, Foldable, Traversable)

type IdentSet a = HashSet (Ident a)

type IdentMap a v = HashMap (Ident a) v

type Exp = L (Simplify.Exp L (Ident Name))

instance Unifiable Val where
  zipMatchM = curry $ \ case
    (Truth x, Truth y) -> pure $ Just [(x, y)]
    (Int x, Int y) | x == y -> pure $ Just []
    (Rational x, Rational y) | x == y -> pure $ Just []
    (Float x, Float y) | x == y -> pure $ Just []
    (Tuple xs, Tuple ys) -> pure $ zipMatch xs ys
    (Cons x xs, Cons y ys) -> runMaybeT . execWriterT $ zipCons x xs y ys
    _ -> pure Nothing

type ZipMatchT f m = WriterT [(Var m f, Var m f)] (MaybeT m)

zipCons :: MonadVar m =>
           Var m Val -> Var m Val ->
           Var m Val -> Var m Val ->
           ZipMatchT Val m ()
zipCons x xs y ys = zipList xs =<< findCons x y ys

zipList :: MonadVar m => Var m Val -> Var m Val -> ZipMatchT Val m ()
zipList xs ys = uncons xs >>= \ case
  Just (x, xs) -> zipList xs =<< findList x ys
  Nothing -> tell [(xs, ys)]

findCons :: MonadVar m => Var m Val -> Var m Val -> Var m Val -> ZipMatchT Val m (Var m Val)
findCons x y ys = eqVar x y >>= \ case
  False -> newVar . Cons y =<< findList x ys
  True -> pure ys

findList :: MonadVar m => Var m Val -> Var m Val -> ZipMatchT Val m (Var m Val)
findList x ys = uncons ys >>= \ case
  Just (y, ys) -> findCons x y ys
  Nothing -> do
    zs <- freshVar
    ys' <- newVar $ Cons x zs
    tell [(ys, ys')]
    pure zs

uncons :: (Alternative m, MonadVar m) => Var m Val -> m (Maybe (Var m Val, Var m Val))
uncons xs = readVar xs >>= \ case
  Just (Cons x xs) -> pure $ Just (x, xs)
  Just _ -> empty
  _ -> pure Nothing

instance Pretty a => Pretty (Val a) where
  pretty = \ case
    Int x -> pretty x
    Float x -> pretty x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x -> "truth" <> lbrace <> pretty x <> rbrace
    Function _ _ _ _ -> "function"
    Cons _ _ -> "function"
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
