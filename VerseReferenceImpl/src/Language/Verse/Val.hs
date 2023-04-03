{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Verse.Val
  ( Val (..)
  , prettyM
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Var
import Control.Monad.Writer.CPS

import Data.Foldable
import Data.Functor
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Ratio
import Data.Unifiable

import Language.Verse.Label
import Language.Verse.Name
import {-# SOURCE #-} Language.Verse.Named (Named)
import Language.Verse.Overload (Overload)
import Language.Verse.Pretty

import Prettyprinter

data Val m a
  = Int !Integer
  | Float {-# UNPACK #-} !Double
  | Rational !Rational
  | Truth a
  | Tuple [a]
  | Module {-# UNPACK #-} !Label !(HashMap Name (Named m a))
  | StructInst {-# UNPACK #-} !Label !(HashMap Name (Named m a))
  | ClassInst {-# UNPACK #-} !Label !(Maybe a) !(HashMap Name (Named m a))
  | Overloads !(Overload m a) a deriving (Functor, Foldable, Traversable)

instance EqVarRef (VarRef m) => Unifiable (Val m) where
  zipMatchM = curry $ \ case
    (Truth x, Truth y) ->
      pure $ Just [(x, y)]
    (Int x, Int y) | x == y ->
      pure $ Just []
    (Rational x, Rational y) | x == y ->
      pure $ Just []
    (Float x, Float y) | if isNaN x then isNaN y else x == y ->
      pure $ Just []
    (Tuple xs, Tuple ys) ->
      pure $ zipMatch xs ys
    (StructInst i xs, StructInst j ys) | i == j -> runMaybeT . execWriterT $
      for_ (HashMap.intersectionWith (,) xs ys) $
        maybe empty tell . uncurry zipMatch
    (ClassInst i x xs, ClassInst j y ys) | i == j -> runMaybeT . execWriterT $ do
      maybe empty tell $ zipMatch x y
      for_ (HashMap.intersectionWith (,) xs ys) $
        maybe empty tell . uncurry zipMatch
    (Overloads x xs, Overloads y ys) ->
      runMaybeT . execWriterT $ zipCons x xs y ys
    _ -> pure Nothing

type ZipMatchT f m = WriterT [(Var m f, Var m f)] (MaybeT m)

zipCons :: MonadVar m =>
           Overload f (Var m (Val f)) -> Var m (Val f) ->
           Overload f (Var m (Val f)) -> Var m (Val f) ->
           ZipMatchT (Val f) m ()
zipCons x xs y ys = zipList xs =<< findCons x y ys

zipList :: MonadVar m => Var m (Val f) -> Var m (Val f) -> ZipMatchT (Val f) m ()
zipList xs ys = uncons xs >>= \ case
  Just (x, xs) -> zipList xs =<< findList x ys
  Nothing -> tell [(xs, ys)]

findCons :: MonadVar m =>
            Overload f (Var m (Val f)) ->
            Overload f (Var m (Val f)) -> Var m (Val f) ->
            ZipMatchT (Val f) m (Var m (Val f))
findCons x y ys =
  if x == y then pure ys
  else newVar . Overloads y =<< findList x ys

findList :: MonadVar m =>
            Overload f (Var m (Val f)) ->
            Var m (Val f) ->
            ZipMatchT (Val f) m (Var m (Val f))
findList x ys = uncons ys >>= \ case
  Just (y, ys) -> findCons x y ys
  Nothing -> do
    zs <- freshVar
    ys' <- newVar $ Overloads x zs
    tell [(ys, ys')]
    pure zs

uncons :: ( Alternative m
          , MonadVar m
          ) => Var m (Val f) -> m (Maybe (Overload f (Var m (Val f)), Var m (Val f)))
uncons xs = readVar xs >>= \ case
  Just (Overloads x xs) -> pure $ Just (x, xs)
  Just _ -> empty
  _ -> pure Nothing

instance ( MonadVarRef m
         , MonadPretty a m
         ) => MonadPretty (Val m a) m where
  prettyM = \ case
    Int x ->
      pure $ pretty x
    Float x ->
      pure $ pretty x
    Rational x | denominator x == 1 ->
      pure $ pretty $ numerator x
    Rational x ->
      pure $ pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x ->
      prettyM x <&> \ doc -> align $ "truth" <> group (braces doc)
    Overloads {} ->
      pure "function"
    Tuple [] ->
      pure "false"
    Tuple xs ->
      align . tupled <$> traverse prettyM xs
    Module i xs ->
      namesM xs <&> \ docs ->
      align $
      "module#" <>
      prettyLabel i <>
      group (braced docs)
    StructInst i xs ->
      namesM xs <&> \ docs ->
      align $
      "struct#" <>
      prettyLabel i <>
      group (braced docs)
    ClassInst i Nothing xs ->
      namesM xs <&> \ docs ->
      align $
      "class#" <>
      prettyLabel i <>
      group (braced docs)
    ClassInst i (Just x) xs -> do
      doc <- prettyM x
      docs <- namesM xs
      pure . align $
        "class#" <>
        prettyLabel i <>
        parens doc <>
        group (braced docs)
    where
      namesM xs =
        traverse
        (\ (k, v) -> prettyM v <&> \ doc -> align $ pretty k <+> ":=" <> group (nest 2 $ line <> doc))
        (HashMap.toList xs)
      tupled =
        group .
        encloseSep
        (flatAlt "( " lparen)
        (flatAlt (hardline <> rparen) rparen)
        ", "
      braces x =
        flatAlt (hardline <> "{ ") lbrace <>
        x <>
        flatAlt (hardline <> rbrace) rbrace
      braced =
        group .
        encloseSep
        (flatAlt (hardline <> "{ ") lbrace)
        (flatAlt (hardline <> rbrace) rbrace)
        ", "
