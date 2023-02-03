{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Verse.Val
  ( Val (..)
  , hoist
  , hoistA
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Var
import Control.Monad.Writer.CPS

import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Ratio
import Data.Unifiable

import Language.Verse.Label
import Language.Verse.Name
import Language.Verse.Overload (Overload)
import Language.Verse.Overload qualified as Overload

import Prettyprinter

data Val f a
  = Int !Integer
  | Float !Double
  | Rational !Rational
  | Truth a
  | Tuple [a]
  | Module !Label !(HashMap Name (f a))
  | StructInst !Label !(HashMap Name (f a))
  | ClassInst !Label (Maybe a) !(HashMap Name (f a))
  | Overloads !(Overload f a) a deriving (Show, Functor, Foldable, Traversable)

instance Zippable f => Unifiable (Val f) where
  zipMatchM = curry $ \ case
    (Truth x, Truth y) ->
      pure $ Just [(x, y)]
    (Int x, Int y) | x == y ->
      pure $ Just []
    (Rational x, Rational y) | x == y ->
      pure $ Just []
    (Float x, Float y) | x == y ->
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

instance (Pretty (f a), Pretty a) => Pretty (Val f a) where
  pretty = \ case
    Int x ->
      pretty x
    Float x ->
      pretty x
    Rational x | denominator x == 1 ->
      pretty $ numerator x
    Rational x ->
      pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x ->
      align $ "truth" <> group (braces $ pretty x)
    Overloads {} ->
      "function"
    Tuple [] ->
      "false"
    Tuple xs ->
      align . tupled $ pretty <$> xs
    Module i xs ->
      align $ "module#" <> prettyLabel i <> group (braced $ names xs)
    StructInst i xs ->
      align $
      "struct#" <>
      prettyLabel i <>
      group (braced $ names xs)
    ClassInst i Nothing xs ->
      align $
      "class#" <>
      prettyLabel i <>
      group (braced $ names xs)
    ClassInst i (Just x) xs ->
      align $
      "class#" <>
      prettyLabel i <>
      parens (pretty x) <>
      group (braced $ names xs)
    where
      names xs =
        (\ (k, v) -> align $ pretty k <+> ":=" <> group (nest 2 $ line <> pretty v)) <$>
        HashMap.toList xs
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

hoist :: (forall b . f b -> g b) -> Val f a -> Val g a
hoist f = runIdentity . hoistA (Identity . f)

hoistA :: Applicative m => (forall b . f b -> m (g b)) -> Val f a -> m (Val g a)
hoistA f = \ case
  Int x -> pure $ Int x
  Float x -> pure $ Float x
  Rational x -> pure $ Rational x
  Truth x -> pure $ Truth x
  Tuple xs -> pure $ Tuple xs
  Module i xs -> Module i <$> traverse f xs
  StructInst i xs -> StructInst i <$> traverse f xs
  ClassInst i x xs -> ClassInst i x <$> traverse f xs
  Overloads x xs -> Overload.hoistA f x <&> \ x -> Overloads x xs
