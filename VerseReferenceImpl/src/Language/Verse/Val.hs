{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Language.Verse.Val
  ( Val (..)
  , Function (..)
  , Label
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Control.Monad.Var
import Control.Monad.Writer.CPS

import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
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
  | Tuple [a]
  | Module !Label !(HashMap Name a)
  | Struct !Label !(IdentMap Name a) !(IdentSet Name) !Exp
  | StructInst !Label !(HashMap Name a)
  | Overload !(Function a) a deriving (Show, Functor, Foldable, Traversable)

data Function a = Function
  !Label
  !(IdentMap Name a)
  !(IdentSet Name)
  !Exp
  !Exp deriving (Show, Functor, Foldable, Traversable)

instance Eq (Function a) where
  Function x _ _ _ _ == Function y _ _ _ _ = x == y

type Label = Word

type IdentSet a = HashSet (Ident a)

type IdentMap a v = HashMap (Ident a) v

type Exp = L (Simplify.Exp L (Ident Name))

instance Unifiable Val where
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
    (StructInst x xs, StructInst y ys) | x == y ->
      pure . Just . toList $ HashMap.intersectionWith (,) xs ys
    (Overload x xs, Overload y ys ) ->
      runMaybeT . execWriterT $ zipCons x xs y ys
    _ -> pure Nothing

type ZipMatchT f m = WriterT [(Var m f, Var m f)] (MaybeT m)

zipCons :: MonadVar m =>
           Function (Var m Val) -> Var m Val ->
           Function (Var m Val) -> Var m Val ->
           ZipMatchT Val m ()
zipCons x xs y ys = zipList xs =<< findCons x y ys

zipList :: MonadVar m => Var m Val -> Var m Val -> ZipMatchT Val m ()
zipList xs ys = uncons xs >>= \ case
  Just (x, xs) -> zipList xs =<< findList x ys
  Nothing -> tell [(xs, ys)]

findCons :: MonadVar m =>
            Function (Var m Val) ->
            Function (Var m Val) -> Var m Val ->
            ZipMatchT Val m (Var m Val)
findCons x y ys =
  if x == y then pure ys
  else newVar . Overload y =<< findList x ys

findList :: MonadVar m =>
            Function (Var m Val) ->
            Var m Val ->
            ZipMatchT Val m (Var m Val)
findList x ys = uncons ys >>= \ case
  Just (y, ys) -> findCons x y ys
  Nothing -> do
    zs <- freshVar
    ys' <- newVar $ Overload x zs
    tell [(ys, ys')]
    pure zs

uncons :: ( Alternative m
          , MonadVar m
          ) => Var m Val -> m (Maybe (Function (Var m Val), Var m Val))
uncons xs = readVar xs >>= \ case
  Just (Overload x xs) -> pure $ Just (x, xs)
  Just _ -> empty
  _ -> pure Nothing

instance Pretty a => Pretty (Val a) where
  pretty = \ case
    Int x -> pretty x
    Float x -> pretty x
    Rational x -> pretty (numerator x) <> pretty '/' <> pretty (denominator x)
    Truth x -> "truth" <> braces (pretty x)
    Overload {} -> "function"
    Tuple [] -> "false"
    Tuple xs -> tupled $ pretty <$> xs
    Module _ xs -> "module" <> braced (names xs)
    Struct i _ _ _ -> "struct#" <> pretty i
    StructInst i xs -> "struct#" <> pretty i <> braced (names xs)
    where
      names xs =
        (\ (k, v) -> pretty k <+> ":=" <+> pretty v) <$>
        HashMap.toList xs
      braced =
        group .
        encloseSep
        (flatAlt "{ " "{")
        (flatAlt " }" "}")
        ", "
