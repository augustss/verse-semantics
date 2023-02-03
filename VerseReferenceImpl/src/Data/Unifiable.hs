{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DefaultSignatures #-}
module Data.Unifiable
  ( Unifiable (..)
  , Zippable (..)
  ) where

import Control.Monad.Var

import Data.Functor.Const

class Traversable f => Unifiable f where
  zipMatchM :: MonadVar m =>
               f (Var m f) ->
               f (Var m f) ->
               m (Maybe [(Var m f, Var m f)])
  default zipMatchM :: (Applicative m, Zippable f) =>
                       f (Var m f) ->
                       f (Var m f) ->
                       m (Maybe [(Var m f, Var m f)])
  zipMatchM x y = pure $ zipMatch x y

class Unifiable f => Zippable f where
  zipMatch :: f a -> f b -> Maybe [(a, b)]

instance Unifiable []

instance Zippable [] where
  zipMatch = curry $ \ case
    (x:xs, y:ys) -> ((x, y):) <$> zipMatch xs ys
    ([], []) -> Just []
    _ -> Nothing

instance Unifiable Maybe

instance Zippable Maybe where
  zipMatch = curry $ \ case
    (Nothing, Nothing) -> Just []
    (Just x, Just y) -> Just [(x, y)]
    _ -> Nothing

instance Unifiable (Const a)

instance Zippable (Const a) where
  zipMatch _ _ = Nothing
