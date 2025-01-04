{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
module IntMap
  ( module Data.IntMap.Strict
  , lookupInsertA
  ) where

import Data.Functor
import Data.IntMap.Internal qualified as Internal
import Data.IntMap.Strict

lookupInsertA
  :: Applicative f
  => Key -> f a -> IntMap a -> f (a, Maybe (IntMap a))
lookupInsertA !k !x = \ case
  t@(Internal.Bin p m l r)
    | Internal.nomatch k p m ->
        x <&> \ x -> (x,) . Just $! Internal.link k (singleton k x) p t
    | Internal.zero k m -> lookupInsertA k x l <&> \ case
        (x, Just l) -> (x,) . Just $! Internal.Bin p m l r
        y -> y
    | otherwise -> lookupInsertA k x r <&> \ case
        (x, Just r) -> (x,) . Just $! Internal.Bin p m l r
        y -> y
  t@(Internal.Tip k' y)
    | k == k' ->
        pure (y, Nothing)
    | otherwise ->
        x <&> \ x -> (x,) . Just $! Internal.link k (singleton k x) k' t
  Internal.Nil ->
    x <&> \ x -> (x,) . Just $! singleton k x
