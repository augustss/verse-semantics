{-# LANGUAGE LambdaCase #-}
module Data.IntMap.Lazy.Extras
  ( lookupInsert
  , findDelete
  ) where

import Data.IntMap.Internal

lookupInsert :: Key -> a -> IntMap a -> Either a (IntMap a)
lookupInsert !k0 x0 t0 = loop k0 x0 t0
  where
    loop k x = \ case
      t@(Bin p m l r)
        | nomatch k p m ->
          Right $! link k (singleton k x) p t
        | zero k m -> case loop k x l of
          Right l -> Right $! Bin p m l r
          l@(Left _) -> l
        | otherwise -> case loop k x r of
          Right r -> Right $! Bin p m l r
          r@(Left _) -> r
      t@(Tip k' y)
        | k == k' -> Left y
        | otherwise ->
          Right $! link k (singleton k x) k' t
      Nil -> Right $! singleton k x

findDelete :: Key -> IntMap a -> (a, IntMap a)
findDelete !k0 t0 = loop k0 t0
  where
    loop k = \ case
      Bin p m l r
        | nomatch k p m -> notFound k
        | zero k m -> case loop k l of
            (x, l) -> (x, ) $! binCheckLeft p m l r
        | otherwise -> case loop k r of
            (x, r) -> (x, ) $! binCheckRight p m l r
      Tip k' y
        | k == k' -> (y, Nil)
        | otherwise -> notFound k
      Nil -> notFound k
    notFound k =
      error $ "findDelete: key " ++ show k ++ " is not an element of the map"
