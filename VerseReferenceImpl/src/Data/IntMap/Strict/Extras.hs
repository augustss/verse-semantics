module Data.IntMap.Strict.Extras
  ( forWithKey_
  ) where

import Data.Functor
import Data.IntMap.Internal

forWithKey_ :: Applicative f => IntMap a -> (Key -> a -> f b) -> f ()
forWithKey_ xs f = loop xs
  where
    loop xs = case xs of
      Nil -> pure ()
      Tip k v -> void $ f k v
      Bin _ m l r
        | m < 0 -> loop r *> loop l
        | otherwise -> loop l *> loop r
{-# INLINE forWithKey_ #-}
