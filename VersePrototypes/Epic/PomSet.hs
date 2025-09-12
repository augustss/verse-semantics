-----------------------------------------------------------------------------
-- |
-- Module    : Epic.PomSet
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- TODO:
--
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- {-# OPTIONS_GHC -Wall -Werror #-}

module Epic.PomSet
  ( (+++), (|||), (+:+), (|:|)
  , distribute, factor
  , toList, fromList, fromListUnordered, unit
  -- , normalize
  , Pom(..)
  ) where

import Control.Monad (ap)
import Data.Foldable (toList)

--------------------------------------------------------
--
--         A PomSet is ...
--
--------------------------------------------------------

-- Invariants:
-- Union is commutative
-- Empty is unit
-- order is preserved in +++

-- Pom is value and spine strict
data Pom a = Empty
           | Unit  !a
           | PomAppend !(Pom a) !(Pom a) -- * union maintaining order: left hand
                                         -- precedes right hand
           | PomUnion !(Pom a) !(Pom a)  -- * unordered union, internal order of
                                         -- arguments are preserved
          deriving (Eq, Functor, Foldable, Traversable)

instance Show a => Show (Pom a) where
  show (Empty)  = "empty"
  show (Unit a) = show a
  show (PomAppend l r) = show l ++ " +++ " ++ show r
  show (PomUnion  l r) = show l ++ " \x222A " ++ show r -- 0x222A is \cup


infixl 5 |||
infixl 6 +++

-- Jeff: these mimic the fixity of (:), no idea if that's right
infixr 5 +:+
infixr 5 |:|

unit :: a -> Pom a
unit = Unit

(+++) :: Pom a -> Pom a -> Pom a
(+++) Empty r     = r
(+++) l     Empty = l
(+++) l     r     = PomAppend l r

(|||) :: Pom a -> Pom a -> Pom a
(|||) Empty r     = r
(|||) l     Empty = l
(|||) l     r     = PomUnion l r

(+:+) :: a -> Pom a -> Pom a
(+:+) x xs = unit x +++ xs

(|:|) :: a -> Pom a -> Pom a
(|:|) x xs = unit x ||| xs

distribute :: Pom a -> Pom a
distribute Empty    = Empty
distribute (Unit a) = Unit a
distribute (PomAppend (PomUnion a b) c) = (a +++ c) ||| (b +++ c) -- case 1
distribute (PomAppend a (PomUnion b c)) = (a +++ b) ||| (a +++ c) -- case 2
distribute (PomAppend a b)              = (distribute a) +++ (distribute b)
distribute (PomUnion  a b)              = (distribute a) ||| (distribute b)

-- TODO: Broken see the factor_inverse property in test
factor :: Eq a => Pom a -> Pom a
factor Empty    = Empty
factor (Unit a) = Unit a
factor (PomUnion (PomAppend a b) (PomAppend c d))
  | a == c = a +++ b ||| d -- in case (1)
  | b == d = a ||| c +++ d -- in case (2)
factor (PomUnion l r)  = PomUnion  (factor l) (factor r)
factor (PomAppend l r) = PomAppend (factor l) (factor r)

-- normalize :: Pom a -> NPom a
-- normalize Empty    = NEmpty
-- normalize (Unit a) = pure a
-- normalize (PomAppend l@Unit{} r) = NUnion $ l +:+ normalize r
-- normalize (PomAppend l r)        =
  -- NUnion $ normalize l ++ normalize r
-- normalize (PomUnion  (PomUnion l r) rr) = normalize l ||| normalize r

-- allUnits :: Pom a -> Bool
-- allUnits (Unit _)        = True
-- allUnits (PomAppend l r) = allUnits l && allUnits r
-- allUnits _               = False


instance Applicative Pom where
  pure = unit
  {-# INLINE (<*>) #-}
  (<*>) = ap -- TODO: get a better definition


instance Monad Pom where
  return = pure
  {-# INLINE (>>=) #-}
  Empty >>= _           = Empty
  (Unit a) >>= h        = h a
  (PomAppend l r) >>= h = (l >>= h) +++ (r >>= h)
  (PomUnion  l r) >>= h = (l >>= h) ||| (r >>= h)

fromListUnordered :: [a] -> Pom a
fromListUnordered [] = Empty
fromListUnordered xs = foldr (|||) Empty $ fmap unit xs

fromList :: [a] -> Pom a
fromList [] = Empty
fromList xs = foldr (+++) Empty $ fmap unit xs
