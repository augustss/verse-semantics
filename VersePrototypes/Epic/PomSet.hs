-----------------------------------------------------------------------------
-- |
-- Module    : Epic.PomSet
-- Copyright : (c) Epic Games
-- License   : CC0
-- Maintainer: jeffrey.young@epicgames.com
-- Stability : experimental
--
-- This module provides a naive implementation of a PomSet: a partially ordered
-- multiset. See the PLS lab page: https://www.pls-lab.org/en/pomset and the
-- original plotkin paper:
-- https://homepages.inf.ed.ac.uk/gdp/publications/Teams.pdf
--
-- TODO:
-- - add appendUnordered and append
-- - add and test the ordering property to TestSuite.Epic.PomSet
-- - refine Eq instance to not be structural equality
-----------------------------------------------------------------------------

{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module Epic.PomSet
  ( (+++), (|||), (+:+), (|:|)
  , distribute, factor
  , toList, fromList, fromListUnordered, unit
  , normalize, isNormalForm
  , Pom(..)
  , member
  ) where

import Control.Monad (ap)
import Data.Foldable (toList)
import Data.Monoid

--------------------------------------------------------
--
--         A PomSet is ...
--
--------------------------------------------------------

-- Invariants:
-- Union is commutative
-- Empty is unit value
-- order is preserved in +++

-- Pom is value and spine strict
-- TODO: Eq instance is structural equality. This means that 'unit 1 `PomUnion`
-- unit 2 /= unit 2 `PomUnion unit 1'
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
  show (PomAppend l r) = "(" ++ show l ++ " +++ " ++ show r ++ ")"
  show (PomUnion  l r) = "(" ++ show l ++ " \x222A " ++ show r ++ ")" -- 0x222A is \cup


infixl 5 |||
infixl 6 +++

-- Jeff: these mimic the fixity of (:)
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
(+:+) a = (unit a +++)

(|:|) :: a -> Pom a -> Pom a
(|:|) a = (unit a |||)

distribute :: Pom a -> Pom a
distribute (PomAppend (PomUnion a b) c) = (a +++ c) ||| (b +++ c) -- case 1
distribute (PomAppend a (PomUnion b c)) = (a +++ b) ||| (a +++ c) -- case 2
distribute (PomAppend a b)              = (distribute a) +++ (distribute b)
distribute (PomUnion  a b)              = (distribute a) ||| (distribute b)
distribute Empty    = Empty
distribute (Unit a) = unit a

-- TODO: Broken see the factor_inverse property in test
factor :: Eq a => Pom a -> Pom a
factor Empty    = Empty
factor (Unit a) = unit a
factor (PomUnion (PomAppend a b) (PomAppend c d))
  | b == d = (a ||| c) +++ d -- (a +++ c) ||| (b +++ c) --> (a ||| b) +++ c
  | a == c = a +++ (b ||| d) -- (a +++ b) ||| (a +++ c) --> a +++ (b ||| c)
factor (PomUnion l r)  = (factor l) ||| (factor r)
factor (PomAppend l r) = (factor l) +++ (factor r)

normalize :: Eq a => Pom a -> Pom a
normalize = until (\x -> go x == x) go
  where
    go :: Pom a -> Pom a
    go Empty    = Empty
    go (Unit a) = pure a
    -- unit rules
    go (PomAppend l Empty) = l
    go (PomUnion  l Empty) = l
    go (PomAppend Empty r) = r
    go (PomUnion Empty r)  = r
    -- Normalization invariants
    go app@(PomAppend PomUnion{} _) = distribute app
    go app@(PomAppend _ PomUnion{}) = distribute app
    -- recursive cases
    go (PomAppend l r) = distribute l +++ distribute r
    go (PomUnion l r)  = distribute l ||| distribute r

isEmpty :: Pom a -> Bool
isEmpty Empty = True
isEmpty _     = False

isNormalForm :: Pom a -> Bool
isNormalForm l = isEmpty l || getAll (go False l)
  where
    go :: Bool -> Pom a -> All
    go _          Unit{} = All True
    go _          Empty  = All False
    go underApp (PomUnion ul ur)
      | underApp  = All False -- we've seen an Append node above this node, so fail
      | otherwise = go underApp ul <> go underApp ur
    go _ (PomAppend al ar) = go True al <> go True ar


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

anyPomSet :: (a -> Bool) -> Pom a -> Bool
anyPomSet p = getAny . foldMap (Any . p)

member :: Eq a => a -> Pom a -> Bool
member e = anyPomSet (== e)

fromListUnordered :: [a] -> Pom a
fromListUnordered [] = Empty
fromListUnordered xs = foldr (|||) Empty $ fmap unit xs

fromList :: [a] -> Pom a
fromList [] = Empty
fromList xs = foldr (+++) Empty $ fmap unit xs
