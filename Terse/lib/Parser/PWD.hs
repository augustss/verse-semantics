{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.PWD
  ( Parser
  , done
  , step
  , satisfy
  , token
  , chainl1
  ) where

import Control.Applicative
import Control.Category ((>>>))

import Data.Foldable

class Memo t where
  memo :: (t -> a) -> t -> a
  memo = id

infixl 4 `Ap`
infixl 3 `Alt`
data ParserF t f a where
  Ap :: !(f (a -> b)) -> !(f a) -> ParserF t f b
  Alt :: !(f a) -> !(f a) -> ParserF t f a
  Pure :: !(Tree a) -> ParserF t f a
  Satisfy :: !(t -> Maybe a) -> ParserF t f a
  Empty :: ParserF t f a

data Tree a
  = One !a
  | Some !(Tree a) !(Tree a) deriving (Functor, Foldable, Traversable)

instance Applicative Tree where
  pure = One
  f <*> x = case f of
    One f -> f <$> x
    Some f g -> Some (f <*> x) (g <*> x)

data Parser t a = Parser
  (Parser t a)
  !(t -> Parser t a)
  (ParserF t (Parser t) a)

unwrap :: Parser t a -> ParserF t (Parser t) a
unwrap (Parser _ _ x) = x

parseEOF :: Parser t a -> Parser t a
parseEOF (Parser x _ _) = x

parse :: Parser t a -> t -> Parser t a
parse (Parser _ f _) = f

done :: Parser t a -> [a]
done = unwrap >>> \ case
  Ap f x -> done f <*> done x
  Alt x y -> done x <|> done y
  Pure x -> toList x
  Satisfy _ -> mempty
  Empty -> mempty

step :: t -> Parser t a -> Maybe (Parser t a)
step t = flip parse t >>> \ case
  (unwrap -> Empty) -> Nothing
  x -> Just x

satisfy :: Memo t => (t -> Maybe a) -> Parser t a
satisfy f = let y = Parser empty (parseSatisfy f) $ Satisfy f in y

parseSatisfy :: Memo t => (t -> Maybe a) -> t -> Parser t a
parseSatisfy f = maybe empty pure . f

token :: (Eq t, Memo t) => t -> Parser t t
token x = satisfy $ \ y -> if
  | x == y -> Just x
  | otherwise -> Nothing

chainl1 :: Memo t => Parser t a -> Parser t (a -> a -> a) -> Parser t a
chainl1 x op = flip ($) <$> x <*> loop
  where
    loop = flip (.) <$> (flip <$> op <*> x) <*> loop <|> pure id

instance Memo t => Functor (Parser t) where
  fmap = (<*>) . pure

instance Memo t => Applicative (Parser t) where
  pure = tree . One
  (<*>) = ap

infixl 4 `ap`
ap :: Memo t => Parser t (a -> b) -> Parser t a -> Parser t b
f `ap` x = Parser
  (parseEOF f `ap'` parseEOF x)
  (memo $ \ t -> parse f t `ap'` x `alt'` parseEOF f `ap'` parse x t) $
  f `Ap` x

infixl 4 `ap'`
ap' :: Memo t => Parser t (a -> b) -> Parser t a -> Parser t b
ap' f x = case (unwrap f, unwrap x) of
  (Empty, _) -> empty
  (_, Empty) -> empty
  (Pure f, Pure x) -> tree $ f <*> x
  _ -> f <*> x

instance Memo t => Alternative (Parser t) where
  empty = let y = Parser y (const y) Empty in y
  (<|>) = alt

infixl 3 `alt`
alt :: Memo t => Parser t a -> Parser t a -> Parser t a
x `alt` y = Parser
  (parseEOF x `alt'` parseEOF y)
  (memo $ \ t -> parse x t `alt'` parse y t) $
  x `Alt` y

infixl 3 `alt'`
alt' :: Memo t => Parser t a -> Parser t a -> Parser t a
alt' x y = case (unwrap x, unwrap y) of
  (Empty, _) -> y
  (_, Empty) -> x
  (Pure x, Pure y) -> tree $ Some x y
  _ -> x <|> y

tree :: Memo t => Tree a -> Parser t a
tree x = let y = Parser y (const empty) $ Pure x in y
