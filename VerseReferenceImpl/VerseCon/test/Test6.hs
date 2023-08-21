{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Applicative

import Control.Monad

import Data.Function
import Data.Functor
import Data.Maybe
import Data.Traversable
import Data.Tuple

import Prelude (Eq (..), Foldable, Int, Integer, Num (..), Traversable, otherwise)

import Text.Show

import Match
import Verse6
import Ref
import Supply

test1 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  fork $ (writeIVar y =<< readIVar x) <|> writeIVar y 1
  writeIVar x 1 <|> writeIVar y 2
  readIVar y

test2 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  fork $ writeIVar y 1 <|> writeIVar y 2
  writeIVar x 1 <|> pure ()
  readIVar y

test3 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  (
    do
      fork $ writeIVar y =<< readIVar x
      empty
    ) <|> (
    do
      writeIVar x 1
      writeIVar y 2
    )
  readIVar y

test4 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  z <- all $ readIVar x <|> readIVar y
  writeIVar x 1
  writeIVar y 2
  readIVar z

test5 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  z <- all $ readIVar x <|> pure 5 <|> readIVar y <|> pure 6
  writeIVar x =<< pure 1 <|> pure 2
  writeIVar y =<< pure 3 <|> pure 4
  readIVar z

test6 = runSupplyT $ runVerseT do
  x <- freshIVar
  y <- freshIVar
  z <- all $ pure 5 <|> readIVar y <|> pure 6
  writeIVar x =<< pure 1 <|> pure 2
  writeIVar y =<< pure 3 <|> pure 4
  readIVar z

data Val a
  = Int {-# UNPACK #-} !Int
  | Cons {-# UNPACK #-} !Int a
  | Tuple [a] deriving (Show, Functor, Foldable, Traversable)

instance RowMatchable Val where
  rowMatch = curry $ \ case
    (Int x, Int y) -> Zip $ guard (x == y) $> Int x
    (Cons x xs, Cons y ys)
      | x == y -> Zip . Just $ Cons x (xs, ys)
      | otherwise -> Uncons (Cons x) xs (Cons y) ys
    (Tuple xs, Tuple ys) -> Zip $ Tuple <$> zipMatch xs ys
    _ -> Zip Nothing

instance Freshenable a m => Freshenable (Val a) m where
  freshen = traverse freshen

instance Monad m => Freshenable () m where
  freshen x = pure x

instance (Freshenable a m, Freshenable b m) => Freshenable (a, b) m where
  freshen (x, y) = (,) <$> freshen x <*> freshen y

instance Monad m => Freshenable Integer m where
  freshen x = pure x

test7 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  fork $ readVar x >>= \ case
    Int x -> unify y =<< newVar (Int $ x + 1)
    _ -> empty
  unify x =<< newVar (Int 1)
  freezeVar y

test8 = runSupplyT $ runVerseT do
  x <- newVar . Cons 1 =<< newVar . Cons 2 =<< freshVar
  y <- newVar . Cons 2 =<< newVar . Cons 3 =<< freshVar
  unify x y
  freezeVar y

test9 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all do
    unify x =<< newVar (Int 1)
    unify x =<< newVar (Int 2)
  readIVar y

test10 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all $ unify x =<< newVar (Int 1)
  readIVar y

test11 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all $ unify x =<< newVar (Int 1)
  unify x =<< newVar (Int 1)
  readIVar y

test12 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all $ unify x =<< newVar (Int 1)
  unify x =<< newVar (Int 2)
  readIVar y

test13 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ unify x y
  unify x =<< newVar (Int 1)
  readIVar z

test14 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ unify x y
  unify x =<< newVar (Int 1)
  unify y =<< newVar (Int 1)
  readIVar z

test15 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ unify x y
  unify x =<< newVar (Int 1)
  unify y =<< newVar (Int 2)
  readIVar z

test16 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all do
    unify x =<< newVar . Int =<< pure 1 <|> pure 2 <|> pure 3
    pure x
  unify x =<< newVar . Int =<< pure 1 <|> pure 2 <|> pure 3
  traverse freezeVar =<< readIVar y

freshValVar :: (MonadRef m, MonadSupply Int m) => VerseT m (Var m Val)
freshValVar = freshVar

test17 = runSupplyT $ runVerseT do
  x <- freshValVar
  y <- freshVar
  z <- all $ unify x y
  unify x y
  readIVar z

test18 = runSupplyT $ runVerseT do
  x0 <- freshValVar
  x1 <- freshVar
  x2 <- freshVar
  x3 <- freshVar
  y <- all do
    unify x0 x1
    unify x2 x3
  unify x0 x2
  readIVar y

test19 = runSupplyT $ runVerseT do
  x0 <- freshValVar
  x1 <- freshVar
  x2 <- freshVar
  x3 <- freshVar
  y <- all do
    unify x0 x1
    unify x2 x3
  unify x0 x2
  unify x0 x3
  unify x2 x1
  readIVar y

test20 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ unify y =<< newVar (Int 1)
  unify x y
  unify x =<< newVar (Int 1)
  readIVar z

test21 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- freshVar
  z <- all $ unify y =<< newVar (Int 1)
  unify x y
  unify x =<< newVar (Int 2)
  readIVar z

test22 = runSupplyT $ runVerseT do
  x <- freshVar
  y <- all $ readVar x
  unify x =<< newVar (Int 1)
  traverse (traverse freezeVar) =<< readIVar y

test23 = runSupplyT $ runVerseT $ do
  x <- freshVar
  y <- newRef x
  all do
    z <- freshVar
    writeRef y z
    unify z x
  unify x =<< newVar (Int 1)
  freezeVar =<< readRef y
