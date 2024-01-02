{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
module Main
  ( main
  ) where

import Control.Applicative
import Control.Monad hiding (join)
import Control.Monad.Fix
import Control.Monad.Ref
import Control.Monad.Supply
import Control.Monad.Verse

import Data.Coerce
import Data.Fix
import Data.Function
import Data.Functor
import Data.Functor.Compose

import Prelude hiding (all)

import Test.HUnit

main :: IO ()
main = runTestTTAndExit $ TestList
  [ test0
  , test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
  ]

pattern Known :: f (Fix (Compose Maybe f)) -> Fix (Compose Maybe f)
pattern Known x = Fix (Compose (Just x))

pattern Unknown :: Fix (Compose Maybe f)
pattern Unknown = Fix (Compose Nothing)

type VarVal m = Fix (Compose (Var m) Val)

data Val a
  = Int {-# UNPACK #-} !Int
  | Tuple [a] deriving (Show, Eq, Functor, Foldable, Traversable)

instance Freshenable a m => Freshenable (Val a) m where
  freshen = traverse freshen

instance Freezable a b m => Freezable (Val a) (Val b) m where
  freeze = traverse freeze

unifyVal
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => VarVal m
  -> VarVal m
  -> VerseT m ()
unifyVal = unify matchVal `on` coerce

matchVal
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Val (VarVal m)
  -> Val (VarVal m)
  -> VerseT m (Match, VerseT m ())
matchVal = curry $ \ case
  (Int x, Int y) -> guard (x == y) $> (SEQ, pure ())
  (Tuple xs, Tuple ys) -> pure (SEQ, unifyValList xs ys)
  _ -> empty

unifyValList
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => [VarVal m]
  -> [VarVal m]
  -> VerseT m ()
unifyValList = curry $ \ case
  ([], []) -> pure ()
  (x:xs, y:ys) -> unifyVal x y *> unifyValList xs ys
  _ -> empty

test0 :: Test
test0 = TestCase $ do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    _ <- one $ readIVar x
    writeIVar x ()
  z @?= Just [()]

test1 :: Test
test1 = TestCase $ do
  z <- runSupplyT $ runVerseT do
    x <- coerce <$> freshVar
    y <- all $ unifyVal x . coerce =<< newVar (Int 1)
    unifyVal x . coerce =<< newVar =<< pure (Int 1) <|> pure (Int 2)
    _ <- readIVar y
    freeze' x
  z @?= Just [Known (Int 1), Known (Int 2)]

test2 :: Test
test2 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    y <- freshIVar
    fork $ do
      xs <- readIVar =<< one (readIVar x)
      writeIVar y xs
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]

test3 :: Test
test3 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    y <- freshIVar
    fork $ writeIVar y =<< readIVar x
    writeIVar x =<< pure (1 :: Int) <|> pure 2
    readIVar y
  z @?= Just [1, 2]

test4 :: Test
test4 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- freshIVar
    fork $ do
      fork $ void $ readIVar x
      void $ readIVar x
    writeIVar x =<< pure (1 :: Int) <|> pure 2
  z @?= Just [(), ()]

test5 :: Test
test5 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVarRef =<< newVar (0 :: Int)
    (<|>)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar y
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar y
    freeze' x
  z @?= Just [Just 0, Just 0]

test6 :: Test
test6 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVarRef =<< newVar (0 :: Int)
    (<|>)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar (y + 1)
      do
        y <- readVar =<< readVarRef x
        writeVarRef x =<< newVar (y + 2)
    freeze' x
  z @?= Just [Just 1, Just 3]

test7 :: Test
test7 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- readIVar =<< all do
      pure () <|> pure ()
      readIVar =<< all do
        pure () <|> pure ()
        coerce <$> freshVar
    unifyVal (head $ head x) . coerce =<< newVar (Int 0)
    freeze' x
  z @?= Just [[[Known (Int 0), Unknown], [Unknown, Unknown]]]

test8 :: Test
test8 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- readIVar =<< all do
      readIVar =<< all do
        coerce <$> freshVar
    y <- readIVar =<< all do
      pure $ head x
    unifyVal (head $ head y) . coerce =<< newVar (Int 0)
    freeze' x
  z @?= Just [[[Known (Int 0)]]]

data Sub
  = Any
  | AnyInt
  | AnInt !Integer deriving (Show, Eq)

instance Monad m => Freshenable Sub m where
  freshen = pure

instance Monad m => Freezable Sub Sub m where
  freeze = pure

unifySub
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Var m Sub
  -> Var m Sub
  -> VerseT m ()
unifySub = unify matchSub

matchSub
  :: (MonadFix m, MonadRef m, MonadSupply Int m)
  => Sub
  -> Sub
  -> VerseT m (Match, VerseT m ())
matchSub = curry $ \ case
  (Any, Any) -> pure (LE, pure ())
  (Any, AnyInt) -> pure (GE, pure ())
  (Any, AnInt _) -> pure (GE, pure ())
  (AnyInt, Any) -> pure (LE, pure ())
  (AnyInt, AnyInt) -> pure (LE, pure ())
  (AnyInt, AnInt _) -> pure (GE, pure ())
  (AnInt _, Any) -> pure (LE, pure ())
  (AnInt _, AnyInt) -> pure (LE, pure ())
  (AnInt x, AnInt y) -> guard (x == y) $> (SEQ, pure ())

test9 :: Test
test9 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar AnyInt
    unifySub x y
    freeze' x
  z @?= Just [Just AnyInt]

test10 :: Test
test10 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar AnyInt
    unifySub x y
    freeze' x
  z @?= Just [Just AnyInt]

test11 :: Test
test11 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- newVar $ AnInt 1
    unifySub x y
    freeze' x
  z @?= Just [Just $ AnInt 1]

test12 :: Test
test12 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- newVar Any
    y <- freshVar
    unifySub x y
    unifySub y =<< newVar (AnInt 1)
    freeze' x
  z @?= Just [Just $ AnInt 1]

test13 :: Test
test13 = TestCase do
  z <- runSupplyT $ runVerseT do
    x <- coerce <$> freshVar
    y <- coerce <$> freshVar
    fork $ unifyVal y . coerce =<< newVar . Tuple =<< readIVar =<< for
      do
        t <- coerce <$> freshVar
        unifyVal t x
        (unifyVal x . coerce =<< newVar (Int 1)) <|>
          (unifyVal t . coerce =<< newVar (Int 2))
        pure t
      do
        \ _ ->
          fmap coerce . newVar . Int =<< pure 1 <|> pure 2
    unifyVal x . coerce =<< newVar (Int 1)
    freeze' y
  z @?= Just [Known (Tuple [Known (Int 1)]), Known (Tuple [Known (Int 2)])]

test14 :: Test
test14 = TestCase do
  void $ runSupplyT $ runVerseT $ void $ readIVar =<< all do
    void $ readIVar =<< for (void $ pure ()) do
      \ _ -> pure () <|> pure ()

test15 :: Test
test15 = TestCase do
  z <- runSupplyT $ runVerseT do
    void $ all do
      x <- coerce <$> freshVar
      void $ all do
        void $ all do
          unifyVal x . coerce =<< newVar . Int =<< pure 2
        unifyVal x . coerce =<< newVar . Int =<< pure 2 <|> pure 2
      unifyVal x . coerce =<< newVar . Int =<< pure 2 <|> pure 2
  z @?= Just [()]
