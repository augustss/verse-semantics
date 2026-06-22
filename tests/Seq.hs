
---------------------------------------------------------------------
-- This module implements the fundamental data types for SLS semantics
---------------------------------------------------------------------

-- Examples are at the bottom: eg1, eg2, etc
--
-- To try it out
--   $ ghci Seq.hs
--   ghci> show eg3
--   ghci> show (canon eg3)

module Seq where

import qualified Data.Set as S

-------------  Pomsets (P a) ---------------
-- This code is a direct transliteration of Fig 9

data P a = EmptyP
         | UnionP (P a) (P a)
         | AppndP (P a) (P a)
         | Unit a
         deriving( Show )

-- Smart constructors for UnionP and AppndP eliminate EmptyP
unionP :: P a -> P a -> P a
unionP EmptyP s = s
unionP s EmptyP = s
unionP s t      = UnionP s t

appndP :: P a -> P a -> P a
appndP EmptyP s = s
appndP s EmptyP = s
appndP s t      = AppndP s t

-- Sequencing -----------
seqP :: P ENV -> P ENV -> P ENV
EmptyP         `seqP` _               = EmptyP
_              `seqP` EmptyP          = EmptyP
(s `UnionP` t) `seqP` r               = (s `seqP` r) `unionP` (t `seqP` r)
(s `AppndP` t) `seqP` r               = (s `seqP` r) `appndP` (t `seqP` r)
(Unit d)       `seqP` (s `UnionP` t) = (Unit d `seqP` s) `unionP` (Unit d `seqP` t)
(Unit d)       `seqP` (s `AppndP` t) = (Unit d `seqP` s) `appndP` (Unit d `seqP` t)
(Unit d1)      `seqP` (Unit d2)
   | isEmptyENV d                     = EmptyP
   | otherwise                        = Unit d
   where
     d = d1 `intersectENV` d2

-- Canonicalisation -----------
canon :: P a -> SL a
-- The output has all the UnionP's on top
canon EmptyP                      = emptySL
canon (s `UnionP` t)              = canon s `unionSL` canon t
canon (Unit d)                    = unitSL d
canon (EmptyP         `AppndP` s) = canon s
canon ((s `AppndP` t) `AppndP` r) = canon (s `AppndP` (t `AppndP` r))
canon ((s `UnionP` t) `AppndP` r) = canon (s `AppndP` r) `unionSL` canon (t `AppndP` r)
canon ((Unit d)       `AppndP` r) = d `consSL` canon r

-------------------------------------------------------
-------------  SL a: sets of lists of a ---------------
-------------------------------------------------------
data SL a = MkSL [[a]]   -- A set of lists of a
                         -- The inner lists are all non-empty

instance Show a => Show (SL a) where
  show (MkSL xs) = "{" ++ show_xs xs ++ "}"
    where
      show_xs [] = ""
      show_xs [x] = show x
      show_xs (x:xs) = show x ++ ", " ++ show xs

emptySL :: SL a
emptySL = MkSL []

unionSL :: SL a -> SL a -> SL a
unionSL (MkSL xs) (MkSL ys) = MkSL (xs ++ ys)

unitSL :: a -> SL a
unitSL x = MkSL [[x]]

consSL :: a -> SL a -> SL a
-- Add the item to the front of each list in the set
consSL x (MkSL xs) = MkSL (map (x :) xs)

slToPom :: SL a -> P a
-- A (SL a) is a particular sort of (P a), one with
-- all the unions at the top
slToPom (MkSL xss) = foldr unionP EmptyP
                     [ foldr consP EmptyP xs | xs <- xss ]
  where
    consP :: a -> P a -> P a
    consP x xs = Unit x `appndP` xs


-------------  ENV ---------------
-- An ENV denotes a set of environments
-- For our purposes here we suppose that there is
-- just two int-value variable "x" and "y", and an ENV is
-- represented by the set of values the "x" can take
--
-- We also assume there are just three Ints, namely 1,2,3

data ENV = MkEnv
             (S.Set Int)   -- Values of x
             (S.Set Int)   -- Values of y

instance Show ENV where
  show (MkEnv xs ys)
    = "{{x=" ++ show (S.toList xs) ++ ", y=" ++ show (S.toList ys) ++ "}}"

isEmptyENV :: ENV -> Bool
isEmptyENV (MkEnv xs ys) = S.null xs || S.null ys

intersectENV :: ENV -> ENV -> ENV
intersectENV (MkEnv xs1 ys1) (MkEnv xs2 ys2)
  = MkEnv (xs1 `S.intersection` xs2) (ys1 `S.intersection` ys2)

anyVal :: S.Set Int
anyVal = S.fromList [1,2,3]

oneVal :: Int -> S.Set Int
oneVal k = S.singleton k

xEquals :: Int -> P ENV
xEquals k = Unit (MkEnv (oneVal k) anyVal)

yEquals :: Int -> P ENV
yEquals k = Unit (MkEnv anyVal (oneVal k))

---------------- Examples -----------------


eg1 :: P ENV
-- (x=2 ||| x=1) ; y=3
eg1 = seqP  (xEquals 2 `unionP` xEquals 1)
            (yEquals 3)

eg2 :: P ENV
-- (x=2 | x=1)
eg2 = xEquals 2 `appndP` xEquals 1

eg3 :: P ENV
-- (x=2 | x=1) ; (y=1 | y=3)
eg3 = seqP (xEquals 2 `appndP` xEquals 1)
           (yEquals 1 `appndP` yEquals 3)

eg4a, eg4b  :: P ENV
-- eg4a: x=1|2; x=1|||2
-- eg4b: x=1|2
eg4a = seqP (xEquals 1 `appndP` xEquals 2)
            (xEquals 1 `unionP` xEquals 2)
eg4b = xEquals 1 `appndP` xEquals 2

eg5a, eg5b  :: P ENV
-- eg5a: x=1|||2; x=1|2
-- eg5b: x=1|||2
eg5a = seqP (xEquals 1 `unionP` xEquals 2)
            (xEquals 1 `appndP` xEquals 2)
eg5b = xEquals 1 `unionP` xEquals 2
