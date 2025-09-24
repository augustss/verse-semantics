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

consP :: a -> P a -> P a
consP x xs = Unit x `appndP` xs

-- Sequencing (*)  -----------
seqP :: P ENV -> P ENV -> P ENV
seqP EmptyP         _ = EmptyP
seqP (s `UnionP` t) r = (s `seqP` r) `unionP` (t `seqP` r)
seqP (s `AppndP` t) r = (s `seqP` r) `appndP` (t `seqP` r)
seqP (Unit d)       s = seq_unit d s

seq_unit :: ENV -> P ENV -> P ENV
seq_unit _  EmptyP         = EmptyP
seq_unit d  (s `UnionP` t) = (Unit d `seqP` s) `unionP` (Unit d `seqP` t)
seq_unit d  (s `AppndP` t) = (Unit d `seqP` s) `appndP` (Unit d `seqP` t)
seq_unit d1 (Unit d2)
   | isEmptyENV d          = EmptyP
   | otherwise             = Unit d
   where
     d = d1 `intersectENV` d2

-- Canonicalisation -----------
canon :: P a -> SL a
-- The output has all the UnionP's on top
canon EmptyP         = emptySL
canon (s `UnionP` t) = canon s `unionSL` canon t
canon (Unit d)       = unitSL d
canon (s `AppndP` t) = canon_app s (canon t)

canon_app :: P a -> SL a -> SL a
canon_app EmptyP         sl = sl
canon_app (Unit d)       sl = d `consSL` sl
canon_app (s `AppndP` t) sl = canon_app s (canon_app t sl)
canon_app (s `UnionP` t) sl = canon_app s sl `unionSL` canon_app t sl

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


------------- Sequencing directly on SL ----------
-- seqSL sl1 sl2 = canon (seqP (slToPom sl1) (slToPom sl2))
-- Lemma: seqP (foldr unionP EmptyP ss) t  = foldr unionP EmptyP [ seqP s t | s <- ss]
-- Lemma: seqP (foldr consP  EmptyP ds) t  = foldr appndP EmptyP [ seqP (Unit d) t | d <- ds]

-- Lemma: canon (foldr unionP EmptyP ss) = foldr unionSL   emptySL (map canon ss)
-- Lemma: canon (foldr appndP EmptyP ss) = foldr canon_app emptySL ss

-- Lemma: seq_unit d (foldr unionP EmptyP ss) = 
{-
seqP (consP x p) t
  = seqP (Unit x `appnd` p) t
  = (Unit x `seqP` t) `appnd` (p `seqP` t)
-}

---------------------------------
-- seqSL sl1 sl2
--  = canon (seqP (slToPom sl1) (slToPom sl2))

seqSL :: SL ENV -> SL ENV -> SL ENV
-- seqSL (MkSL xss) sl2
--  = canon (seqP (foldr unionP EmptyP [foldr consP EmptyP xs | xs <- xss])
--                (slToPom sl2))
--  = canon (foldr unionP EmptyP [seqP (foldr consP EmptyP xs) (slToPom sl2)
--                               | xs <- xss ])
--  = foldr unionSL emptySL (map canon [seqP (foldr consP EmptyP xs) (slToPom sl2)
--                                     | xs <- xss ])
--  = foldr unionSL emptySL [canon (seqP (foldr consP EmptyP xs) (slToPom sl2))
--                          | xs <- xss ]
--  = foldr unionSL emptySL
--    [ foldr canon_app emptySL [seqP (Unit d) (slToPom sl2) | d <- xs ]
--    | xs <- xss ]
--  = foldr unionSL emptySL
--    [ foldr op emptySL xs | xs <- xss ]
--    where
--      op :: ENV -> SL ENV -> SL ENV
--      op d sl = canon_app (seq_unit d (slToPom sl2)) sl

seqSL (MkSL xss) sl2
  = foldr unionSL emptySL
    [ foldr op emptySL xs | xs <- xss ]
    where
      op :: ENV -> SL ENV -> SL ENV
      op d sl = canon_app (seq_unit d (foldr unionP EmptyP
                                          [ foldr consP EmptyP ys | ys <- yss ]))
                          sl

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
