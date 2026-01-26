module Pi where

import Prelude hiding ( pi )
import Data.List hiding (union)
import Control.Applicative
import Control.Monad

----------------------------------------------------------------------

newtype Set a = Set [a]

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

instance Ord a => Eq (Set a) where
  Set xs == Set ys = usort xs == usort ys

instance Ord a => Ord (Set a) where
  Set xs `compare` Set ys = usort xs `compare` usort ys

instance (Ord a, Show a) => Show (Set a) where
  show (Set xs) = "{" ++ intercalate "," (map show (usort xs)) ++ "}"

instance Functor Set where
  fmap f (Set xs) = Set (map f xs)

instance Applicative Set where
  pure x = Set [x]
  Set fs <*> Set xs = Set (fs <*> xs)

instance Alternative Set where
  empty = Set []
  Set xs <|> Set ys = Set (xs ++ ys)

instance Monad Set where
  Set xs >>= k = Set [ y | x <- xs, let Set ys = k x, y <- ys ]

instance MonadPlus Set where
  mzero = empty
  mplus = (<|>)

ins :: a -> Set a -> Set a
ins x (Set xs) = Set (x:xs)

union :: Set a -> Set a -> Set a
Set xs `union` Set ys = Set (xs++ys)

mem :: Eq a => a -> Set a -> Bool
x `mem` Set xs = x `elem` xs

set :: [a] -> Set a
set xs = Set xs

from :: Set a -> [a]
from (Set xs) = xs

qall :: (a -> Bool) -> Set a -> Bool
qall f (Set xs) = all f xs

size :: Set a -> Int
size (Set xs) = length xs

(@@) :: Eq a => Set (a,b) -> a -> b
Set xys @@ x = head [ y | (x',y) <- xys, x'==x ]

----------------------------------------------------------------------

class Alternative m => Power m where
  power :: m a -> m (Set a)

instance Power Set where
  power (Set xs) = Set (power xs)

instance Power [] where
  power []     = [ empty ]
  power (x:xs) = empty : [ ins x ys | ys <- xss ] ++ tail xss where xss = power xs

class Alternative m => Collapse m where
  collapse :: m a -> Set a

instance Collapse [] where
  collapse xs = Set xs

instance Collapse Set where
  collapse s = s

----------------------------------------------------------------------

newtype M a = M [Set a]

instance Ord a => Eq (M a) where
  M xs == M ys = xs == ys

instance Ord a => Ord (M a) where
  M xs `compare` M ys = xs `compare` ys

instance (Ord a, Show a) => Show (M a) where
  show (M xs) = show xs

instance Functor M where
  fmap f (M xs) = M [ fmap f s | s <- xs ]

instance Applicative M where
  pure x = M [pure x]
  M fs <*> M xs = M [f <*> x|f<-fs,x<-xs]

instance Alternative M where
  empty = M []
  M xs <|> M ys = M (xs ++ ys)

instance Monad M where
  M xs >>= k = M [ y | x <- xs, let M ys = unionS $ fmap k x, y <- ys ]

unionS :: Set (M a) -> M a
unionS (Set xs) = M (sets xs)
 where
  sets :: [M a] -> [Set a]
  sets xs
    | null as   = []
    | otherwise = Set as : sets ass
   where
    as  = concat [ a | M (Set a:_) <- xs ]
    ass = [ M as | M (_:as) <- xs ]

instance MonadPlus M where
  mzero = empty
  mplus = (<|>)

instance Power M where
  power (M xss) =
    M [ Set (combo yss)
      | Set yss <- power xss
      ]
   where
    combo :: [Set a] -> [Set a]
    combo [] = [ empty ]
    combo (Set as : ss) =
      [ s1 `union` s2
      | s1 <- tail $ power as
      , s2 <- combo ss
      ]

instance Collapse M where
  collapse (M xs) = Set [ x | Set ys <- xs, x <- ys ]

----------------------------------------------------------------------

pi :: (MonadPlus m, Collapse m, Power m, Eq a) => m a -> m (a,b) -> m (m (a,b))
pi dom rng =
  (\fun -> (\x -> (x, fun @@ x)) `fmap` dom) `fmap` funs
 where
  as = collapse dom

  xyss =
    power $
      fmap snd $
        mfilter (\(x,(x',y)) -> x==x') $
          (,) <$> dom <*> rng

  funs =
    mfilter (\xys ->
      qall (\x -> size (set [ y
                            | (x',y) <-from$ xys
                            , x==x'
                            ]) == 1) as
    ) xyss

----------------------------------------------------------------------
-- examples

-- fun(x:=1|2){ (x=1;(3|4)) | (x=2;5) }
ex1 = pi [1,2] [(1,3),(1,4),(2,5)]

-- fun(x:=1|2){ (3|4|5) }
ex2 = pi (M [pure 1, pure 2])
         (M [ Set [(x,3)|x<-[1..2]]
            , Set [(x,4)|x<-[1..2]]
            , Set [(x,5)|x<-[1..2]]
            ])

-- fun(x:=1|2){ x=1|x=2|fail }
ex3 = pi (M [pure 1, pure 2])
         (M [ Set [(1,1)]
            , Set [(2,2)]
            , Set []
            ])


