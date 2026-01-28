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

{-
class Alternative m => Power m where
  power :: m a -> m (Set a)

instance Power Set where
  power (Set xs) = Set (power xs)

instance Power [] where
  power []     = [ empty ]
  power (x:xs) = empty : [ ins x ys | ys <- xss ] ++ tail xss where xss = power xs
-}

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

(+++) :: M a -> M a -> M a
M xs +++ M ys = M (xs++ys)

unionS :: Set (M a) -> M a
unionS (Set xs) = M (sets xs)
 where
  sets :: [M a] -> [Set a]
  sets xs =
    case [ a | M (Set a:_) <- xs ] of
      [] -> []
      as -> Set (concat as) : sets [ M as | M (_:as) <- xs ]

instance MonadPlus M where
  mzero = empty
  mplus = (<|>)

{-
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
-}

{-
  power (M []) =
    M [ empty ]

  power (M (Set [] : ss)) =
    power (M ss)

  power (M (Set [x] : ss)) =
    M (empty : [ ins x s | s <- rr ] ++ tail rr)
   where
    M rr = power (M ss)

  power (M (Set (x:xs) : ss)) =
   where
    M rr = power (M (Set xs : ss)) 

data Elt a = X | This a | Rest a
 deriving ( Eq, Ord, Show )
-}

instance Collapse M where
  collapse (M xs) = Set [ x | Set ys <- xs, x <- ys ]

----------------------------------------------------------------------
{-
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
    mfilter (isFunction as) xyss


isFunction as xys = qall (\x -> size (set [ y
                         | (x',y) <-from$ xys
                         , x==x'
                         ]) == 1) as
-}

----------------------------------------------------------------------
-- examples

-- fun(x:=1|2){ (x=1;(3|4)) | (x=2;5) }
ex 1 = pi (M [pure 1,pure 2])
          (M [pure (1,3),pure (1,4),pure (2,5)])

-- fun(x:=1|2){ (3|4|5) }
ex 2 = pi (M [pure 1, pure 2])
          (M [ Set [(x,3)|x<-[1..2]]
             , Set [(x,4)|x<-[1..2]]
             , Set [(x,5)|x<-[1..2]]
             ])

-- fun(x:=1|2){ (x|4|x+1) }
ex 3 = pi (M [pure 1, pure 2])
          (M [ Set [(x,x)|x<-[1..2]]
             , Set [(x,4)|x<-[1..2]]
             , Set [(x,x+1)|x<-[1..2]]
             ])

-- fun(x:=1|2){ x=1|x=2|fail }
ex 4 = pi (M [pure 1, pure 2])
          (M [ Set [(1,1)]
             , Set [(2,2)]
             , Set []
             ])

-- fun(x:(even|odd)){ 3|x }
ex 5 = pi (M [Set [0,2,4], Set [1,3]])
          (M [ Set [(x,3)|x<-[0..4]]
             , Set [(x,x)|x<-[0..4]]
             ])

-- fun(x:(even|odd)){ 3|x }
ex 6 = pi (M [Set [1,2], Set [3]])
          (M [ Set [(x,10)|x<-[1..3]]
             , Set [(x,20)|x<-[1..3]]
             ])

ex 7 = pi d r 

d = M [ Set [1,2,3] ]
r = M [ Set [(x,7)|x<-[1,2]]
      , Set [(x,8)|x<-[1,2]]
      , Set [(x,9)|x<-[3]]
--      , Set [(x,8)|x<-[3]]
      ]


main =
  do sequence_
       [ do putStrLn ("--EXAMPLE:" ++ show i ++ "--")
            printEx (ex i)
       | i <- [1..7]
       ]
  
printEx (M xs) =
  sequence_
  [ printAlt x | x <- xs ]

printAlt (Set []) = putStrLn "{}"
printAlt (Set [x]) = putStrLn ("{ " ++ show x ++ " }")
printAlt (Set xs) = sequence_ [ putStrLn ( (if i==0 then "{ " else "  ")
                                        ++ show x ++ (if i==n-1 then " }" else ",")) | let n=length xs, (i,x) <- [0..] `zip` xs ]

class Pi m where
  pi :: Eq a => m a -> m (a,b) -> m (m (a,b))

instance Pi Set where
  pi (Set []) _ =
    Set [ Set [] ]

  pi (Set (x:xs)) (Set ys) =
    Set [ Set ((x,y):xys)
        | (x',y) <- ys
        , x==x'
        , Set xys <- unSet $ pi (Set xs) (Set ys)
        ]

unSet (Set xs) = xs

instance Pi [] where
  pi [] _ =
    [[]]

  pi (x:xs) ys =
    [ (x,y):xys
    | (x',y) <- ys
    , x==x'
    , xys <- pi xs ys
    ]

instance Pi M where
  pi (M []) _ =
    M [ Set [ M [] ] ]

  pi (M [xs]) (M [ys]) =
    M [ fmap (M . (:[])) $ pi xs ys ]

  pi (M [xs]) (M []) =
    M []

  pi (M [Set xs]) (M (Set ys : yss)) =
    M $ unM (pi (M [Set xs]) (M [Set ys]))
     ++ unM (unionS (Set
          [ (\f1 f2 -> unionS (Set [f1, f2]))
        <$> pi (M [Set xs1]) (M [Set ys])
        <*> pi (M [Set xs2]) (M yss)
          | (xs1@(_:_),xs2@(_:_)) <- parts xs
          ] 
        ))
     ++ unM (pi (M [Set xs]) (M yss))

  pi (M (xs:xss)) rng =
        (+++)
    <$> pi (M [xs]) rng
    <*> pi (M xss) rng

{-    
    M [ Set [ M (unM f1 ++ unM f2)
            | f1 <- unSet fs1
            , f2 <- unSet fs2
            ]
      | fs1 <- unM $ pi (M [xs]) rng
      , fs2 <- unM $ pi (M xss) rng
      ]
-}

unM (M xss) = xss

parts []     = [([],[])]
parts (x:xs) = [ (x:as,bs) | (as,bs) <- ps ]
            ++ [ (as,x:bs) | (as,bs) <- ps ]
 where
  ps = parts xs
