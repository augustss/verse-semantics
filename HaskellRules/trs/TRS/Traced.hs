module TRS.Traced(
  Traced(..), term, trace, start, toList,
  showTrace, showRevTrace,
  ) where

data Traced a = a :<-- [(String,a)]
  deriving (Show)

term :: Traced a -> a
term (x :<-- _) = x

trace :: Traced a -> [(String,a)]
trace (_ :<-- tr) = tr

start :: a -> Traced a
start x = x :<-- []

-- should get deprecated because the old trace format is no good
toList :: Traced a -> [(String,a)]
toList (x :<-- [])         = [("",x)]
toList (x :<-- ((n,y):tr)) = (n,x) : toList (y :<-- tr)

instance Functor Traced where
  fmap f (x :<-- tr) = f x :<-- [ (n,f y) | (n,y) <- tr ]

-- traced things are only identified by their top-level term
instance Eq a => Eq (Traced a) where
  (x :<-- _) == (y :<-- _) = x == y

instance Ord a => Ord (Traced a) where
  (x :<-- _) `compare` (y :<-- _) = x `compare` y

showTrace, showRevTrace :: Show a => Traced a -> [String]
showTrace (x :<-- tr) =
  reverse (show x : concat [ ["  --"++n++"-->", show y] | (n,y) <- tr ])

showRevTrace (x :<-- tr) =
  show x : concat [ ["  <--"++n++"--", show y] | (n,y) <- tr ]

