module TRS.Traced(
  Traced(..), term, trace, start, (++>), toList, loop,
  showTrace, showRevTrace, filterTrace
  ) where
import Epic.Print

data Traced a = a :<-- [(String,a)]
  deriving (Show)

filterTrace :: (String -> Bool) -> Traced t -> Traced t
filterTrace p (x :<-- nys) = x :<-- [(n, y) | (n, y) <- nys, p n ]

term :: Traced a -> a
term (x :<-- _) = x

trace :: Traced a -> [(String,a)]
trace (_ :<-- tr) = tr

start :: a -> Traced a
start x = x :<-- []

(++>) :: (a -> (String,a)) -> Traced a -> Traced a
f ++> (x :<-- tr) = let (s,y) = f x in y :<-- ((s,x):tr)

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

instance Pretty a => Pretty (Traced a) where
  pPrint (x :<-- tr) = foldr1 ($+$) $ trDocs ++ [pPrint x]
    where
      trDocs = concat [ [pPrint e, text ("---" ++ msg ++ "--->")] | (msg, e) <- reverse tr ]

loop :: Eq a => Traced a -> Traced a
loop (xx :<-- tr) = xx :<-- find xx tr
 where
  find _x []     = []
  find  x ((s,y):sys)
    | y == x    = [(s,y)]
    | otherwise = (s,y) : find x sys

showTrace, showRevTrace :: Pretty a => Traced a -> [String]
showTrace (x :<-- tr) =
  reverse (prettyShow x : concat [ ["  --"++n++"-->>", prettyShow y] | (n,y) <- tr ])

showRevTrace (x :<-- tr) =
  prettyShow x : concat [ ["  <--"++n++"--", prettyShow y] | (n,y) <- tr ]
