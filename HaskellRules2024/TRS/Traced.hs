{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module TRS.Traced(
  Traced(..), term, trace, start, (++>), loop,
  showTrace, showRevTrace, filterTrace,
  displayTrace, displayRevTrace
  ) where
import Epic.Print

data Traced a = a :<-- [(String,a)]
  --    (e, [(sn,en), ..., (s1,e1)])
  -- represents the sequence of steps
  --    e1 --s1--> e2 --s2--> ... en --sn--> e
  deriving (Show)

term :: Traced a -> a
term (x :<-- _) = x

trace :: Traced a -> [(String,a)]
trace (_ :<-- tr) = tr

start :: a -> Traced a
start x = x :<-- []

(++>) :: (a -> (String,a)) -> Traced a -> Traced a
f ++> (x :<-- tr) = let (s,y) = f x in y :<-- ((s,x):tr)

instance Functor Traced where
  fmap f (x :<-- tr) = f x :<-- [ (n,f y) | (n,y) <- tr ]

-- traced things are only identified by their top-level term
instance Eq a => Eq (Traced a) where
  (x :<-- _) == (y :<-- _) = x == y

instance Ord a => Ord (Traced a) where
  (x :<-- _) `compare` (y :<-- _) = x `compare` y

loop :: Eq a => Traced a -> Traced a
loop (xx :<-- tr) = xx :<-- find xx tr
 where
  find _x []     = []
  find  x ((s,y):sys)
    | y == x    = [(s,y)]
    | otherwise = (s,y) : find x sys

displayTrace, displayRevTrace :: Pretty a => Traced a -> IO ()
displayTrace    tr = mapM_ putStrLn (showTrace tr)
displayRevTrace tr = mapM_ putStrLn (showRevTrace tr)

instance Pretty a => Pretty (Traced a) where
  pPrint tr = vcat (pPrintTrace tr)

showTrace, showRevTrace :: Pretty a => Traced a -> [String]
showTrace    tr = map render (pPrintTrace    tr)
showRevTrace tr = map render (pPrintRevTrace tr)

pPrintTrace, pPrintRevTrace :: forall a. Pretty a => Traced a -> [Doc]
pPrintTrace (res_expr :<-- tr) =  go 1 empty (reverse tr) -- Print forwards
  where
    go :: Int -> Doc -> [(String,a)] -> [Doc]
    go _ herald [] = [pp_item herald res_expr]
    go n herald ((s,e):ses) = pp_item herald e : go (n+1) (mkarrow n s) ses

    pp_item herald e = vcat [text "", herald, indent (pPrint e)]
      -- (text "") adds a blank line

    mkarrow n s = text (show n ++ ":--"++s++"-->")

pPrintRevTrace (x :<-- tr) =  -- Print backwards, with terminal state first
  pPrint x : [text ("<--"++n++"--") <+> pPrint y | (n,y) <- tr ]

filterTrace :: (String -> Bool) -> Traced t -> Traced t
filterTrace p (x :<-- nys) = x :<-- go nys
  where
    go [] = []
    go ((n2, x2) : l)
      | p n2 = (n2, x2) : go l
    go ((_,x1) : l@((n2, x2) : nxs))
      | p n2 = ("...", x1) : (n2, x2) : go nxs
      | otherwise = go l
    go [(_, x1)] = [("...", x1)]
