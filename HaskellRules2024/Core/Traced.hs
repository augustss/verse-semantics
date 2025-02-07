{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveFunctor #-}

module Core.Traced(
  Traced(..), TraceStep(..), Verbosity,
  term, trace, start, (++>), loop,
  filterTrace,
  displayTrace, displayRevTrace,
  PrettyBrief(..)
  ) where
import Prelude hiding( (<>) )
import Epic.Print

data TraceStep a
  = TS { ts_str     :: String     -- Describes the sep
       , ts_verb    :: Verbosity  -- Show this rule at verbosity rw_verb and above
       , ts_payload :: a }
  deriving( Functor, Show )

type Verbosity = Int
  -- At verbosity level V, when displaying a trace,
  -- show only rewrites that have verbosity <= V.

data Traced a = a :<-- [TraceStep a]
  --    (e, [(sn,en), ..., (s1,e1)])
  -- represents the sequence of steps
  --    e1 --s1--> e2 --s2--> ... en --sn--> e
  deriving (Show)

term :: Traced a -> a
term (x :<-- _) = x

trace :: Traced a -> [TraceStep a]
trace (_ :<-- tr) = tr

start :: a -> Traced a
start x = x :<-- []

(++>) :: (a -> TraceStep a) -> Traced a -> Traced a
f ++> (x :<-- tr) = ts_payload step :<-- (step { ts_payload = x } : tr)
  where
    step = f x

instance Functor Traced where
  fmap f (x :<-- tr) = f x :<-- map (fmap f) tr

-- Traced things are only identified by their top-level term
instance Eq a => Eq (Traced a) where
  (x :<-- _) == (y :<-- _) = x == y

instance Ord a => Ord (Traced a) where
  (x :<-- _) `compare` (y :<-- _) = x `compare` y

loop :: Eq a => Traced a -> Traced a
loop (xx :<-- tr) = xx :<-- find xx tr
 where
  find _x []     = []
  find  x (step:steps)
    | ts_payload step == x  = [step]
    | otherwise             = step : find x steps

displayTrace, displayRevTrace :: PrettyBrief a => Traced a -> IO ()
displayTrace    tr = mapM_ putStrLn (showTrace tr)
displayRevTrace tr = mapM_ putStrLn (showRevTrace tr)

instance PrettyBrief a => Pretty (Traced a) where
  pPrint tr = vcat (pPrintTrace tr)

showTrace, showRevTrace :: PrettyBrief a => Traced a -> [String]
showTrace    tr = map render (pPrintTrace    tr)
showRevTrace tr = map render (pPrintRevTrace tr)

class Pretty a => PrettyBrief a where
   pPrintBrief :: a -> Doc

pPrintTrace, pPrintRevTrace :: forall a. PrettyBrief a => Traced a -> [Doc]
pPrintTrace (res_expr :<-- tr) =  go 1 empty (reverse tr) -- Print forwards
  where
    go :: Int -> Doc -> [TraceStep a] -> [Doc]
    go _ herald [] = [pp_item herald res_expr]
    go n herald (step : steps) = pp_item herald (ts_payload step)
                                 : go (n+1) (mkarrow n step) steps

    pp_item herald expr = vcat [text "", herald, indent (pPrint expr)]
                          -- (text "") adds a blank line

    mkarrow n step = text (show n ++ ":--" ++ ts_str step ++ "-->")
                     <> braces (pPrintBrief (ts_payload step))

pPrintRevTrace (x :<-- tr)  -- Print backwards, with terminal state first
  = pPrint x : [text ("<--"++ ts_str step ++"--") <+> pPrint (ts_payload step) | step <- tr]

filterTrace :: (String -> Bool) -> Traced t -> Traced t
filterTrace p (x :<-- nys) = x :<-- go nys
  where
    go [] = []
    go (step : steps)
      | p (ts_str step) = step : go steps
    go (step1 : step2 : steps)
      | p (ts_str step2) = step1 { ts_str = "..." } : step2 : go steps
      | otherwise        = go (step2 : steps)
    go [step] = [step { ts_str = "..." }]
