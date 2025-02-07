{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveFunctor #-}

module Core.Traced(
  Traced(..), TraceStep(..), tsPayload, setTsPayload, updTsPayload,
  Verbosity, verbosityAll,
  term, trace, start, (++>), loop,
  filterTrace,
  displayTrace, displayTraceV,
  PrettyBrief(..)
  ) where
import Prelude hiding( (<>) )
import Epic.Print

data TraceStep a
  = TS { ts_str     :: String     -- Describes the sep
       , ts_verb    :: Verbosity  -- Show this rule at verbosity rw_verb and above
       , ts_payload :: a }        -- Payload just /before/ the step
  deriving( Functor, Show )

tsPayload :: TraceStep a -> a
tsPayload = ts_payload

setTsPayload  :: TraceStep a -> a -> TraceStep a
setTsPayload ts x = ts { ts_payload = x }

updTsPayload :: (a -> a) -> TraceStep a -> TraceStep a
updTsPayload = fmap

type Verbosity = Int
  -- At verbosity level V, when displaying a trace,
  -- show only rewrites that have verbosity <= V.
  -- Typical levels are 1,2,3

verbosityAll :: Verbosity
verbosityAll = 100

data Traced a = a :<-- [TraceStep a]
  --    (e, [(sn,en), ..., (s1,e1)])
  -- represents the sequence of steps
  --    e1 --s1--> e2 --s2--> ... en --sn--> e
  -- That is, the most recent step is at the head of the list
  deriving (Show)

term :: Traced a -> a
term (x :<-- _) = x

trace :: Traced a -> [TraceStep a]
trace (_ :<-- tr) = tr

start :: a -> Traced a
start x = x :<-- []

(++>) :: (a -> TraceStep a) -> Traced a -> Traced a
f ++> (x :<-- tr) = tsPayload step :<-- ((step `setTsPayload` x) : tr)
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
    | tsPayload step == x  = [step]
    | otherwise            = step : find x steps

displayTrace :: PrettyBrief a => Traced a -> IO ()
displayTrace = displayTraceV verbosityAll

displayTraceV :: PrettyBrief a => Verbosity -> Traced a -> IO ()
displayTraceV verb tr = mapM_ displayDoc (pPrintTrace verb tr)

instance PrettyBrief a => Pretty (Traced a) where
  pPrint tr = vcat (pPrintTrace verbosityAll tr)

class Pretty a => PrettyBrief a where
   pPrintBrief :: a -> Doc

pPrintTrace :: forall a. PrettyBrief a => Verbosity -> Traced a -> [Doc]
pPrintTrace verb (res_expr :<-- tr)
  =  go 1 False empty (reverse tr) -- Print forwards
  where
    go :: Int -> Bool   -- True <=> print the payload regardless of
                        --          the verbosity of the next step
              -> Doc
              -> [TraceStep a] -> [Doc]
    go _ _ herald []
      = [pp_item herald res_expr]
    go n show_anyway herald (step : steps)
      | show_step || show_anyway
      = pp_item herald (tsPayload step)
        : go (n+1) show_step (mkarrow n step) steps

      | otherwise = go (n+1) False (herald $$ mkarrow n step) steps
      where
        show_step = verb >= ts_verb step

    pp_item herald expr = vcat [text "", herald, indent (pPrint expr)]
                          -- NB: (text "") adds a blank line

    mkarrow n step = text (show n ++ ":--" ++ ts_str step ++ "-->")
                     <> braces (pPrintBrief (ts_payload step))

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
