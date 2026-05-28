{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveFunctor #-}

module Core.Traced(
  Traced(..), TraceStep(..), tsPayload, setTsPayload, updTsPayload,
  Verbosity, verbosityAll,
  getTerm, getTrace, start, (++>), loop, normalize,
  Fuel, lotsOfSteps,
  NormResult(..), showNormResult,
  filterTrace,
  displayTrace, displayTraceV, pPrintTrace,
  PrettyBrief(..)
  ) where
import Prelude hiding( (<>) )
import Epic.Print

--------------------------------------------------------------------------------
--
--             Data types
--
--------------------------------------------------------------------------------

data Traced a = a :<-- [TraceStep a]
  --    (e, [(sn,en), ..., (s1,e1)])
  -- represents the sequence of steps
  --    e1 --s1--> e2 --s2--> ... en --sn--> e
  -- That is, the most recent step is at the head of the list
  deriving (Show)

data TraceStep a
  = TS { ts_payload :: a          -- Payload just after the step
       , ts_str     :: String     -- Describes the step
       , ts_verb    :: Verbosity  -- Show this rule at verbosity rw_verb and above
                                  -- Smaller => more likely to be shown
    }  deriving( Functor, Show )

type Verbosity = Int
  -- When displaying a trace att verbosity level V (i.e. --traceVerbosity=V)
  -- show only rewrites that have verbosity <= V.
  -- Typical levels are 1,2,3


--------------------------------------------------------------------------------
--
--             Running a sequence of rules
--
--------------------------------------------------------------------------------

type Fuel = Int

lotsOfSteps :: Fuel
lotsOfSteps = 10000

data NormResult
  = NormOK        -- No rewrites apply
  | NormExpired   -- We ran out of fuel
  | NormInvalid   -- A rewrite produced an invalid output
                  -- according to the `valid` predicate
  deriving( Eq )

instance Show NormResult where
   show = showNormResult

showNormResult :: NormResult -> String
showNormResult NormOK      = "reached a normal form"
showNormResult NormExpired = "ran out of fuel (Unexpected)"
showNormResult NormInvalid = "reached an invalid expression -- yikes!"

-- Repeatedly apply the first in the
-- list of possiblities returned by the rule
normalize :: (a -> Maybe (TraceStep a))   -- How to take a step
          -> (a -> Bool)                  -- Validity predicate
          -> Fuel -> a -> (NormResult, Traced a)
normalize step valid fuel orig_e
  = go fuel [TS{ ts_str = "Initial", ts_verb = 0, ts_payload = orig_e }] orig_e
  where
    go fuel_left tr e
      = case step e of
          Nothing -> (NormOK, e :<-- tr)
          Just ts@(TS { ts_payload = e' })
            | fuel_left==0   -> (NormExpired, e  :<-- tr)
            | not (valid e') -> (NormInvalid, e' :<-- (ts : tr))
            | otherwise      -> go (fuel_left-1) (ts : tr) e'

--------------------------------------------------------------------------------
--
--             Functions
--
--------------------------------------------------------------------------------

tsPayload :: TraceStep a -> a
tsPayload = ts_payload

setTsPayload  :: TraceStep a -> a -> TraceStep a
setTsPayload ts x = ts { ts_payload = x }

updTsPayload :: (a -> a) -> TraceStep a -> TraceStep a
updTsPayload = fmap

verbosityAll :: Verbosity
verbosityAll = 100

getTerm :: Traced a -> a
getTerm (x :<-- _) = x

getTrace :: Traced a -> [TraceStep a]
getTrace (_ :<-- tr) = tr

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
pPrintTrace show_verb (_ :<-- tr)
  =  go 1 (reverse tr) -- Print forwards
  where
    go :: Int -> [TraceStep a] -> [Doc]
    go _  [] = []

    go n (step : steps) = pp_item n step : go (n+1) steps

    pp_item n step@(TS { ts_payload = payload, ts_verb = step_verb })
      | step_verb <= show_verb
      = vcat [ mkarrow n step
             , indent (pPrint payload)
             , text "" ]   -- NB: (text "") adds a blank line
      | otherwise
      = mkarrow n step

    mkarrow n (TS { ts_payload = payload, ts_str = rulename })
      = int n <> braces (pPrintBrief payload) <+>
        text "--->" <+> text rulename

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
