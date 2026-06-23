{-# LANGUAGE PatternSynonyms, ViewPatterns, RankNTypes, ScopedTypeVariables, DeriveFunctor #-}

module Core.Traced(
  Traced(..), TraceStep(..), tsPayload, setTsPayload, updTsPayload,
  pattern (:<--),
  Verbosity, verbosityAll,
  Validity(..),
  getTerm, getTrace, traceLength, traceNormResult, traceSummary, appendTrace,
  start, (++>), loop, normalize,
  Fuel, lotsOfSteps,
  NormResult(..), showNormResult,
  filterTrace,
  displayTrace, displayTraceV, pPrintTrace,
  PrettyBrief(..)
  ) where
import Prelude hiding( (<>) )
import qualified Prelude as SG ( (<>) )
import Epic.Print

--------------------------------------------------------------------------------
--
--             Data types
--
--------------------------------------------------------------------------------

data Traced a                        -- In order of execution, earliest first
  = Step (TraceStep a) (Traced a)
  | Done NormResult
  deriving (Show, Functor)

data TraceStep a
  = TS { ts_payload :: a          -- Payload just after the step
       , ts_str     :: String     -- Describes the step
       , ts_verb    :: Verbosity  -- Show this rule at verbosity rw_verb and above
                                  -- Smaller => more likely to be shown
    }  deriving( Functor, Show, Eq )

type Verbosity = Int
  -- When displaying a trace att verbosity level V (i.e. --traceVerbosity=V)
  -- show only rewrites that have verbosity <= V.
  -- Typical levels are 1,2,3


data NormResult
  = NormOK        -- No rewrites apply
  | NormExpired   -- We ran out of fuel
  | NormInvalid Doc   -- A rewrite produced an invalid output
                      -- according to the `valid` predicate
  deriving( Eq )

instance Show NormResult where
   show = render . showNormResult

showNormResult :: NormResult -> Doc
showNormResult NormOK          = text "reached a normal form"
showNormResult NormExpired     = text "ran out of fuel (Unexpected)"
showNormResult (NormInvalid d) = sep [ text "reached an invalid expression -- yikes!"
                                     , nest 2 d ]

-- Traced things are only identified by their top-level term
instance Eq a => Eq (Traced a) where
  Done x     == Done y     = x == y
  (Step x _) == (Step y _) = ts_payload x == ts_payload y
  _          == _          = False

-- Why do we need Ord???
-- instance Ord a => Ord (Traced a) where
--  (x :<-- _) `compare` (y :<-- _) = x `compare` y

--------------------------------------------------------------------------------
--             Back-compat only
--------------------------------------------------------------------------------


{-# COMPLETE (:<--) #-}
pattern (:<--) :: a -> [TraceStep a] -> Traced a
  --    (e, [(sn,en), ..., (s1,e1)])
  -- represents the sequence of steps
  --    e1 --s1--> e2 --s2--> ... en --sn--> e
  -- That is, the most recent step is at the /head/ of the list
pattern res :<-- steps <- (swizzle -> (res,steps))
  where
    _res :<-- steps = go (Done NormOK) steps
       where
         go acc []     = acc
         go acc (s:ss) = go (Step s acc) ss

swizzle :: Traced a -> (a, [TraceStep a])
swizzle (Done {})         = error "swizzle :<--"
swizzle (Step step steps) = go [] step steps
  where
    go acc s (Done {})     = (ts_payload s, s:acc)
    go acc s1 (Step s2 ss) = go (s1:acc) s2 ss

start :: a -> Traced a
start x = x :<-- []

(++>) :: (a -> TraceStep a) -> Traced a -> Traced a
f ++> (x :<-- tr) = tsPayload step :<-- ((step `setTsPayload` x) : tr)
  where
    step = f x

loop :: Eq a => Traced a -> Traced a
loop (xx :<-- tr) = xx :<-- find xx tr
 where
  find _x []     = []
  find  x (step:steps)
    | tsPayload step == x  = [step]
    | otherwise            = step : find x steps

--------------------------------------------------------------------------------
--
--             Running a sequence of rules
--
--------------------------------------------------------------------------------

type Fuel = Int

lotsOfSteps :: Fuel
lotsOfSteps = 10000

normalize :: (a -> Maybe (TraceStep a))   -- How to take a step
          -> (a -> Validity)                  -- Validity predicate
          -> Fuel -> a -> Traced a
-- `normalize` produces the steps lazily, so you can display earlier
-- ones even if a later one crashes.
normalize step valid fuel orig_e
  = Step initial_step (go fuel orig_e)
  where
    initial_step = TS{ ts_str = "Initial", ts_verb = 0, ts_payload = orig_e }
    go fuel_left e
      | fuel_left == 0       = Done NormExpired
      | Invalid d <- valid e = Done (NormInvalid d)
      | otherwise            = case step e of
                                   Nothing -> Done NormOK
                                   Just ts -> Step ts (go (fuel_left-1) (ts_payload ts))

traceNormResult :: Traced a -> NormResult
traceNormResult (Done nr)   = nr
traceNormResult (Step _ ss) = traceNormResult ss

data Validity = Valid | Invalid Doc

instance Monoid Validity where
  mempty = Valid

instance Semigroup Validity where
  Valid      <> x          = x
  x          <> Valid      = x
  Invalid as <> Invalid bs = Invalid (as $$ bs)

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
getTerm (Done {})           = error "getTerm"
getTerm (Step ts (Done {})) = ts_payload ts
getTerm (Step _  ss)        = getTerm ss

traceSummary :: Traced a -> (NormResult, Int, a)
traceSummary (Done {}) = error "traceSummary"
traceSummary (Step step1 steps) = go 1 step1 steps
  where
    go n s (Done nr)   = (nr, n, ts_payload s)
    go n _ (Step s ss) = go (n+1) s ss

traceLength :: Traced a -> Int
traceLength (Done {})   = 0
traceLength (Step _ ss) = 1 + traceLength ss

appendTrace :: Traced a -> Traced a -> Traced a
appendTrace (Step s ss) t = Step s (appendTrace ss t)
appendTrace (Done {})   t = t

getTrace :: Traced a -> [TraceStep a]
getTrace (_ :<-- tr) = tr

displayTrace :: PrettyBrief a => Traced a -> IO ()
displayTrace = displayTraceV verbosityAll

displayTraceV :: PrettyBrief a => Verbosity -> Traced a -> IO ()
displayTraceV verb tr = mapM_ displayDoc (pPrintTrace verb tr)

instance PrettyBrief a => Pretty (Traced a) where
  pPrint tr = vcat (pPrintTrace verbosityAll tr)

class Pretty a => PrettyBrief a where
   pPrintBrief :: a -> Doc

pPrintTrace :: forall a. PrettyBrief a => Verbosity -> Traced a -> [Doc]
-- The pretty-printer for traces
-- It takes a verbosity level to control which steps are abbreviated
pPrintTrace show_verb tr
  =  go 1 tr
  where
    go :: Int -> Traced a -> [Doc]
    go _  (Done nr) = [ text "Done:" <+> showNormResult nr ]

    go n (Step step steps) = pp_item n step : go (n+1) steps

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
filterTrace p = go
  where
    go (Done nr) = Done nr
    go (Step step steps)
      | p (ts_str step) = Step step (go steps)
    go (Step step1 (Step step2 steps))
      | p (ts_str step2) = Step step1 { ts_str = "..." } $
                           Step step2 (go steps)
      | otherwise        = go (Step step2 steps)
    go (Step step (Done nr)) = Step (step { ts_str = "..." }) (Done nr)
