{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module TRS.TRSS(
  Rule,
  name,
  (-=),
  SRec(..),
  step,
  NormResult(..),
  normalFormFuelTracePlain,
  TRSystem(..),
  noRules,
  addDone,
  stepS,
  ) where

import Epic.List( nubKey )
import Epic.Print(Pretty, prettyShow)
import TRS.Traced
import qualified Data.Set as S
import System.IO.Unsafe
import Text.Printf

--------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
type Rule s a = RuleEnv a -> a -> [(String, a, s)]

class SRec t where
  -- RuleEnv can contain some environment (like flags) needed during reduction
  data RuleEnv t
  -- Convert a rule that matches at the top level to a rule that matches everywhere
  rec :: Rule s t -> Rule s t


instance Show (Rule s t) where
  show _ = "<<Rule>>"

-- This is used to give rules names.
infix 7 `name`   -- must bind tighter than <>
name :: String -> [(a, s)] -> [(String, a, s)]
name str as = [(str, a, s) | (a, s) <- as]

-- Remove a named rule.
infixl 8 -=
(-=) :: Rule s a -> String -> Rule s a
rule -= nm = \ env a -> filter ((/= nm) . fst3) (rule env a)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

noRules :: Rule s a
noRules _ _ = []
--------------------------------------------------------------------------------



step :: (Ord a, SRec a) => Rule s a -> Rule s a
step rule env tt = nubKey snd3 $ rec rule env tt

stepS :: (Ord a, SRec a) => TRSystem s a -> a -> [(String, a, s)]
stepS sys tt = t1s ++ t2s
  where
    -- t1s = step (rules sys) (ruleEnv sys) tt
    t1s = nubKey snd3 $ rec (rules  sys) (ruleEnv sys) tt
    t2s = nubKey snd3 $ {- rec -} (rules2 sys) (ruleEnv sys) tt
  -- case step (rules sys) (ruleEnv sys) tt of
  --   -- HACK: see comment on TRSystem
  --   -- If rules did nothing, then try rules2.
  --   [] -> nub $ rec (rules2 sys) (ruleEnv sys) tt
  --   xs -> xs

data NormResult s a = NormResult
  { nrDone :: [Traced (a, s)]   -- ^ All terms that have no children
  , nrLeft :: [Traced (a, s)]   -- ^ Unexplored terms due to timeout
  , nrCycl :: [Traced (a, s)]   -- ^ Terms whose successors are all "back"-edges
  }
  deriving (Show)

-- -- Traces are produced in reverse order, i.e. final result first
-- normalFormsFuelTracePlain :: (Ord a, Rec a, Pretty a) => TRSystem s a -> Int -> a -> NormResult a
-- normalFormsFuelTracePlain sys an at = go an S.empty [start at]
--  where
--   go  0 _seen trs@(_:_)   = NormResult { nrDone = [], nrLeft = trs }
--   go _n _seen []          = NormResult { nrDone = [], nrLeft = [] }
--   go  n  seen (ttr@(t:<--tr):trs)
-- --    | Debug.Trace.trace ("go: " ++ show (rn tr, t)) False = undefined
--     | t `S.member` seen = stepper "SEEN" ttr $
--                           go n seen trs
--     | null ts'          = stepper "DONE" ttr $
--                           addDone ttr $ go n seen' trs
--     | otherwise         =
--       stepper "STEP" ttr $
--       go (n-1) seen' ([t':<--((s,t):tr) | (s,t') <- ts'] ++ trs)
--    where
--     seen' = S.insert t seen
--     ts'   = stepS sys t

singleStep :: Bool
singleStep = False

stepper :: (Pretty a) => String -> Traced a -> b -> b
stepper msg (t:<--tr) x | singleStep = unsafePerformIO $ do
  let s = case tr of ((ss,_):_) -> ss; _ -> "REFL"
  printf "%s %10s %s\n" msg s (prettyShow t)
  _ <- getLine
  pure x
              | otherwise = x

addDone :: Traced (a, s) -> NormResult s a -> NormResult s a
addDone a nr = nr{ nrDone = a : nrDone nr }

-- Like normalFormsFuelTrace, but only does a depth first search
normalFormFuelTracePlain :: (Pretty s, Monoid s, Ord a, SRec a, Pretty a) => TRSystem s a -> Int -> a -> NormResult s a
normalFormFuelTracePlain sys an at = go an S.empty (start (at, mempty))
 where
  -- go :: Int -> S.Set a -> Traced (a, s) -> NormResult s a
  go 0 _    tr   = NormResult { nrDone = [], nrLeft = [tr], nrCycl = [] }
  go n seen ttr@((t, state) :<-- tr)
    | null ts'   = stepper "done" ttr $ NormResult { nrDone = [ttr], nrLeft = [], nrCycl = [] }
    | null ts''  = -- error "normalFormFuelTracePlain: no children (maybe there are structural rules?)"  -- a loop
                   -- error ("NFTP-crash! \n t = " ++ prettyShow t ++ "\n ts' = " ++ prettyShow ts' ++ "\n ts'' = " ++ prettyShow ts'' ++ "\n ttr = " ++ prettyShow ttr ++ "\n seen = " ++ prettyShow seen )
                   NormResult { nrDone = [], nrLeft = [], nrCycl = [ttr] }
    | otherwise  =
      stepper "STEP" ttr $
      go (n-1) seen' ((t', state <> state') :<-- ((s, (t, state)) : tr))
    where
      seen' = S.insert t seen
      ts'   = stepS sys t
      ts''  = filter ((`S.notMember` seen) . snd3) ts'
      (s, t', state') = head ts''


--------------------------------------------------------------------------------------------------------

-- The rules2 field has rules that are used when none of the rules
-- field apply anymore.
-- This is a hack and not really a normal TRS.

data TRSystem s t = TRSystem
  { sname               :: !String                    -- short system name, should be an identfier
  , description         :: !String                    -- longer system description
  , ruleEnv             :: !(RuleEnv t)               -- environment for running rule execution
  , preProcess          :: !(RuleEnv t -> t -> t)     -- prepare a term for rule application, e.g., ANF
  , postProcess         :: !(RuleEnv t -> t -> t)     -- post processing, e.g., undo ANF
  , rules               :: !(Rule s t)                -- rewrite rules
  , rules2              :: !(Rule s t)                -- hack
  , rulesHaveStructural :: !Bool                      -- are any rules structural? (slower)
  , confluenceRules     :: !(Rule s t)                -- structural rules for equivalence test
  , validExpr           :: !(RuleEnv t -> t -> Bool)  -- is t valid for reduction
  }
--  deriving (Show)

instance Show (TRSystem s t) where
  show _ = "<<TRSystem>>"
