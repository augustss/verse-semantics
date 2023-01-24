{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module TRS.TRS(
  Rule,
  name,
  Rec(..),
  step,
  NormResult(..),
  normalFormsFuelTracePlain,
  normalFormFuelTracePlain,
  ) where

import Epic.List( nub )
import TRS.Traced
import qualified Data.Set as S
--import Control.Monad( unless )
--import qualified Debug.Trace
--import Data.Set( Set )
import System.IO.Unsafe
import Text.Printf

--------------------------------------------------------------------------------

type Rule a = RuleEnv a -> a -> [(String, a)]


instance Show (Rule t) where
  show _ = "<<Rule>>"

-- This is used to give rules names.
infix 7 `name`   -- must bind tighter than <>
name :: String -> [a] -> [(String, a)]
name s as = [(s,a) | a <- as]

--------------------------------------------------------------------------------

class Rec t where
  -- RuleEnv can contain some environment (like flags) needed during reduction
  data RuleEnv t
  -- Convert a rule that matches at the top level to a rule that matches everywhere
  rec :: Rule t -> Rule t

step :: forall a . (Ord a, Rec a) => Rule a -> Rule a
step rule env tt = nub $ rec rule env tt

data NormResult a = NormResult
  { nrDone :: [Traced a]   -- All terms that have no children
  , nrLeft :: [Traced a]   -- Unexplored terms due to timeout
  }
  deriving (Show)

-- Traces are produced in reverse order, i.e. final result first
normalFormsFuelTracePlain :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> NormResult a
normalFormsFuelTracePlain env an rule at = go an S.empty [start at]
 where
--  go  n  _ trs | Debug.Trace.trace ("go: " ++ show (n, length trs)) False = undefined
  go  0 _seen trs@(_:_)   = NormResult { nrDone = [], nrLeft = trs }
  go _n _seen []          = NormResult { nrDone = [], nrLeft = [] }
  go  n  seen (ttr@(t:<--tr):trs)
--    | Debug.Trace.trace ("go: " ++ show (rn tr, t)) False = undefined
    | t `S.member` seen = stepper "SEEN" ttr $ go n seen trs
    | null ts'          = stepper "DONE" ttr $ addDone ttr $ go n seen' trs
    | otherwise         =
      stepper "STEP" ttr $
      go (n-1) seen' ([t':<--((s,t):tr) | (s,t') <- ts'] ++ trs)
   where
    seen' = S.insert t seen
    ts'   = step rule env t
--    rn [] = "refl"
--    rn ((s,_):_) = s

singleStep :: Bool
singleStep = False

stepper :: (Show a) => String -> Traced a -> b -> b
stepper msg (t:<--tr) x | singleStep = unsafePerformIO $ do
  let s = case tr of ((ss,_):_) -> ss; _ -> "REFL"
  printf "%s %10s %s\n" msg s (show t)
  _ <- getLine
  pure x
              | otherwise = x

addDone :: Traced a -> NormResult a -> NormResult a
addDone a nr = nr{ nrDone = a : nrDone nr }

-- Like normalFormsFuelTrace, but only does a depth first search
normalFormFuelTracePlain :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> NormResult a
normalFormFuelTracePlain env n rule t = dfs env n rule t

dfs :: (Show a, Ord a, Rec a) => RuleEnv a -> Int -> Rule a -> a -> NormResult a
dfs env an rule at = go an S.empty (start at)
 where
  go 0 _    tr   = NormResult { nrDone = [], nrLeft = [tr] }
  go n seen ttr@(t :<-- tr)
    | null ts'   = stepper "done" ttr $ NormResult { nrDone = [ttr], nrLeft = [] }
    | null ts''  = error "normalFormFuelTracePlain.dfs : no children"  -- a loop.  Maybe because of structural rules.
    | otherwise  =
      stepper "STEP" ttr $
      go (n-1) seen' (t' :<-- ((s, t) : tr))
    where
      seen' = S.insert t seen
      ts'   = step rule env t
      ts''  = filter ((`S.notMember` seen) . snd) ts'
      (s, t') = head ts''
