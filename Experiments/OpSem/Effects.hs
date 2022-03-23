module OpSem.Effects(
  Effect(..),
  Effects,
  memberEffect, topLevelEffects, subContextEffects,
  commutesWithEffects, commutativeEffects, noEffects,
  ) where
import qualified Data.Set as S

--------------------------------
--
-- Effect
--  Possible effects
--  This is a bit of a mish-mash of actual effects
--  and sets of effects.
--------------------------------
data Effect
  = Failure   -- 0 results
  | Decides   -- 0/1 results
  | Iterates  -- 0..n results

  -----
  | Allocates -- heap allocation
  | Reads     -- heap read
  | Writes    -- heap write

  -----
  | Interacts -- I/O

{- ---- Later ---------
  | Succeeds  -- 1 result
  | Transacts -- = { Allocates, Reads, Writes }
  -----
  | Total     -- = { }, i.e., no effects
  | Pure      -- = { Diverges }, may diverge
--------------------  -}
  deriving (Eq, Ord, Show)

type Effects = S.Set Effect

memberEffect :: Effect -> Effects -> Bool
-- (memberEffect e es) returns True if effect 'e'
-- is allowed by effects 'es'
memberEffect Failure effs | any (`S.member` effs) [Decides, Iterates] = True
memberEffect Decides effs | any (`S.member` effs) [Iterates] = True
memberEffect f effs = S.member f effs

-- The top level can use the store (read/write, etc)
-- But you cannot fail, nor produce multiple results.
topLevelEffects :: Effects
topLevelEffects = S.fromList [Interacts] `S.union` storeEffects

-- Effects on the store
storeEffects :: Effects
storeEffects = S.fromList [Allocates, Reads, Writes]

-- In a subcontext we inherit the store effects and add iteration
subContextEffects :: Effects -> Effects
subContextEffects effs =
  S.singleton Iterates `S.union` (storeEffects `S.intersection` effs)

-- Limit effects to those that commute with every other effect.
-- Used when we need to suspend unknown effects.
commutativeEffects :: Effects -> Effects
commutativeEffects effs =
  effs `S.intersection` S.fromList [Allocates]  -- XXX Succeeds?

commutesWithEffects :: [Effect] -> Effects -> Effects
commutesWithEffects fs effs = foldr commutes effs fs
  where
    commutes :: Effect -> Effects -> Effects
    -- XXX approximate by saying "nothing" commutes
    commutes _ x = commutativeEffects x

noEffects :: Effects
noEffects = S.empty
