module OpSem.Effects(
  Effect(..),
  Effects,
  memberEffect, topLevelEffects, iterContextEffects,
  nonCommutativeEffects, nonCommutativeWithEffects, noEffects,
  globalEffects,
  mkEffects,
  ) where
import Data.List

-- TODO:
--  * think carefully about what effects commute
--  * include divergence as an effect

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
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype Effects = E { unE :: [Effect] }
  deriving (Eq)

instance Show Effects where
  showsPrec p (E fs) = showsPrec p fs

-- Is the effect local to the current context?
isLocalEffect :: Effect -> Bool
isLocalEffect f = f `elem` [Failure, Decides, Iterates]

isUndoable :: Effect -> Bool
isUndoable f = f `notElem` [Interacts]

memberEffect :: Effect -> Effects -> Bool
-- (memberEffect e es) returns True if effect 'e'
-- is allowed by effects 'es'
memberEffect Failure (E fs) = not $ null $ intersect fs [Failure, Decides, Iterates]
memberEffect Decides (E fs) = not $ null $ intersect fs [Decides, Iterates]
memberEffect f (E fs) = f `elem` fs

globalEffects :: Effects -> Effects
globalEffects (E fs) = E $ filter (not . isLocalEffect) fs

-- The top level can use the store (read/write, etc)
-- But you cannot fail, nor produce multiple results.
topLevelEffects :: Effects
topLevelEffects = E $ [Interacts] `union` storeEffects

-- Effects on the store
storeEffects :: [Effect]
storeEffects = [Allocates, Reads, Writes]

-- In an iteration subcontext we inherit the global effects and add iteration.
iterContextEffects :: Effects -> Effects
iterContextEffects (E fs) = E $ [Iterates] `union` filter ok fs
  where ok f = not (isLocalEffect f) && isUndoable f

-- Effects to those that commute with every other effect.
-- Used when we need to suspend unknown effects.
-- Currently, only Allocates commutes with everything.
nonCommutativeEffects :: Effects
nonCommutativeEffects = E
  [ Failure, Decides, Iterates, Reads, Writes, Interacts ]

-- Return all the effects that don't commute with fs.
-- Use when holding effects fs.
nonCommutativeWithEffects :: [Effect] -> Effects
nonCommutativeWithEffects fs = E $ foldr union [] $ map nonComm fs
  where
    comm Reads = allEffects \\ [Writes]  -- Reads commute with everything except Writes
    comm Allocates = allEffects          -- Allocates commutes with everything
    comm _ = []                          -- Nothing else commutes
    nonComm f = allEffects \\ comm f
    allEffects = [minBound .. maxBound]      

noEffects :: Effects
noEffects = E []

mkEffects :: [Effect] -> Effects
mkEffects = E . nub
