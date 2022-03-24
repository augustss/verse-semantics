{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
module OpSem.OpX(
  Frame,
  emptyFrame,
  ContextId,
  Context(..),
  HeapId(..),
  HeapAddr,
  Heap, Heaps,
  mkHeap, lookupHeap, insertHeap, allocHeap, keysHeap, idHeap,
  getHeapValue,
  Value(..),
  Target,
  OpX(..),
  pattern FailX,
  PrimOp,
  Ref,
  Store,
  emptyStore, newStore, readStore, writeStore, mapStore,
  module OpSem.Effects,
  opEffects,
  ) where
import Data.List(intercalate, find)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import GHC.Stack
import Text.PrettyPrint.HughesPJClass

import OpSem.Effects
import OpSem.Error
import OpSem.Exp(Name, SExp)

--------------------------------
--
-- Frame
--  A Frame maps a Name in an Exp to its Value
--
--------------------------------

type Frame = M.Map Name Value

emptyFrame :: Frame
emptyFrame = M.empty

--------------------------------
--
-- HeapId
--  A HeapId uniquely identifies a cell in the heaps
--
--------------------------------

data HeapId = HeapId !ContextId !HeapAddr
  -- ContextId: you can only instantiate variables in the current context
  --            (the "flexible" ones)
  -- Also distinguishes distinct varibles with the same HeapAddr
  --      (HeapAddr's are local to a context)
  deriving (Eq, Ord)

instance Show HeapId where
  show (HeapId c h) = "R" ++ show c ++ "." ++ show h

--------------------------------
--
-- Heap
--  A Heap maps a HeapAddr to its Value within a particular Heap.
--  Each Heap cell corresponds to a logical variable.
--  Uninstantiated logical variables map to Nothing.
--  There is one Heap in each Context.
--
--------------------------------

type HeapAddr = Int

data Heap = Heap
  { heap_next     :: !HeapId    -- Next free heap cell
  , heap_contents :: !(M.Map HeapAddr (Maybe Value))
  }
  deriving (Show, Eq)

type Heaps = [Heap]

-- Create an empty heap
mkHeap :: ContextId -> Heap
mkHeap ci = Heap{ heap_next = HeapId ci 0, heap_contents = M.empty }

-- Find the contents of a Heap cell.
lookupHeap :: HasCallStack => HeapAddr -> Heap -> Maybe Value
lookupHeap a h =
  case M.lookup a (heap_contents h) of
    Nothing -> internalError $ "lookupHeap: not in heap " ++ show a
    Just mv -> mv

-- Insert a Value in the Heap.
insertHeap :: HeapAddr -> Maybe Value -> Heap -> Heap
insertHeap a mv h = h{ heap_contents = M.insert a mv (heap_contents h) }

-- Allocate a new heap cell.
allocHeap :: Heap -> (Heap, HeapId)
allocHeap (Heap h@(HeapId ci a) m) = (Heap (HeapId ci (a+1)) (M.insert a Nothing m), h)

-- Get all HeapId for a Heap.
keysHeap :: Heap -> [HeapId]
keysHeap (Heap (HeapId ci _) m) = [ HeapId ci a | a <- M.keys m ]

-- The unique Heap identifier.
idHeap :: Heap -> ContextId
idHeap (Heap (HeapId ci _) _) = ci

-- Find the heap contents at the given HeapId.
-- Looks in all the Heaps.
getHeapValue :: Heaps -> HeapId -> Maybe Value
getHeapValue heaps (HeapId ci h) =
  let
    heap = fromMaybe (internalError $ "getHeapValue: " ++ show ci) $
             find ((== ci) . idHeap) heaps
  in
    lookupHeap h heap

--------------------------------
--
-- Context
--  A ContextId uniquely identifies a Context to distingush different heaps.
--  A Context holds all the state for a local execution.
--
--------------------------------
type ContextId = Int

data Context = Ctx
  { ctx_heap   :: !Heap
  , ctx_ops    :: ![OpX]
  , ctx_next   :: !(Maybe Context)  -- Backtrack point, always built by ChoiceX
  , ctx_effects:: !Effects          -- Allowed effects (at birth of the Context)
  }
  deriving (Eq, Show)

--------------------------------
--
-- Store
--  The Store holds the values for ref cells.
--
--------------------------------

type Ref = Int

data Store = Store !Ref !(M.Map Ref Value)
  deriving (Eq, Show)

emptyStore :: Store
emptyStore = Store 0 M.empty

newStore :: Value -> Store -> (Store, Ref)
newStore v (Store r m) = (Store (r+1) (M.insert r v m), r)

readStore :: Ref -> Store -> Value
readStore r (Store _ m) =
  fromMaybe (internalError "readStore") $ M.lookup r m

writeStore :: Ref -> Value -> Store -> Store
writeStore r v (Store n m) = Store n (M.insert r v m)

mapStore :: (Value -> Value) -> Store -> Store
mapStore f (Store r m) = Store r (M.map f m)

--------------------------------
--
-- Value
--  A Value is a WHNF or an uninstantiated logical variables.
--
--------------------------------
data Value = VInteger Integer
           | VArray [Value]
           | VPrimOp [Effect] PrimOp
           | VFun { vf_frame    :: Frame
                  , vf_arg_name :: Name
                  , vf_body     :: SExp
                  }
           | VRef Ref        -- pointer to a cell in the Store
           | VHeap HeapId    -- possibly unsettled logical variable
  deriving (Eq)

type PrimOp = String

instance Show Value where
  show (VInteger i) = show i
  show (VArray vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show (VPrimOp _ o) = "Prim" ++ o
  show (VFun f n e) = "(VFun " ++ show f ++ " " ++ show n ++ " (" ++ show e ++"))"
  show (VRef r) = "(VRef " ++ show r ++ ")"
  show (VHeap h) = "[" ++ show h ++ "]"

instance Pretty Value where
  pPrint (VFun _f n c) = sep [text "VFun" <+> pPrint n, nest 2 (pPrint c)]
  pPrint v = text $ show v

--------------------------------
--
-- OpX
--
--
--------------------------------

type Target = Value   -- used to indicate the this is the result of an operation

data OpX
  = UnifyX  { unifyx_left   :: Value
            , unifyx_right  :: Value
            }
  | ChoiceX { choicex_left  :: [OpX]
            , choicex_right :: [OpX]
            }
  | IfX     { targetx       :: Target
            , ifx_cond      :: Context
            , ifx_exports   :: [(Name, HeapId)]    -- Bound in condition, can be
                                                   -- used in 'then' branch (only)
            , ifx_then      :: (Frame, SExp)
            , ifx_else      :: (Frame, SExp)       -- XXX This could be [OpX]
            }
  | CallX   { targetx       :: Target
            , callx_fun     :: Value
            , callx_arg     :: Value
            }
  | ForX    { targetx       :: Target
            , forx_arr      :: [Value]  -- The result of the 'for' is accumulated here.
            , forx_dom      :: Context
            , forx_exports  :: [(Name, HeapId)]
            , forx_body     :: (Frame, SExp)
            }
  | RangeX  { targetx       :: Target   -- tgt = :array
            , rangex_arr    :: Value    -- Returns the elts of the array, successively
            }
  | PushEffectsX {  -- Ensure that the given effects are allowed, and then limit the
                    -- effects to that set until PopEffectsX.
            push_effects    :: [Effect]
            }
  | PopEffectsX
  deriving (Eq, Show)

-- An OpX that is guaranteed to fail.
pattern FailX :: OpX
pattern FailX = UnifyX (VInteger 0) (VInteger 1)

-- The effects of the PrimOps
opEffects :: PrimOp -> [Effect]
opEffects "+" = []
opEffects "-" = []
opEffects "*" = []
opEffects "negate" = []
opEffects "abs" = []
opEffects "div" = [Decides]
opEffects "<" = [Decides]
opEffects "<=" = [Decides]
opEffects ">" = [Decides]
opEffects ">=" = [Decides]
opEffects "read" = [Reads]
opEffects "write" = [Writes]
opEffects "new_" = [Allocates]
opEffects "print" = [Interacts]
opEffects s = internalError $ "opEffects: " ++ s

