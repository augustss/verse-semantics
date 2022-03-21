{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE PatternSynonyms #-}
module OpSem.OpX(
  Frame,
  emptyFrame,
  ContextId,
  Context(..),
  --Effect(..),
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
  ) where
import Data.List(intercalate, find)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import GHC.Stack
import Text.PrettyPrint.HughesPJClass

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
    Nothing -> error $ "lookupHeap: not in heap " ++ show a
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
    heap = fromMaybe (error $ "getHeapValue: " ++ show ci) $
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
  --, ctx_effects:: ![Effects]         -- allowed effects, used as a stack
  }
  deriving (Eq, Show)

--------------------------------
--
-- Effect
--  Possible effects
--
--------------------------------
{-
data Effect
  = Failure   -- 0 results
  | Success   -- 1 result
  | Decides   -- 0/1 results
  | Iterates  -- 0..n results
  -----
  | Allocates -- heap allocation
  | Reads     -- heap read
  | Writes    -- heap write
  -----
  | Interacts -- I/O
  deriving (Eq, Ord, Show)

type Effects = S.Set Effect
-}
--------------------------------
--
-- Value
--  A Value is a WHNF or an uninstantiated logical variables.
--
--------------------------------
data Value = VInteger Integer
           | VArray [Value]
           | VPrimOp PrimOp
           | VFun { vf_frame    :: Frame
                  , vf_arg_name :: Name
                  , vf_body     :: SExp
                  }
           | VHeap HeapId    -- Possibly unsettled logical variable
  deriving (Eq)

type PrimOp = String

instance Show Value where
  show (VInteger i) = show i
  show (VArray vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show (VPrimOp o) = "Prim" ++ o
  show (VFun f n e) = "(VFun " ++ show f ++ " " ++ show n ++ " (" ++ show e ++"))"
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
  deriving (Eq, Show)

-- An OpX that is guaranteed to fail.
pattern FailX :: OpX
pattern FailX = UnifyX (VInteger 0) (VInteger 1)
