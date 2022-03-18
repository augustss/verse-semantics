{-# OPTIONS_GHC -Wall #-}
module OpSem.OpX(
  Frame,
  ContextId,
  Context(..),
  --Effect(..),
  HeapId(..),
  HeapAddr,
  Heap,
  Value(..),
  Target,
  OpX(..),
  PrimOp,
  ) where
import Data.List(intercalate)
import qualified Data.Map as M
import Text.PrettyPrint.HughesPJClass

import OpSem.Exp(Name, Exp)

--------------------------------
--
-- Frame
--  A Frame maps a Name in an Exp to its Value
--
--------------------------------

type Frame = M.Map Name Value

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
--  There is one Heap in each Context.
--
--------------------------------

type HeapAddr = Int
type Heap = M.Map HeapAddr (Maybe Value)

--------------------------------
--
-- Context
--  A ContextId uniquely identifies a Context to distingush different heaps.
--  A Context holds all the state for a local execution.
--
--------------------------------
type ContextId = [Int] -- XXX

data Context = Ctx
  { ctx_id     :: ![Int]
  , ctx_heap   :: !Heap
  , ctx_done   :: ![OpX]
  , ctx_ops    :: ![OpX]
  , ctx_parent :: !(Maybe Context)
  , ctx_next   :: !(Maybe Context)  -- backtrack point
  --, ctx_effects:: ![Effect]         -- allowed effects
  , ctx_hold   :: !Bool             -- hold all sequential effects XXX use mask?
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
  deriving (Eq, Show)  
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
                  , vf_body     :: Exp
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
  = UnifyX Value Value
  | CallX   { targetx :: Target, callx_fun :: Value, callx_arg :: Value }
  | ChoiceX { choice_left :: [OpX], choice_right :: [OpX] }
  | IfX     { targetx :: Target
            , ifx_cond :: Context,
              ifx_exports :: [(Name, HeapAddr)]
            , ifx_then :: (Frame, Exp), ifx_else :: (Frame, Exp) }

  | ForX    { targetx :: Target
            , forx_arr :: [Value]  -- The result of the 'for' is accumulated here.
            , forx_dom :: Context,
              forx_exports :: [(Name, HeapAddr)]
            , forx_body :: (Frame, Exp) }
  | RangeX  { targetx :: Target, rangex_arr :: Value }
  | FailX
  | ErrorX

  deriving (Eq, Show)
