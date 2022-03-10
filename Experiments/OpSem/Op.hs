{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module OpSem.Op(
  Op(..),
  Reg(..),
  Seq(..),
  Value(..),
  Closure(..),
  Frame(..),
  ContextId(..),
  HeapAddr,
  HeapId(..),
  ) where
import Data.List(intercalate)
import Data.Map(Map)
import Text.PrettyPrint.HughesPJClass

import OpSem.Exp(Name)
import OpSem.Misc()

--------------------------------
--
-- Registers
--
--------------------------------

data Reg = Reg { reg_name  :: Name }
  deriving (Eq)

instance Pretty Reg where
  pPrint (Reg n) = text n

--  deriving (Show)
instance Show Reg where
  show (Reg n) = n

data Seq = Seq { sq_choice :: Reg }
  deriving (Eq, Show)

instance Pretty Seq where
  pPrint (Seq r) = pPrint r

--------------------------------
--
-- Op codes
--
--------------------------------

data Op
  = Atom { target :: Reg, op_atom :: Value }    -- Set target to this value

  | PushFrame String [Name] [Op]   -- The String is only for debugging
  | EndFrame

  | Unify Reg Reg  -- r1 = r2
  | Assign Reg Reg -- r1 := r2, first operand must be uninstantiated

  | MkArray { target :: Reg, elts :: [Reg] }
  | Call { argSq :: Seq, retSq :: Seq, target :: Reg, fun :: Reg, arg :: Reg }
  | Function { target :: Reg, argName :: Name, body :: [Op] }
  | EndFun Seq Reg
  | Choice { choice_in :: Reg, choice_left :: [Op], choice_right :: [Op] }
  | Failure
  | PrimBinOp { binOp :: String, target :: Reg, arg1 :: Reg, arg2 :: Reg }
  | Iterate { it_name :: String, it_context :: Reg, domain :: [Op], success :: [Op], failur :: [Op] }
  | EndDomain Seq

  -- NextFor: hacky loop implementation mechanism
  -- The arrAcc is where the resulting array is accumulated.
  -- This reg is never used anywhere else, so the invariant
  -- that a frame value never changes is ignored.
  -- The nx_domain holds the ContextId for the context of the domain.
  | NextFor { nx_domain :: Reg, arrAcc :: Reg, domValue :: Reg, isq :: Seq, osq :: Seq }   -- append domValue to the arrAcc and start next iteration
  | RangeOp { choice_in :: Reg, choice_out :: Reg, target :: Reg, arr :: Reg }
  --- 
  | Dump String
  | ErrorOp String
  | Stop Seq Reg  -- Just for testing, print the accum and stop
  deriving (Eq, Show)

instance Pretty Op where
  pPrintList _ ops = vcat $ map pPrint ops
  pPrint (Atom r a) =
    text "Atom" <+> pPrint r <+> pPrint a
  pPrint (PushFrame s ns ops) =
    text "PushFrame" <+> text s <+> brackets (text (intercalate "," ns)) $$
    nest 2 (pPrint ops)
  pPrint EndFrame =
    text "EndFrame"
  pPrint (Unify r1 r2) =
    text "Unify" <+> pPrint r1 <+> pPrint r2
  pPrint (Assign r1 r2) =
    text "Assign" <+> pPrint r1 <+> pPrint r2
  pPrint (MkArray r rs) =
    text "MkArray" <+> pPrint r <+> pPrint rs
  pPrint (Call r1 r2 r3 r4 r5) =
    text "Call" <+> pPrint r1 <+> pPrint r2 <+> pPrint r3 <+> pPrint r4 <+> pPrint r5
  pPrint (Function r1 r2 ops) =
    text "Function" <+> pPrint r1 <+> pPrint r2 $$
    nest 2 (pPrint ops)
  pPrint (EndFun sq r) =
    text "EndFun" <+> pPrint sq <+> pPrint r
  pPrint (Choice r1 ops1 ops2) =
    text "Choice" <+> pPrint r1 $$
    nest 2 (pPrint ops1) $$
    text "  -------- second branch" $$
    nest 2 (pPrint ops2) $$
    text "  -------- end Choice"
  pPrint Failure =
    text "Failure"
  pPrint (PrimBinOp op r1 r2 r3) =
    text "PrimBinOp" <+> pPrint op <+> pPrint r1 <+> pPrint r2 <+> pPrint r3
  pPrint (Iterate s c ops1 ops2 ops3) =
    text "Iterate" <+> text s <+> pPrint c $$
    nest 2 (pPrint ops1) $$
    text "  -------- success" $$
    nest 2 (pPrint ops2) $$
    text "  -------- failure" $$
    nest 2 (pPrint ops3) $$
    text "  -------- end Iterate"
  pPrint (EndDomain sq) =
    text "EndDomain" <+> pPrint sq
  pPrint (NextFor r1 r2 r3 r4 r5) =
    text "NextFor" <+> pPrint r1 <+> pPrint r2 <+> pPrint r3 <+> pPrint r4 <+> pPrint r5
  pPrint (RangeOp r1 r2 r3 r4) =
    text "RangeOp" <+> pPrint r1 <+> pPrint r2 <+> pPrint r3 <+> pPrint r4
  pPrint (Dump s) =
    text "Dump" <+> text (show s)
  pPrint (ErrorOp s) =
    text "ErrorOp" <+> text (show s)
  pPrint (Stop sq r) =
    text "Stop" <+> pPrint sq <+> pPrint r

--------------------------------
--
-- Values
-- Values are what is stored in the heap.
-- All values except VHeap are in WHNF.
--
--------------------------------

data Value = VInteger Integer
           | VArray [Value]
           | VFun Name Closure      -- Frame is captured when we build the closure
           | VContextId ContextId   -- Only used in 'for' loops
           | VDummy
           | VHeap String HeapId    -- Possibly unsettled logical variable (String for debugging)
  deriving (Eq)

instance Show Value where
  show (VInteger i) = show i
  show (VFun n c) = "(VFun " ++ show n ++ show c ++")"
  show (VArray vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show (VHeap s h) = s++"[" ++ show h ++ "]"
  show (VContextId c) = show c
  show VDummy = "VDummy"

instance Pretty Value where
  pPrint (VFun n c) = sep [text "VFun" <+> pPrint n, nest 2 (pPrint c)]
  pPrint v = text $ show v

data Closure = Closure Frame [Op]
  deriving (Show, Eq)

instance Pretty Closure where
  pPrint (Closure fr ops) =
    text "Closure" $$ nest 2 (pPrint fr $$ pPrint ops)

--------------------------------
--
-- Frames
--  Variable bindings.
--  They correspond to the lexical structure of the code.
--  A Frame is immutable, but if the Value is a VHeap, then
--  the actual value can be refined over time.
-----
data Frame = Frame
  { fr_name   :: String           -- Name for debugging
  , fr_vals   :: Map Name Value   -- The value is (VHeap heap_id) for logical variables
                                  --     but is any value for lamba-bound variables,
                                  --     and after expungeFrame.
  , fr_parent :: Maybe Frame      -- Lexical parent
  }
  deriving (Eq, Show)

instance Pretty Frame where
  pPrint Frame{..} = text "Frame" $$ nest 2 (vcat
    [ text "fr_name =" <+> text fr_name
    , text "fr_vals =" <+> pPrint fr_vals
    , sep [text "fr_parent =", nest 2 $ pPrint fr_parent]
    ])

--------------------------------
--
-- ContextId
--  A ContextId uniquely identifies a Context
--
--------------------------------

newtype ContextId = ContextId Int
  deriving (Eq, Ord, Enum)

instance Show ContextId where
  show (ContextId c) = "C" ++ show c

instance Pretty ContextId where
  pPrint = text . show

--------------------------------
--
-- HeapId
--  A ContextId uniquely identifies a cell in the heaps
--
--------------------------------

type HeapAddr = Int

data HeapId = HeapId !ContextId !HeapAddr
  -- ContextId: you can only instantiate variables in the current context
  --            (the "flexible" ones)
  -- Also distinguishes distinct varaibles with the same HeapAddr
  --      (HeapAddr's are local to a context)
  deriving (Eq, Ord)

instance Show HeapId where
  show (HeapId (ContextId c) h) = "R" ++ show c ++ "." ++ show h

