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
  = Atom { target :: Name, op_atom :: Value }    -- Set target to this value

  | PushFrame String [Name] [Op]   -- The String is only for debugging

  | Unify Name Name  -- r1 = r2

  | MkArray  { target :: Name, elts :: [Name] }
  | Call     { target :: Name, fun :: Name, arg :: Name }
  | Function { target   :: Name    -- Where the function closure should go
             , fun_arg  :: Name
             , fun_body :: [Op]
             , fun_res  :: Name }  -- This holds the result of fun_body
  | RangeOp  { target :: Name, arr :: Name }

  | Choice { choice_left :: [Op], choice_right :: [Op] }
  | If     { for_dom :: [Op], if_then, if_else :: [Op] }
  | For    { target :: Name, for_dom :: [Op], for_body :: [Op], for_res :: Name }

  | ErrorOp String
  deriving (Eq, Show)

data OpX
  = UnifyX Value Value
  | CallX   { targetx :: Value, callx_fun :: Value, callx_arg :: Value }
  | ChoiceX { choice_left :: [OpX], choice_right :: [OpX] }
  | IfX     { ifx_cond :: Context, ifx_exports :: [(Name,HeapId)]
            , ifx_then :: Body, ifx_else :: Body }

  | ForX    { forx_dom :: Context, for_exports :: [(Name,HeapId)]
            , forx_body :: Body }

  | RangeX  {}

  | ExtendArrayX   HeapId Value  -- Extend the array with one more value
  | FinaliseArrayX HeapId        -- Finished with creating the array; now we can
                                 -- ask its size, and know when indexing is out of bounds

data Context = Cxt { cxt_heap   :: Heap
                   , cxt_done   :: [OpX]  -- Reversed
                   , cxt_ops    :: [OpX]
                   , cxt_parent :: Context
                   , cxt_next   :: WhatNext }

data WhatNext = AllDone              -- Top level
              | NextChoice Context   -- Backtrack point
              | IfThenElse <then> <else>

data Body = Body { body_ops   :: [Op]
                 , body_frame :: Frame }


step :: Context -> Context
step cxt@(Cxt { cxt_done = done, cxt_ops = [] })
  | not (null done)
  = cxt { cxt_done = [], cxt_opts = reverse done }
step cxt@(Cxt { cxt_ops = op:ops })
  = step1 cxt@(Cxt { cxt_ops = ops }) op


step1 :: Context -> OpX -> Context
step1 cxt (UnifyX v1 v2)
  = cxt & setHeap h' & addResiduals residual_ops
    -- Do not retry the residuals, else you get an
    -- infinite loop for a stuck unification
  where
    (h', residual_ops) = unify (cxt_heap cxt) v1 v2

step1 cxt op@(CallX { callx_target = target,callx_fun = fun, callx_args = arg })
  | Just (VFun bound_nm parent_frame body_ops) = getValue cxt fun
  = case getValue cxt fun of
      Just (VFun bound_nm parent_frame body_ops)  -- Function closure
        -> -- Main payload!   Rename body, inline the instructions
           cxt & setHeap h' & addOps body_opxs
        where
          (h', body_opxs) = rename (cxt_heap cxt) frame_w_binding body_ops
          frame_w_binding = makeFrame "CallX" [(bound_nm,arg)] parent_frame

      Just (VArray vals)
        -> case getValue cxt arg of
             Just (VInteger idx) ->
               -> case indexArray vals idx of
                    Just val -> addOps [UnifyX target (vals!!idx)] cxt
                    Nothing  -> failure cxt
             Just (VHeap {}) -> suspend op cxt
             _               -> wrong "Bad index in array indexing"

      Just (VHeap {}) -> suspend op cxt
      _               -> wrong "Bad function in CallX"


suspend :: OpX -> Context -> Context
suspend op cxt = addResiduals [op] cxt

failure :: Context -> Context
-- Expects ctx_next If or For

rename :: Heap -> Frame -> [Op] -> (Heap, [OpX])
rename h f []       = (h, [])
rename h f (op:ops) = (h2, opx1 ++ opxs2)
  where
   (h1, opxs1) = rename1 h f op
   (h2, opxs2) = rename h1 f ops

--------------------------------------
-- Atom, Function, and MakeArray do "lightweight execution",
-- by directly creating values
rename1 :: Heap -> Frame -> Op -> (Heap, [OpX])
rename1 h f (Atom { target = name, op_atom = value })
  = (h, [UnifyX (lookupFrame f name) value])

rename1 h f (Function { target = tgt, fun_arg = arg, fun_body = body, fun_res = res })
  = (h, [UnifyX tgt_val fun_val])
  where
    tgt_val = lookupFrame f tgt
    fun_val = VFun { vf_frame = f, vf_arg_name = arg
                   , vf_body = body, vf_res = res }

rename1 h f (MkArray { target = name, elts = values })
  = (h, [UnifyX tgt_val arr_val])
  where
    tgt_val = lookupFrame f tgt
    arr_val = VArray (map (lookupFrame f) elts)

rename1 h f (Unify n1 n2)
  = (h, [UnifyX (lookupFrame f n1) (lookupFrame f n2)])

rename1 h f (PushFrame str ns ops)
  = rename h' f' ops
  where
    (h', f') = extendFrame h f ns


--------------------------------------
-- Utility functions

indexArray :: [Value] -> Int -> Maybe Value
-- Index the array of values; Nothing if out of boudnds

extendFrame :: Heap -> Frame -> [Name] -> (Heap, Frame)

lookupFrame :: Frame -> Name -> Value
getValue :: Context -> Value -> Value
unify :: Heap -> Value -> Value -> (Heap, [OpX])
-- Returns zero or more residual suspended UnifyX instructions
-- Zero instructions returned => unify succeeded


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
           | VPrimOp PrimOp
           | VArray [Value]
           | VFun { vf_frame    :: Frame
                  , vf_arg_name :: Name
                  , vf_body     :: [Op]
                  , vf_res      :: Name }
           | VContextId ContextId   -- Only used in 'for' loops
           | VDummy
           | VHeap String HeapId    -- Possibly unsettled logical variable (String for debugging)
  deriving (Eq)

type PrimOp = String

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
-- NB: Isomorphic to Map Name Value

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

