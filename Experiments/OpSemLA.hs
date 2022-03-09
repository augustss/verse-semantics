{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module OpSemLA where
import Data.List
import Control.Monad.State.Strict
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import Data.String
import GHC.Stack
--import Text.PrettyPrint hiding (semi)
import Text.PrettyPrint.HughesPJClass hiding (semi)
import Ex
import Debug.Trace

debug, moreDebug, stepDebug, stepFrameDebug, sqDebug :: Bool
{-
debug = True
moreDebug = True
stepDebug = True
stepFrameDebug = True
sqDebug = True
-}
debug = False
moreDebug = False
stepDebug = False
stepFrameDebug = False
sqDebug = False

assert :: HasCallStack => String -> Bool -> a -> a
assert s False _ = error $ "assert: " ++ s
assert _ True  a = a

assertM :: (HasCallStack, Monad m) => String -> Bool -> m ()
assertM s False = error $ "assert: " ++ s
assertM _ True  = pure ()

{- XXX This is what I'd like to do, but I can't figure out how.
import Control.Monad.Extra(concatMapM)
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f as = concat <$> mapM f as
-}

pp :: (Pretty a) => a -> IO ()
pp = putStrLn . prettyShow

showListWith :: (a -> String) -> [a] -> String
showListWith f = ("[" ++) . (++ "]") . intercalate "," . map f

instance (Pretty k, Pretty v) => Pretty (Map k v) where
  pPrint = pPrint . M.toList

--------------------------------
--
-- Code
--
--------------------------------

{- BNF syntax for the language
   e ::= x
      |  k
      |  (s1 | s2)
      |  (e = k)
      |  x := e
      |  (e1,...,en)
      |  e[i]
      |  e1 + e2
      |  :false
      |  for(s1){e2}
      |  do{s}
      |  :e
   s ::= def {x1,...} in e
-}

type Name = String

data Exp = Var Name
         | Con Integer
         | Semi Exp Exp  -- e1; e2 === (e1, e2)[1]
         | Where Exp Exp  -- e1 where e2 === (e1, e2)[0]
         | Alt SExp SExp
         | Equal Exp Exp
         | Set Name Exp
         | SetAny Name
         | Array [Exp]   -- (e1, ..., en)  aka  array{e1, ..., en}
         | Plus Exp Exp
         | Fail
         | For SExp SExp
         | If SExp SExp SExp
         | Do SExp
         | Range Exp     -- :e
         | Lam Name SExp
         | AppS Exp Exp
         | AppI Exp Exp
         | Error
  deriving (Show)

data SExp     -- A scope-limiting construct
  = Def [Name]   -- Bring these variables into scope
        Exp      -- In this expression
  deriving (Show)

---------------------
--      Sugar
---------------------

instance Num Exp where
  (+) = Plus
  fromInteger = Con

instance IsString Exp where
  fromString = Var

infixl 4 |||
(|||) :: Exp -> Exp -> Exp
x ||| y = Alt (addDef x) (addDef y)

infixl 3 #
(#) :: Exp -> Exp -> Exp
(#) = Pair

infixl 5 ===
(===) :: Exp -> Exp -> Exp
(===) = Equal

infix 2 :=
pattern (:=) :: Name -> Exp -> Exp
pattern (:=) x e = Set x e

pattern Fst :: Exp -> Exp
pattern Fst e = AppS e (Con 0)
pattern Snd :: Exp -> Exp
pattern Snd e = AppS e (Con 1)
pattern Pair :: Exp -> Exp -> Exp
pattern Pair e1 e2 = Array [e1, e2]
pattern Sel :: Exp -> Integer -> Exp
pattern Sel e i = AppS e (Con i)

-- Sequencing, evaluate both and return second
infixl 1 `semi`
semi :: Exp -> Exp -> Exp
semi x y = Semi x y

-- Sequencing, evaluate both and return first
infix 1 `wher`
wher :: Exp -> Exp -> Exp
wher x y = Where x y

for :: Exp -> Exp -> Exp
for e1 e2 = For (addDef e1) (addDef e2)

iF :: Exp -> Exp -> Exp -> Exp
iF e1 e2 e3 = If (addDef e1) (addDef e2) (addDef e3)

doo :: Exp -> Exp
doo e = Do (addDef e)

lam :: Name -> Exp -> Exp
lam n e = Lam n (addDef e)

var :: Name -> Exp
var = SetAny

-- Add all variables defined in the current scope.
addDef :: HasCallStack => Exp -> SExp
addDef e | xs /= nub xs = error $ "Duplicate := " ++ show (e, xs)
         | otherwise = Def xs e
  where xs = findSet e

findSet :: Exp -> [Name]
findSet Var {}   = []
findSet Con {}   = []
findSet (Semi e1 e2) = findSet e1 ++ findSet e2
findSet (Where e1 e2) = findSet e1 ++ findSet e2
findSet Alt {}   = []
findSet Fail     = []
findSet For {}   = []
findSet If {}   = []
findSet Do {}    = []
findSet Lam {}   = []
findSet (AppS  e1 e2) = findSet e1 ++ findSet e2
findSet (AppI  e1 e2) = findSet e1 ++ findSet e2
findSet (Equal e1 e2) = findSet e1 ++ findSet e2
findSet (Set x e) = x : findSet e
findSet (SetAny x) = [x]
findSet (Array es) = concatMap findSet es
findSet (Plus e1 e2) = findSet e1 ++ findSet e2
findSet (Range e) = findSet e
findSet Error = []

--------------------------
--
-- Convert an Exp to a list of Op
--
--------------------------

data CompileState = CompileState
  { nextTemp :: !Int,    -- next temp reg
    tempRegs :: [Name],  -- temporary registers in this frame
    cops :: [Op]         -- generated ops so in this frame
  }
  deriving (Show)

type C = State CompileState

newName :: (Int -> String) -> C Name
newName f = do
  s <- get
  let t = succ (nextTemp s)
  put s{nextTemp = t}
  pure $ f t

newReg' :: String -> C Reg
newReg' p = do
  n <- newName $ \ t -> p ++ show t
  s <- get
  put s{tempRegs = n : tempRegs s}
  pure Reg { reg_name = n }

newReg :: C Reg
newReg = newReg' "%"

newRegSq :: HasCallStack => C Reg
newRegSq = newReg' "$"

emit :: Op -> C ()
emit op = modify $ \ s -> s { cops = cops s ++ [op] }

{-
hackOpt :: [Op] -> [Op]
hackOpt [PushFrame _ [n] [Atom {op_target = Reg n', op_atom = a},Load (Reg n''),EndFrame],Store t,EndOps]
  | n == n', n == n'' = hackOpt [Atom t a, EndOps]
hackOpt (PushFrame _ [] ops : rs)
  | last ops == EndFrame = hackOpt (init ops ++ rs)
hackOpt (PushFrame _ [n] [Atom {op_target = Reg n', op_atom = AnInteger i},Load (Reg n''),EndFrame] : rs)
  | n == n', n == n'' = LoadInteger i : hackOpt rs
hackOpt (PushFrame s ns ops : rs) = PushFrame s ns (hackOpt ops) : hackOpt rs
hackOpt (Function t n ops : rs) = Function t n (hackOpt ops) : hackOpt rs
hackOpt (Choice ops1 ops2 : rs) = Choice (hackOpt ops1) (hackOpt ops2) : hackOpt rs
hackOpt (Iterate n c d s f : rs) = Iterate n c (hackOpt d) (hackOpt s) (hackOpt f) : hackOpt rs
hackOpt (op : rs) = op : hackOpt rs
hackOpt [] = []
-}

data Seq = Seq { sq_choice :: Reg }
  deriving (Eq, Show)

instance Pretty Seq where
  pPrint (Seq r) = pPrint r

expToReg :: Seq -> Exp -> C (Seq, Reg)
expToReg sq (Var n) = pure (sq, Reg n)
expToReg sq (Con i) = do
  t <- newReg
  emit $ Atom t (VInteger i)
  pure (sq, t)
expToReg sq (Semi e1 e2) = do
  (sq1, _) <- expToReg sq e1
  expToReg sq1 e2
expToReg sq (Where e1 e2) = do
  (sq1, r1) <- expToReg sq e1
  (sq2, _)  <- expToReg sq1 e2
  pure (sq2, r1)
expToReg sq (Alt e1 e2) = do
  t <- newReg
  sq' <- Seq <$> newRegSq
  op1 <- sexpToReg sq sq' t e1
  op2 <- sexpToReg sq sq' t e2
  let rsq = sq_choice sq
  emit $ Choice rsq [op1, EndFrame] [op2, EndFrame]
  pure (sq', t)
expToReg sq (Equal e1 e2) = do
  (sq1, r1) <- expToReg sq e1
  (sq2, r2) <- expToReg sq1 e2
  emit $ Unify r1 r2
  pure (sq2, r2)
expToReg sq (Set n e) =
  expToReg sq $ Equal (Var n) e
expToReg sq (SetAny n) =
  expToReg sq (Var n)
expToReg sq (Array es) = do
  let f s [] = pure (s, [])
      f s (x:xs) = do (s', r) <- expToReg s x; (s'', rs) <- f s' xs; pure (s'', r:rs)
  (sq', rs) <- f sq es
  t <- newReg
  emit $ MkArray t rs
  pure (sq', t)
expToReg sq (Plus e1 e2) = do
  (sq1, r1) <- expToReg sq e1
  (sq2, r2) <- expToReg sq1 e2
  t <- newReg
  emit $ Add t r1 r2
  pure (sq2, t)
expToReg sq Fail = do
  emit Failure
  t <- newReg                -- we must return something, but this reg will never be set
  pure (sq, t)
expToReg sq (For e1 e2) = do
  t <- newReg  -- Final result, not instantiated until the array is complete.
  a <- newReg' "%%"  -- Accumulate the resulting array here
  c <- newReg  -- Domain context
  lsq <- Seq <$> newReg' "$$" -- Choice sequencing in the loop.  Like 'a', hackily updated.
#if 0
  xxxsq <- newRegSq
  o1 <- sexpToOps' {-dsq-}(Seq xxxsq) (\ sq' _r -> [EndDomain sq']) e1
  o2 <- sexpToOps' lsq (\ sq' v -> [NextFor c a v lsq sq']) e2
  msg <- newName (\ n -> "for-ctx" ++ show n)
  emit $ MkArray a []
  emit $ Assign (sq_choice lsq) (sq_choice sq)
  emit $ Iterate msg c [o1] [o2] [Unify t a, EndOps]
  emit $ Atom xxxsq VDummy
#else
  dsq <- newSeq
  o1 <- sexpToOps' dsq (\ sq' _r -> [EndDomain sq']) e1
  o2 <- sexpToOps' lsq (\ sq' v -> [NextFor c a v lsq sq']) e2
  msg <- newName (\ n -> "for-ctx" ++ show n)
  emit $ MkArray a []
  --emit $ Assign (sq_choice lsq) (sq_choice sq)
  xsq <- newRegSq
  emit $ Assign (sq_choice lsq) xsq
  emit $ Iterate msg c [o1] [o2] [Unify t a, EndOps]
  emit $ Assign xsq (sq_choice sq)
#endif
  pure (lsq, t)
expToReg sq (Range e) = do
  t <- newReg
  (sq', r) <- expToReg sq e
  rsq <- newRegSq
  emit $ RangeOp (sq_choice sq') rsq t r
  pure (Seq rsq, t)
expToReg sq (If e1 e2 e3) = do
  dsq <- newSeq
  t <- newReg
  -- XXX Use _sq
  o1 <- sexpToOps' dsq (\ sq' _r -> [EndDomain sq']) e1
  rosq <- newRegSq
  let osq = Seq rosq
  o2 <- sexpToReg sq osq t e2
  o3 <- sexpToReg sq osq t e3
  c <- newReg
  msg <- newName (\ n -> "if-ctx" ++ show n)
  -- The o1 sequence has a PushFrame without a matching EndFrame.
  -- The EndDomain instruction expunges this frame and pushes on the
  --   in the parent context.
  -- The success continuation has the EndFrame to pop this.
  -- The failure continuation changes to the parent context, and just pushes the failure ops.
  emit $ Iterate msg c [o1] [o2, EndFrame] [o3, EndOps]
  pure (osq, t)
expToReg sq (Do e) = do
  t <- newReg
  sq' <- Seq <$> newRegSq
  o <- sexpToReg sq sq' t e
  emit o
  pure (sq', t)
expToReg sq (Lam n e) = do
  os <- sexpToOps' (Seq $ Reg $ "$" ++ n) (\ osq r -> [EndFun osq r]) e
  t <- newReg
  emit $ Function t n [os]
  pure (sq, t)
-- XXX This wrong.  AppS should make sure there is exactly one result
-- Could use something like
--    f(a)  -->  if (x:=f[a]) then x else WRONG
expToReg sq (AppS e1 e2) = do
  (sq1, r1) <- expToReg sq e1
  (sq2, r2) <- expToReg sq1 e2
  t <- newReg
  sqr <- Seq <$> newRegSq
  emit $ Call sq2 sqr t r1 r2
  pure (sq2, t)
expToReg sq (AppI e1 e2) = do
  (sq1, r1) <- expToReg sq e1
  (sq2, r2) <- expToReg sq1 e2
  t <- newReg
  sqr <- Seq <$> newRegSq
  emit $ Call sq2 sqr t r1 r2
  pure (sq2, t)
expToReg sq Error = do
  emit $ ErrorOp "Error"
  t <- newReg
  pure (sq, t)
--expToReg x = error $ show x

sexpToOps' :: HasCallStack => Seq -> (Seq -> Reg -> [Op]) -> SExp -> C Op
sexpToOps' sq ops (Def ns e) = do
  olds <- get
  put olds{ cops = [], tempRegs = [] }
  (sq', r) <- expToReg sq e
  s <- get
  put olds{ nextTemp = nextTemp s + 1 }
  let tmps = tempRegs s
      msg = "fr" ++ show (nextTemp s)
  pure $ PushFrame msg (ns ++ tmps) (cops s ++ ops sq' r)

sexpToReg :: HasCallStack => Seq -> Seq -> Reg -> SExp -> C Op
sexpToReg sq tsq t =
  sexpToOps' sq $ \ sq' r -> [Unify t r, Assign (sq_choice tsq) (sq_choice sq'), EndFrame]

newSeq :: C Seq
newSeq = do
  rsq <- newRegSq
  emit $ Atom rsq VDummy
  pure $ Seq rsq

-- The main compiler
comp :: SExp -> [Op]
comp e = evalState se cs
  where cs = CompileState{ nextTemp = 0, cops = [], tempRegs = [] }
        se = do
          t <- newReg
          sq <- newSeq
          osq <- Seq <$> newRegSq
          op <- sexpToReg sq osq t e
          is <- gets cops
          let ops = is ++ [op, Stop osq t]
          pure $ [PushFrame "comp" [reg_name t, reg_name (sq_choice sq), reg_name (sq_choice osq)] ops]

--------------------------------
--
-- Machine state
--
--------------------------------

data Reg = Reg { reg_name  :: Name }
  deriving (Eq)

instance Pretty Reg where
  pPrint (Reg n) = text n

--  deriving (Show)
instance Show Reg where
  show (Reg n) = n

data Op
  = Atom { target :: Reg, op_atom :: Value }
    -- Set r to this value

  | PushFrame String [Name] [Op]   -- The String is only for debugging
  | EndFrame

  | Unify Reg Reg  -- r1 = r2
  | Assign Reg Reg -- r1 := r2, first operand must be uninstantiated

  | MkArray { target :: Reg, elts :: [Reg] }
  | Call { argSq :: Seq, retSq :: Seq, target :: Reg, fun :: Reg, arg :: Reg }
  | Function { target :: Reg, argName :: Name, body :: [Op] }
  | EndFun Seq Reg
  | Choice { choice_in :: Reg, choice_left :: [Op], choice_right :: [Op] }
  | EndOps
  | Failure
  | Add { target :: Reg, arg1 :: Reg, arg2 :: Reg }
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
    "Choice" <+> pPrint r1 $$
    nest 2 (pPrint ops1) $$
    text "  -------- second branch" $$
    nest 2 (pPrint ops2) $$
    text "  -------- end Choice"
  pPrint EndOps =
    text "EndOps"
  pPrint Failure =
    text "Failure"
  pPrint (Add r1 r2 r3) =
    text "Add" <+> pPrint r1 <+> pPrint r2 <+> pPrint r3
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

-------------------------------------------------

data Value = VInteger Integer
           | VArray [Value]
           | VFun Frame Name [Op]   -- Frame is captured when we build the closure
           | VHeap String HeapId    -- Possibly unsettled logical variable (String for debugging)
           | VContextId ContextId   -- Only used in 'for' loops
           | VDummy
  deriving (Eq)

instance Show Value where
  show (VInteger i) = show i
  show VFun{} = "VFun{}"
  show (VArray vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show (VHeap s h) = s++"[" ++ show h ++ "]"
  show (VContextId c) = show c
  show VDummy = "VDummy"

instance Pretty Value where
  pPrint = text . show

-----
-- Global execution state
----

data RunState =  -- The global state of the machine
  RunState
  { rs_contexts       :: !(Map ContextId Context)   -- all active contexts
  , rs_nextContextId  :: !ContextId                 -- contextId
  , rs_currentContext :: !ContextId                 -- currently active context
  }
  deriving (Show)

newtype ContextId = ContextId Int
  deriving (Eq, Ord, Enum)
instance Show ContextId where
  show (ContextId c) = "C" ++ show c
instance Pretty ContextId where
  pPrint = text . show

startRunState :: RunState
startRunState = RunState { rs_contexts = M.empty, rs_nextContextId = ci, rs_currentContext = ci }
  where ci = ContextId 0

-----
-- State monad holding the global state
-----
type R = State RunState

-- Add the given context to the active contexts
newContextId :: R ContextId
newContextId = do
  rs <- get
  let ci = rs_nextContextId rs
  put rs{ rs_nextContextId = succ ci }
  pure ci

-- Update a stored context
updateContext :: Context -> R ()
updateContext ctx =
  modify $ \ rs -> rs{ rs_contexts = M.insert (ctx_id ctx) ctx (rs_contexts rs) }

-- Get a context
getContext :: ContextId -> R Context
getContext ci = fromMaybe (error "get") . M.lookup ci <$> gets rs_contexts

-- Get the currently executing context
getCurContext :: R Context
getCurContext = getContext =<< gets rs_currentContext

-- Set the currently executing context
setCurContextId :: ContextId -> R ()
setCurContextId ci = modify $ \ rs -> rs{ rs_currentContext = ci }

modifyCurContext :: (Context -> Context) -> R ()
modifyCurContext f = updateContext =<< (f <$> getCurContext)

runRunState :: R a -> a
runRunState ra = evalState ra startRunState

-----
-- Context for expression evaluation.
--  The current context changes as evaluation proceeds.
-----
data Context
  = Ctx { ctx_name     :: !String            -- for debugging
        , ctx_id       :: !ContextId         -- Id of this context

        , ctx_frame    :: !Frame             -- The lexical environment, maps names to values

        , ctx_heap     :: !Heap              -- Stores logical variables
        , ctx_heapAddr :: !HeapAddr          -- Next heap address

        , ctx_ops      :: ![Op]              -- Program counter

        , ctx_stack    :: ![StackFrame]      -- The call stack that "belongs" to this context

        , ctx_susps    :: ![Suspension]

        , ctx_next     :: Maybe Context -- NB Context not ContextId; this is what
                                        -- lets us backtrack to an "old" state.

        -- Only relevant when ctx_next = Nothing
        , ctx_parent   :: Maybe ContextId    -- Does not vary
        , ctx_failure  :: [Op]          -- Do this if the head Op in ctx_ops fails
                                        -- Does not vary
        , ctx_success  :: [Op]          -- ToDo: could this just be the tail of ctx_ops?
                                        -- Does not vary
        -- ToDo: put failure and success into parent

        }
  deriving (Show)

{-
   ctx_next :: NextContext

data NextContext = ParentContext ContextId [Op] [Op]
                 | NextContext Context
-}

instance Pretty Context where
  pPrint Ctx{..} = text "Ctx" $$ nest 2 (vcat
    [ text "ctx_name =" <+> text ctx_name
    , text "ctx_id =" <+> text (show ctx_id)
    , text "ctx_frame =" <+> pPrint ctx_frame
    , text "ctx_heap =" <+> pPrint ctx_heap
    , text "ctx_heapAddr =" <+> pPrint ctx_heapAddr
    , text "ctx_ops =" <+> pPrint ctx_ops
    , text "ctx_stack =" <+> pPrint ctx_stack
    , text "ctx_susps =" <+> pPrint ctx_susps
    , text "ctx_next =" <+> pPrint ctx_next
    , text "ctx_parent =" <+> pPrint ctx_parent
    , text "ctx_failure =" <+> pPrint ctx_failure
    , text "ctx_success =" <+> pPrint ctx_success
    ])

data StackFrame
  = ContFrame String Frame [Op]     -- end of a PushFrame
  | ContFun Value Value Frame [Op] -- Return from a function call,
                             -- unifying the result with the value
  | ContOps [Op]             -- end of a join (Alt)
  deriving (Show)

instance Pretty StackFrame where
  pPrint (ContFrame s fr ops) =
    text ("ContFrame " ++ s) $$ nest 2 (vcat
      ["fr =" <+> pPrint fr
      ,"ops =" <+> pPrint ops
      ])
  pPrint (ContFun sq target fr ops) =
    text "ContFun" $$ nest 2 (vcat
      ["sq =" <+> pPrint sq
      ,"target =" <+> pPrint target
      ,"fr =" <+> pPrint fr
      ,"ops =" <+> pPrint ops
      ])
  pPrint (ContOps ops) =
    text "ContOps" $$ nest 2 (vcat
      ["ops =" <+> pPrint ops
      ])

showStackFrame :: StackFrame -> String
showStackFrame = head . words . show

data Suspension = Susp
  { susp_waitingFor :: [HeapId]   -- for faster runnable check  XXX not used
  , susp_cont       :: SuspCont
  }
  deriving (Show)

instance Pretty Suspension where
  pPrint = text . showSusp -- XXX

showSusp :: Suspension -> String
showSusp (Susp hs s) = head (words (show s)) ++ show hs

data SuspCont
  = SuspUnify Value Value
  | SuspAdd Value Value Value
  | SuspCall Value Value Value Value Value
  | SuspDomain Seq ContextId
  | SuspRange Reg Reg Frame Reg Value
  | SuspChoice Value Frame [Op] [Op]
  deriving (Show)

addSusp :: [HeapId] -> SuspCont -> R ()
addSusp hs susp = do
  when debug $ do
    ctx <- getCurContext
    traceM $ "addSusp: ci=" ++ show (ctx_id ctx) ++ " " ++ showSusp (Susp hs susp)
  modifyCurContext $ \ ctx -> ctx{ ctx_susps = ctx_susps ctx ++ [Susp hs susp] }

-----
-- Store for logical variables.
--   There is one heap for each context.
-----
type HeapAddr = Int
type Heap = Map HeapAddr (Maybe Value)
            -- Nothing => no one has instantiated this variable yet
            -- This is just for assertion-checking; we could equally
            --      well use (Map HeapAddr Value)
            -- We can change (x :-> Nothing) to (x :-> Just val); but once
            -- we add (x :-> Just val) to a Heap, we never change that binding

data HeapId = HeapId !ContextId !HeapAddr
  -- ContextId: you can only instantiate variables in the current context
  --            (the "flexible" ones)
  -- Also distinguishes distinct varaibles with the same HeapAddr
  --      (HeapAddr's are local to a context)
  deriving (Eq, Ord)

instance Show HeapId where
  show (HeapId (ContextId c) h) = "R" ++ show c ++ "." ++ show h

-- Remove all references to HeapAddrs in the Heap from the value
--   Runtime error if this is circular: just spot when you pass
--   the same HeapId a second time.  This is WRONG; verifier
--   should reject.
expunge :: HasCallStack => ContextId -> Heap -> Value -> Value
expunge ci heap = value []
  where
    value :: [HeapAddr] -> Value -> Value
    -- 's' tracks the HeapIds we have seen already, for loop detection
    value _ v@VInteger{} = v
    value s (VArray vs) = VArray (map (value s) vs)
    value s (VFun fr n os) = VFun (frame s fr) n os
    value s v@(VHeap _ (HeapId ci' h))
      | ci /= ci' = v
      | h `elem` s = error $ "WRONG: expunge recursion:\n" ++ show (h, s, heap)
      | otherwise =
          case M.lookup h heap of
            Nothing -> error "expunge: not in heap"
            Just Nothing -> error $ "expunge: WRONG: uninstantiated " ++ show (h, ci, heap)  -- XXX is it wrong
            Just (Just v') -> value (h:s) v'
    value _ v@VContextId{} = v
    value _ VDummy = VDummy
    frame s Frame{..} = Frame{ fr_name = fr_name
                             , fr_parent = assert "expunge" (fr_parent `hasNoHeapIdsFrom` ci) fr_parent
                             , fr_vals = M.map (value s) fr_vals
                             }

-- XXX Implement this
hasNoHeapIdsFrom :: Maybe Frame -> ContextId -> Bool
hasNoHeapIdsFrom _ _ = True

-- As expunge, but for a Frame
expungeFrame :: HasCallStack => ContextId -> Heap -> Frame -> Frame
expungeFrame ci heap fr = f $ expunge ci heap (VFun fr "" [])
  where f (VFun fr' "" []) = fr'
        f _ = error "impossible"

-----
-- Variable bindings.
--  They correspond to the lexical structure of the code.
--  A Frame is immutable, but if the Value is a VHeap, then
--  the actual value can be refined overt time.
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

ctxDump :: Context -> String
ctxDump ctx = "ctx=" ++ ctx_name ctx ++ ": fr=" ++ fr_name (ctx_frame ctx) ++ "\n"
              ++ "  frame=" ++ show (fr_vals (ctx_frame ctx)) ++ "\n"
              ++ "  heap=" ++ show (ctx_heap ctx) ++ "\n"
              ++ concatMap (\ s -> "  " ++ s ++ "\n") (map stkDump (ctx_stack ctx))
  where stkDump s = take 1200 (show s)

ctxDumps :: Context -> R String
ctxDumps ctx = do
  let s = ctxDump ctx
  contexts <- gets rs_contexts
  (s ++) <$> maybe (pure "") ctxDumps ((contexts M.!) <$> ctx_parent ctx)

pushFrame :: (HasCallStack) => [Op] -> Frame -> Context -> Context
-- (pushFrame ops fr ctx) returns ctx' which executes (ops, fr),
-- returning to ctx's (ops,fr) when that is done.
pushFrame ops fr ctx =
  let cs = getCallStack callStack
      line = srcLocStartLine $ snd $ head cs
  in ctx{ ctx_ops = ops, ctx_frame = fr, ctx_stack = ContFrame (show line) (ctx_frame ctx) (ctx_ops ctx) : ctx_stack ctx }

pushOps :: HasCallStack => [Op] -> Context -> Context
pushOps ops ctx =
--  assert "pushOps" (not $ null $ ctx_ops ctx) $
  ctx{ ctx_ops = ops, ctx_stack = ContOps (ctx_ops ctx) : ctx_stack ctx }

assign :: Reg -> Value -> Context -> Context
-- Preconditions:
--   * The register is in the current frame
--   * The register is unbound
assign r@Reg{..} val ctx =
  let fr = ctx_frame ctx
      heap = ctx_heap ctx
  in  case M.lookup reg_name (fr_vals fr) of
        Just (VHeap _ (HeapId ci h)) ->
          assert "assign" (ci == ctx_id ctx) $
          case M.lookup h heap of
            Nothing -> error $ "assign: not in heap " ++ show (r, h)
            Just Nothing -> setHeapAddr h (Just val) ctx
            Just (Just v) -> error $ "assign: heap already set " ++ show (r, h, v)
        Just v -> error $ "assign: already set " ++ show (r, v)
        Nothing -> error $ "assign: not in frame " ++ show r

assignSq :: Value -> Value -> Context -> Context
assignSq v1 v2 _ | sqDebug && trace ("assignSq: " ++ show v1 ++ " := " ++ show v2) False = undefined
assignSq v1 v2 ctx | v1 == v2 = ctx
assignSq (VHeap _ (HeapId ci h)) val ctx =
  assert "assignSq" (ci == ctx_id ctx) $
  case M.lookup h (ctx_heap ctx) of
    Nothing -> error $ "assignSq: not in heap " ++ show h
    Just Nothing -> setHeapAddr h (Just val) ctx
    Just (Just v) -> error $ "assignSq: heap already set " ++ show (h, v)
assignSq tgt val _ = error $ "assignSq: " ++ show (tgt, val)

newHeapIds :: Int -> Context -> ([HeapAddr], Context)
newHeapIds n ctx =
  let hs = take n [ h .. ]
      h = ctx_heapAddr ctx
  in  (hs
      ,ctx{ctx_heapAddr = h+n, ctx_heap = foldr (uncurry M.insert) (ctx_heap ctx) (zip hs (repeat Nothing)) }
      )

makeFrame :: String -> [(Name, Value)] -> Frame -> Frame
makeFrame msg nvs prnt =
  Frame { fr_name = msg
        , fr_vals = M.fromList [(n, v) | (n, v) <- nvs ]
        , fr_parent = Just prnt }

loadValue :: Reg -> Context -> Value
loadValue r ctx = loadValue' (reg_name r) (Just (ctx_frame ctx))
  where
    loadValue' :: Name -> Maybe Frame -> Value
    loadValue' n Nothing = error $ "loadValue: not found " ++ show n
    loadValue' n (Just fr) = fromMaybe (loadValue' n (fr_parent fr)) $ M.lookup n $ fr_vals fr

-- Follow VHeap indirections
follow :: Value -> R Value
follow = follow' []

follow' :: [HeapId] -> Value -> R Value
follow' s (VHeap _ h) | h `elem` s = error $ "follow': loop " ++ show (h, s)
follow' s v@(VHeap _ h) = do
  mv <- getHeap h
  case mv of
    Just v' -> follow' (h:s) v'
    _ -> pure v
follow' _ v = pure v

getHeap :: HeapId -> R (Maybe Value)
getHeap (HeapId ci h) = do
  contexts <- gets rs_contexts
  maybe (error "getHeap 1") (pure . fromMaybe (error "getHeap 2") . M.lookup h . ctx_heap) $ M.lookup ci contexts

unify :: Value -> Value -> R ()
unify v1 v2 | moreDebug && trace ("unify: " ++ show (v1, v2)) False = undefined
unify v1 v2 | v1 == v2 = pure ()
            | otherwise = do
                ctx <- getCurContext
                v1' <- follow v1
                v2' <- follow v2
                unify' (ctx_id ctx) v1' v2'

unify' :: ContextId -> Value -> Value -> R ()
unify' ci v1 v2 | moreDebug && trace ("unify': " ++ show (ci, v1, v2)) False = undefined
unify' _ v1 v2 | v1 == v2 = pure ()
unify' ci (VHeap _ h1) v2 | isFlex h1 ci = setHeap h1 v2
unify' ci v1 (VHeap _ h2) | isFlex h2 ci = setHeap h2 v1
unify' _ v1@(VHeap _ h1) v2 = suspendUnify h1 v1 v2
unify' _ v1 v2@(VHeap _ h2) = suspendUnify h2 v1 v2
unify' _ (VArray vs1) (VArray vs2)
  | length vs1 /= length vs2 = failure "unify' 1"
  | otherwise = zipWithM_ unify vs1 vs2
unify' _ VFun{} VFun{} = error "WRONG: comparing functions"
unify' _ _ _ = failure "unify' 2"

isFlex :: HeapId -> ContextId -> Bool
isFlex (HeapId c _) ci = c == ci

suspendUnify :: HeapId -> Value -> Value -> R ()
suspendUnify h v1 v2 = addSusp [h] $ SuspUnify v1 v2

-- Set a heap location in the current heap.
-- Can only be used to instantiate variables, not modifying the heap.
setHeap :: HeapId -> Value -> R ()
setHeap h v | moreDebug && trace ("setHeap: " ++ show (h, v)) False = undefined
setHeap (HeapId ci h) v = do
  ctx <- getCurContext
  assertM "setHeap" (ci == ctx_id ctx)
  case M.lookup h (ctx_heap ctx) of
    Nothing -> error $ "setHeap: not in heap " ++ show h
    Just (Just vv) -> error $ "setHeap: already set " ++ show (h, vv)
    Just Nothing -> modifyCurContext $ setHeapAddr h (Just v)

-- Blindly set the heap contents.  Used for non-monotonic updates.
setHeapAddr :: HasCallStack => HeapAddr -> Maybe Value -> Context -> Context
setHeapAddr h mv _ | moreDebug && trace ("setHeapAddr: " ++ show (h, mv)) False = undefined
setHeapAddr h (Just (VHeap s (HeapId ci h'))) ctx | h == h' && ci == ctx_id ctx = error $ "setHeapAddr: cycle " ++ s
setHeapAddr h mv ctx = ctx{ ctx_heap = M.insert h mv (ctx_heap ctx) }

-- When the domain fails, backtrack if possible.
-- When no more backtracking remains, use failure continuation.
failure :: HasCallStack => String -> R ()
failure msg = do
  ctx <- getCurContext
  when debug $
    traceM $ "failure: " ++ msg ++ " ctx=" ++ ctx_name ctx ++ " next=" ++ show (isJust (ctx_next ctx))
  case ctx of
    Ctx{ ctx_parent = Nothing } -> error "failure: no parent"
    Ctx{ ctx_next = Just nctx } ->
      -- The nctx has the same context id, just an old heap etc.
      assert "failure" (ctx_id ctx == ctx_id nctx) $
      updateContext nctx
    Ctx{ ctx_next = Nothing, ctx_parent = Just pci, ctx_failure = fOps } -> do
      setCurContextId pci
      modifyCurContext $ \ c -> pushOps fOps c
      --ctx <- getCurContext
      --traceM $ "failure: fail branch\n" ++ prettyShow ctx

addOp :: Value -> Value -> Value -> R ()
addOp dst src1 src2 = do
  src1' <- follow src1
  src2' <- follow src2
  case (src1', src2') of
    (VInteger i1, VInteger i2) -> unify dst (VInteger $ i1 + i2)
    (VHeap _ h1, _) -> addSusp [h1] (SuspAdd dst src1' src2')
    (_, VHeap _ h2) -> addSusp [h2] (SuspAdd dst src1' src2')
    _ -> failure "addOp"  -- WHNF, but not integers

-- Call f with argument a, but first wait for f to be in WHNF
callOp :: Value -> Value -> Value -> Value -> Value -> R ()
callOp _ _ t f a | debug && trace ("callOp " ++ show (t,f,a)) False = undefined
callOp sqa sqt t f a = do
  f' <- follow f
  case f' of
    VFun fr n ops -> apply sqa sqt t fr n ops a
    VArray vs -> do
      modifyCurContext $ assignSq sqt sqa
      a' <- follow a
      case a' of
        VInteger (fromInteger -> i) | i >= 0 && i < length vs -> unify t (vs !! i)
                                    | otherwise -> failure "callOp 1"
        VHeap _ h -> addSusp [h] (SuspCall sqa sqt t f' a')
        _ -> failure "callOp 2" -- XXX maybe WRONG
    VHeap _ h -> addSusp [h] (SuspCall sqa sqt t f' a)
    v -> error $ "Call: not a function/array " ++ show v

apply :: Value -> Value -> Value -> Frame -> Name -> [Op] -> Value -> R ()
-- Does not make a new Context/Heap; 
apply sqa sqt target fr argName ops arg =
  modifyCurContext $ \ ctx ->
  ctx{ ctx_ops   = ops  -- The 'ops' comes from the function closure
     , ctx_frame = makeFrame "apply" [(argName, arg), ("$" ++ argName, sqa)] fr
                        -- The 'fr' comes from the function closure
     , ctx_stack = ContFun sqt target (ctx_frame ctx) (ctx_ops ctx)
                   : ctx_stack ctx }

rangeOp :: Reg -> Reg -> Frame -> Reg -> Value -> R ()
rangeOp sqin sqout fr t a = do
  a' <- follow a
  case a' of
    VArray vs -> choices sqin sqout fr t vs
    VHeap _ h -> addSusp [h] (SuspRange sqin sqout fr t a')
    v -> error $ "rangeOp: not an array " ++ show v

choices :: Reg -> Reg -> Frame -> Reg -> [Value] -> R ()
choices _ _ _ _ [] = failure "choices"
-- XXX how should sq be treated
choices sqin sqout fr target xs = do  -- should behave like t := (x[0] | x[1] | ...)
  let ops = foldr1 (\ x y -> Choice sqin [x, EndFrame] [y, EndFrame]) (map (Atom target) xs)
  modifyCurContext $ pushFrame [ops, Assign sqout sqin, EndFrame] fr

choiceOp :: Value -> Frame -> [Op] -> [Op] -> R ()
choiceOp sqin fr ops1 ops2 = do
  sqin' <- follow sqin
  case sqin' of
    VHeap _ h -> do
      when debug $
        traceM $ "choiceOp: suspending on " ++ show h
      addSusp [h] (SuspChoice sqin fr ops1 ops2)
    _ -> do
      when debug $ do
        ctx <- getCurContext
        traceM $ "choiceOp: cloning\n" ++ prettyShow ctx
      -- The Choice instruction sequences end with an EndFrame.
      -- This is the corresponding pushFrame, which run in
      -- frame where the Choice was originally executed.
      modifyCurContext $ pushFrame ops1 fr
      ctx <- getCurContext
      let
        -- NB: both ctx1 and ctx2 start with the same heap
        ctx1 = ctx{ ctx_next = Just ctx2 }
        ctx2 = ctx{ ctx_ops = ops2
                  , ctx_name = ctx_name ctx ++ "-next"
                  }
      --error $ "ctx1=\n" ++ prettyShow ctx1 ++ "\nctx2=\n" ++ prettyShow ctx2
      updateContext ctx1

getOp :: R Op
-- Get the next Op from ctx_ops, and remove it from the list
getOp = do
  ctx <- getCurContext
  case ctx_ops ctx of
    [] -> error "getOp: no ops"
    op : ops -> do updateContext ctx{ctx_ops = ops}; pure op

-- For debugging
getFrames :: Context -> [Frame]
getFrames ctx = loop (Just (ctx_frame ctx))
  where loop (Just fr) = fr : loop (fr_parent fr)
        loop Nothing = []

getNexts :: Context -> [Context]
getNexts = maybe [] (\ c -> c : getNexts c) . ctx_next

getParents :: ContextId -> R [Context]
getParents ci = do
  c <- gets ((M.! ci) . rs_contexts)
  case ctx_parent c of
    Nothing -> pure [c]
    Just pci -> (c:) <$> getParents pci

dumpFrame :: Frame -> R String
dumpFrame fr | M.null (fr_vals fr) = pure ""
dumpFrame fr = do
  let nvs = M.toList (fr_vals fr)
  vs' <- mapM (followDeep . snd) nvs
  pure $ "  frame=" ++ fr_name fr ++ ": " ++ intercalate ", " (zipWith (\ (n, _) v -> n ++ " :-> " ++ show v) nvs vs') ++ "\n"

followDeep :: Value -> R Value
followDeep v = do
  v' <- follow v
  case v' of
    VArray vs -> VArray <$> mapM followDeep vs
    _ -> pure v'

stepR :: R ()
stepR = do
  op  <- getOp
  ctx <- getCurContext
  when stepDebug $ do
    traceM $ "stepR ctx=" ++ ctx_name ctx ++ "(" ++ show (ctx_id ctx) ++ ") fr=" ++ fr_name (ctx_frame ctx) ++ ": " ++ take 150 (show op)

  case op of
    Iterate n c d s f -> do
      nci <- newContextId
      let nctx =
            Ctx{ ctx_name = n
               , ctx_id = nci
               , ctx_ops = d
               , ctx_frame = Frame { fr_name = "fr-Iterate", fr_vals = M.empty, fr_parent = Just pfr }
               , ctx_stack = []
               , ctx_heap = M.empty
               , ctx_heapAddr = 0
               , ctx_susps = []
               , ctx_parent = Just (ctx_id ctx)
               , ctx_success = s
               , ctx_failure = f
               , ctx_next = Nothing
               }
          pfr = ctx_frame ctx
      modifyCurContext $ assign c (VContextId nci)
      setCurContextId nci
      updateContext nctx

    Choice rin ops1 ops2 -> choiceOp (loadValue rin ctx) (ctx_frame ctx) ops1 ops2

      -- The domain of an if/for has reached the end.
      -- So we are about to abandon the current context, ctx; in the case of 'for'
      --    we may come back to (ctx_next ctx).  All other fields are truly abandoned.
      -- The success continuation will now be run in the environment
      --    with the domain frame added.
      -- All traces of the domain heap must be expunged from the
      --    frame, since we are abandoning it.
    EndDomain sq | Ctx { ctx_ops = [], ctx_susps = [], ctx_parent = Just pci, ctx_id = ci
                       , ctx_success = sOps, ctx_frame = fr, ctx_heap = heap } <- ctx -> do
      vsq <- follow (loadValue (sq_choice sq) ctx)
      assertM "EndDomain sq" (vsq == VDummy)
      setCurContextId pci  -- Switch to parent context
      let fr' = fr{ fr_name = fr_name fr ++ "-domain", fr_vals = M.filterWithKey notTemp (fr_vals fr) }
          notTemp k _ = let c = head k in c /= '%' && c /= '$'
          fr'' = expungeFrame ci heap fr'
      when debug $ do
        traceM $ "EndDomain: fr' = " ++ prettyShow fr'
        traceM $ "           fr'' = " ++ prettyShow fr''
      modifyCurContext $ pushFrame sOps fr''
    EndDomain sq | Ctx{ ctx_ops = [], ctx_susps = susps, ctx_parent = Just pci, ctx_id = ci } <- ctx -> do
      setCurContextId pci
      addSusp (concatMap susp_waitingFor susps) (SuspDomain sq ci)

    PushFrame msg ns ops -> do
      let (hs, ctx') = newHeapIds (length ns) ctx
          fr = makeFrame msg (zipWith (\ n h -> (n, VHeap n $ HeapId (ctx_id ctx) h)) ns hs) (ctx_frame ctx)
      updateContext $ pushFrame ops fr ctx'

    -- EndFrame, EndFun, EndOps are always the last instruction in the [Op]
    EndFrame | Ctx{ ctx_ops = [], ctx_stack = ContFrame _ fr ops : stk } <- ctx ->
      modifyCurContext $ \ c -> c{ ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }

    -- The ops for a function lacks the trailing EndFrame.
    -- So we pop it here.
    EndFun rsq ret | Ctx{ ctx_ops   = []   -- EndFun is the last instruction
                         , ctx_stack = ContFrame _ xfr xops : ContFun sqt target fr ops : stk
                         } <- ctx -> do
      assertM ("EndFun " ++ show (xfr, xops)) (null xops && fr_name xfr == "apply")
      unify target (loadValue ret ctx)
      unify sqt (loadValue (sq_choice rsq) ctx)
      modifyCurContext $ \ c -> c{ ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }

    EndOps | Ctx{ ctx_ops = [], ctx_stack = ContOps ops : stk } <- ctx ->
      modifyCurContext $ \ c -> c{ ctx_ops = ops, ctx_stack = stk }


    -- NextFor is the last instruction of a PushFrame (of locals),
    -- so that frame is popped.  The second frame that is popped,
    -- and used, is pushed by EndDomain.
    NextFor rc ra rv lsq osq | Ctx{ ctx_ops = []
                                  , ctx_stack = ContFrame _ _xfr _xops : ContFrame _ fr ops : stk } <- ctx -> do
      -- dctx is the domain context, in case we need to iterate
      let dctx =
            case loadValue rc ctx of
              VHeap _ (HeapId _ h) ->
                case M.lookup h (ctx_heap ctx) of
                  Just (Just (VContextId c)) -> c
                  _ -> error "impossible: NextFor 3"
              _ -> error "impossible: NextFor 4"
      -- Update the sequencing cell
      case loadValue (sq_choice lsq) ctx of
        VHeap _ (HeapId ci h) -> do
          assertM "NextFor lsq" (ci == ctx_id ctx)
          when (lsq /= osq) $
            modifyCurContext $ setHeapAddr h (Just $ loadValue (sq_choice osq) ctx)
        _ -> error "impossible: NextFor 5"
      -- Find the array being accumulated
      case loadValue ra ctx of
        VHeap _ (HeapId ci h) -> do
          assertM "NextFor ra" (ci == ctx_id ctx)
          case M.lookup h (ctx_heap ctx) of
            Just (Just (VArray vs)) -> do
              let va = VArray (vs ++ [loadValue rv ctx])
              when debug $ do
                vs' <- mapM follow $ vs ++ [loadValue rv ctx]
                traceM $ "NextFor appends: " ++ show ((vs, loadValue rv ctx), vs')
                traceM $ "   " ++ showListWith showStackFrame (ctx_stack ctx)
              -- Violating heap invariant by updating a WHNF value.
              
              updateContext $
                setHeapAddr h (Just va) $
                  ctx{ ctx_ops = ops
                     , ctx_frame = fr
                     , ctx_stack = stk
                     }
              setCurContextId dctx
              when debug $
                traceM $ "  switch to " ++ show dctx
              --ctx <- getCurContext
              --traceM $ "NextFor:\n" ++ prettyShow ctx
              --traceM "--------------------"
              failure "NextFor"
            _ -> error "impossible: NextFor 1"
        _ -> error "impossible: NextFor 2"

    RangeOp sqin sqout t r -> rangeOp sqin sqout (ctx_frame ctx) t (loadValue r ctx)
    --Atom t v -> modifyCurContext $ assign t v
    Atom t v -> unify (loadValue t ctx) v
    MkArray t rs -> modifyCurContext $ assign t (VArray [ loadValue r ctx | r <- rs])
    Unify r1 r2 -> unify (loadValue r1 ctx) (loadValue r2 ctx)
    Assign r1 r2 -> modifyCurContext $ assignSq (loadValue r1 ctx) (loadValue r2 ctx)
    Add t x y -> addOp (loadValue t ctx) (loadValue x ctx) (loadValue y ctx)
    Function t n ops -> modifyCurContext $ assign t (VFun (ctx_frame ctx) n ops)
    Call sqa sqt t f a -> callOp (loadValue (sq_choice sqa) ctx) (loadValue (sq_choice sqt) ctx) (loadValue t ctx) (loadValue f ctx) (loadValue a ctx)
    Failure -> failure "Failure"
    ErrorOp s -> error $ "ErrorOp: " ++ s
    Dump msg -> do
      s <- ctxDumps ctx
      traceM ("Dump: " ++ msg ++ ": " ++ s)

    _ -> error $ "stepR op = " ++ show op ++ "\n" ++ show ctx

  when stepFrameDebug $ do
    c <- getCurContext
    ps <- getParents (ctx_id c)
    let showNexts p = "    " ++ ctx_name p ++ ": nexts=" ++ showListWith (unwords . take 2 . words . show . head . ctx_ops) (getNexts p)
    traceM $ "  ctx=" ++ ctx_name c ++ "(" ++ show (ctx_id c) ++ ")" ++
             "\n" ++ intercalate "\n" (map showNexts ps) ++ "\n" ++
             "  susps=" ++ showListWith showSusp (ctx_susps c)

    frdumps <- mapM dumpFrame $ getFrames c
    traceM $ concat $ reverse frdumps

  runSuspensions



-- There have been changes in the current context,
-- so check which suspensions that can be run.
-- Anything that is now runnable must be in the current context
-- or in subcontexts, since we cannot affect supercontexts.
runSuspensions :: R ()
runSuspensions = do
  susps <- ctx_susps <$> getCurContext
  when (debug && not (null susps)) $ do
    ci <- ctx_id <$> getCurContext
    traceM $ "runSuspensions: susps=" ++ show ci ++ showListWith showSusp susps
  trySusps [] susps
  where
    trySusps tried [] = modifyCurContext $ \ ctx -> ctx{ ctx_susps = tried }
    trySusps tried (susp:untried) = do
      -- Remove the susp we are trying
      modifyCurContext $ \ ctx -> ctx{ ctx_susps = tried ++ untried }
      did <- trySuspension susp
      if did then do
        -- Something happened, so try again.
        -- Any modification, e.g., new suspensions or context switch will persist.
        -- XXX New suspensions will be appended, is that correct?
        runSuspensions
       else
        trySusps (tried ++ [susp]) untried  -- try the next one

trySuspension :: Suspension -> R Bool
--trySuspension susp | debug && trace ("trySuspension " ++ show susp) False = undefined
trySuspension susp@(Susp hs sc) = do
  let isWHNF h = isJust <$> getHeap h
  oks <- mapM isWHNF hs
  if or oks then do
    when debug $
      traceM $ "trySuspension: wake " ++ show susp
    case sc of
      SuspUnify v1 v2   -> unify v1 v2
      SuspAdd v1 v2 v3  -> addOp v1 v2 v3
      SuspCall v1 v2 v3 v4 v5 -> callOp v1 v2 v3 v4 v5
      SuspDomain sq pci -> do
        pctx <- getCurContext
        setCurContextId pci
        modifyCurContext $ \ c ->
          assert "trySuspension SuspDomain ops" (null (ctx_ops c)) $
          c{ ctx_ops = [EndDomain sq] }
        ctx <- getCurContext
        assertM "trySuspension SuspDomain parent " (ctx_parent ctx == Just (ctx_id pctx))
        --traceM $ "trySuspension:\n" ++ prettyShow ctx
      SuspRange v1 v2 v3 v4 v5 -> rangeOp v1 v2 v3 v4 v5
      SuspChoice sqin fr ops1 ops2 -> choiceOp sqin fr ops1 ops2
    pure True
   else
    pure False

run :: [Op] -> [Value]
run ops = runRunState $ do
  ci <- newContextId
  let loop = do
        ctx <- getCurContext
        case ctx of
          Ctx {ctx_ops = [Stop sq r], ctx_heap = heap, ctx_susps = susps}
            | null susps -> do
              vsq <- follow (loadValue (sq_choice sq) ctx)
              assertM "run: sq" (vsq == VDummy)
              case expunge ci heap (loadValue r ctx) of
                VArray vs -> pure vs
                v@VInteger{} -> pure [v]
                vv -> error $ "top level not array: " ++ show vv
            | otherwise -> error "run: susps not empty"
          _ -> do stepR; loop
      ictx =
        Ctx{ ctx_name = "ctx_run"
           , ctx_id = ci
           , ctx_heap = M.empty
           , ctx_heapAddr = 1
           , ctx_frame = Frame { fr_name = "run", fr_vals = M.empty, fr_parent = Nothing }
           , ctx_ops = ops
           , ctx_susps = []
           , ctx_stack = []
           , ctx_parent = Nothing
           , ctx_success = [ErrorOp "ctx_success"]
           , ctx_failure = [ErrorOp "ctx_failure"]
           , ctx_next = Nothing
           }
  setCurContextId ci
  updateContext ictx
  loop

adjExp :: Exp -> SExp
adjExp = addDef . addFor
  where addFor e = for ("&it" := e) "&it"

compExp :: Exp -> [Op]
compExp = comp . adjExp

ev :: Exp -> [Value]
ev = run . compExp

---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (show $ ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (show $ ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (show $ ev e)

unimp :: (Show a) => String -> a -> Exp -> Ex String
unimp n _r e = Ex ("unimp: " ++ n) Nothing (show $ ev e)

---------------------
-- Simple, single valued tests.
---------------------
test101 = ok "test101" [5] $
  5

test102 = ok "test102" [42] $
  5 + 37

test103 = ok "test103" [(5,37)] $
  5 # 37

test104 = ok "test104" [(1,2,3,4)] $
  Array [1,2,3,4]

test100s = mapM_ testEx
  [test101,test102,test103,test104
  ]

---------------------
-- Variable scopes
---------------------
test201 = ok "test201" [(5,5)] $
  ("x" := 5) # "x"

test202 = ok "test202" [(5,5)] $
  "x" # ("x" := 5)

test203 = ok "test203" [(7,6)] $
  "x"+1 # ("x" := 6)

test204 = ok "test204" [(7,6,6,5)] $
  Array ["x"+1, "x" := "y", "y" := "z"+1, "z" := 5]

test205 = bad "test205" $
  ("x" := 1) # ("x" := 2)

test206 = bad "test206" $
  "x"

test207 = ok "test207" [(3,4)] $
  3 # doo ("x":= 4)

test208 = bad "test208" $
  "x" # doo ("x":= 4)

-- Check that mutual recursion fails
test209 = bad "test209" $
  "x" := "y" `semi` "y" := "x"

test210 = ok "test210" [(1,(2,3))] $
  "x" := (1 # "y") `semi`
  "y" := (2 # "z") `semi`
  "z" := 3 `semi`
  "x"

test211 = bad "test211" $
  "x" := 1 `semi` "x" := 2

-- The x1 used to be x, but shadowing is not allowed
test212 = ok "test212" [(1,2)] $
  "x" := 2 `semi` (doo ("x1" `wher` "x1" := 1) # "x")

test200s :: IO ()
test200s = mapM_ testEx
  [test201,test202,test203,test204,test205,test206,test207,test208,test209,test210,test211,test212
  ]

---------------------
-- 0/1 results
---------------------

test301 = ok "test301" [(3,3)] $
  ("x" := 3) # ("x" === 3)

test302 = ok "test302" [3] $
  ("x" := 1+"y") `semi` "y" := 2 `semi` ("x" === 3)

test303 = ok "test303" [(3,3)] $
  ("x" === 3) # ("x" := 3)

test304 = ok "test304" [20] $
  ("a" := Array [10,20,30]) `semi` Sel "a" 1

test305 = ok "test305" [20] $
  Sel "a" 1 `wher` ("a" := Array [10,20,30])

test306 = ok "test306" ([]::[()]) $
  ("a" := Array [10,20,30]) `semi` Sel "a" 3

test307 = ok "test307" [(1,1)] $
  "t" := Pair 1 (Fst "t")

-- Test that when evaluating z the x is fully determined.
test308 = ok "test308" [5] $
  "x" := "y" `semi` "y" := 5 `semi` "z" := ("x"===5)

test309 = ok "test309" ([]::[()]) $
  ("x" := 3) # ("x" === 4)

test310 = ok "test310" ([]::[()]) $
  ("x" === 4) # ("x" := 3)

-- Deadlock
test311 = bad "test311" $
  "y" := iF ("z"===1) (1|||2) (3|||4) `semi` "z":= 5|||6 `semi` ("y" # "z")

test300s :: IO ()
test300s = mapM_ testEx
  [test301,test302,test303,test304,test305,test306,test307,test308,test309,test310,test311
  ]

---------------------
-- Multi-valued
---------------------

test401 = ok "test401" [1,2] $
  1 ||| 2

test402 = ok "test402" [2,3,3,4] $
  (1 ||| 2) + (1 ||| 2)

test403 = ok "test403" [2,4] $
  ("x" := 1 ||| 2) + "x"

-- Should fail, since variables in ||| do not escape
test404 = bad "test404" $
  (("x" := 1) ||| 2) + "x"

test405 = ok "test405" [(4,(1,3)),(5,(1,4)),(5,(2,3)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := 3 ||| 4) # ("x" # "y")

test406 = ok "test406" [(2,(1,1)),(5,(1,4)),(4,(2,2)),(6,(2,4))] $
  ("x" := 1 ||| 2) + ("y" := "x" ||| 4) # ("x" # "y")

test407 = ok "test407" [4] $
  ("x" := 1 ||| 2) + ("x" === 2)

test408 = ok "test408" [(1,1),(2,2)] $
  Pair "x" ("x" := 1 ||| 2)

test409 = ok "test409" [(7,(1,1)),(7,(2,2)),(1,(1,1)),(2,(2,2))] $
  Pair ("y" := (7 ||| "x")) (Pair "x" ("x" := (1 ||| 2)))

-- x's value should not be delayed, because x's RHS has no depenedncies
test410 = ok "test410" [((1,7),1)] $
         Pair ("x" := (Pair 1 7 |||
                       Pair "y" ("y" := 2)))
              (Fst "x" === 1)

test411 = ok "test411" [(1,1)] $
  "x" := 1 ||| 2 `semi` "y" := ("x" === 1) `semi` ("x" # "y")

-- Fails (equalLenient)
test412 = ok "test412" [(1,1)] $
  "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Cascaded forward references
test413 = ok "test413" [3,7,2,2] $
  "x" := ("y" ||| 2)  `semi`
  "y" := (3 ||| "z")  `semi`
  "z" := 7            `semi`
  "x"

-- Choice under if
test414 = ok "test414" [(1,5),(1,6),(2,5),(2,6)] $
  "x" := 1 `semi`
  iF ("x" === 1) (1|||2) (3|||4) # (5|||6)

-- Choice under if, must suspend
test415 = ok "test415" [(1,5),(1,6),(2,5),(2,6)] $
  iF ("x" === 1) (1|||2) (3|||4) # (5|||6) `wher`
  "x" := 1

test400s :: IO ()
test400s = mapM_ testEx
  [test401,test402,test403,test404,test405,test406,test407,test408,test409,test410,test411,test412,test413,test414,test415
  ]

---------------------
-- Error/strictness
---------------------

-- Generates an error, as it should
test501 = bad "test501"
  Error

-- Generates an error, as it should
test502 = bad "test502" $
  Error `semi` 1

-- Generates an error, as it should
test503 = bad "test504" $
   (2 # Error) `semi` 1

test500s :: IO ()
test500s = mapM_ testEx
  [test501,test502,test503
  ]

---------------------
-- for
---------------------

test601 = ok "test601" [(5,5,5)] $
  for (1|||2|||3) 5

test602 = ok "test602" [(1,2,3)] $
  for ("x" := 1|||2|||3) "x"

test603 = ok "test603" [((1,4),(1,5),(2,4),(2,5),(3,4),(3,5))] $
  for ("x" := 1|||2|||3 `semi` "y" := 4|||5) ("x" # "y")

test604 = ok "test604" [((1,4),(2,4),(3,4)),
                        ((1,5),(2,5),(3,5))] $
  "y" := 4|||5 `semi` for ("x" := 1|||2|||3) ("x" # "y")

test605 = ok "test605" [(((1,4),(2,4),(3,4)),
                       ((1,5),(2,5),(3,5)))] $
  for ("y" := 4|||5) $ for ("x" := 1|||2|||3) ("x" # "y")

test606 = ok "test606" [(88,88),(88,99),(99,88),(99,99)] $
  for (0|||1) (88 ||| 99)

test607 = ok "test607" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test608 = ok "test608" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

test609 = ok "test609" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test610 = ok "test610"
            [((11,11),(21,21)),((11,11),(21,42)),((11,11),(32,21)),((11,11),(32,42))
            ,((11,42),(21,21)),((11,42),(21,42)),((11,42),(32,21)),((11,42),(32,42))
            ,((32,11),(21,21)),((32,11),(21,42)),((32,11),(32,21)),((32,11),(32,42))
            ,((32,42),(21,21)),((32,42),(21,42)),((32,42),(32,21)),((32,42),(32,42))]
            $
  for ("x" := 10|||20) $
    for ("y" := 30|||40)
      (("x1"|||"y1") `wher` ("x1" := "x" + 1 `semi` "y1" := "y" + 2))

test600s :: IO ()
test600s = mapM_ testEx
  [test601,test602,test603,test604,test605,test606,test607,test608,test609,test610
  ]

---------------------
-- Functions
---------------------

test701 = ok "test701" [5] $
  "f" := lam "v" ("v" + 1) `semi`
  AppS "f" 4

test702 = ok "test702" [11] $
  "w" := 7 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  AppS "f" 4

test703 = ok "test703" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  AppS "f" 4

test704 = ok "test704" [11] $
  "f" := lam "v" ("w" + "v") `semi`
  "w" := 7 `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

-- f is called before it is defined
test705 = ok "test705" [11] $
  "y" := AppS "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test706 = ok "test706" [11] $
  "f" := doo ("w" := 7 `semi` lam "v" ("w" + "v")) `semi`
  "y" := AppS "f" "t" `semi`
  "t" := 4 `semi`
  "y"

-- Function defined after it is used;
-- but the call is f[e], so we deadlock
test707 = ok "test707" [11] $
  "y" := AppI "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test708 = ok "test708" [10,11] $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppI "f" 10

{- AppS doesn't check for single value
test709 = bad "test709" $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppS "f" 10
-}

test700s :: IO ()
test700s = mapM_ testEx
  [test701,test702,test703,test704,test705,test706,test707,test708 --,test709
  ]

---------------------
-- Unification
---------------------
test801 = ok "test801" [1] $
  var "x" `semi`
  "x" === 1 `semi`
  "x"

test802 = ok "test802" [1] $
  var "x" `semi`
  ("x" # 2) === (1 # 2) `semi`
  "x"

test803 = ok "test803" [(1,2)] $
  var "x" `semi`
  var "y" `semi`
  ("x" # 2) === (1 # "y")

test804 = ok "test804" [1] $
  "f" := lam "xy" (Fst "xy" === Snd "xy") `semi`
  var "x" `semi`
  AppS "f" ("x" # 1) `semi`
  "x"

test805 = ok "test805" [6] $
  "f" := lam "xyz" ((var "x" # var "y" # var "z") === "xyz" `semi` "x" + "y" + "z") `semi`
  AppS "f" (1 # 2 # 3)

test806 = bad "test806" $
  var "x" `semi` "x"+1

test800s :: IO ()
test800s = mapM_ testEx
  [test801,test802,test803,test804,test805,test806--,test807,test808,test809
  ]

---------------------
-- Conditional
---------------------

test901 = ok "test901" [1] $
  iF (1 === 1) 1 2

test902 = ok "test902" [2] $
  iF (0 === 1) 1 2

test903 = ok "test903" [10] $
  iF ("x" := 10) "x" 2

test904 = ok "test904" [2] $
  iF Fail 1 2

test905 = ok "test905" [1] $
  iF ("x" := 1 `semi` "x" === 1) 1 2

test906 = ok "test906" [2] $
  iF ("x" := 1 `semi` "x" === 0) 1 2

test907 = ok "test907" [1] $
  iF ("x" === 1 `semi` "x" := 1) 1 2

test908 = ok "test908" [2] $
  iF ("x" === 0 `semi` "x" := 1) 1 2

test909 = ok "test909" [1] $
  "x" := 10 `semi`
  iF ("x" === 10) 1 2

test910 = ok "test910" [2] $
  "x" := 0 `semi`
  iF ("x" === 10) 1 2

test911 = ok "test911" [1] $
  "y" := iF ("x" === 10) 1 2 `semi`
  "x" := 10 `semi`
  "y"

test912 = ok "test912" [2] $
  "y" := iF ("x" === 10) 1 2 `semi`
  "x" := 0 `semi`
  "y"

test913 = ok "test913" [1] $
  iF ("x":=1) "x" 20

test914 = ok "test914" [1] $
  iF ("x" := (1 ||| 2)) 1 20

test915 = ok "test915" [2] $
  iF ("x" := (Fail ||| 2)) "x" 20

test916 = ok "test916" [1] $
  iF ("x" := (1 ||| Fail)) "x" 20

test917 = ok "test917" [20] $
  iF ("x" := (Fail ||| Fail)) "x" 20

test918 = ok "test918" [3] $
  iF ("x" := (Fail ||| (Fail ||| 3))) "x" 20

test900s :: IO ()
test900s = mapM_ testEx
  [test901,test902,test903,test904,test905,test906,test907,test908,test909,test910,test911,test912
  ,test913,test914,test915,test916,test917,test918
  ]

---------------------
-- Range
---------------------

test1001 = ok "test1001" [(1,2,3)] $
  for ("x" := Range (Array [1,2,3])) "x"

test1002 = ok "test1002" [(102,103,104)] $
  "xs" := for ("x" := 1|||2|||3) ("x" + 1) `semi`
  for ("y" := Range "xs") ("y" + 100)

test1003 = ok "test1003" [((1,2),(1,4),(1,5))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 1) "xy"

test1004 = ok "test1004" ([]::[()]) $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test1005 = ok "test1005" [((2,3),(2,3))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#3, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test1006 = ok "test1006" [(2,3)] $
  "a" := for ("x" := Range "xs") ("x" + 1) `semi`
  "xs" := Array[1,2] `semi`
  "a"

test1000s :: IO ()
test1000s = mapM_ testEx
  [test1001,test1002,test1003,test1004,test1005,test1006
  ]

--------

testAll :: IO ()
testAll = do
  test100s
  test200s
  test300s
  test400s
  test500s
  test600s
  test700s
  test800s
  test900s
  test1000s
