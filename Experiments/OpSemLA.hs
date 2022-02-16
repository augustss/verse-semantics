{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
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
import Ex
import Debug.Trace

debug :: Bool
debug = False

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

doo :: Exp -> Exp
doo e = Do (addDef e)

lam :: Name -> Exp -> Exp
lam n e = Lam n (addDef e)
          

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
  { env :: !(Map Name Reg),
    nextTemp :: !Int,
    cops :: [Op]  -- generated ops so far
  }
  deriving (Show)

type C = State CompileState

cLookup :: Name -> C Reg
cLookup n = do
  m <- gets env
  case M.lookup n m of
    Nothing -> error $ "cLookup: " ++ show n
    Just r -> pure r

newReg :: C Reg
newReg = do
  s <- get
  let t = succ (nextTemp s)
  put s{nextTemp = t}
  pure $ Reg { reg_name = tmpName t }

tmpName :: Int -> Name
tmpName t = "%" ++ show t

emit :: Op -> C ()
emit op = modify $ \ s -> s { cops = cops s ++ [op] }

expToReg :: Exp -> C Reg
expToReg (Var n) = cLookup n
expToReg (Con i) = do t <- newReg; emit $ Atom t (AnInteger i); pure t
expToReg (Semi e1 e2) = expToReg e1 >> expToReg e2
expToReg (Where e1 e2) = do
  r1 <- expToReg e1
  _ <- expToReg e2
  pure r1
expToReg (Alt e1 e2) = do
  t <- newReg
  op1 <- sexpToOps e1
  op2 <- sexpToOps e2
  emit $ Choice ([op1, Store t, EndAlt]) ([op2, Store t, EndAlt])
  pure t
expToReg (Equal e1 e2) = do
  r1 <- expToReg e1
  r2 <- expToReg e2
  emit $ Unify r1 r2
  pure r1
expToReg (Set n e) =
  expToReg $ Equal (Var n) e
expToReg (Array es) = do
  rs <- mapM expToReg es
  t <- newReg
  emit $ MkArray t rs
  pure t
expToReg (Plus e1 e2) = do
  r1 <- expToReg e1
  r2 <- expToReg e2
  t <- newReg
  emit $ Add t r1 r2
  pure t
expToReg Fail = do
  emit Failure
  newReg                -- we must return something, but this reg will never be set
expToReg (For _e1 _e2) = undefined
expToReg (If _e1 _e2 _e3) = undefined
expToReg (Do e) = do
  t <- newReg
  o <- sexpToOps e
  emit o
  emit (Store t)
  pure t
expToReg (Range _e) = undefined
expToReg (Lam n e) = do
  os <- withNames [n] $ sexpToOps e
  t <- newReg
  emit $ Function t n [os, EndFun]
  pure t
expToReg (AppS e1 e2) = do
  r1 <- expToReg e1
  r2 <- expToReg e2
  t <- newReg
  emit $ Call t r1 r2
  pure t
expToReg (AppI _e1 _e2) = undefined
expToReg Error = undefined

sexpToOps :: SExp -> C Op
sexpToOps (Def ns e) = do
  olds <- get
  put olds{ cops = [] }
  r <- withNames ns $ expToReg e
  s <- get
  put olds{ nextTemp = nextTemp s }
  let tmps = [ tmpName t | t <- [nextTemp olds + 1 .. nextTemp s] ]
  pure $ PushFrame (ns ++ tmps) (cops s ++ [Load r, EndFrame])

withNames :: [Name] -> C a -> C a
withNames ns ca = do
  olds <- get
  put olds { env = foldr (uncurry M.insert) (env olds) [ (n, Reg { reg_name = n }) | n <- ns]
           }
  a <- ca
  s <- get
  put s{ env = env olds }
  pure a

-- The main compiler
comp :: SExp -> [Op]
comp e = evalState se cs
  where cs = CompileState{ env = M.empty, nextTemp = 1, cops = [] }
        se = do
          op <- sexpToOps e
          pure $ [op, Drain, Stop]

--------------------------------
--
-- Machine state
--
--------------------------------

data Atom
  = AnInteger Integer
  deriving (Eq, Show)

data Op
  = Atom Reg Atom
  | Unify Reg Reg
  | MkArray { target :: Reg, elts :: [Reg] }
  | Call { target :: Reg, fun :: Reg, arg :: Reg }
  | Function { target :: Reg, argName :: Name, body :: [Op] }
  | Choice [Op] [Op]
  | Failure
  | Add { target :: Reg, arg1 :: Reg, arg2 :: Reg }
  | PushFrame [Name] [Op]
  | EndFrame
  | EndFun
  | EndAlt
  | Load Reg
  | Store Reg
  | Drain
  | Stop   -- Just for testing, print the accum and stop
  deriving (Eq, Show)

data Frame = Frame { fr_vals   :: Map Name Value
                   , fr_parent :: Maybe Frame  -- Lexical parent
                   }
  deriving (Eq, Show)

type HeapId = Int
type Heap = Map HeapId (Maybe Value)

-- Context for expression evaluation
data Context
  = Ctx { ctx_heap   :: Heap
        , ctx_heapId :: !HeapId          -- Next heap id
        , ctx_frame  :: Frame
        , ctx_ops    :: [Op]
        , ctx_stack  :: [Continuation]   -- The call stack that "belongs" to this context
        , ctx_susp   :: [Suspension]
        , ctx_next   :: [Context]        -- Backtrack points
        , ctx_parent :: Maybe Context
        , ctx_accum  :: Value            -- argument/result
        }
  deriving (Show)

data Suspension
  = SuspUnify Value Value
  | SuspAdd Value Value Value
  | SuspCall Value Value Value
  deriving (Show)

data Continuation
  = ContFun Value Frame [Op] -- Return from a function call,
                             -- unifying the result with the value
  | ContAlt [Op]             -- end of a join (Alt)  
  | ContFrame Frame [Op]     -- end of a PushFrame
  deriving (Show)

data Value = VInteger Integer
           | VArray [Value]
           | VFun Frame Name [Op]   -- Frame is captured when we build the closure
           | VHeap HeapId
  deriving (Eq)

instance Show Value where
  show (VInteger i) = show i
  show VFun{} = "VFun{}"
  show (VArray vs) = "(" ++ intercalate "," (map show vs) ++ ")"
  show (VHeap h) = "H[" ++ show h ++ "]"

expunge :: HasCallStack => Heap -> Value -> Value
-- Remove all references to HeapIds in the Heap from the value
-- Runtime error if this is circular: just spot when you pass
--   the same HeapId a second time.  This is WRONG; verifier
--   should reject.
expunge heap = value []
  where
    value _ v@VInteger{} = v
    value s (VArray vs) = VArray (map (value s) vs)
    value s (VFun fr n os) = VFun (frame s fr) n os
    value s v@(VHeap h) | h `elem` s = error "WRONG: expunge recursion"
                       | otherwise =
                         case M.lookup h heap of
                           Nothing -> v  -- Not in this heap
                           Just Nothing -> error $ "WRONG: uninstantiated " ++ show h  -- XXX is it wrong
                           Just (Just v') -> value (h:s) v'
    frame s Frame{..} = Frame{ fr_vals = M.map (value s) fr_vals, fr_parent = fmap (frame s) fr_parent }

type Nat = Int

data Reg = Reg { reg_name  :: Name }
  deriving (Eq)

--  deriving (Show)
instance Show Reg where
  show (Reg n) = n

type R = State Context

assert :: String -> Bool -> R ()
assert s False = error $ "assert: " ++ s
assert _ True  = pure ()

assign :: Reg -> Value -> Context -> Context
-- Preconditions:
--   * The register is in the current fram
--   * The value is unbound
assign r@Reg{..} val ctx =
  let fr = ctx_frame ctx
      heap = ctx_heap ctx
  in  case M.lookup reg_name (fr_vals fr) of
        Just (VHeap h) ->
          case M.lookup h heap of
            Nothing -> error $ "assign: not in heap " ++ show (r, h)
            Just Nothing -> ctx{ctx_heap = M.insert h (Just val) heap}
            Just (Just v) -> error $ "assign: heap already set " ++ show (r, h, v)
        Just v -> error $ "assign: already set " ++ show (r, v)
        Nothing -> error $ "assign: not in frame " ++ show r


------------------------------------------
--       The evaluator
------------------------------------------

step :: Context -> Context

step ctx | debug && trace ("step " ++ show (take 1 (ctx_ops ctx))) False = undefined

--------- Draining suspensions ------------
step ctx@Ctx { ctx_ops = Drain : _, ctx_susp = susp : susps } =
  trySusp susp ctx{ ctx_susp = susps }
step ctx@Ctx { ctx_ops = Drain : ops, ctx_susp = [] } =
  ctx{ ctx_ops = ops }

--------- Function return ------------
step ctx@Ctx { ctx_ops = [EndFun], ctx_stack = ContFun res fr ops : stk } =
  --trace ("EndFun " ++ show (res, ctx_accum ctx)) $
  unify res (ctx_accum ctx) ctx{ ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }

--------- Join (Alt) return ------------
step ctx@Ctx { ctx_ops = [EndAlt], ctx_stack = ContAlt ops : stk } =
  ctx { ctx_ops = ops, ctx_stack = stk }

--------- PushFrame return ------------
step ctx@Ctx { ctx_ops = [EndFrame], ctx_stack = ContFrame fr ops : stk } =
  ctx { ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }


--------- Choice ------------
step ctx@Ctx { ctx_ops = Choice ops1 ops2 : ops
              , ctx_stack = old_stack
              , ctx_next = old_next } = ctx1
  where
    -- NB: both ctx1 and ctx2 start with the same heap
    ctx1 = ctx { ctx_ops = ops1
               , ctx_stack = ContAlt ops : old_stack
               , ctx_next = ctx2 : old_next }
    ctx2 = ctx { ctx_ops = ops2
               }

--------- Simple ------------
step actx@Ctx { ctx_ops = op : aops } =
  let ctx = actx{ctx_ops = aops}
  in  case op of
        Atom r (AnInteger i) -> assign r (VInteger i) ctx
        PushFrame ns ops ->
          let (hs, ctx') = newHeapIds (length ns) ctx
              fr = makeFrame (zip ns $ map VHeap hs) ctx'
          in  ctx'{ ctx_ops = ops, ctx_frame = fr
                  , ctx_stack = ContFrame (ctx_frame ctx) (ctx_ops ctx) : ctx_stack ctx }

        Load r -> ctx{ ctx_accum = loadValue r ctx }
        Store r -> storeValue r (ctx_accum ctx) ctx
        MkArray t rs -> storeValue t (VArray [ loadValue r ctx | r <- rs]) ctx
        Unify r1 r2 -> unify (loadValue r1 ctx) (loadValue r2 ctx) ctx
        Failure -> failure ctx
        Add t x y -> addOp (loadValue t ctx) (loadValue x ctx) (loadValue y ctx) ctx
        Function t n ops -> assign t (VFun (ctx_frame ctx) n ops) ctx
        Call t f a -> callOp (loadValue t ctx) (loadValue f ctx) (loadValue a ctx) ctx
        _ -> error $ "step " ++ show op

step _ = undefined

newHeapIds :: Int -> Context -> ([HeapId], Context)
newHeapIds n ctx =
  let hs = [ h .. h+n-1 ]
      h = ctx_heapId ctx
  in  (hs
      ,ctx{ctx_heapId = h+n, ctx_heap = foldr (uncurry M.insert) (ctx_heap ctx) (zip hs (repeat Nothing)) }
      )

makeFrame :: [(Name, Value)] -> Context -> Frame
makeFrame nvs ctx = Frame { fr_vals = M.fromList [(n, v) | (n, v) <- nvs ], fr_parent = Just $ ctx_frame ctx }

loadValue :: Reg -> Context -> Value
loadValue r ctx = loadValue' (reg_name r) (Just (ctx_frame ctx))
  where
    loadValue' :: Name -> Maybe Frame -> Value
    loadValue' n Nothing = error $ "loadValue: not found " ++ show n
    loadValue' n (Just fr) = fromMaybe (loadValue' n (fr_parent fr)) $ M.lookup n $ fr_vals fr

storeValue :: Reg -> Value -> Context -> Context
storeValue r v ctx = unify (loadValue r ctx) v ctx

-- Follow VHeap indirections
follow :: Value -> Context -> Value
follow (VHeap h) ctx | Just v <- getHeap h ctx = follow v ctx
follow v _ = v

unify :: Value -> Value -> Context -> Context
unify v1 v2 ctx | v1 == v2 = ctx
                | otherwise = unify' (follow v1 ctx) (follow v2 ctx) ctx

unify' :: Value -> Value -> Context -> Context
unify' v1 v2 ctx | v1 == v2 = ctx
unify' (VHeap h1) v2 ctx | isFlex h1 ctx = setHeap h1 v2 ctx
unify' v1 (VHeap h2) ctx | isFlex h2 ctx = setHeap h2 v1 ctx
unify' v1@VHeap{} v2 ctx = suspendUnify v1 v2 ctx
unify' v1 v2@VHeap{} ctx = suspendUnify v1 v2 ctx
unify' (VArray vs1) (VArray vs2) ctx
  | length vs1 == length vs2 = failure ctx
  | otherwise = foldr (uncurry suspendUnify) ctx (zip vs1 vs2)
unify' VFun{} VFun{} _ = error "WRONG: comparing functions"
unify' _ _ ctx = failure ctx

suspendUnify :: Value -> Value -> Context -> Context
suspendUnify v1 v2 = addSusp $ SuspUnify v1 v2

failure :: Context -> Context
failure _ctx = error "failure: unimplemented"

isFlex :: HeapId -> Context -> Bool
isFlex h ctx = M.member h (ctx_heap ctx)

-- Set a heap location in the current heap.
setHeap :: HeapId -> Value -> Context -> Context
setHeap h v ctx =
  case M.lookup h (ctx_heap ctx) of
    Nothing -> error $ "setHeap: not in heap " ++ show h
    Just (Just vv) -> error $ "setHeap: already set " ++ show (h, vv)
    Just Nothing -> ctx{ ctx_heap = M.insert h (Just v) (ctx_heap ctx) }

-- Find the heap contents for a HeapId
getHeap :: HeapId -> Context -> Maybe Value
getHeap h Ctx{ctx_heap = heap, ctx_parent = parent}
  | Just mv <- M.lookup h heap = mv
  | otherwise =
  case parent of
    Nothing -> error $ "getHeap: unset heap ID " ++ show h
    Just ctx -> getHeap h ctx

getWHNF :: Context -> Value -> Maybe Value
getWHNF ctx v =
  case follow v ctx of
    VHeap _ -> Nothing
    v' -> Just v'

addOp :: Value -> Value -> Value -> Context -> Context
addOp dst src1 src2 ctx =
  case traverse (getWHNF ctx) [src1, src2] of
    Just [VInteger i1, VInteger i2] -> unify dst (VInteger $ i1 + i2) ctx
    Just _ -> failure ctx
    Nothing -> addSusp (SuspAdd dst src1 src2) ctx

callOp :: Value -> Value -> Value -> Context -> Context
callOp t f a ctx | debug && trace ("callOp " ++ show ((t,f,a),(follow t ctx,follow f ctx, follow a ctx))) False = undefined
callOp t f a ctx =
  case getWHNF ctx f of
    Just (VFun fr n ops) -> apply t fr n ops a ctx
    Just (VArray vs) ->
      case getWHNF ctx a of
        Just (VInteger (fromInteger -> i)) | i >= 0 && i < length vs -> unify t (vs !! i) ctx
                                           | otherwise -> failure ctx
        Just _ -> failure ctx -- XXX maybe WRONG
        Nothing -> addSusp (SuspCall t f a) ctx
    Just v -> error $ "Call: not a function/array " ++ show v
    Nothing -> addSusp (SuspCall t f a) ctx

apply :: Value -> Frame -> Name -> [Op] -> Value -> Context -> Context
apply target fr argName ops arg ctx =
  let ctx' =
        ctx{ ctx_ops = ops
           , ctx_stack = ContFun target (ctx_frame ctx) (ctx_ops ctx) : ctx_stack ctx
           , ctx_frame = fr }
      fr' = makeFrame [(argName, arg)] ctx'
  in  ctx' { ctx_frame = fr' }

addSusp :: Suspension -> Context -> Context
addSusp susp _ | debug && trace ("addSusp: " ++ show susp) False = undefined
addSusp susp ctx = ctx{ ctx_susp = ctx_susp ctx ++ [susp] }

trySusp :: Suspension -> Context -> Context
trySusp susp | debug && trace ("trySusp " ++ show susp) False = undefined
trySusp (SuspUnify v1 v2) = unify v1 v2
trySusp (SuspAdd v1 v2 v3) = addOp v1 v2 v3
trySusp (SuspCall v1 v2 v3) = callOp v1 v2 v3

run :: [Op] -> [Value]
run ops = loop Ctx{ ctx_ops = ops
                  , ctx_frame = Frame { fr_vals = M.empty, fr_parent = Nothing }
                  , ctx_stack = []
                  , ctx_next = []
                  , ctx_heap = M.empty
                  , ctx_heapId = 1
                  , ctx_susp = []
                  , ctx_parent = Nothing
                  , ctx_accum = VInteger 0
                  }
  where
    loop Ctx {ctx_ops = [Stop], ctx_heap = heap, ctx_accum = v} =
      [expunge heap v]
    loop ctx = loop (step ctx)

ev :: Exp -> [Value]
ev = run . comp . addDef


---------------------
--      Tests
---------------------

ok :: (Show a) => String -> a -> Exp -> Ex String
ok n r e = Ex n (Just $ show r) (show $ ev e)

bad :: String -> Exp -> Ex String
bad n e = Ex n Nothing (show $ ev e)

bug :: (Show a) => String -> a -> Exp -> Ex String
bug n _r e = Ex ("bug: " ++ n) Nothing (show $ ev e)

--ok _ _ e = addDef e
--bad _ _ e = addDef e

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

x207 =
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

test300s :: IO ()
test300s = mapM_ testEx
  [test301,test302,test303,test304,test305,test306,test307,test308,test309,test310
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
test412 = bug "test412" [(1,1)] $
  "y" := ("x" === 1) `semi` "x" := 1 ||| 2 `semi` ("x" # "y")

-- Cascaded forward references
test413 = ok "test413" [3,7,2,2] $
         "x" := ("y" ||| 2)  `semi`
         "y" := (3 ||| "z")  `semi`
         "z" := 7            `semi`
         "x"

test400s :: IO ()
test400s = mapM_ testEx
  [test401,test402,test403,test404,test405,test406,test407,test408,test409,test410,test411,test412,test413
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

test606 = ok "test606" [(1,2,3),(1,2,99),(1,99,3),(1,99,99),(99,2,3),(99,2,99),(99,99,3),(99,99,99)] $
  for ("x" := 1|||2|||3) ("x" ||| 99)

test607 = ok "test607" [(1,2,3)] $
  for ("x" := 1|||2|||"y" `semi` "y" := "z" `semi` "z" := 3) "x"

test608 = ok "test608" [(2,3,4)] $
  for ("x" := 1|||2|||3) ("y" `wher` "y" := "x" + 1)

test600s :: IO ()
test600s = mapM_ testEx
  [test601,test602,test603,test604,test605,test606,test607,test608
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
test707 = bad "test707" $
  "y" := AppI "f" "t" `semi`
  "w" := 7 `semi`
  "t" := 4 `semi`
  "f" := lam "v" ("w" + "v") `semi`
  "y"

test708 = ok "test708" [10,11] $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppI "f" 10

test709 = bad "test709" $
  "f" := lam "v" ("v" ||| "v" + 1) `semi`
  AppS "f" 10

test700s :: IO ()
test700s = mapM_ testEx
  [test701,test702,test703,test704,test705,test706,test707,test708,test709
  ]

---------------------
-- Not yet sorted
---------------------

test32 = ok "test32" [(1,2,3)] $
  for ("x" := Range (Array [1,2,3])) "x"

test33 = ok "test33" [(102,103,104)] $
  "xs" := for ("x" := 1|||2|||3) ("x" + 1) `semi`
  for ("y" := Range "xs") ("y" + 100)

-- The variable x gets Id 2, and so does y.
-- If eval does not resolve all the variables in the Def
-- then the x will wrongly be resolved as 2.
test34 = bad "test34" $
  "xs" := Do (Def ["x"] (1 # "x")) `semi`
  doo ("y" := 2 `semi` Snd "xs")

test35 = ok "test35" [((1,2),(1,4),(1,5))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 1) "xy"

test36 = ok "test36" ([]::[()]) $
  "xys" := Array [1#2, 2#3, 1#4, 2#4, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

test37 = ok "test37" [((2,3),(2,3))] $
  "xys" := Array [1#2, 2#3, 1#4, 2#3, 1#5] `semi`
  for ("xy" := Range "xys" `semi` Fst "xy" === 2) ("xy" `wher` Snd "xy" === 3)

-- The t1 used to be t, but shadowing is not allowed.
test38 = ok "test38" [62,134] $
  "v" := "t" + 1 `semi`
  "x" := doo ( "z" := "v" + "t1" `semi` "t1" := 6 `semi` "z" ) `semi`
  "t" := 55 ||| 127 `semi`
  "x"

-- This test doesn't work, but it should.
test46 = bug "test46" [(2,3)] $
  "a" := for ("x" := Range "xs") ("x" + 1) `semi`
  "xs" := (1#2) `semi`
  "a"

testUnsorted :: IO ()
testUnsorted = mapM_ testEx
  [test32,test33,test34,test35,test36,test37,test38,test46
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
  testUnsorted
