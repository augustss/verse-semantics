{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module OpSem.EvalExp where
import Control.Monad.State.Strict
import Data.Function((&))
import Data.List(find)
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack(HasCallStack)
import Debug.Trace

import OpSem.DSL(for)
import OpSem.Exp
import OpSem.Misc
import OpSem.OpX

stepDebug, ifDebug, forDebug :: Bool
stepDebug = False
ifDebug = False
forDebug = False

-- For debugging
compExp :: Exp -> [OpX]
compExp e =
  fst $
  linValue M.empty [] M.empty $
  Do $
  addDef e

linToHeap :: Heap -> ContextId -> Frame -> Value -> Exp -> ([OpX], Heap)
linToHeap h ci fr tgt e =
  let ls = execState (expToHeap tgt e)
             LState{ ls_heap = h, ls_frame = fr, ls_contextId = ci, ls_ops = []}
  in  (ls_ops ls, ls_heap ls)

linValue :: Heap -> ContextId -> Frame -> Exp -> ([OpX], Heap)
linValue h ci fr e =
  let ls = execState (expValue e)
             LState{ ls_heap = h, ls_frame = fr, ls_contextId = ci, ls_ops = []}
  in  (ls_ops ls, ls_heap ls)

data LState = LState
  { ls_heap      :: !Heap         -- The heap we are extending
  , ls_frame     :: !Frame        -- The current environment
  , ls_contextId :: !ContextId    -- ContextId for created HeapId
  , ls_ops       :: ![OpX]        -- Generated OpXs
  }
  deriving (Show)

type L a = State LState a

expValue :: Exp -> L Value
expValue (Var n) = do
  fr <- gets ls_frame
  pure $ lookupFrame fr n
expValue (Con i) =
  pure $ VInteger i
expValue (Semi e1 e2) = do
  _ <- expValue e1
  expValue e2
expValue (Where e1 e2) = do
  v1 <- expValue e1
  _ <- expValue e2
  pure v1
expValue (Alt e1 e2) = do
  tgt <- newVHeap
  altToHeap tgt e1 e2
  pure tgt
expValue (Equal e1 e2) = do
  v1 <- expValue e1
  v2 <- expValue e2
  emitOps [UnifyX v1 v2]
  pure v1
expValue (Set n e) = expValue (Equal (Var n) e)
expValue (SetAny n) = do
  fr <- gets ls_frame
  pure $ lookupFrame fr n
expValue (Array es) = do
  vs <- mapM expValue es
  pure $ VArray vs
expValue (PrimBin op e1 e2) = do
  tgt <- newVHeap
  primBinToHeap tgt op e1 e2
  pure tgt
expValue Fail = do
  emitOps [FailX]
  newVHeap
expValue (For d@(Def ns _) e) = do
  tgt <- newVHeap
  ci <- gets ls_contextId
  fr <- gets ls_frame
  ctx <- mkContext ci fr (Do d)
  let nas = zip ns (M.keys (ctx_heap ctx))  -- Uses the fact that ns are the first variables allocated
  emitOps [ForX tgt [] ctx nas (fr, Do e)]
  pure tgt
expValue (If c@(Def ns _) t e) = do
  tgt <- newVHeap
  ci <- gets ls_contextId
  fr <- gets ls_frame
  ctx <- mkContext ci fr (Do c)
  let nas = zip ns (M.keys (ctx_heap ctx))  -- Uses the fact that ns are the first variables allocated
  emitOps [IfX tgt ctx nas (fr, Do t) (fr, Do e)]
  pure tgt
expValue (Do e) = sexpValue e
expValue (Let (Def ns e1) e2) = expValue (Do (Def ns (e1 `Semi` e2)))
expValue (Range e) = do
  tgt <- newVHeap
  v <- expValue e
  emitOps [RangeX tgt v]
  pure tgt
expValue (Lam n e) = do
  fr <- gets ls_frame
  pure $ VFun fr n (Do e)
expValue (App f a) = do
  tgt <- newVHeap
  appToHeap tgt f a
  pure tgt
expValue Error = do
  emitOps [ErrorX]
  newVHeap

expToHeap :: Value -> Exp -> L ()
-- optimizations to avoid cell allocation
expToHeap tgt (Alt e1 e2) = altToHeap tgt e1 e2
expToHeap tgt (PrimBin op e1 e2) = primBinToHeap tgt op e1 e2
expToHeap tgt (App f a) = appToHeap tgt f a
expToHeap _ Fail = emitOps [FailX]
-- fallback
expToHeap tgt e = do
  v <- expValue e
  emitOps [UnifyX tgt v]

altToHeap :: Value -> SExp -> SExp -> L ()
altToHeap tgt e1 e2 = do
  ops1 <- getOpsOf $ expToHeap tgt $ Do e1
  ops2 <- getOpsOf $ expToHeap tgt $ Do e2
  emitOps [ChoiceX ops1 ops2]

primBinToHeap :: Value -> PrimOp -> Exp -> Exp -> L ()
primBinToHeap tgt op e1 e2 = do
  arg <- expValue $ Array [e1, e2]
  emitOps [CallX tgt (VPrimOp op) arg]

appToHeap :: Value -> Exp -> Exp -> L ()
appToHeap tgt f a = do
  vf <- expValue f
  va <- expValue a
  emitOps [CallX tgt vf va]

sexpValue :: SExp -> L Value
sexpValue (Def ns e) = do
  nvs <- mapM (\ n -> do v <- newVHeap; pure (n, v)) ns
  withBinds nvs $ expValue e

lookupFrame :: Frame -> Name -> Value
lookupFrame fr n =
  fromMaybe (wrong $ "Not in scope: " ++ show n) $ M.lookup n fr

extendFrame :: Frame -> [(Name, Value)] -> Frame
extendFrame fr nvs = foldr (uncurry M.insert) fr nvs

heapAlloc :: Heap -> (Heap, HeapAddr)
heapAlloc h =
  let a | M.null h = 0
        | otherwise = fst (M.findMax h) + 1
  in  (M.insert a Nothing h, a)

newVHeap :: L Value
newVHeap = do
  ls <- get
  let (h, a) = heapAlloc (ls_heap ls)
      ci = ls_contextId ls
  put $ ls { ls_heap = h }
  pure $ VHeap $ HeapId ci a

withBinds :: [(Name, Value)] -> L a -> L a
withBinds nvs la = do
  ls <- get
  put ls{ ls_frame = extendFrame (ls_frame ls) nvs }
  a <- la
  ls' <- get
  put ls'{ ls_frame = ls_frame ls }
  pure a

emitOps :: [OpX] -> L ()
emitOps ops =
  modify $ \ ls -> ls{ ls_ops = ls_ops ls ++ ops }

getOpsOf :: L () -> L [OpX]
getOpsOf l = do
  ls <- get
  put ls{ ls_ops = [] }
  l
  ls' <- get
  put ls'{ ls_ops = ls_ops ls }
  pure $ ls_ops ls'

wrong :: HasCallStack => String -> a
wrong s = error $ "WRONG: " ++ s

mkContext :: ContextId -> Frame -> Exp -> L Context
mkContext pci fr e = do
  -- This allocates a cell just to get a unique address that can be used
  -- to create the ContextId for the new Context.
  v <- newVHeap
  let l = case v of VHeap (HeapId _ a) -> a; _ -> undefined
      (ops, heap) = linValue M.empty ci fr e
      ci = l : pci
  pure $ Ctx
      { ctx_id = ci
      , ctx_heap = heap
      , ctx_done = []
      , ctx_ops = ops
      , ctx_parent = Nothing
      , ctx_effects = [Iterates]
      , ctx_next = Nothing
      , ctx_hold = False
      }

run :: Exp -> Value
run e =
  case step ctx of
    StepFailed -> error "run: StepFailed"
    StepDone ctx' -> expunge ci (ctx_heap ctx') tgt
    _ -> wrong "run: deadlock"
  where
    ctx = Ctx
      { ctx_id = ci
      , ctx_heap = heap'
      , ctx_done = []
      , ctx_ops = ops
      , ctx_parent = Nothing
      , ctx_effects = [Success, Interacts]
      , ctx_next = Nothing
      , ctx_hold = False
      }
    ci = []
    addr = 0
    (ops, heap') = linToHeap heap ci M.empty tgt e'
    tgt = VHeap (HeapId ci addr)
    heap = M.singleton addr Nothing
    e' = for ("&it" `Set` e) (Var "&it")

instance Eval Value where
  eval e =
    case run e of
      VArray vs -> vs
      v -> error $ "run returned " ++ show v

--------------------------------------------------------------

expunge :: HasCallStack => ContextId -> Heap -> Value -> Value
expunge ci heap = value []
  where
    value :: [HeapAddr] -> Value -> Value
    -- 's' tracks the HeapIds we have seen already, for loop detection
    value _ v@VInteger{} = v
    value s (VArray vs) = VArray (map (value s) vs)
    value s (VFun fr n e) = VFun (frame s fr) n e
    value s v@(VHeap (HeapId ci' h))
      | ci /= ci' = v
      | h `elem` s = error $ "WRONG: expunge recursion:\n" ++ show (h, s, heap)
      | otherwise =
          case M.lookup h heap of
            Nothing -> error "expunge: not in heap"
            Just Nothing -> error $ "expunge: WRONG: uninstantiated " ++ show (h, ci, heap)  -- XXX is it wrong?
            Just (Just v') -> value (h:s) v'
    value _ v@VPrimOp{} = v
    frame s fr = M.map (value s) fr

-- XXX Implement this
hasNoHeapIdsFrom :: Maybe Frame -> ContextId -> Bool
hasNoHeapIdsFrom _ _ = True

getCurrentContexts :: Context -> [Context]
getCurrentContexts c = c : maybe [] getCurrentContexts (ctx_parent c)

getValue :: Context -> Value -> Value
getValue ctx = follow' []
  where follow' s (VHeap h) | h `elem` s = error $ "follow': loop " ++ show (h, s)
        follow' s v@(VHeap h) =
          case getHeapValue ctx h of
            Just v' -> follow' (h:s) v'
            _ -> v
        follow' _ v = v

-- Find the heap contents at the given HeapId.
-- Looks up the parent chain for heaps.
getHeapValue :: Context -> HeapId -> Maybe Value
getHeapValue ctx (HeapId ci h) =
  let
    ctxs = getCurrentContexts ctx
    hctx = fromMaybe (error "getHeapValue 1") $ find ((== ci) . ctx_id) ctxs
  in
    fromMaybe (error "getHeapValue 2") $ M.lookup h (ctx_heap hctx)

addResiduals :: [OpX] -> Context -> Context
addResiduals os c = c{ ctx_done = ctx_done c ++ os }

addOps :: [OpX] -> Context -> Context
addOps os c = c{ ctx_ops = os ++ ctx_ops c }

setHeap :: Heap -> Context -> Context
setHeap h c = c{ ctx_heap = h }

isFlex :: HeapId -> ContextId -> Bool
isFlex (HeapId c _) ci = c == ci

getWHNF :: Context -> Value -> Maybe Value
getWHNF ctx v =
  case getValue ctx v of
    VHeap{} -> Nothing
    v' -> Just v'

setHeapCell :: HeapId -> Value -> Context -> Context
setHeapCell (HeapId ci h) v ctx =
  assert "setHeapCell" (ci == ctx_id ctx) $
  case M.lookup h (ctx_heap ctx) of
    Nothing -> error $ "setHeap: not in heap " ++ show h
    Just (Just vv) -> error $ "setHeap: already set " ++ show (h, vv)
    Just Nothing -> ctx & setHeapAddr h (Just v)

-- Blindly set the heap contents.
setHeapAddr :: HasCallStack => HeapAddr -> Maybe Value -> Context -> Context
setHeapAddr h mv ctx = ctx{ ctx_heap = M.insert h mv (ctx_heap ctx) }

unify :: Context -> Value -> Value -> Step1Result
unify ctx av1 av2 = unify' (getValue ctx av1) (getValue ctx av2)
  where
    unify' :: Value -> Value -> Step1Result
    unify' v1 v2 | v1 == v2 = Step1Done ctx
    unify' (VHeap h1) v2 | isFlex h1 (ctx_id ctx) = Step1Done $ ctx & setHeapCell h1 v2
    unify' v1 (VHeap h2) | isFlex h2 (ctx_id ctx) = Step1Done $ ctx & setHeapCell h2 v1
    unify' v1@(VHeap _) v2 = ctx & suspend (UnifyX v1 v2)
    unify' v1 v2@(VHeap _) = ctx & suspend (UnifyX v1 v2)
    unify' (VArray vs1) (VArray vs2)
      | length vs1 /= length vs2 = Step1Failed
      | otherwise = Step1Done $ ctx & addOps (zipWith UnifyX vs1 vs2)
    unify' VFun{} VFun{} = error "WRONG: comparing functions"
    unify' _ _ = Step1Failed

data StepResult
  = StepDone Context    -- finished successfully
  | StepNotDone Context -- something happened, but it didn't finish
  | StepNothing         -- no steps taken
  | StepFailed          -- failed
  deriving (Show)

-- Run a Context as far as possible.
step :: Context -> StepResult
step = step' False False

-- Repeatedly step, keeping track if any actual steps were taken.
-- The first flag indicates that a step has happened since the start,
-- the second flag that a step has happened in the last pass.
step' :: Bool -> Bool -> Context -> StepResult
step' _ _ ctx@(Ctx { ctx_done = [], ctx_ops = [] }) =
  -- We are done, no ops, no residuals
  StepDone ctx
step' some did ctx@(Ctx { ctx_done = done, ctx_ops = [] })
  | stepDebug && trace ("step retry " ++ show done) False = undefined
  -- We took some steps in the last pass, retry the (non-empty) residuals again
  | did = step' some False ctx{ ctx_done = [], ctx_ops = done, ctx_hold = False }
  -- We took no steps in the last pass, but some since the start.
  | some = StepNotDone ctx{ ctx_done = [], ctx_ops = done, ctx_hold = False }
  -- We have taken no steps whatsoever.
  | otherwise = StepNothing
step' some did ctx@(Ctx { ctx_ops = op:ops }) =
  -- Take a single step
  let ctx' = ctx{ ctx_ops = ops }
  in  case step1 ctx' op of
        Step1Suspend ctx'' -> ctx'' & step' some did
        Step1Done ctx''    -> ctx'' & step' True True
        Step1Failed        ->
          -- Evaluation failed, backtrack if possible.
          case ctx_next ctx' of
            Nothing -> StepFailed
            Just ctx'' -> ctx'' & step' True True

data Step1Result
  = Step1Done Context     -- executed the instruction
  | Step1Suspend Context  -- instruction could not execute
  | Step1Failed           -- execution failed
  deriving (Show)

-- Try to execute a single OpX, return the new Context if
-- it is possible, or Nothing if the op needs to suspend.
step1 :: Context -> OpX -> Step1Result
step1 _ op | stepDebug && trace ("step1 " ++ show op) False = undefined
step1 ctx (UnifyX v1 v2) = unify ctx v1 v2
step1 ctx op@CallX{ targetx = tgt, callx_fun = fun, callx_arg = arg } =
  case getValue ctx fun of
    -- Function closure
    VFun{ vf_arg_name = arg_name, vf_frame = frame, vf_body = body } ->
      --trace ("adding VFun " ++ show body_opxs) $
      -- Main payload!   Rename body, inline the instructions
      Step1Done $ ctx & addClosure tgt (frame_w_binding, body)
      where
        frame_w_binding = extendFrame frame [(arg_name, arg)]

    -- Primitive function, always with an array argument.
    -- Requires all array elements in WHNF.
    VPrimOp sop ->
      case getValue ctx arg of
        VArray vs
          | Just vs' <- mapM (getWHNF ctx) vs ->
            case primOp sop vs' of
              Just v -> unify ctx tgt v
              Nothing -> Step1Failed
          | otherwise -> ctx & suspend op
        VHeap{} -> ctx & suspend op
        _ -> error "Bad VPrimOp arg"

    VArray vals ->
      case getValue ctx arg of
        VInteger idx
          | i >= 0 && i < length vals -> 
            Step1Done $ ctx & addOps [UnifyX tgt (vals!!i)]
          | otherwise -> Step1Failed
          where i = fromInteger idx
        VHeap {} -> ctx & suspend op
        _        -> wrong "Bad index in array indexing"

    -- XXX need to hold all effects here
    VHeap {} -> ctx & holdEffects & suspend op  -- Unknown effects in the function
    _        -> wrong "Bad function in CallX"

step1 ctx op@(ChoiceX ops1 ops2)
  | ctx_hold ctx = ctx & suspend op
  | otherwise =
    let ctx2 = ctx & addOps ops2
        ctx1 = ctx{ ctx_next = Just ctx2 } & addOps ops1
    in  Step1Done ctx1

step1 ctx op@IfX{ targetx = tgt, ifx_cond = cond, ifx_exports = nas, ifx_then = (then_frame, then_exp), ifx_else = els } =
  case step cond{ ctx_parent = Just ctx } of
    res | ifDebug && trace ("IfX evals " ++ show res) False -> undefined
    StepNotDone cond' -> Step1Done $
      -- domain did not finish, so suspend and hold off unknown effects
      ctx & holdEffects & addResiduals [op{ ifx_cond = cond' }]
    StepDone cond' -> Step1Done $ ctx & addClosure tgt (extendFrame then_frame ext, then_exp)
      where ext = [ (n, expunge ci (ctx_heap cond') (VHeap (HeapId ci a))) | (n, a) <- nas ]
            ci = ctx_id cond'
    StepFailed -> Step1Done $ ctx & addClosure tgt els
    StepNothing -> ctx & holdEffects & suspend op
step1 ctx op@ForX{ targetx = tgt, forx_arr = arr, forx_dom = dom, forx_exports = nas, forx_body = (body_frame, body_exp) } =
  case step dom{ ctx_parent = Just ctx } of
    res | forDebug && trace ("ForX evals " ++ show res) False -> undefined
    StepNotDone dom' -> Step1Done $
      -- domain did not finish, so suspend and hold off unknown effects
      ctx & holdEffects & addResiduals [op{ forx_dom = dom' }]
    StepDone dom' -> Step1Done $
      ctx' &
      addOps [op{ forx_arr = arr ++ [res], forx_dom = dom'' }] &
      addClosure res (extendFrame body_frame ext, body_exp)
      where ext = [ (n, expunge ci (ctx_heap dom') (VHeap (HeapId ci a))) | (n, a) <- nas ]
            ci = ctx_id dom'
            (ctx', res) = allocCell ctx
            dom'' = nextIter dom'
    StepFailed -> unify ctx tgt (VArray arr)  -- the for loop has finished, so deliver the array
    StepNothing -> ctx & holdEffects & suspend op
step1 ctx op@(RangeX tgt arr) =
  case getValue ctx arr of
    VArray vs ->
      case vs of
        []  -> Step1Failed
        [v] -> unify ctx tgt v
        _   -> Step1Done $ ctx & addOps [foldr1 (\ op1 op2 -> ChoiceX [op1] [op2]) $ map (UnifyX tgt) vs]
    VHeap{} -> ctx & suspend op
    _ -> error "RangeX bar arg"
step1 _ FailX = Step1Failed
step1 _ctx ErrorX = error "Error"

allocCell :: Context -> (Context, Value)
allocCell ctx =
  let (h, a) = heapAlloc (ctx_heap ctx)
  in  (ctx{ ctx_heap = h }, VHeap $ HeapId (ctx_id ctx) a)

-- Move to next iteration.  If there is no backtrack point, make the next iteration just fail.
nextIter :: Context -> Context
nextIter ctx =
  assert "nextIter" (null $ ctx_ops ctx) $
  fromMaybe (ctx{ctx_ops = [FailX]}) $ ctx_next ctx

addClosure :: Value -> (Frame, Exp) -> Context -> Context
addClosure tgt (frame, body) ctx = 
  ctx & setHeap h' & addOps body_opxs
  where
    (body_opxs, h') = linToHeap (ctx_heap ctx) (ctx_id ctx) frame tgt body

suspend :: OpX -> Context -> Step1Result
suspend op ctx = Step1Suspend $ addResiduals [op] ctx

holdEffects :: Context -> Context
holdEffects ctx = ctx { ctx_hold = True }

primOp :: PrimOp -> [Value] -> Maybe Value
primOp op [v1@(VInteger i1), VInteger i2] = do
  let arith f = Just $ VInteger $ i1 `f` i2
      compar f = if i1 `f` i2 then Just v1 else Nothing
  case op of
    "+"  -> arith (+)
    "-"  -> arith (-)
    "*"  -> arith (*)
    "div" | i2 == 0 -> Nothing
          | otherwise -> arith div
    "<"  -> compar (<)
    "<=" -> compar (<=)
    ">"  -> compar (>)
    ">=" -> compar (>=)
    _    -> error $ "Unknown primop " ++ op
primOp _ _ = Nothing
