{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module OpSem.EvalExp(compExp, run) where
import Control.Monad.State.Strict
import Data.Function((&))
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack(HasCallStack)
import Debug.Trace

import OpSem.DSL(for, do_)
import OpSem.Error
import OpSem.Exp
import OpSem.Misc
import OpSem.OpX

-- ToDo:
--  Check that ChoiceX has Iterates
--  Check that failure has Failure/Decides/Iterates
--  Add opcodes PushEffect/PopEffect to limit allowed effects
--  Add VString
--  Add VDouble?
--  Replace VInteger by VRational?

stepDebug, ifDebug, forDebug :: Bool
stepDebug = False
ifDebug = False
forDebug = False

-- For debugging
compExp :: Exp -> [OpX]
compExp e =
  fst $
  linValue (mkHeap 0) emptyFrame $
  Do $
  addDef e

-- linValue throws away the Value from expValue
-- It is used in mkContext, only for 'if' and 'for',
-- both of which discard the domain value.
linValue :: Heap -> Frame -> Exp -> ([OpX], Heap)
-- Takes an Exp, compiles it (on the fly):
--    replaces identifiers with HeapIds
--    linearises to [OpX]
--    executes constructors by building a Value
linValue h fr e =
  let init_ls = LState{ ls_heap = h, ls_frame = fr, ls_ops = []}
      ls      = execState (expValue e) init_ls
  in  (ls_ops ls, ls_heap ls)

data LState = LState
  { ls_heap      :: !Heap         -- The heap we are extending
  , ls_frame     :: !Frame        -- The current environment
  , ls_ops       :: ![OpX]        -- Generated OpXs
  }
  deriving (Show)

type L a = State LState a

-- Convert the Exp to a sequence of OpX
-- and return the Value of it.
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
  emitOp $ UnifyX v1 v2
  pure v1
expValue (Set n e) =
  expValue (Equal (Var n) e)
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
expValue (PrimUn op e1) = do
  tgt <- newVHeap
  primUnToHeap tgt op e1
  pure tgt
expValue Fail = do
  tgt <- newVHeap
  emitOp FailX
  pure tgt
expValue (For d e) = do
  tgt <- newVHeap
  forToHeap tgt d e
  pure tgt
expValue (If c t e) = do
  tgt <- newVHeap
  ifToHeap tgt c t e
  pure tgt
expValue (Do e) =
  sexpValue e
expValue (Let (Def ns e1) e2) =
  expValue (Do (Def ns (e1 `Semi` e2)))
expValue (Range e) = do
  tgt <- newVHeap
  v <- expValue e
  emitOp $ RangeX tgt v
  pure tgt
expValue (Lam n e) = do
  fr <- gets ls_frame
  pure $ VFun fr n e
expValue (App f a) = do
  tgt <- newVHeap
  appToHeap tgt f a
  pure tgt
expValue Error = do
  tgt <- newVHeap
  emitOp $ CallX tgt (VPrimOp [] "Error") (VArray [])
  pure tgt
expValue Wrong = do
  tgt <- newVHeap
  emitOp $ CallX tgt (VPrimOp [] "Wrong") (VArray [])
  pure tgt

-- Convert the Exp to a sequence of OpX
-- and put the result in Target.
expToHeap :: Target -> Exp -> L ()
-- optimizations to avoid cell allocation
expToHeap tgt (Alt e1 e2) = altToHeap tgt e1 e2
expToHeap tgt (PrimBin op e1 e2) = primBinToHeap tgt op e1 e2
expToHeap tgt (For d e) = forToHeap tgt d e
expToHeap tgt (App f a) = appToHeap tgt f a
expToHeap tgt (If c t e) = ifToHeap tgt c t e
-- fallback, this always works, even in the cases above
expToHeap tgt e = do
  v <- expValue e
  emitOp $ UnifyX tgt v

altToHeap :: Target -> SExp -> SExp -> L ()
altToHeap tgt e1 e2 = do
  ops1 <- getOpsOf $ expToHeap tgt $ Do e1
  ops2 <- getOpsOf $ expToHeap tgt $ Do e2
   -- Could give same Heap to e1 and e2 (they run in alternative worlds)
   -- but need to return a Heap whose next-free-loc is the max of the two.
   -- But it's easier just to thread the heap from e1 into e2.
  emitOp $ ChoiceX ops1 ops2

primBinToHeap :: Target -> PrimOp -> Exp -> Exp -> L ()
primBinToHeap tgt op e1 e2 = do
  arg <- expValue $ Array [e1, e2]
  emitOp $ CallX tgt (VPrimOp (opEffects op) op) arg

primUnToHeap :: Target -> PrimOp -> Exp -> L ()
primUnToHeap tgt op e1 = do
  arg <- expValue e1
  emitOp $ CallX tgt (VPrimOp (opEffects op) op) arg

appToHeap :: Target -> Exp -> Exp -> L ()
appToHeap tgt f a = do
  vf <- expValue f
  va <- expValue a
  emitOp $ CallX tgt vf va

forToHeap :: Target -> SExp -> SExp -> L ()
forToHeap tgt d@(Def ns _) e = do
  fr <- gets ls_frame
  ctx <- mkContext (Do d)
  let nas = zip ns (keysHeap (ctx_heap ctx))  -- Uses the fact that ns are the first variables allocated
  emitOp $ ForX { targetx = tgt, forx_arr = []
                , forx_dom = ctx, forx_exports = nas
                , forx_body = (fr, e) }

ifToHeap :: Target -> SExp -> SExp -> SExp -> L ()
ifToHeap tgt c@(Def ns _) t e = do
  fr <- gets ls_frame
  ctx <- mkContext (Do c)
  let nas = zip ns (keysHeap (ctx_heap ctx))  -- Uses the fact that ns are the first variables allocated
  emitOp $ IfX { targetx = tgt
               , ifx_cond = ctx, ifx_exports = nas
               , ifx_then = (fr, t), ifx_else = (fr, e) }

sexpValue :: SExp -> L Value
sexpValue (Def ns e) = do
  nvs <- mapM (\ n -> do v <- newVHeap; pure (n, v)) ns
  withBinds nvs $ expValue e

lookupFrame :: Frame -> Name -> Value
lookupFrame fr n =
  fromMaybe (wrong $ "Not in scope: " ++ show n) $ M.lookup n fr

extendFrame :: Frame -> [(Name, Value)] -> Frame
extendFrame fr nvs = foldr (uncurry M.insert) fr nvs

newVHeap :: L Value
newVHeap = do
  ls <- get
  let (h, a) = allocHeap (ls_heap ls)
  put $ ls { ls_heap = h }
  pure $ VHeap a

withBinds :: [(Name, Value)] -> L a -> L a
withBinds nvs la = do
  ls <- get
  put ls{ ls_frame = extendFrame (ls_frame ls) nvs }
  a <- la
  ls' <- get
  put ls'{ ls_frame = ls_frame ls }
  pure a

emitOp :: OpX -> L ()
emitOp op =
  modify $ \ ls -> ls{ ls_ops = ls_ops ls ++ [op] }

-- Execure l and capture the ops it generates.
getOpsOf :: L () -> L [OpX]
getOpsOf l = do
  ls <- get
  put ls{ ls_ops = [] }
  l
  ls' <- get
  put ls'{ ls_ops = ls_ops ls }
  pure $ ls_ops ls'

mkContext :: Exp -> L Context
mkContext e = do
  pci <- gets $ idHeap . ls_heap
  fr <- gets ls_frame
  -- This allocates a cell just to get a unique address that can be used
  -- to create the ContextId for the new Context.
  let ci = succ pci  -- The new ContextId is the old one + 1
      (ops, heap) = linValue (mkHeap ci) fr e
  pure $ Ctx
      { ctx_heap = heap
      , ctx_ops = ops
      , ctx_effects = noEffects  -- placeholder
      , ctx_next = Nothing
      }

-- Run a closed expression.  Assumes Def has been inserted in the appropriate places.
run :: Exp -> Value
run e =
  case step [] emptyStore ctx'' of
    StepFailed -> wrong "run: StepFailed"
    StepDone _ rctx -> expunge (ctx_heap rctx) tgt
    _ -> wrong "run: deadlock"
  where
    ctx = Ctx
      { ctx_heap = mkHeap 0
      , ctx_ops = []
      , ctx_effects = topLevelEffects
      , ctx_next = Nothing
      }
    (ctx', tgt) = ctx & allocCell
    ctx'' = ctx' & addClosureCtx tgt (emptyFrame, Def [] e)

-- Just for Tests.hs
-- eval accepts a multivalued expression, so wrap it in a 'for' to get an array.
instance Eval Value where
  evalMany e =
    case run $ for ("&it" `Set` e) (Var "&it") of
      VArray vs -> vs
      v -> internalError $ "run returned " ++ show v
  eval e = run (do_ e)

--------------------------------------------------------------

-- Get rid of all references to heap cells with ContextId ci.
--
-- Fails (with WRONG) if the input value reaches any uninstantiated
-- heap cells.  E.g this is WRONG
--      if (i:int) then (i=1; i) else 4
-- Any circularity is flagged as WRONG.
expunge :: HasCallStack => Heap -> Value -> Value
expunge heap = value []
  where
    ci = idHeap heap
    value :: [HeapAddr] -> Value -> Value
    -- 's' tracks the HeapIds we have seen already, for loop detection
    value _ v@VInteger{} = v
    value s (VArray vs) = VArray (map (value s) vs)
    value s (VFun fr n e) = VFun (frame s fr) n e
    value s v@(VHeap (HeapId ci' h))
      | ci /= ci' = v
      | h `elem` s = wrong $ "expunge recursion:\n" ++ show (h, s, heap)
      | otherwise =
          case lookupHeap h heap of
            Nothing -> wrong $ "expunge: uninstantiated " ++ show (h, ci, heap)
            Just v' -> value (h:s) v'
    value _ v@VPrimOp{} = v
    value _ v@VRef{} = v
    frame s fr = M.map (value s) fr

-- Follow VHeap indirections that point to instantiated cells.
getValue :: StepState -> Value -> Value
getValue ss = follow' []
  where follow' s (VHeap h) | h `elem` s = wrong $ "follow': loop " ++ show (h, s)
        follow' s v@(VHeap h) =
          case getHeapValue (getAllHeaps ss) h of
            Just v' -> follow' (h:s) v'
            _ -> v
        follow' _ v = v

-- Add instructions to retry later.
addResiduals :: [OpX] -> StepState -> StepState
addResiduals os ss = ss{ ss_suspended = reverse os ++ ss_suspended ss }

-- Add instructions to execute next.
addOps :: [OpX] -> StepState -> StepState
addOps os ss = ss{ ss_context = addOpsCtx os (ss_context ss) }

addOpsCtx :: [OpX] -> Context -> Context
addOpsCtx os c = c{ ctx_ops = os ++ ctx_ops c }

setHeap :: Heap -> Context -> Context
setHeap h c = c{ ctx_heap = h }

-- Does the heap pointer refer to a flexible variable?
isFlex :: HeapId -> ContextId -> Bool
isFlex (HeapId c _) ci = c == ci

-- Get an actual value.
getWHNF :: StepState -> Value -> Maybe Value
getWHNF ss v =
  case getValue ss v of
    VHeap{} -> Nothing
    v' -> Just v'

-- Set an (uninstantiated) logical variable.
setHeapCell :: HeapId -> Value -> StepState -> StepState
setHeapCell h v ss = ss{ ss_context = setHeapCellCtx h v (ss_context ss) }

setHeapCellCtx :: HeapId -> Value -> Context -> Context
setHeapCellCtx (HeapId ci h) v ctx =
  assert "setHeapCell" (ci == idHeap (ctx_heap ctx)) $
  case lookupHeap h (ctx_heap ctx) of
    Just vv -> internalError $ "setHeap: already set " ++ show (h, vv)
    Nothing -> ctx{ ctx_heap = insertHeap h (Just v) (ctx_heap ctx) }

-- Unify two values.
--   Will set flexible variables.
--   Will suspend on inflexible variables.
unify :: StepState -> Value -> Value -> Step1Result
unify ss av1 av2 = unify' (getValue ss av1) (getValue ss av2)
  where
    ci = idHeap $ ctx_heap $ ss_context ss
    unify' :: Value -> Value -> Step1Result
    unify' v1 v2 | v1 == v2 = Step1Done ss
    unify' (VHeap h1) v2 | isFlex h1 ci = Step1Done $ ss & setHeapCell h1 v2
    unify' v1 (VHeap h2) | isFlex h2 ci = Step1Done $ ss & setHeapCell h2 v1
    unify' v1@(VHeap _) v2 = ss & suspend (UnifyX v1 v2)
    unify' v1 v2@(VHeap _) = ss & suspend (UnifyX v1 v2)
    unify' (VArray vs1) (VArray vs2)
      | length vs1 /= length vs2 = Step1Failed
      | otherwise = Step1Done $ ss & addOps (zipWith UnifyX vs1 vs2)
    unify' VFun{} VFun{} = wrong "comparing functions"
    unify' VPrimOp{} VPrimOp{} = wrong "comparing primops"
    unify' _ _ = Step1Failed

-- Results of taking as many steps as possible.
data StepResult
  = StepDone Store Context    -- Finished successfully; ctx_ops = []
                              -- use ctx_next for further results
  | StepNotDone Store Context -- Something happened, but it didn't finish:
                              --  (ctx_ops /= []), but they are all stuck
  | StepNothing               -- No steps taken; degenerate form of StepNotDone
  | StepFailed                -- Failed; hit FailX
  deriving (Show)

-- Run a Context as far as possible.
-- Repeatedly execute the [OpX] in the Context, until
-- nothing further happens, or the [OpX] is empty
-- Invariant: input Context has ctx_ops non-empty
step :: Heaps -> Store -> Context -> StepResult
step phs st ctx =
  case stepPass StepState{ ss_suspended = [], ss_effects = ctx_effects ctx
                         , ss_any = False, ss_context = ctx, ss_heaps = phs, ss_store = st } of
    StepNotDone st' ctx' -> step phs st' ctx'
    res -> res

-- StepState is the state while executing stepPass.
-- At any point a valid Context can be gotten by
-- ss_context{ ctx_ops = reverse ss_suspended ++ ctx_ops ss_context }
data StepState = StepState
  { ss_suspended  :: ![OpX]          -- suspended instructions, in reverse order
  , ss_any        :: !Bool           -- something has changed
  , ss_context    :: !Context        -- executing context
  , ss_heaps      :: !Heaps          -- all the heaps in outer contexts
  , ss_effects    :: !Effects        -- currently allowed effects
  , ss_store      :: !Store          -- refcell storage
  }
  deriving (Show)

-- Make one pass over the ctx_ops.
stepPass :: StepState -> StepResult
stepPass StepState{ ss_suspended = done, ss_any = anyStep, ss_store = st, ss_context = ctx@Ctx{ ctx_ops = [] } }
  | null done   = StepDone st ctx
  | not anyStep = StepNothing
  | otherwise   = StepNotDone st ctx{ ctx_ops = reverse done }
stepPass ss@StepState{ ss_context = ctx@Ctx { ctx_ops = op:ops } } =
  -- Take a single step
  let ctx' = ctx{ ctx_ops = ops }
      ss' = ss{ ss_context = ctx' }
  in  case step1 ss' op of
        Step1Suspend ss'' -> stepPass ss''
        Step1Done ss''    -> stepPass ss''{ ss_any = True }
        Step1Failed      ->
          -- Evaluation failed, backtrack if possible.
          case ctx_next ctx' of
            Nothing    -> StepFailed
            Just ctx'' -> stepPass ss'{ ss_context = ctx'', ss_any = True }

-- Result of exeucting a single OpX
data Step1Result
  = Step1Done    StepState  -- Completely executed the instruction
                            -- did not add anything to ss_suspended.
  | Step1Suspend StepState  -- Instruction could not fully execute,
                            -- has been residualized into ss_suspended.
  | Step1Failed             -- Instruction failed.
  deriving (Show)

-- ToDo: is there really a difference between Step1Suspend and Step1Done
-- Maybe need Step1Nothing?

-- Try to execute a single OpX.
-- The OpX has already been removed from the ctx_ops
step1 :: StepState -> OpX -> Step1Result
step1 ss op | stepDebug && trace ("step1 ctxId=" ++ show (idHeap (ctx_heap (ss_context ss))) ++
                                 {-" effs=" ++ show (ss_effects ss) ++ -} ": " ++ show op) False = undefined
step1 ss (UnifyX v1 v2) = unify ss v1 v2
step1 ss op@CallX{ targetx = tgt, callx_fun = fun, callx_arg = arg } =
  case getValue ss fun of
    -- Function closure
    VFun{ vf_arg_name = arg_name, vf_frame = frame, vf_body = body } ->
      -- Main payload!  Linearize the body, inline the instructions
      Step1Done $ ss & addClosure tgt (frame_w_binding, body)
      where
        -- Extend the frame with the argument binding
        frame_w_binding = extendFrame frame [(arg_name, arg)]

    -- Primitive function.
    -- primOpX requires all array elements in WHNF.
    VPrimOp effs sop
      | not (all (`memberEffect` ctx_effects (ss_context ss)) (opEffects sop))
        -> wrong $ "effect not allowed for " ++ show sop
      | any (isHeldEffect ss) effs -> ss & suspendPrim sop op
      | otherwise ->
      case getValue ss arg of
        VArray vs
          | Just vs' <- mapM (getWHNF ss) vs ->
            primOpX ss tgt sop vs'
          | otherwise ->
            ss & suspendPrim sop op
        VHeap{} ->
          ss & suspendPrim sop op
        v -> primOpX ss tgt sop [v]

    -- Array indexing.
    VArray vals ->
      case getValue ss arg of
        VInteger idx
          | i >= 0 && i < length vals -> 
            Step1Done $ ss & addOps [UnifyX tgt (vals!!i)]
          | otherwise -> Step1Failed
          where i = fromInteger idx
        VHeap {} -> ss & suspend op
        _        -> wrong "Bad index in array indexing"

    VHeap {} -> ss & holdAllEffects & suspend op  -- Unknown effects in the function

    _        -> wrong "Bad function in CallX"

step1 ss op@(ChoiceX ops1 ops2)
  | isHeldEffect ss Iterates = ss & suspend op
  | otherwise =
    let ctx  = ss_context ss
        ctx1 = ctx & addOpsCtx ops1
        ctx2 = ctx & addOpsCtx ops2
    in  Step1Done ss{ ss_context = ctx1{ ctx_next = Just ctx2 } }
    -- Make two contexts, one for each branch, prepending ops to the rest of the ops
    -- And then chain them together, so that we do ctx2 when ctx1 is done.
    -- NB: ctx1 and ctx2 start from the /same/ Heap;
    --     this is what implements "backtracking".

step1 ss op@IfX{ targetx = tgt, ifx_cond = cond, ifx_exports = nas
               , ifx_then = (then_frame, then_exp), ifx_else = els } =
  -- Run the cond, with all the outer heaps
  case step (getAllHeaps ss) (ss_store ss) (setSubEffects ss cond) of
    res | ifDebug && trace ("IfX evals " ++ show res) False -> undefined
    StepNotDone _st _cond ->
      -- cond did not finish, so suspend and hold off unknown effects.
      ss & holdAllEffects & suspend op

    StepDone st' cond' ->
      -- Condition succeeded, run the 'then' branch with the domain frame.
      Step1Done $
      ss &
      addClosure tgt (extendFrame then_frame ext, then_exp) &
      setStore (expungeStore (ctx_heap cond') st')
      where
         ext :: [(Name, Value)] -- The Values have no references to the Heap of cond'
         ext = [ (n, expunge (ctx_heap cond') (VHeap a))
               | (n, a) <- nas ]

    StepFailed ->
      -- Condition failed, run the 'else' branch
      Step1Done $
      ss & addClosure tgt els
    StepNothing ->
      -- Nothing happened, just suspend again.
      ss & holdAllEffects & suspend op

step1 ss op@ForX{ targetx = tgt, forx_arr = arr, forx_dom = adom
                , forx_exports = nas, forx_body = (body_frame, body_exp) } =
  -- Run the domain, with all the outer heaps
  case step (getAllHeaps ss) (ss_store ss) (setSubEffects ss adom) of
    res | forDebug && trace ("ForX evals " ++ show res) False -> undefined
    StepNotDone _st _dom ->
      -- domain did not finish, so suspend and hold off unknown effects
      ss & holdAllEffects & suspend op
    StepDone st' dom' ->
      -- domain finished, run the body with the domain frame.
      -- The body puts the value in res, which is appended to
      -- the accumulating array.
      -- After the body execution we will run the ForX again for the next iteration,
      -- but with the domain updated to the next backtrack point.
      Step1Done $
      ss{ ss_context =
            ctx' &
            addOpsCtx [op{ forx_arr = arr ++ [res], forx_dom = dom'' }] &
            addClosureCtx res (extendFrame body_frame ext, body_exp)
        } &
      setStore (expungeStore (ctx_heap dom') st')
      where ext = [ (n, expunge (ctx_heap dom') (VHeap a)) | (n, a) <- nas ]
            (ctx', res) = allocCell $ ss_context ss  -- allocate a result cell in parent context
            dom'' = nextIter dom'
    StepFailed ->
      -- The for loop has finished, so deliver the array.
      -- Note: StepFailed is not returned until the are no
      -- more backtrack points.
      unify ss tgt (VArray arr)
    StepNothing ->
      -- Nothing happened, just suspend again.
      ss & holdAllEffects & suspend op

step1 ss op@(RangeX tgt arr) =
  case getValue ss arr of
    VArray vs ->
      case vs of
        []  -> Step1Failed
        [v] -> unify ss tgt v
        _   ->
          -- Create nested ChoiceX for all the array values.
          Step1Done $
          ss &
          addOps [foldr1 (\ op1 op2 -> ChoiceX [op1] [op2]) $ map (UnifyX tgt) vs]
    VHeap{} -> ss & suspend op
    _ -> wrong "RangeX bad arg"

setStore :: Store -> StepState -> StepState
setStore st ss = ss{ ss_store = st }

expungeStore :: Heap -> Store -> Store
expungeStore h st = mapStore (expunge h) st

isHeldEffect :: StepState -> Effect -> Bool
isHeldEffect ss eff = not $ memberEffect eff (ss_effects ss)

-- Set the correct effects for a sub-context
setSubEffects :: StepState -> Context -> Context
setSubEffects ss ctx =
  --trace ("setSubEffects: " ++ show (subContextEffects (ctx_effects (ss_context ss)))) $
  ctx{ ctx_effects = subContextEffects (ctx_effects (ss_context ss)) }

getAllHeaps :: StepState -> Heaps
getAllHeaps ss = ctx_heap (ss_context ss) : ss_heaps ss

-- Create a new uninstantiated heap cell
allocCell :: Context -> (Context, Value)
allocCell ctx =
  let (h, a) = allocHeap (ctx_heap ctx)
  in  (ctx{ ctx_heap = h }, VHeap a)

-- Move to next iteration.
-- If there is no backtrack point, make the next iteration just fail.
nextIter :: Context -> Context
nextIter ctx =
  assert "nextIter" (null $ ctx_ops ctx) $
  fromMaybe (ctx{ctx_ops = [FailX]}) $ ctx_next ctx

-- Linearize the Exp with the given Frame and insert
-- those ops to be executed next.
addClosure :: Target -> (Frame, SExp) -> StepState -> StepState
addClosure tgt cl ss = ss{ ss_context = addClosureCtx tgt cl (ss_context ss) }

addClosureCtx :: Target -> (Frame, SExp) -> Context -> Context
addClosureCtx tgt (frame, body) ctx =
  ctx & setHeap (ls_heap ls) & addOpsCtx (ls_ops ls)
  where
    ls = execState (expToHeap tgt $ Do body) init_ls
    init_ls = LState{ ls_heap = ctx_heap ctx, ls_frame = frame, ls_ops = []}

-- Residualize the given op and report suspension.
suspend :: OpX -> StepState -> Step1Result
suspend op ss = Step1Suspend $ addResiduals [op] ss

-- Suspend a PrimOp, this needs to suspend effects
-- that don't commute.
suspendPrim :: PrimOp -> OpX -> StepState -> Step1Result
suspendPrim sop op ss =
  ss & holdEffects (opEffects sop) & suspend op

-- Hold off all effects that don't commute with the given effects
holdEffects :: [Effect] -> StepState -> StepState
holdEffects effs ss = ss { ss_effects = commutesWithEffects effs (ss_effects ss) }

-- Hold off all (sequential) effects.
holdAllEffects :: StepState -> StepState
holdAllEffects ss = ss { ss_effects = commutativeEffects (ss_effects ss) }

-- Execute a PrimOp.
primOpX :: StepState -> Target -> PrimOp -> [Value] -> Step1Result
primOpX _ _ "Error" _ = explicitError "Error called"
primOpX _ _ "Wrong" _ = wrong "called"
primOpX ss tgt "print" vs =
  -- XXX for now, just trace
  let vs' = map (expunge $ ctx_heap $ ss_context ss) vs
  in  trace ("print: " ++ show vs') $
      unify ss tgt (VArray [])
primOpX ss tgt "new_" [v] =
  let (ss', ref) = newRefCell v ss
  in  unify ss' tgt (VRef ref)
primOpX ss tgt "read" [VRef r] =
  unify ss tgt (readStore r (ss_store ss))
primOpX ss tgt "write" [VRef r, v] =  -- XXX probably don't need v in WHNF
  let st = writeStore r v (ss_store ss)
  in  unify (setStore st ss) tgt (VArray [])
primOpX ss tgt sop vs =
  case primOp sop vs of
    Just v -> unify ss tgt v
    Nothing -> Step1Failed

newRefCell :: Value -> StepState -> (StepState, Ref)
newRefCell v ss =
  let (st, r) = newStore v (ss_store ss)
  in  (ss{ ss_store = st }, r)

-- Execute a pure PrimOp.
-- Nothing indicates that the op failed.
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
    _    -> internalError $ "Unknown primop " ++ op
primOp op [VInteger i] = do
  let arith f = Just $ VInteger $ f i
  case op of
    "negate" -> arith negate
    "abs" -> arith abs
    _    -> internalError $ "Unknown primop " ++ op
primOp _ _ = Nothing

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
