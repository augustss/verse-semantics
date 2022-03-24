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
--  Add opcodes PushEffect/PopEffect to limit allowed effects
--  Don't use trace for Print
--  Use n-ary ChoiceX?
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
  case step noEffects [] emptyStore ctx'' of
    StepFailed -> wrong "run: StepFailed"
    StepDone _ rctx | completed rctx -> expunge (ctx_heap rctx) tgt
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
    unify' v1 v2 | v1 == v2 = ss & done
    unify' (VHeap h1) v2 | isFlex h1 ci = ss & setHeapCell h1 v2 & done
    unify' v1 (VHeap h2) | isFlex h2 ci = ss & setHeapCell h2 v1 & done
    unify' v1@(VHeap _) v2 = ss & suspend (UnifyX v1 v2)
    unify' v1 v2@(VHeap _) = ss & suspend (UnifyX v1 v2)
    unify' (VArray vs1) (VArray vs2)
      | length vs1 /= length vs2 = failure ss
      | otherwise = ss & addOps (zipWith UnifyX vs1 vs2) & done
    unify' VFun{} VFun{} = wrong "comparing functions"
    unify' VPrimOp{} VPrimOp{} = wrong "comparing primops"
    unify' _ _ = failure ss

-- The op failed.
failure :: StepState -> Step1Result
failure ss
  | not $ allowedEffects ss [Failure] = wrong $ "effect not allowed: failure"
  | otherwise = Step1Failed

-- The has finished/
done :: StepState -> Step1Result
done = Step1Done

-- Residualize the given op and report suspension.
suspend :: OpX -> StepState -> Step1Result
suspend op ss = Step1Suspend $ addResiduals [op] ss

---------------------------------------------------------------
--
--                Taking a step
-- 
---------------------------------------------------------------

-- StepResult: Results of taking as many steps as possible.
data StepResult
  = StepDone Store Context    -- Some progress; if ctx_ops = [] we are done,
                              --   if not, try again later
  | StepNothing               -- No steps taken, try again later
  | StepFailed                -- Failed
  deriving (Show)

-- True when the context has no remaining instructions
completed :: Context -> Bool
completed = null . ctx_ops

-- Run a Context as far as possible.
-- Repeatedly execute the [OpX] in the Context, until
-- nothing further happens, or the [OpX] is empty.
-- Invariant: input Context has ctx_ops non-empty
--
-- The outer Heaps are passed in, but they are read-only
-- so they are not returned in StepResult
--
-- The currenly held effects are passed in so this sub-computation
-- can hold those as well.
step :: Effects -> Heaps -> Store -> Context -> StepResult
step held phs ast actx =
  case stepPass held phs ast actx of
    StepDone st' ctx' | not (completed ctx') -> step' st' ctx'
    res -> res
  where
    -- Called when some steps have happened.
    step' st ctx =
      case stepPass held phs st ctx of
        StepNothing -> StepDone st ctx
        StepDone st' ctx' | not (completed ctx') -> step' st' ctx'
        res -> res

-- StepState is the state while executing stepPass.
-- At any point a valid Context can be obtained by
-- ss_context{ ctx_ops = reverse ss_suspended ++ ctx_ops ss_context }
data StepState = StepState
  { ss_suspended  :: ![OpX]          -- suspended instructions, in reverse order
  , ss_anyStep    :: !Bool           -- something has changed
  , ss_context    :: !Context        -- executing context
  , ss_held       :: !Effects        -- currently held effects
  , ss_store      :: !Store          -- refcell storage

  -- This field is never mutated; it's just in the "state"
  -- because it reduces the number of arguments etc.
  , ss_heaps      :: !Heaps          -- all the heaps in outer contexts
  }
  deriving (Show)

-- Make one pass over the ctx_ops.
stepPass :: Effects -> Heaps -> Store -> Context -> StepResult
stepPass held phs st ctx = stepPass' startState
  where
    startState =
      StepState{ ss_suspended = [], ss_held = globalEffects held
               , ss_allowed = [ctx_effects ctx]
               , ss_anyStep = False, ss_context = ctx
               , ss_heaps = phs, ss_store = st }

stepPass' :: StepState -> StepResult
stepPass' StepState{ ss_suspended = susp, ss_anyStep = anyStep
                   , ss_store = st, ss_allowed = fss
                   , ss_context = ctx@Ctx{ ctx_ops = [] } }
  -- We have run out of ctx_ops
  | length fss /= 1 = internalError "bad effect stack"
  | not anyStep = StepNothing
  | otherwise   = StepDone st ctx{ ctx_ops = reverse susp }

-- Remove an adjacent pair of PushEffectsX&PopEffects.
-- These instruction remain in the suspended stream until they meet up
-- like this, since they must execute on every pass to maintain allowed effects.
stepPass' ss@StepState{ ss_context =
                        ctx@Ctx { ctx_ops = PushEffectsX _:PopEffectsX:ops } } =
  stepPass' ss{ ss_context = ctx{ctx_ops = ops} }

stepPass' ss@StepState{ ss_context = ctx@Ctx { ctx_ops = op:ops } } =
  -- Take a single step
  let ctx' = ctx{ ctx_ops = ops }
      ss' = ss{ ss_context = ctx' }
  in  case step1 ss' op of
        Step1Suspend ss'' -> stepPass' ss''
        Step1Done ss''    -> stepPass' ss''{ ss_anyStep = True }
        Step1Failed       ->
          -- Evaluation failed, backtrack if possible.
          case ctx_next ctx' of
            Nothing    -> StepFailed
            Just ctx'' -> stepPass' ss'{ ss_context = ctx'', ss_anyStep = True }

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
                                 " held=" ++ show (ss_held ss) ++ ": " ++ show op) False = undefined
step1 ss (UnifyX v1 v2) = unify ss v1 v2
step1 ss op@CallX{ targetx = tgt, callx_fun = fun, callx_arg = arg } =
  case getValue ss fun of
    -- Function closure
    VFun{ vf_arg_name = arg_name, vf_frame = frame, vf_body = body } ->
      -- Main payload!  Linearize the body, inline the instructions
      ss & addClosure tgt (frame_w_binding, body) & done
      where
        -- Extend the frame with the argument binding
        frame_w_binding = extendFrame frame [(arg_name, arg)]

    -- Primitive function.
    -- primOpX requires all array elements in WHNF.
    VPrimOp effs sop
      | not $ allowedEffects ss (opEffects sop) -> wrong $ "effect not allowed for " ++ show sop
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
            ss & addOps [UnifyX tgt (vals!!i)] & done
          | otherwise -> failure ss
          where i = fromInteger idx
        VHeap {} -> ss & suspend op
        _        -> wrong "Bad index in array indexing"

    VHeap {} -> ss & holdAllEffects & suspend op  -- Unknown effects in the function

    _        -> wrong "Bad function in CallX"

step1 ss op@(ChoiceX ops1 ops2)
  | not $ allowedEffects ss [Iterates] = wrong $ "effect not allowed for ChoiceX"
  | isHeldEffect ss Iterates = ss & suspend op
  | otherwise =
    let ctx  = ss_context ss
        ctx1 = ctx & addOpsCtx ops1
        ctx2 = ctx & addOpsCtx ops2
    in  done ss{ ss_context = ctx1{ ctx_next = Just ctx2 } }
    -- Make two contexts, one for each branch, prepending ops to the rest of the ops
    -- And then chain them together, so that we do ctx2 when ctx1 is done.
    -- NB: ctx1 and ctx2 start from the /same/ Heap;
    --     this is what implements "backtracking".

step1 ss op@IfX{ targetx = tgt, ifx_cond = cond, ifx_exports = nas
               , ifx_then = (then_frame, then_exp), ifx_else = els } =
  -- Run the cond, with all the outer heaps
  case step (ss_held ss) (getAllHeaps ss) (ss_store ss) (setIterEffects ss cond) of
    res | ifDebug && trace ("IfX evals " ++ show res) False -> undefined
    StepDone _st cond' | not (completed cond') ->
      -- cond did not finish, so suspend and hold off unknown effects.
      ss & holdAllEffects & suspend op

    StepDone st' cond' ->
      -- Condition succeeded, run the 'then' branch with the domain frame.
      ss &
        addClosure tgt (extendFrame then_frame ext, then_exp) &
        setStore (expungeStore (ctx_heap cond') st') &
        done
      where
         ext :: [(Name, Value)] -- The Values have no references to the Heap of cond'
         ext = [ (n, expunge (ctx_heap cond') (VHeap a))
               | (n, a) <- nas ]

    StepFailed ->
      -- Condition failed, run the 'else' branch
      ss & addClosure tgt els & done
    StepNothing ->
      -- Nothing happened, just suspend again.
      ss & holdAllEffects & suspend op

step1 ss op@ForX{ targetx = tgt, forx_arr = arr, forx_dom = adom
                , forx_exports = nas, forx_body = (body_frame, body_exp) } =
  -- Run the domain, with all the outer heaps
  case step (ss_held ss) (getAllHeaps ss) (ss_store ss) (setIterEffects ss adom) of
    res | forDebug && trace ("ForX evals " ++ show res) False -> undefined
    StepDone _st dom' | not (completed dom') ->
      -- domain did not finish, so suspend and hold off unknown effects
      ss & holdAllEffects & suspend op
    StepDone st' dom' ->
      -- domain finished, run the body with the domain frame.
      -- The body puts the value in res, which is appended to
      -- the accumulating array.
      -- After the body execution we will run the ForX again for the next iteration,
      -- but with the domain updated to the next backtrack point.
      ss{ ss_context =
            ctx' &
            addOpsCtx [op{ forx_arr = arr ++ [res], forx_dom = dom'' }] &
            addClosureCtx res (extendFrame body_frame ext, body_exp)
        } &
        setStore (expungeStore (ctx_heap dom') st') &
        done
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
        []  -> failure ss
        [v] -> unify ss tgt v
        _   ->
          -- Create nested ChoiceX for all the array values.
          ss &
            addOps [foldr1 (\ op1 op2 -> ChoiceX [op1] [op2]) $ map (UnifyX tgt) vs] &
            done
    VHeap{} -> ss & suspend op
    _ -> wrong "RangeX bad arg"

allowedEffects :: StepState -> [Effect] -> Bool
allowedEffects ss es = all (`memberEffect` ctx_effects (ss_context ss)) es

setStore :: Store -> StepState -> StepState
setStore st ss = ss{ ss_store = st }

expungeStore :: Heap -> Store -> Store
expungeStore h st = mapStore (expunge h) st

isHeldEffect :: StepState -> Effect -> Bool
-- (isHeldEffect ss e) returns True if we are
-- NOT free to perform effect 'e' in state 'ss'
isHeldEffect ss eff = memberEffect eff (ss_held ss)

-- Set the correct effects for an iteration sub-context;
-- specifically in the domain of an 'if' or 'for'
setIterEffects :: StepState -> Context -> Context
setIterEffects ss ctx =
  --trace ("setSubEffects: " ++ show (subContextEffects (ctx_effects (ss_context ss)))) $
  ctx{ ctx_effects = iterContextEffects (ctx_effects (ss_context ss)) }
    -- How do we compute the effects allowable in the sub-context?
    -- Do we want the *current* allowed effects, or the *birth* effects mask of the context?
    -- We can't just use "current", because
    --     x := if <cond> then...   -- Disables choice if <cond> is stuck
    --     y := for (i:=1..n)       -- Want to allow choice in the domain
    -- We want birth effects of the context, but what about side effects on
    --    mutable variables? Something about global vs local effects?a


{-   x := (p^ := p^ + 1 | p^ := p^ * 2)
     -- Effects all happen, left to right
     -- Check with Tim
-}

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

-- Suspend a PrimOp, this needs to suspend effects
-- that don't commute.
suspendPrim :: PrimOp -> OpX -> StepState -> Step1Result
suspendPrim sop op ss =
  ss & holdEffects (opEffects sop) & suspend op

-- Hold off all effects that don't commute with the given effects.
holdEffects :: [Effect] -> StepState -> StepState
holdEffects fs ss = ss { ss_held = nonCommutativeWithEffects fs }

-- Hold off all (sequential) effects.
holdAllEffects :: StepState -> StepState
holdAllEffects ss = ss { ss_held = nonCommutativeEffects }

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
    Nothing -> failure ss

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
