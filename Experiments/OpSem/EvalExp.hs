{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module OpSem.EvalExp(compExp, run) where
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
  linValue (mkHeap []) emptyFrame $
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
expValue Fail = do
  tgt <- newVHeap
  emitOp $ RangeX tgt (VArray [])
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
  emitOp $ CallX tgt (VPrimOp "Error") (VArray [])
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
   -- but need to return a Heap whose next-free-loc is the max of the two
   -- But it's easier just to thread the heap from e1 into e2.
  emitOp $ ChoiceX ops1 ops2

primBinToHeap :: Target -> PrimOp -> Exp -> Exp -> L ()
primBinToHeap tgt op e1 e2 = do
  arg <- expValue $ Array [e1, e2]
  emitOp $ CallX tgt (VPrimOp op) arg

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

wrong :: HasCallStack => String -> a
wrong s = error $ "WRONG: " ++ s

mkContext :: Exp -> L Context
mkContext e = do
  pci <- gets $ idHeap . ls_heap
  fr <- gets ls_frame
  -- This allocates a cell just to get a unique address that can be used
  -- to create the ContextId for the new Context.
  v <- newVHeap
  let l = case v of VHeap (HeapId _ a) -> a; _ -> undefined
      ci = l : pci  -- The new ContextId is the old one with a unique Int prepended
      (ops, heap) = linValue (mkHeap ci) fr e
  pure $ Ctx
      { ctx_heap = heap
      , ctx_done = []
      , ctx_ops = ops
      , ctx_parent = Nothing
      --, ctx_effects = [Iterates]
      , ctx_next = Nothing
      , ctx_hold = False
      }

-- Run a closed expression.  Assumes Def has been inserted in the appropriate places.
run :: Exp -> Value
run e =
  case step ctx'' of
    StepFailed -> error "run: StepFailed"
    StepDone rctx -> expunge (ctx_heap rctx) tgt
    _ -> wrong "run: deadlock"
  where
    ctx = Ctx
      { ctx_heap = mkHeap []
      , ctx_done = []
      , ctx_ops = []
      , ctx_parent = Nothing
      --, ctx_effects = [Success, Interacts]
      , ctx_next = Nothing
      , ctx_hold = False
      }
    (ctx', tgt) = ctx & allocCell
    ctx'' = ctx' & addClosure tgt (emptyFrame, Def [] e)

-- Just for Tests.hs
-- eval accepts a multivalued expression, so wrap it in a 'for' to get an array.
instance Eval Value where
  eval e =
    case run $ for ("&it" `Set` e) (Var "&it") of
      VArray vs -> vs
      v -> error $ "run returned " ++ show v

--------------------------------------------------------------

-- Get rid of all references to heap cells with ContextId ci.
--
-- Fails (with WRONG) if the input value reaches any uninstantiated
-- heap cells.  E.g this is WRONG
--      if (i:int) then (i=1; i) else 4
-- Any circularity is flagged as an error.
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
    frame s fr = M.map (value s) fr

-- Get all active contexts.
getCurrentContexts :: Context -> [Context]
getCurrentContexts c = c : maybe [] getCurrentContexts (ctx_parent c)

-- Follow VHeap indirections that point to instantiated cells.
getValue :: Context -> Value -> Value
getValue ctx = follow' []
  where follow' s (VHeap h) | h `elem` s = wrong $ "follow': loop " ++ show (h, s)
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
    hctx = fromMaybe (error "getHeapValue 1") $ find ((== ci) . idHeap . ctx_heap) ctxs
  in
    lookupHeap h (ctx_heap hctx)

-- Add instructions to retry later.
addResiduals :: [OpX] -> Context -> Context
addResiduals os c = c{ ctx_done = ctx_done c ++ os }

-- Add instructions to execute next.
addOps :: [OpX] -> Context -> Context
addOps os c = c{ ctx_ops = os ++ ctx_ops c }

setHeap :: Heap -> Context -> Context
setHeap h c = c{ ctx_heap = h }

-- Does the heap pointer refer to a flexible variable?
isFlex :: HeapId -> ContextId -> Bool
isFlex (HeapId c _) ci = c == ci

-- Get actual values.
getWHNF :: Context -> Value -> Maybe Value
getWHNF ctx v =
  case getValue ctx v of
    VHeap{} -> Nothing
    v' -> Just v'

-- Set an (uninstantiated) logical variable.
setHeapCell :: HeapId -> Value -> Context -> Context
setHeapCell (HeapId ci h) v ctx =
  assert "setHeapCell" (ci == idHeap (ctx_heap ctx)) $
  case lookupHeap h (ctx_heap ctx) of
    Just vv -> error $ "setHeap: already set " ++ show (h, vv)
    Nothing -> ctx{ ctx_heap = insertHeap h (Just v) (ctx_heap ctx) }

-- Unify two values.
--   Will set flexible variables.
--   Will suspend on inflexible variables.
unify :: Context -> Value -> Value -> Step1Result
unify ctx av1 av2 = unify' (getValue ctx av1) (getValue ctx av2)
  where
    ci = idHeap (ctx_heap ctx)
    unify' :: Value -> Value -> Step1Result
    unify' v1 v2 | v1 == v2 = Step1Done ctx
    unify' (VHeap h1) v2 | isFlex h1 ci = Step1Done $ ctx & setHeapCell h1 v2
    unify' v1 (VHeap h2) | isFlex h2 ci = Step1Done $ ctx & setHeapCell h2 v1
    unify' v1@(VHeap _) v2 = ctx & suspend (UnifyX v1 v2)
    unify' v1 v2@(VHeap _) = ctx & suspend (UnifyX v1 v2)
    unify' (VArray vs1) (VArray vs2)
      | length vs1 /= length vs2 = Step1Failed
      | otherwise = Step1Done $ ctx & addOps (zipWith UnifyX vs1 vs2)
    unify' VFun{} VFun{} = wrong "comparing functions"
    unify' VPrimOp{} VPrimOp{} = wrong "comparing primops"
    unify' _ _ = Step1Failed

-- Results of taking as many steps as possible.
data StepResult
  = StepDone Context    -- Finished successfully; ctx_ops = []
                        -- use ctx_next for further results
  | StepNotDone Context -- Something happened, but it didn't finish:
                        --  (ctx_ops /= []), but they are all stuck
  | StepNothing         -- No steps taken; degenerate form of StepNotDone
  | StepFailed          -- Failed; hit FailX
  deriving (Show)


{-
type ParentHeaps = [Heap]
step :: ParentHeaps -> Context -> StepResult
-- Makes it clear that the ParentHeaps are not mutated: good!

step :: Context -> StepResult
-- StepNotDone => a heap mutation took place (in this context's heap),
--                but all ctx_ops are stuck

-- Precondition: (ctx_ops c) is non-empty
step c = assert "step" (not (null (ctx_opc c))) (ppr c) $
         case stepPass c of
           StepNotDone c' -> case step c' of
                               StepNothing -> StepNotDone c'
                               other       -> other
           StepDone c'    -> StepDone c'
           StepNothing    -> StepNothing
           StepFailed     -> StepFailed

stepPass :: Context -> StepResult
-- StepNotDone => a heap mutation took place (in this context's heap),
--                so it's worth trying another iteration stepPass

-- ToDo: Localise ctx_done to step'; remove from Context
--   residual [OpX] becomes an accumulating parameter of step'
-- Ditto ctx_hold!
-}

-- Run a Context as far as possible.
-- Repeatedly exectue the [OpX] in the Context, until
-- nothing further happens, or the [OpX] is empty
-- Invariant: input Context has ctx_ops non-empty
step :: Context -> StepResult
step = step' False False

-- Repeatedly call step1, keeping track if any actual steps were taken.
-- The first flag indicates that a step has happened since the start,
-- the second flag that a step has happened in the last pass.
step' :: Bool   -- Step has happened since step' was called
      -> Bool   -- Step has happened in "this pass" of [OpX]
      -> Context -> StepResult
step' _ _ ctx@(Ctx { ctx_done = [], ctx_ops = [] }) =
  -- We are done, no ops, no residuals
  StepDone ctx
step' some did ctx@(Ctx { ctx_done = done, ctx_ops = [] })
  | stepDebug && trace ("step retry " ++ show done) False = undefined
  -- We took some steps in the last pass, retry the (non-empty) residuals again.
  -- Note: when restarting, effects are no longer held.
  | did = step' some False ctx{ ctx_done = [], ctx_ops = done, ctx_hold = False }
  -- We took no steps in the last pass, but some since the start.
  -- Note: when retrying later, effects are no longer held.
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
            Nothing    -> StepFailed
            Just ctx'' -> ctx'' & step' True True

-- Result of exeucting a single OpX
data Step1Result
  = Step1Done    Context  -- Completely executed the instruction
                          -- did not add anything to ctx_done
  | Step1Suspend Context  -- Instruction could not fully execute,
                          -- has been residualized into ctx_done
  | Step1Failed           -- Instruction failed
  deriving (Show)

-- ToDo: is there really a difference between Step1Suspend and Step1Done
-- Maybe need Step1Nothing?

-- Try to execute a single OpX.
-- The OpX has already been removed from the ctx_ops
step1 :: Context -> OpX -> Step1Result
step1 _ op | stepDebug && trace ("step1 " ++ show op) False = undefined
step1 ctx (UnifyX v1 v2) = unify ctx v1 v2
step1 ctx op@CallX{ targetx = tgt, callx_fun = fun, callx_arg = arg } =
  case getValue ctx fun of
    -- Function closure
    VFun{ vf_arg_name = arg_name, vf_frame = frame, vf_body = body } ->
      -- Main payload!  Linearize the body, inline the instructions
      Step1Done $ ctx & addClosure tgt (frame_w_binding, body)
      where
        -- Extend the frame with the argument binding
        frame_w_binding = extendFrame frame [(arg_name, arg)]

    -- Primitive function, always with an array argument.
    -- Requires all array elements in WHNF.
    VPrimOp sop ->
      case getValue ctx arg of
        VArray vs
          | Just vs' <- mapM (getWHNF ctx) vs ->
            primOpX ctx tgt sop vs'
          | otherwise ->
            ctx & suspend op
        VHeap{} ->
          -- TODO: if the PrimOp has effects, then those effects have to be held.
          ctx & suspend op
        _ -> error "Bad VPrimOp arg"

    -- Array indexing.
    VArray vals ->
      case getValue ctx arg of
        VInteger idx
          | i >= 0 && i < length vals -> 
            Step1Done $ ctx & addOps [UnifyX tgt (vals!!i)]
          | otherwise -> Step1Failed
          where i = fromInteger idx
        VHeap {} -> ctx & suspend op
        _        -> wrong "Bad index in array indexing"

    VHeap {} -> ctx & holdEffects & suspend op  -- Unknown effects in the function

    _        -> wrong "Bad function in CallX"

step1 ctx op@(ChoiceX ops1 ops2)
  | ctx_hold ctx = ctx & suspend op
  | otherwise =
    let ctx1 = ctx & addOps ops1
        ctx2 = ctx & addOps ops2
    in  Step1Done (ctx1 { ctx_next = Just ctx2 })
    -- Make two contexts, one for each branch, prepending ops to the rest of the ops
    -- And then chain them together, so that we do ctx2 when ctx1 is done.
    -- NB: ctx1 and ctx2 start from the /same/ Heap;
    --     this is what implements "backtracking".

step1 ctx op@IfX{ targetx = tgt, ifx_cond = cond, ifx_exports = nas
                , ifx_then = (then_frame, then_exp), ifx_else = els } =
  -- Run the cond (with the parent freshly set)
  case step cond{ ctx_parent = Just ctx } of
    res | ifDebug && trace ("IfX evals " ++ show res) False -> undefined
    StepNotDone cond' ->
      -- cond did not finish, so suspend and hold off unknown effects.
      -- suspend with the updated condition to reflect the partial work.
      Step1Done $
      ctx & holdEffects & addResiduals [op{ ifx_cond = cond' }]

    StepDone cond' ->
      -- Condition succeeded, run the 'then' branch with the domain frame
      Step1Done $
      ctx & addClosure tgt (extendFrame then_frame ext, then_exp)
      where
         ext :: [(Name, Value)] -- The Values have no references to the Heap of cond'
         ext = [ (n, expunge (ctx_heap cond') (VHeap a))
               | (n, a) <- nas ]

    StepFailed ->
      -- Condition failed, run the 'else' branch
      Step1Done $
      ctx & addClosure tgt els
    StepNothing ->
      -- Nothing happened, just suspend again.
      ctx & holdEffects & suspend op

step1 ctx op@ForX{ targetx = tgt, forx_arr = arr, forx_dom = dom, forx_exports = nas, forx_body = (body_frame, body_exp) } =
  -- Run the domain (with the parent freshly set)
  case step dom{ ctx_parent = Just ctx } of
    res | forDebug && trace ("ForX evals " ++ show res) False -> undefined
    StepNotDone dom' ->
      -- domain did not finish, so suspend and hold off unknown effects
      Step1Done $
      ctx & holdEffects & addResiduals [op{ forx_dom = dom' }]
    StepDone dom' ->
      -- domain finished, run the body with the domain frame.
      -- The body puts the value in res, which is appended to
      -- the accumulating array.
      -- After the body execution we will run the ForX again for the next iteration,
      -- but with the domain updated to the next backtrack point.
      Step1Done $
      ctx' &
      addOps [op{ forx_arr = arr ++ [res], forx_dom = dom'' }] &
      addClosure res (extendFrame body_frame ext, body_exp)
      where ext = [ (n, expunge (ctx_heap dom') (VHeap a)) | (n, a) <- nas ]
            (ctx', res) = allocCell ctx
            dom'' = nextIter dom'
    StepFailed ->
      -- The for loop has finished, so deliver the array.
      -- Note: StepFailed is not returned until the are no
      -- more backtrack points.
      unify ctx tgt (VArray arr)
    StepNothing ->
      -- Nothing happened, just suspend again.
      ctx & holdEffects & suspend op

step1 ctx op@(RangeX tgt arr) =
  case getValue ctx arr of
    VArray vs ->
      case vs of
        []  -> Step1Failed
        [v] -> unify ctx tgt v
        _   ->
          -- Create nested ChoiceX for all the array values.
          Step1Done $
          ctx &
          addOps [foldr1 (\ op1 op2 -> ChoiceX [op1] [op2]) $ map (UnifyX tgt) vs]
    VHeap{} -> ctx & suspend op
    _ -> error "RangeX bad arg"

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
  fromMaybe (ctx{ctx_ops = [RangeX undefined (VArray [])]}) $ ctx_next ctx

-- Linearize the Exp with the given Frame and insert
-- those ops to be executed next.
addClosure :: Target -> (Frame, SExp) -> Context -> Context
addClosure tgt (frame, body) ctx =
  ctx & setHeap (ls_heap ls) & addOps (ls_ops ls)
  where
    ls = execState (expToHeap tgt $ Do body) init_ls
    init_ls = LState{ ls_heap = ctx_heap ctx, ls_frame = frame, ls_ops = []}

-- Residualize the given op and report suspension.
suspend :: OpX -> Context -> Step1Result
suspend op ctx = Step1Suspend $ addResiduals [op] ctx

-- Hold off all (sequential) effects.
holdEffects :: Context -> Context
holdEffects ctx = ctx { ctx_hold = True }

-- Execute a PrimOp.
primOpX :: Context -> Target -> PrimOp -> [Value] -> Step1Result
primOpX _ _ "Error" _ = error "Error called"
primOpX ctx tgt sop vs =
  case primOp sop vs of
    Just v -> unify ctx tgt v
    Nothing -> Step1Failed

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
    _    -> error $ "Unknown primop " ++ op
primOp _ _ = Nothing
