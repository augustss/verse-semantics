{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module OpSem.Eval(run) where
import Data.List ( intercalate )
import Control.Monad.State.Strict
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import GHC.Stack ( HasCallStack )
import Text.PrettyPrint.HughesPJClass hiding (semi)
import Debug.Trace

import OpSem.Exp ( Name )
import OpSem.Misc ( assert, assertM, showListWith )
import OpSem.Op

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
stepDebug = True
stepFrameDebug = False
sqDebug = False

--------------------------------
--
-- Machine state
--
--------------------------------

-----
-- Global execution state
----

data RunState =  -- The global state of the machine
  RunState
  { rs_contexts       :: !(Map ContextId Context)   -- all active contexts
  , rs_nextContextId  :: !ContextId                 -- Next free contextId
  , rs_currentContext :: !ContextId                 -- currently active context
  }
  deriving (Show)

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
        , ctx_failure  :: Closure       -- Do this if the head Op in ctx_ops fails
                                        -- Does not vary
        , ctx_success  :: Closure       -- ToDo: could this just be the tail of ctx_ops?
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
  = ContFrame Frame [Op]     -- end of a PushFrame
  | ContFun Value Value Frame [Op] -- Return from a function call,
                             -- unifying the result with the value
  | ContOps [Op]             -- end of a join (Alt)
  deriving (Show)

instance Pretty StackFrame where
  pPrint (ContFrame fr ops) =
    text "ContFrame" $$ nest 2 (vcat
      [text "fr =" <+> pPrint fr
      ,text "ops =" <+> pPrint ops
      ])
  pPrint (ContFun sq target fr ops) =
    text "ContFun" $$ nest 2 (vcat
      [text "sq =" <+> pPrint sq
      ,text "target =" <+> pPrint target
      ,text "fr =" <+> pPrint fr
      ,text "ops =" <+> pPrint ops
      ])
  pPrint (ContOps ops) =
    text "ContOps" $$ nest 2 (vcat
      [text "ops =" <+> pPrint ops
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
  | SuspPrimBin String Value Value Value
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
type Heap = Map HeapAddr (Maybe Value)
            -- Nothing => no one has instantiated this variable yet
            -- This is just for assertion-checking; we could equally
            --      well use (Map HeapAddr Value)
            -- We can change (x :-> Nothing) to (x :-> Just val); but once
            -- we add (x :-> Just val) to a Heap, we never change that binding

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
    value s (VFun n (Closure fr os)) = VFun n (Closure (frame s fr) os)
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
expungeFrame ci heap fr = f $ expunge ci heap (VFun "" (Closure fr []))
  where f (VFun "" (Closure fr' [])) = fr'
        f _ = error "impossible"

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
  ctx{ ctx_ops = ops, ctx_frame = fr, ctx_stack = ContFrame (ctx_frame ctx) (ctx_ops ctx) : ctx_stack ctx }

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
    Ctx{ ctx_next = Nothing, ctx_parent = Just pci, ctx_failure = Closure fr fOps } -> do
      setCurContextId pci
      modifyCurContext $ \ c -> pushFrame fOps fr c
      --ctx <- getCurContext
      --traceM $ "failure: fail branch\n" ++ prettyShow ctx

primBinOp :: String -> Value -> Value -> Value -> R ()
primBinOp op dst src1 src2 = do
  src1' <- follow src1
  src2' <- follow src2
  case (src1', src2') of
    (VHeap _ h1, _) -> addSusp [h1] (SuspPrimBin op dst src1' src2')
    (_, VHeap _ h2) -> addSusp [h2] (SuspPrimBin op dst src1' src2')
    (v1, v2) -> primBin op dst v1 v2

primBin :: String -> Value -> Value -> Value -> R ()
primBin op dst v1@(VInteger i1) (VInteger i2) = do
  let arith f = unify dst $ VInteger $ i1 `f` i2
      compar f = if i1 `f` i2 then unify dst v1 else failure "comparison"
  case op of
    "+" -> arith (+)
    "-" -> arith (-)
    "*" -> arith (*)
    "div" | i2 == 0 -> failure "div by 0"
          | otherwise -> arith div
    "<" -> compar (<)
    "<=" -> compar (<=)
    ">" -> compar (>)
    ">=" -> compar (>=)
    _ -> error $ "Unknown primop " ++ op
primBin op _ _ _ = failure $ "primBin " ++ op

-- Call f with argument a, but first wait for f to be in WHNF
callOp :: Value -> Value -> Value -> Value -> Value -> R ()
callOp _ _ t f a | debug && trace ("callOp " ++ show (t,f,a)) False = undefined
callOp sqa sqt t f a = do
  f' <- follow f
  case f' of
    VFun n cls -> apply sqa sqt t n cls a
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

apply :: Value -> Value -> Value -> Name -> Closure -> Value -> R ()
-- Does not make a new Context/Heap; 
apply sqa sqt target argName (Closure fr ops) arg =
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
      -- The two Choice instruction sequences end with an EndFrame.
      -- This is the corresponding pushFrame, so the ops run in
      -- frame where the Choice was originally executed.
      modifyCurContext $ pushFrame ops1 fr
      ctx <- getCurContext
      let
        -- NB: both ctx1 and ctx2 start with the same heap
        ctx1 = ctx{ ctx_next = Just ctx2 }
        ctx2 = ctx{ ctx_ops = ops2
                  , ctx_name = ctx_name ctx ++ "-next"
                  }
      let showNexts p = "    " ++ ctx_name p ++ ": nexts=" ++ showListWith (unwords . take 2 . words . show . head . ctx_ops) (getNexts p)
      traceM $ "########## " ++ showNexts ctx1
      --traceM $ "========\nctx1=\n" ++ prettyShow ctx1
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
    --let showNexts p = "    " ++ ctx_name p ++ ": nexts=" ++ showListWith (show . ctx_id) (getNexts p)
    let showNexts p = "    " ++ ctx_name p ++ ": nexts=" ++ showListWith (unwords . drop 4 . take 7 . words . show . head . ctx_ops) (getNexts p)
    traceM $ "::::: " ++ showNexts ctx

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
               , ctx_success = Closure pfr s
               , ctx_failure = Closure pfr f
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
                       , ctx_success = Closure _fr sOps, ctx_frame = fr, ctx_heap = heap } <- ctx -> do
      vsq <- follow (loadValue (sq_choice sq) ctx)
      assertM "EndDomain sq" (vsq == VDummy)
      setCurContextId pci  -- Switch to parent context
      let fr' = fr{ fr_name = fr_name fr ++ "-domain", fr_vals = M.filterWithKey notTemp (fr_vals fr) }
          notTemp k _ = let c = head k in c /= '%' && c /= '$'
          fr'' = expungeFrame ci heap fr'
      when debug $ do
        traceM $ "EndDomain: fr' = " ++ prettyShow fr'
        traceM $ "           fr'' = " ++ prettyShow fr''
      -- XXX We ought to push _fr here, but it seems to work anyway.  Why?
      modifyCurContext $ pushFrame sOps fr''
    EndDomain sq | Ctx{ ctx_ops = [], ctx_susps = susps, ctx_parent = Just pci, ctx_id = ci } <- ctx -> do
      setCurContextId pci
      addSusp (concatMap susp_waitingFor susps) (SuspDomain sq ci)

    PushFrame msg ns ops -> do
      let (hs, ctx') = newHeapIds (length ns) ctx
          fr = makeFrame msg (zipWith (\ n h -> (n, VHeap n $ HeapId (ctx_id ctx) h)) ns hs) (ctx_frame ctx)
      updateContext $ pushFrame ops fr ctx'

    -- EndFrame, EndFun are always the last instruction in the [Op]
    EndFrame | Ctx{ ctx_ops = [], ctx_stack = ContFrame fr ops : stk } <- ctx ->
      modifyCurContext $ \ c -> c{ ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }

    -- The ops for a function lacks the trailing EndFrame.
    -- So we pop it here.
    EndFun rsq ret | Ctx{ ctx_ops   = []   -- EndFun is the last instruction
                         , ctx_stack = ContFrame xfr xops : ContFun sqt target fr ops : stk
                         } <- ctx -> do
      assertM ("EndFun " ++ show (xfr, xops)) (null xops && fr_name xfr == "apply")
      unify target (loadValue ret ctx)
      unify sqt (loadValue (sq_choice rsq) ctx)
      modifyCurContext $ \ c -> c{ ctx_ops = ops, ctx_frame = fr, ctx_stack = stk }

    -- NextFor is the last instruction of a PushFrame (of locals),
    -- so that frame is popped.  The second frame that is popped,
    -- and used, is pushed by EndDomain.
    NextFor rc ra rv lsq osq | Ctx{ ctx_ops = []
                                  , ctx_stack = ContFrame _xfr _xops : ContFrame fr ops : stk } <- ctx -> do
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
              do
                c <- getCurContext
                traceM $ ":: next " ++ show (isJust (ctx_next c))
              failure "NextFor"
            _ -> error "impossible: NextFor 1"
        _ -> error "impossible: NextFor 2"

    RangeOp sqin sqout t r -> rangeOp sqin sqout (ctx_frame ctx) t (loadValue r ctx)
    --Atom t v -> modifyCurContext $ assign t v
    Atom t v -> unify (loadValue t ctx) v
    --MkArray t rs -> modifyCurContext $ assign t (VArray [ loadValue r ctx | r <- rs])
    MkArray t rs -> unify (loadValue t ctx) (VArray [ loadValue r ctx | r <- rs])
    Unify r1 r2 -> unify (loadValue r1 ctx) (loadValue r2 ctx)
    Assign r1 r2 -> modifyCurContext $ assignSq (loadValue r1 ctx) (loadValue r2 ctx)
    PrimBinOp o t x y -> primBinOp o (loadValue t ctx) (loadValue x ctx) (loadValue y ctx)
    Function t n ops -> unify (loadValue t ctx) (VFun n (Closure (ctx_frame ctx) ops))
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
  when (moreDebug && not (null susps)) $ do
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
      SuspPrimBin op v1 v2 v3  -> primBinOp op v1 v2 v3
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
      ifr = Frame { fr_name = "run", fr_vals = M.empty, fr_parent = Nothing }
      ictx =
        Ctx{ ctx_name = "ctx_run"
           , ctx_id = ci
           , ctx_heap = M.empty
           , ctx_heapAddr = 1
           , ctx_frame = ifr
           , ctx_ops = ops
           , ctx_susps = []
           , ctx_stack = []
           , ctx_parent = Nothing
           , ctx_success = Closure ifr [ErrorOp "ctx_success"]
           , ctx_failure = Closure ifr [ErrorOp "ctx_failure"]
           , ctx_next = Nothing
           }
  setCurContextId ci
  updateContext ictx
  loop
