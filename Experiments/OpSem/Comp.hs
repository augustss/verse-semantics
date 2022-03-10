{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module OpSem.Comp(comp, compExp) where
import Control.Monad.State.Strict
import GHC.Stack ( HasCallStack )

import OpSem.Exp
import OpSem.Op

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


adjExp :: Exp -> SExp
adjExp = addDef . addFor
  where addFor e = for ("&it" := e) (Var "&it")

compExp :: Exp -> [Op]
compExp = comp . adjExp

