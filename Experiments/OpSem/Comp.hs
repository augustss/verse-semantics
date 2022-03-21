{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module OpSem.Comp(comp, compExp) where
import Control.Monad.State.Strict
import GHC.Stack ( HasCallStack )

import OpSem.DSL(for)
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

expToReg :: Seq -> Reg -> Exp -> C Seq
expToReg sq t (Var n) = do
  emit $ Unify t (Reg n)
  pure sq
expToReg sq t (Con i) = do
  emit $ Atom t (VInteger i)
  pure sq
expToReg sq t (Semi e1 e2) = do
  t1 <- newReg
  sq1 <- expToReg sq t1 e1
  expToReg sq1 t e2
expToReg sq t (Where e1 e2) = do
  t2 <- newReg
  sq1 <- expToReg sq t e1
  sq2  <- expToReg sq1 t2 e2
  pure sq2
expToReg sq t (Alt e1 e2) = do
  sq' <- Seq <$> newRegSq
  op1 <- sexpToReg sq sq' t e1
  op2 <- sexpToReg sq sq' t e2
  let rsq = sq_choice sq
  emit $ Choice rsq [op1, EndFrame] [op2, EndFrame]
  pure sq'
expToReg sq t (Equal e1 e2) = do
  sq1 <- expToReg sq t e1
  sq2 <- expToReg sq1 t e2
  pure sq2
expToReg sq t (Set n e) =
  expToReg sq t $ Equal (Var n) e
expToReg sq t (SetAny n) =
  expToReg sq t (Var n)
expToReg sq t (Array es) = do
  let f s [] = pure (s, [])
      f s (x:xs) = do r <- newReg; s' <- expToReg s r x; (s'', rs) <- f s' xs; pure (s'', r:rs)
  (sq', rs) <- f sq es
  emit $ MkArray t rs
  pure sq'
expToReg sq t (PrimBin op e1 e2) = do
  r1 <- newReg
  r2 <- newReg
  sq1 <- expToReg sq r1 e1
  sq2 <- expToReg sq1 r2 e2
  emit $ PrimBinOp op t r1 r2
  pure sq2
expToReg sq _ Fail = do
  emit Failure
  pure sq
expToReg sq t (For e1 e2) = do
  a <- newReg' "%%"  -- Accumulate the resulting array here
  c <- newReg        -- Domain context
  lsq <- Seq <$> newReg' "$$" -- Choice sequencing in the loop.  Like 'a', hackily updated.
  dsq <- newSeq
  o1 <- sexpToOps' dsq (\ sq' _r -> [EndDomain sq']) e1
  o2 <- sexpToOps' lsq (\ sq' v -> [NextFor c a v lsq sq']) e2
  msg <- newName (\ n -> "for-ctx" ++ show n)
  emit $ MkArray a []
#if 1
  xsq <- newRegSq
  emit $ Assign (sq_choice lsq) xsq
  emit $ Iterate msg c [o1] [o2] [Unify t a, EndFrame]
  emit $ Assign xsq (sq_choice sq)
#else
  -- This should work, but doesn't.
  emit $ Assign (sq_choice lsq) (sq_choice sq)
  emit $ Iterate msg c [o1] [o2] [Unify t a, EndFrame]
#endif
  pure lsq
expToReg sq t (Range e) = do
  r <- newReg
  sq' <- expToReg sq r e
  rsq <- newRegSq
  emit $ RangeOp (sq_choice sq') rsq t r
  pure $ Seq rsq
expToReg sq t (If e1 e2 e3) = do
  dsq <- newSeq
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
  emit $ Iterate msg c [o1] [o2, EndFrame] [o3, EndFrame]
  pure osq
expToReg sq t (Do e) = do
  sq' <- Seq <$> newRegSq
  o <- sexpToReg sq sq' t e
  emit o
  pure sq'
expToReg sq t (Let (Def ns e1) e2) =
  expToReg sq t (Do (Def ns (e1 `Semi` e2)))
expToReg sq t (Lam n e) = do
  os <- sexpToOps' (Seq $ Reg $ "$" ++ n) (\ osq r -> [EndFun osq r]) e
  emit $ Function t n [os]
  pure sq
expToReg sq t (App e1 e2) = do
  r1 <- newReg
  r2 <- newReg
  sq1 <- expToReg sq r1 e1
  sq2 <- expToReg sq1 r2 e2
  sqr <- Seq <$> newRegSq
  emit $ Call sq2 sqr t r1 r2
  pure sq2
expToReg sq _ Error = do
  emit $ ErrorOp "Error"
  pure sq
expToReg sq _ Wrong = do
  emit $ ErrorOp "Wrong"
  pure sq
--expToReg x = error $ show x

sexpToOps' :: HasCallStack => Seq -> (Seq -> Reg -> [Op]) -> SExp -> C Op
sexpToOps' sq ops (Def ns e) = do
  olds <- get
  put olds{ cops = [], tempRegs = [] }
  r <- newReg
  sq' <- expToReg sq r e
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
  where addFor e = for ("&it" `Set` e) (Var "&it")

compExp :: Exp -> [Op]
compExp = hackOpt . comp . adjExp

hackOpt :: [Op] -> [Op]
hackOpt (PushFrame _ [n] [Atom {target = Reg n', op_atom = a},Unify t (Reg n''),i@Assign{}, EndFrame] : rs)
  | n == n', n == n'' = hackOpt (Atom t a : i : rs)
{-
hackOpt (PushFrame _ [] ops : rs)
  | last ops == EndFrame = hackOpt (init ops ++ rs)
hackOpt (PushFrame _ [n] [Atom {op_target = Reg n', op_atom = AnInteger i},Load (Reg n''),EndFrame] : rs)
  | n == n', n == n'' = LoadInteger i : hackOpt rs
-}
hackOpt (PushFrame s ns ops : rs) = PushFrame s ns (hackOpt ops) : hackOpt rs
hackOpt (Function t n ops : rs) = Function t n (hackOpt ops) : hackOpt rs
hackOpt (Choice sq ops1 ops2 : rs) = Choice sq (hackOpt ops1) (hackOpt ops2) : hackOpt rs
hackOpt (Iterate n c d s f : rs) = Iterate n c (hackOpt d) (hackOpt s) (hackOpt f) : hackOpt rs
hackOpt (op : rs) = op : hackOpt rs
hackOpt [] = []
