module SExp(denSemDesugar, denSem) where
import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe
import FrontEnd.Expr(SrcExpr(..), Aperture(..), Lit(..))
import qualified FrontEnd.Expr as E
import ENV(ENV)
import Oper as O
import SemSeqENV(sem)
--import Epic.Print

denSemDesugar :: SrcExpr -> IO Oper
denSemDesugar = return . syntax

denSem :: Oper -> IO [ENV]
denSem = return . sem

-- Monad for generating new names
type N a = State Int a

newVar :: String -> N Ident
newVar s = do
  i <- get
  put (i+1)
  return $ Ident $ s ++ "_" ++ show i

newVars :: Int -> String -> N [Ident]
newVars n s = replicateM n (newVar s)

ident :: E.Ident -> Ident
ident (E.Ident _ i) = Ident i

us :: Ident
us = Ident "_"

isUs :: Ident -> Bool
isUs = (us ==)

seqs :: [Oper] -> Oper
seqs = foldr (.:>:) NoOp

-- Make sure the Ident is not _
asVar :: String -> Ident -> N (Oper, Ident)
asVar s (Ident "_") = do
  o <- newVar s
  pure (Exi o, o)
asVar _ o = pure (NoOp, o)

-- Put SrcExpr into a variable, special case if
-- it's already a variable.
toVar :: String -> SrcExpr -> N (Oper, Ident)
toVar _ (Variable x) = pure (NoOp, ident x)
toVar s e = do
  o <- newVar s
  op <- srcExprToOperN us o e
  pure (Exi o :>: op, o)

-- The final result ends up in 'res'
syntax :: SrcExpr -> Oper
syntax = syntax' us (Ident "res")

syntax' :: Ident -> Ident -> SrcExpr -> Oper
syntax' i o e = Scope $ evalState (srcExprToOperN i o e) 1

-- Hack some identifiers into primitives.
getPrim :: String -> Maybe (O.PrimOp, Int)
getPrim s =
  case s of
    "operator'+'"  -> Just (Padd, 2)
    "operator'<='" -> Just (PLE,  2)
    "int"          -> Just (Pint, 1)
    "any"          -> Just (Pany, 1)
    _              -> Nothing

apply :: Ident -> Ident -> Ident -> Oper
apply o f@(Ident s) a
  | Just (p,1) <- getPrim s = o :=@@(p,[a])
  | otherwise               = o :=@ (f,a)

srcExprToOperN :: Ident -> Ident -> SrcExpr -> N Oper
srcExprToOperN = to where
  to u o expr =
    case expr of
      -- Hack around things pulled in from the prelude.
      DefineE (E.Ident _ s) _
        | isJust (getPrim s) -> pure NoOp  -- delete operator'...' :=
      -------------------------------------
      Lit (LInt k)         -> pure $ u .:=  k  .:>: o .:=  k
      Variable x
        | E.Ident l "_"<-x -> let v = E.Ident l "x" in to u o (Blk [DefineV v, Variable v])
        | otherwise        -> pure $ u .:=: x' .:>: o .:=: x' where x' = ident x
--      EPrim p              -> to $ Var $ Ident $ drop 1 $ show p
      ApplyD (Variable (E.Ident _ s)) e | Just (p, n) <- getPrim s ->
        case (n, e) of
          (1, _) -> do (op, x) <- toVar "a" e; pure $ op .:>: (o:=@@(p,[x]) :>: u .:=: o)
          (2, Array [e1, e2]) -> do (op1,x1) <- toVar "a" e1; (op2,x2) <- toVar "a" e2;
                                    pure $ op1 .:>: op2 .:>: (o:=@@(p,[x1,x2]) :>: u .:=: o)
          _ -> error $ "Bad primop use: " ++ show expr
      ApplyD e0 e1         -> do
        (op0, f) <- toVar "f" e0
        (op1, a) <- toVar "a" e1
        pure $ seqs [op0, op1, o:=@(f, a), u .:=: o]
      Unify e0 e1
        | isUs u, isUs o, Variable x <- e0, Lit (LInt k) <- e1 -> pure $ ident x := k
        | isUs u, isUs o, Variable x <- e0, Variable y   <- e1 -> pure $ ident x :=: ident y
        | otherwise        -> do
            (opo, o') <- asVar "o" o
            op0 <- to u o' e0
            op1 <- to u o' e1
            pure $ seqs [opo, op0, op1]
      Seq e0 e1            -> do
        op0 <- to us us e0
        op1 <- to  u  o e1
        pure $ op0 .:>: op1
      Choice e0 e1         -> (:|:) <$> to u o e0 <*> to u o e1
      E.Fail               -> pure O.Fail
      DefineV i            -> pure $ Exi $ ident i
      DefineE i e          -> do
        let i' = ident i
        op <- to u i' e
        pure $ seqs [Exi i', op, o .:=: i']
      DefineIE i e          -> do
        let i' = ident i
        op <- to i' o e
        pure $ seqs [Exi i', op, u .:=: i']
      Array es
        | isUs u           -> do
          ss <- newVars (length es) "s"
          ops <- zipWithM (to us) ss es
          pure $ seqs $ map Exi ss ++ ops ++ [ o :=<> ss ]
        | otherwise        -> do
          ts <- newVars (length es) "t"
          ss <- newVars (length es) "s"
          ops <- sequence (zipWith3 to ts ss es)
          pure $ seqs $ map Exi ts ++ map Exi ss ++ ops ++ [ u :=<> ts, o :=<> ss ]
      E.All e              -> do
        y <- newVar "y"
        op <- to us y e
        pure $ seqs [u .:=: o, O.All o op y]
      If3 e0 e1 e2         -> do
        op0 <- to us us e0
        op1 <- to  u  o e1
        op2 <- to  u  o e2
        pure $ If op0 op1 op2
      OfType e1 _ e2       -> to u o $ ApplyD e2 e1
      Range e              -> do
        (op, f) <- toVar "t" e
        (opu, u') <- asVar "u" u
        pure $ seqs [op, opu, apply o f u']
      Blk es               -> Scope <$> to u o (blkToExpr es)
      Function Closed e0 _ e1 -> do
        (opu, u') <- asVar "h" u
        i <- newVar "i"
        x <- newVar "x"
        k <- newVar "k"
        y <- newVar "y"
        c0 <- to i x e0
        c1 <- to k y e1
--  cq <- checkQ q u e0 -- XXX
        pure $ opu .:>: (o :=\ (i, Exi x :>: c0, Exi k :>: k :=@(u', x) :>: c1, y))
      Function Open e0 _ e1 -> do
        (opu, h) <- asVar "h" u
        i <- newVar "i"
        x <- newVar "x"
        k <- newVar "k"
        y <- newVar "y"
        c0 <- to i x e0
        c1 <- to k y e1
--  cq <- checkQ q u e0 -- XXX
        pure $ opu .:>: (o :=\ (i,
                                NoOp,
                                If (Exi x :>: c0)
                                   (Exi k :>: k :=@(h, x) :>: c1)
                                   (y :=@(h, i)),
                                y
                               )
                        )
      e -> error $ "srcExprToOperN: cannot handle " ++ show e

blkToExpr :: [SrcExpr] -> SrcExpr
blkToExpr [] = Array []
blkToExpr es = foldr1 Seq es


(.:=) :: Ident -> Integer -> Oper
Ident "_" .:= _ = NoOp
i         .:= k = i := k

(.:=:) :: Ident -> Ident -> Oper
Ident "_" .:=: _ = NoOp
i         .:=: j = i :=: j

infixr 4 .:>:
(.:>:) :: Oper -> Oper -> Oper
NoOp .:>: o    = o
o    .:>: NoOp = o
o1   .:>: o2   = o1 :>: o2
