{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module FrontEnd.CoreSimp(simpCore) where
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import FrontEnd.Expr(Ident(..))
import FrontEnd.Core
import FrontEnd.Eval
import Epic.Print
--import Debug.Trace

-- Do some Core simplifications to enhance readability.
simpCore :: Core -> Core
simpCore =
  simpFail .
  simpSeq . simpAlias .
  simpSeq . simpAlias .
  simpSeq . simpAlias .
  simpAny

-- Get rid of values in Seq
simpSeq :: Core -> Core
simpSeq = evalSeq flg
  where flg = EFlags { underLambda = True, traceEval = False, steps = 1000 }

-- This is a version of APP-LAM for inlined 'any'.
-- I.e., any[e] = e
simpAny :: Core -> Core
simpAny = f
  where
    f (CApplyVV (Var (Ident _ "any")) v) = f $ CValue v
    f e = composOp f e

simpFail :: Core -> Core
simpFail = f
  where
    f (CDef [x] (CApplyVV (VArray []) (Var x'))) | x == x' = CFail
    f e = composOp f e

-- When there are definitions of the form x=y,
-- then get rid of one of the variables.
-- Favor getting rid of temporaries.
-- This is a version of the BIND rule.
simpAlias :: Core -> Core
simpAlias = fc . g
  where
    fc (CDef h e) | Just d <- bind h e = fc d
    fc (CLambda i is cov e1 e2) =  -- CLambda has weird scoping, temporarily change it
      case fc (CLam i (CDef is (CSeq [e1, e2]))) of
        CLam i' (CDef is' (CSeq [e1', e2'])) -> CLambda i' is' cov e1' e2'
        CLam i' (CSeq [e1', e2']) -> CLambda i' [] cov e1' e2'
        e -> error $ "simpAlias: CLambda " ++ prettyShow e
    fc e = composOpC fc fv fh e
    fh (HLam x (CDef h e)) | Just d <- lam x h e = fh d
    fh e = composOpH fc fv fh e
    fv = composOpV fc fv fh

    -- x = (y = e)  -->  x = y; y = e
    g (CUnify (CVar x) e@(CUnify (CVar y) _)) =
      CSeq [CUnify (CVar x) (CVar y), e]
    g e = composOp g e

    bind h e =
      case runState (findB h e) Nothing of
        (e', Just (x, y)) ->
          let (x', y') = if isTempIdent x then (y, x) else (x, y)
          in  Just $ cDef (h \\ [y']) $ subst y' (Var x') e'
        _ -> Nothing

    lam v h e =
      case runState (findB (v:h) e) Nothing of
        (e', Just (x, y)) | v == x || v == y ->
          -- Eliminate y' in favor of x'
          let (x', y') = if isTempIdent x then (y, x) else (x, y)
          in  Just $ HLam x' $ cDef (h \\ [x, y]) $ subst y' (Var x') e'
        _ -> Nothing

    findB h e = do
      me <- get
      if isJust me then
        pure e  -- Already found, just keep going
       else
        case e of
          CUnify (CVar x) (CVar y) | elem x h, elem y h, x /= y -> do
            put $ Just (x, y)
            pure (CVar x)
          CUnify e1 e2 -> CUnify e1 <$> findB h e2
          CSeq es -> CSeq <$> mapM (findB h) es
          CSucceeds b -> CSucceeds <$> findB h b
          _ -> pure e

isTempIdent :: Ident -> Bool
isTempIdent (Ident _ ('$':_)) = True
isTempIdent _ = False
