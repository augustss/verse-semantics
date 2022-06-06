{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleContexts #-}
module CoreSimp(simpCore) where
import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Expr(Ident(..))
import Core
import Eval

-- Do some Core simplifications to enhance readability.
simpCore :: Core -> Core
simpCore =
  simpSeq . simpAlias .
  simpSeq . simpAlias . simpAny

-- Get rid of values in Seq
simpSeq :: Core -> Core
simpSeq = evalSeq flg
  where flg = Flags { underLambda = True, traceEval = False }

-- This is a version of APP-LAM for inlined 'any'.
-- I.e., any[e] = e
simpAny :: Core -> Core
simpAny = f
  where
    f (CApply (Var (Ident _ "any")) v) = f $ CValue v
    f e = composOp f e

-- When there are definitions of the form x=y,
-- then get rid of one of the variables.
-- Favor getting rid of temporaries.
-- This is a version of the BIND rule.
simpAlias :: Core -> Core
simpAlias = f . g
  where
    f (CDef h e) | Just d <- bind h e = f d
    f (CLam x (CDef h e)) | Just d <- lam x h e = f d
    f e = composOp f e

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
        (e', Just (x, y)) ->
          let (x', y') = if isTempIdent x && x /= v then (y, x) else (x, y)
              v' = if v == y' then x' else v
          in  Just $ CLam v' $ cDef (h \\ [y']) $ subst y' (Var x') e'
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
          CUnify e1 e2 -> CUnify <$> findB h e1 <*> findB h e2
          CSeq es -> CSeq <$> mapM (findB h) es
          CSucceeds b -> CSucceeds <$> findB h b
          _ -> pure e

isTempIdent :: Ident -> Bool
isTempIdent (Ident _ ('$':_)) = True
isTempIdent _ = False
