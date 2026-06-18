{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module FrontEnd.ToCore
  ( {-convertToCore, convertToPrepdCore
  , convert, -}
  toCoreEff
  ) where

import Prelude hiding (pi)

import qualified Core.Expr as Core
--import qualified Core.Bind as Bind

--import FrontEnd.Desugar
--import FrontEnd.Error
import FrontEnd.Expr as Src
--import FrontEnd.Flags

-- Epic libraries
--import Epic.Print

-- General Haskell libraries
--import qualified Data.Set as S
--import Data.List   ( sort, group )
--import Control.Monad( when )
--import Debug.Trace ( traceM )

{-
--------------------------------------------------------
--
--             Convert to Core
--
--------------------------------------------------------


convertToCore :: Flags -> SrcCore -> IO Core.Expr
convertToCore flags src
  = runD flags Core.Fail $
    do { core <- convert src
       ; traceD "Convert to core" (pPrint core)
       ; return core }

convertToPrepdCore :: Flags -> SrcCore -> IO Core.Expr
convertToPrepdCore flags src
  = runD flags Core.Fail $
    do { core <- convert src
       ; traceD "Convert to core" (pPrint core)

       ; let prepd_core = Core.prep core
       ; traceD "Prep'd core" (pPrint prepd_core)

       ; return prepd_core }

--------------------------------------------------------
--
--             Adding scopes and converting
--
--    Replace  x:=t by   exists x. ...(x=t)...
--        "x:=t"    is represented by     DefineE x t
--
--------------------------------------------------------

convert :: SrcCore -> DsM Core.Expr
convert e = conv S.empty (Check effTop e)

conv :: S.Set Src.Ident -> SrcExpr -> DsM Core.Expr
-- The input expression is in BigCore, after desugaring,
-- but still with x := e stuff
-- In  (conv sc expr), `sc` is a set of identifiers already in scope
--     to allow us to complain about shadowing
conv sc = expr
  where
    -- variables
    expr (DefineV i)    = expr (Variable i)           -- exists x --> x
    expr (Variable i)   = do when (i `S.notMember` sc) (errUndefined [i])
                             pure (Core.Var (toCoreIdent i))

    -- basic cases
    expr (Lit a)        = pure (Core.Lit a)
    expr (EPrim op)     = pure (Core.Op op)
    expr (Array es)     = Core.Tup <$> mapM expr es
    expr (Truth e)      = Core.Tru <$> expr e
    expr (ApplyD e1 e2) = (Core.:@:) <$> expr e1 <*> expr e2

    -- binding/scope
    expr (Exists is e)  = coreExis is <$> conv (foldr S.insert sc is) e
    expr (Lam i e)      = (Core.Lam . Bind.bind (toCoreIdent i)) <$> convD (S.insert i sc) e

    -- combinators
    expr (Seq e1 e2)    = coreMkSeq <$> expr e1 <*> expr e2
    expr (Unify e1 e2)  = (Core.:=:) <$> expr e1 <*> expr e2
    expr (Choice e1 e2) = (Core.:|:) <$> exprD e1 <*> exprD e2
    expr Fail           = pure Core.Fail

    -- verification
    expr (Verify is e)      = coreVerify is <$> convD (foldr S.insert sc is) e
    expr (Check eff e)      = case toCoreEff eff of
                                Nothing   -> do { traceM $ "ignoring unsupported effect: " ++ show eff
                                                ; exprD e }
                                Just ceff -> Core.mkCheck ceff <$> exprD e
    expr (Some e)           = Core.Some <$> exprD e
    expr (Guard v e)        = (Core.:>>:) <$> expr v <*> convD sc e

    -- iter constructs
    expr (One e)        = Core.mkOne <$> exprD e
    expr (All e)        = Core.mkAll <$> exprD e
    expr (If3 e1 e2 e3) = Core.mkIf  <$> exprD (eSeq [e1, eThunk e2]) <*> exprD e3
    expr (For2 e1 e2)   = Core.mkFor <$> exprD (eSeq [e1, eThunk e2])

    -- catch all, impossible case?
    expr e = error (show e)

    -- for a new scope context, using current in-scope set
    exprD     e = fst <$> defs sc  e
    convD sc' e = fst <$> defs sc' e

    defs :: S.Set Ident -> SrcExpr -> DsM (Core.Expr, S.Set Ident)
    -- `e` starts a new scoping context.  Wrap it in an `Exists`
    defs sc' e = do { (is, e', s) <- defs' sc' e
                    ; pure (coreExis is e', s) }

    defs' :: S.Set Ident -> SrcExpr -> DsM ([Ident], Core.Expr, S.Set Ident)
    -- Find identifers bound in `e`, and return them
    -- along with extended scope-set and transformed `e`.
    -- 'as' is the set of in-scope variables
    defs' as e = do
      let -- Get all binders from e
          is = getVisibleBinders e

          -- errM: multiply-defined variables
          -- E.g.   f() := { x:int; x:float; x }   is illegal
          -- This is an error
          errM = filter ((> 1) . length) $ group $ sort is

          -- errS: find varaiables ones that are already in scope, hence shadowed
          -- E.g. f(x:int) := { x:float; x }    Here the inner x shadows the outer
          -- This is only a warning
          errS = [ (i, j) | i <- is, j <- filter (== i) (S.toList as) ]
          sc' :: S.Set Ident = foldr S.insert as is

      e' <- conv sc' e
      errMultiple errM
      errShadow errS
      pure (is, e', sc')

-- smart constructor to declutter (Exists x) --> x trails
coreMkSeq :: Core.Expr -> Core.Expr -> Core.Expr
coreMkSeq v  e2 | Core.isVal v = e2
coreMkSeq e1 e2                = e1 Core.:>: e2

coreExis :: [Ident] -> Core.Expr -> Core.Expr
coreExis is e = Core.mkExis (map toCoreIdent is) e

coreVerify :: [Ident] -> Core.Expr -> Core.Expr
coreVerify is e = Core.Verify (Bind.bindList (map toCoreIdent is) ([], e))

toCoreIdent :: Ident -> Core.Ident
toCoreIdent (Ident _ s) = Core.Name s
-}

toCoreEff :: Eff -> Maybe Core.Effect
-- ToDo: silently ignoring side effects for now
toCoreEff (Eff { eff_card = card, eff_side = _side })
  = case card of
      CSucceeds -> Just Core.Succeeds
      CDecides  -> Just Core.Decides
      CFails    -> Just Core.Fails
      CIterates -> Just Core.Iterates

{-
errShadow :: [(Ident, Ident)] -> DsM ()
errShadow is = do
  report_error <- getDFlagsX fReportError
  case report_error of
    ErrError ->
      case is of
        [] -> pure ()
        (i@(Ident li _), (Ident lj _)) : _ -> errorMessage $ "shadowing: " ++ prettyShow (li, i, lj)
    ErrWarning ->
      mapM_ (\ (i@(Ident li _), (Ident lj _)) -> traceM $ "warning shadowing " ++ prettyShow (li, i, lj)) is
    ErrNone ->
      pure ()

errMultiple :: [[Ident]] -> DsM ()
errMultiple =
  mapM_ (\ is -> errorMessage $ "multiply defined: " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

errUndefined :: [Ident] -> DsM ()
errUndefined is = do
  report_error <- getDFlagsX fReportError
  case report_error of
    ErrError ->
      case is of
        [] -> pure ()
        i@(Ident l _) : _ -> errorMessage $ "undefined: " ++ prettyShow (l, i)
    ErrWarning ->
      mapM_ reportScopeErr is
    ErrNone ->
      pure ()

reportScopeErr :: Ident -> DsM ()
reportScopeErr i@(Ident _l _) = do
  do { -- traceM $ "scopeCheck: warning undefined " ++ prettyShow (_l, i)
       putScopeErr i }

-}
