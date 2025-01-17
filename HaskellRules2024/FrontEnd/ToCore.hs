{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module FrontEnd.ToCore(
    convertToCore
  ) where

import Prelude hiding (pi)

import qualified Rules.Core as Core
import qualified TRS.Bind   as TRS

import FrontEnd.Desugar
import FrontEnd.Error
import FrontEnd.Expr as Src
import FrontEnd.Flags

-- Epic libraries
import Epic.Print

-- General Haskell libraries
import qualified Data.Set as S
import Data.List   ( sort, group )
import Control.Monad( when )
import Debug.Trace ( traceM )

--------------------------------------------------------
--
--             Convert to Core
--
--------------------------------------------------------


convertToCore :: Flags -> SrcCore -> IO (Core.Expr, [DError])
convertToCore flags src = runD flags (convert src)

{-
convert :: SrcCore -> Rules.Expr
convert (Variable i)   = Rules.Var (toCoreIdent i)
convert (Array ts)     = Rules.Tup (map convert ts)
convert (EPrim op)     = Rules.Op op
convert (Lit lit)      = Rules.Lit lit
convert (ApplyD t1 t2) = convert t1 Rules.:@: convert t2
convert (Unify t1 t2)  = convert t1 Rules.:=: convert t2
convert (Choice t1 t2) = convert t1 Rules.:|: convert t2
convert (Seq ts)       = foldr add (convert last_t) rest_ts
                       where
                         (rest_ts, last_t) = unSeq ts
                         add (Variable{}) e = e
                         add t            e = convert t Rules.:>: e
                         -- This `add` is aimed at (exists x; e), which will by now
                         -- have turned into  exists x ..... (x; e)....
                         -- We don't really want that "x;" just useless clutter.

convert (Exists is t)     = foldr do_one (convert t) is
                          where
                            do_one :: Src.Ident -> Rules.Expr -> Rules.Expr
                            do_one i e = Rules.Exi (TRS.bind (toCoreIdent i) e)

convert (Guard v t)   = convert v Rules.:>>: convert t
convert (Lam i t)     = Rules.Lam (TRS.bind (toCoreIdent i) (convert t))
convert (Some v)      = Rules.Some (convert v)
convert (Check fxs t) = foldr addCheck (convert t) fxs
convert (Src.Verify is t) = Rules.Verify (TRS.bindList (map toCoreIdent is) ([], convert t))
convert Src.Fail      = Rules.Fail
convert (Src.All e)         = Rules.mkAll (convert e)
convert (Src.One e)         = Rules.mkOne (convert e)
convert (Src.IfThunk e1 e2) = Rules.mkIfThunk (convert e1) (convert e2)
convert (Src.ForThunk e)    = Rules.mkForThunk (convert e)
convert (Src.Truth e) = Rules.Tru (convert e)
--convert (Src.Iter e1 e2 e3) = Rules.Iter (convert e1) (convert e2) (convert e3)
convert e = impossible "convert" e
-}


--------------------------------------------------------
--
--             Adding scopes and converting
--
--    Replace  x:=t by   exists x. ...(x=t)...
--        "x:=t"    is represented by     DefineE x t
--
--------------------------------------------------------

convert :: SrcCore -> DsM Core.Expr
convert e = conv S.empty (Check [] e)

conv :: S.Set Src.Ident -> SrcExpr -> DsM Core.Expr
-- The input expression is in BigCore, after desugaring,
-- but still with x := e stuff
-- In  (conv sc expr), `sc` is a set of identifiers already in scope
--     to allow us to complain about shadowing
conv sc = expr
  where
    -- variables
    expr (DefineE i e)  = expr (Unify (Variable i) e) -- x:=e     --> x=e
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
    expr (Lam i e)      = (Core.Lam . TRS.bind (toCoreIdent i)) <$> convD (S.insert i sc) e

    --expr (Let e1 e2) = do { (is, e1'', sc') <- defs' sc e1
    --                      ; e2' <- scopeD sc' e2
    --                      ; pure $ eExists is $ eSeq [e1'', e2'] }

    -- combinators
    expr (Seq es)       = Core.coreSeq <$> mapM expr es
    expr (Unify e1 e2)  = (Core.:=:) <$> expr e1 <*> expr e2
    expr (Choice e1 e2) = (Core.:|:) <$> exprD e1 <*> exprD e2
    expr Fail           = pure Core.Fail

    -- verification
    expr (Verify is e)      = coreVerify is <$> convD (foldr S.insert sc is) e
    expr (Check [] e)       = exprD e
    expr (Check (fx:fxs) e) = do errEff fx
                                 maybe id Core.mkCheck (toCoreEff fx) <$> exprD (Check fxs e)
    expr (Some e)           = Core.Some <$> exprD e
    expr (Guard v e)        = (Core.:>>:) <$> expr v <*> convD sc e

    -- iter constructs
    expr (One e)        = Core.mkOne <$> exprD e
    expr (All e)        = Core.mkAll <$> exprD e
    expr (If3 e1 e2 e3) = Core.mkIfThunk <$> exprD (eSeq [e1, eThunk e2]) <*> exprD e3
    expr (For2 e1 e2)   = Core.mkForThunk <$> exprD (eSeq [e1, eThunk e2])

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

coreExis :: [Ident] -> Core.Expr -> Core.Expr
coreExis is e = Core.mkExis (map toCoreIdent is) e

coreVerify :: [Ident] -> Core.Expr -> Core.Expr
coreVerify is e = Core.Verify (TRS.bindList (map toCoreIdent is) ([], e))

toCoreIdent :: Ident -> Core.Ident
toCoreIdent (Ident _ s) = Core.Name s

toCoreEff :: Eff -> Maybe Core.Effect
toCoreEff ESucceeds = Just Core.Succeeds
toCoreEff EDecides  = Just Core.Decides
toCoreEff EFails    = Just Core.Fails
toCoreEff _         = Nothing -- error "toCoreEff" (prettyShow eff)

errShadow :: [(Ident, Ident)] -> DsM ()
errShadow is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      (i@(Ident li _), (Ident lj _)) : _ -> errorMessage $ "shadowing: " ++ prettyShow (li, i, lj)
   else
    mapM_ (\ (i@(Ident li _), (Ident lj _)) -> traceM $ "warning shadowing " ++ prettyShow (li, i, lj)) is

errMultiple :: [[Ident]] -> DsM ()
errMultiple =
  mapM_ (\ is -> errorMessage $ "multiply defined: " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

errUndefined :: [Ident] -> DsM ()
errUndefined is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      i@(Ident l _) : _ -> errorMessage $ "undefined: " ++ prettyShow (l, i)
   else
    mapM_ reportScopeErr is

errEff :: Eff -> DsM ()
errEff eff = case toCoreEff eff of
               Nothing -> traceM $ "unsupported effect: " ++ show eff
               Just _  -> pure ()

reportScopeErr :: Ident -> DsM ()
reportScopeErr i@(Ident l _) = do
  putScopeErr i;
  traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i)
