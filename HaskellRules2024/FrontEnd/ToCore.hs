{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans -Wno-dodgy-imports #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module FrontEnd.ToCore(
    convertToCore
  ) where

import Prelude hiding (pi)

import qualified Rules.Core as Rules
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
import Debug.Trace ( traceM )

--------------------------------------------------------
--
--             Convert to Core
--
--------------------------------------------------------

convertToCore :: Flags -> SrcCore -> IO Rules.Expr
convertToCore flags src
  = runD flags $
    do { with_exis <- addScope src
       ; return (convert with_exis) }

convert :: SrcCore -> Rules.Expr
convert (Variable i)   = Rules.Var (toCoreIdent i)
convert (Array ts)     = Rules.Arr (map convert ts)
convert (EPrim op)     = Rules.Op op
convert (Lit lit)      = Rules.Lit lit
convert (ApplyD t1 t2) = convert t1 Rules.:@: convert t2
convert (Unify t1 t2)  = convert t1 Rules.:=: convert t2
convert (Choice t1 t2) = convert t1 Rules.:|: convert t2
convert (Seq ts)       = foldr ((Rules.:>:) . convert) (convert last_t) rest_ts
                       where
                         (rest_ts, last_t) = unSeq ts

convert (Exists is t)     = foldr do_one (convert t) is
                          where
                            do_one :: Src.Ident -> Rules.Expr -> Rules.Expr
                            do_one i e = Rules.Exi (TRS.bind (toCoreIdent i) e)

convert (One t)       = Rules.One (convert t)
convert (All t)       = Rules.All (convert t)
convert (Lam i t)     = Rules.Lam (TRS.bind (toCoreIdent i) (convert t))
convert (Guard v t)   = convert v Rules.:>>: convert t
convert (Some v)      = Rules.Some (convert v)
convert (Check fxs t) = foldr add_check (convert t) fxs
                          where
                            add_check fx e = Rules.Check (toCoreEff fx) e
convert (Src.Verify is t) = Rules.Verify (TRS.bindList (map toCoreIdent is) ([], convert t))
convert Src.Fail          = Rules.Fail
convert e = impossible e

toCoreIdent :: Ident -> Rules.Ident
toCoreIdent (Ident _ s) = Rules.Name s

toCoreEff :: Eff -> Rules.Effect
toCoreEff eff
  | eff == effSucceeds = Rules.Succeeds
  | eff == effDecides  = Rules.Decides
  | otherwise          = error "toCoreEff" (prettyShow eff)


--------------------------------------------------------
--
--             Adding scopes
--
--    Replace  x:=t by   exists x. ...(x=t)...
--        "x:=t"    is represented by     DefineE x t
--
--------------------------------------------------------

addScope :: SrcCore -> D SrcCore
addScope e = scope S.empty (Block e)

scope :: S.Set Src.Ident -> SrcExpr -> D SrcExpr
-- The input expression is in BigCore, after desugaring,
-- but still with x := e stuff
-- In  (scope sc expr), `sc` is a set of identifiers already in scope
--     to allow us to complain about shadowing
scope sc = expr
  where
    -- x := e  -->   x = e
    expr (DefineV i)   = pure $ Variable i
    expr (DefineE i e) = Unify (Variable i) <$> expr e

    expr e@Src.Lit{} = pure e
    expr e@EPrim{}   = pure e
    expr e@(Variable i) | i `S.member` sc = pure e
                        | otherwise = do errUndefined [i]; pure e
    expr (Array es) = Array <$> mapM expr es
    expr (Seq es) = seqE <$> mapM expr es
    expr (ApplyD e1 e2) = ApplyD <$> expr e1 <*> expr e2

    -- Desugar (if e1 then e2 else e3) --> one{ (e1; \_.e2) | (\_.e3) }[]
    -- The key point is that existentials bound in e1 scope over e2
    expr (If3 e1 e2 e3) = expr (eForce (One (Choice (seqE [e1, eThunk e2])
                                                    (eThunk e3))))

    expr (For2 e1 e2) = do
      (is, e1', sc') <- defs' sc e1
      For2B is e1' <$> scopeD sc' e2

    expr (Block e)   = exprD e
    expr (Let e1 e2) = do { (is, e1'', sc') <- defs' sc e1
                          ; e2' <- scope sc' e2
                          ; pure $ eExists is $ seqE [e1'', e2'] }

    expr (Unify e1 e2) = Unify <$> expr e1 <*> expr e2

    expr (Choice e1 e2) = Choice <$> exprD e1 <*> exprD e2
    expr Src.Fail       = pure Src.Fail

    expr (Src.Check fx e) = Src.Check fx <$> exprD e
    expr (Src.Some e)     = Src.Some <$> expr e
    expr (Src.Guard v e)  = Src.Guard <$> expr v <*> expr e

    expr (OfType e1 eff e2) = OfType <$> exprD e1 <*> pure eff <*> exprD e2

    expr (Exists _ e)      = expr e
    expr (Src.Lam i e)     = Src.Lam i <$> scopeD (S.insert i sc) e
    expr (Src.Verify is e) = Src.Verify is <$> scopeD sc' e
      where sc' = foldr S.insert sc is
    expr (Src.One e) = Src.One <$> exprD e
    expr (Src.All e) = Src.All <$> exprD e
    expr e = impossible e

    -- exprD for a new scope context, using current in-scope set
    exprD e = fst <$> defs sc e

    -- exprD for a new scope context, with an extended in-scope set
    scopeD sc' e = fst <$> defs sc' e

    defs :: S.Set Ident -> SrcExpr -> D (SrcExpr, S.Set Ident)
    -- `e` starts a new scoping context.  Wrap it in an `Exists`
    defs sc' e = do { (is, e', s) <- defs' sc' e
                   ; pure (eExists is e', s) }

    defs' :: S.Set Ident -> SrcExpr -> D ([Ident], SrcExpr, S.Set Ident)
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

      e' <- scope sc' e
      errMultiple errM
      errShadow errS
      pure (is, e', sc')


errShadow :: [(Ident, Ident)] -> D ()
errShadow is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      (i@(Ident li _), (Ident lj _)) : _ -> errorMessage $ "shadowing: " ++ prettyShow (li, i, lj)
   else
    mapM_ (\ (i@(Ident li _), (Ident lj _)) -> traceM $ "warning shadowing " ++ prettyShow (li, i, lj)) is

errMultiple :: [[Ident]] -> D ()
errMultiple =
  mapM_ (\ is -> errorMessage $ "multiply defined: " ++ prettyShow (head is) ++
                         prettyShow [ l | Ident l _ <- is ])

errUndefined :: [Ident] -> D ()
errUndefined is = do
  no_warn <- getDFlagsX fNoWarn
  if no_warn then
    case is of
      [] -> pure ()
      i@(Ident l _) : _ -> errorMessage $ "undefined: " ++ prettyShow (l, i)
   else
    mapM_ (\ i@(Ident l _) -> traceM $ "scopeCheck: warning undefined " ++ prettyShow (l, i)) is

