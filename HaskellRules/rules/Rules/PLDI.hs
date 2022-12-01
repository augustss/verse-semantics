{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.PLDI(systemPLDI, systemPLDIG, systemPLDIS) where

import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
import Control.Monad( guard )
import Data.List --( sort, find, union, (\\), delete, intersect )
import Data.Maybe
--import Data.Functor.Classes (Show1(liftShowList))
import Debug.Trace

--------------------------------------------------------------------------------

systemPLDI :: TRSystem Expr
systemPLDI = TRSystem
  { sname               = "PLDI"
  , description         = "PLDI submission (ELIM-DEF without structural)"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = check validE . anf
  , postProcess         = finalSubst
  , rules               = allRules <> rulesDerefS <> rulesGarbageCollection
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = validE
  }

systemPLDIG :: TRSystem Expr
-- PLDI without garbage collection
systemPLDIG = TRSystem
  { sname               = "PLDIG"
  , description         = "PLDI submission - ELIM-DEF"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = check validE . anf
  , postProcess         = finalSubst
  , rules               = allRules <> rulesDerefS
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = validE
  }

systemPLDIS :: TRSystem Expr
systemPLDIS = TRSystem
  { sname               = "PLDIS"
  , description         = "PLDI submission -DEREF-S + SUBST-S, SWAP-S"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = check validE . anf
  , postProcess         = finalSubst
  , rules               = allRules <> rulesS <> rulesGarbageCollection
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = validE
  }

-- Check that an expression is in the subset defined by the PLDI grammar.
validE :: Expr -> Bool
validE = expr
  where
    expr e@Val{} = value e
    expr (Lam (Bind _ e)) = expr e
    expr (_ :=: _) = False
    expr (e1 :>: e2) = expru e1 && expr e2
    expr (e1 :|: e2) = expr e1 && expr e2
    expr (e1 :@: e2) = value e1 && value e2
    expr (Def (Bind _ e)) = expr e
    expr (One e) = expr e
    expr (All e) = expr e
    expr Fail = True
    expr Wrong = True
    expr (Split e v1 v2) = expr e && value v1 && value v2
    expr _ = undefined -- GHC bug
    expru (v :=: e) = value v && expr e
    expru e = expr e
    value Var{} = True
    value e = hnf e
    hnf Int{} = True
    hnf Op{}  = True
    hnf (Arr vs) = all scalar vs
    hnf (LAM _ e) = expr e
    hnf _ = False
    scalar Var{} = True
    scalar Int{} = True
    scalar Op{} = True
    scalar _ = False

-- Make the expression obey the PLDI grammar,
-- i.e., valid (anf e) == True
anf :: Expr -> Expr
anf = expr
  where
    expr e@Var{} = e
    expr e@Int{} = e
    expr e@Op{}  = e
    expr (Arr es) =
      let (ds, a) = arr es
      in  binds ds a
    expr (Lam (Bind i e)) = Lam (Bind i (expr e))
    expr e@(_ :=: _) =
      case expru e of
        -- Bare unifications not allowed as an expression
        eu@(v :=: _) -> eu :>: v
        eu -> eu
    expr (e1 :>: e2) = expru e1 :>: expr e2
    expr (e1 :|: e2) = expr e1 :|: expr e2
    expr (e1 :@: e2) =
      let i1:i2:_ = identsNotIn (free (e1 :@: e2))
          (ds1, v1) = value i1 e1
          (ds2, v2) = value i2 e2
          ds = ds1 ++ ds2
      in  binds ds (v1 :@: v2)
    expr (Def (Bind i e)) = Def (Bind i (expr e))
    expr (One e) = One $ expr e
    expr (All e) = All $ expr e
    expr e@Fail = e
    expr e@Wrong = e
    expr (Split e e1 e2) =
      let i1:i2:_ = identsNotIn (free (Split e e1 e2))
          (ds1, v1) = value i1 e1
          (ds2, v2) = value i2 e2
          ds = ds1 ++ ds2
      in  binds ds (Split (expr e) v1 v2)

    -- Expression or unification
    expru (e1 :=: e2) =
      case (expr e1, expr e2) of
        (e1'@Val{}, e2') -> e1' :=: e2'
        (e1', e2') -> DEF x $ (Var x :=: e1') :>: (Var x :=: e2') :>: Var x
          where x = identNotIn (free (e1',  e2'))
    expru e = expr e

    value _ e@Var{} = ([], e)
    value _ e@Int{} = ([], e)
    value _ e@Op{}  = ([], e)
    value _ (Lam (Bind x e)) = ([], Lam (Bind x (expr e)))
    value _ (Arr es) = arr es
    value i e = ([(i, expr e)], Var i)

    scalar _ e@Var{} = ([], e)
    scalar _ e@Int{} = ([], e)
    scalar _ e@Op{}  = ([], e)
    scalar i e = ([(i, expr e)], Var i)

    arr es =
      let is = identsNotIn $ free es
          (dss, vs) = unzip $ zipWith scalar is es
          ds = concat dss
      in  (ds, Arr vs)

    binds :: [(Ident, Expr)] -> Expr -> Expr
    binds [] b = b
    binds ((i,e):ds) b = DEF i $ (Var i :=: e) :>: binds ds b

--------------------------------------------------------------------------------

implies :: Bool -> Bool -> Bool
b1 `implies` b2 = b1 <= b2

update :: Int -> a -> [a] -> [a]
update _ _ [] = undefined
update 0 v (_:vs) = v:vs
update i v (v':vs) = v' : update (i-1) v vs

--------------------------------------------------------------------------------
-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (DEF _ e) = isChoiceFree e  -- NOTE: new
isChoiceFree (Op op :@: _) = isChoiceFreeOp op  -- NOTE: not in POPL submission
isChoiceFree (Split _ _ _) = True
isChoiceFree Wrong     = True
isChoiceFree _         = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False -- May generate choices from the embedded functions.
isChoiceFreeOp _ = True

--------------------------------------------------------------------------------

pattern SCL :: Expr -> Expr
pattern SCL v <- (getSCL -> Just v)
--  where SCL v = Val v

getSCL :: Expr -> Maybe Expr
getSCL v@Var{} = Just v
getSCL v@Int{} = Just v
getSCL v@Op{} = Just v
getSCL _ = Nothing

pattern HVAL :: Expr -> Expr
pattern HVAL v <- (getH -> Just v)
--  where HVAL h = h

getH :: Value -> Maybe Expr
getH v@Arr{} = Just v
getH v@Lam{} = Just v
getH _ = Nothing

--------------------------------------------------------------------------------
-- contexts

-- scope contexts

-- choice contexts

choiceX, choiceX1 :: Expr -> [(EContext, Expr)]
-- CX context
choiceX lhs = choiceX1 lhs ++ [(id,lhs)]
-- CX context, CX /= hole
choiceX1 lhs =
  do cx :=: e <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((:=: e) . ctx, hole)
 ++
  do ce :=: cx <- [lhs]
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :=:) . ctx, hole)
 ++
  do cx :>: e <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((:>: e) . ctx, hole)
 ++
  do ce :>: cx <- [lhs]
     guard (isChoiceFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :>:) . ctx, hole)
 ++
  do DEF x cx <- [lhs]
     (ctx, hole) <- choiceX cx
     pure (Def . Bind x . ctx, hole) -- hopefully this is sound!

-- scope contexts
-- SX context
scopeX :: Expr -> [(EContext, Expr)]
scopeX lhs =
  do hole :|: e <- [lhs]
     pure ((:|: e), hole)
 ++
  do e :|: hole <- [lhs]
     pure ((e :|:), hole)
 ++
  do One hole <- [lhs]
     pure (One, hole)
 ++
  do All hole <- [lhs]
     pure (All, hole)
 ++
  do Split hole f g <- [lhs]
     pure (\ e -> Split e f g, hole)

allX :: Ident -> Expr -> [(EContext, Expr)]
allX xx e = allX1 xx e ++ [(id, e)]

allX1 :: Ident -> Expr -> [(EContext, Expr)]
allX1 xx lhs =
  -- No expressions in Var, Int, Op, Arr
  do LAM x e <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- allX xx e
     pure (LAM x . ctx, hole)
 ++
  do e1 :=: e2 <- [lhs]
     (ctx, hole) <- allX xx e1
     pure ((:=: e2) . ctx, hole)
 ++
  do e1 :=: e2 <- [lhs]
     (ctx, hole) <- allX xx e2
     pure ((e1 :=:) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- allX xx e1
     pure ((:>: e2) . ctx, hole)
 ++
  do e1 :>: e2 <- [lhs]
     (ctx, hole) <- allX xx e2
     pure ((e1 :>:) . ctx, hole)
 ++
  do e1 :|: e2 <- [lhs]
     (ctx, hole) <- allX xx e1
     pure ((:|: e2) . ctx, hole)
 ++
  do e1 :|: e2 <- [lhs]
     (ctx, hole) <- allX xx e2
     pure ((e1 :|:) . ctx, hole)
 ++
  do e1 :@: e2 <- [lhs]
     (ctx, hole) <- allX xx e1
     pure ((:@: e2) . ctx, hole)
 ++
  do e1 :@: e2 <- [lhs]
     (ctx, hole) <- allX xx e2
     pure ((e1 :@:) . ctx, hole)
 ++
  do DEF x e <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- allX xx e
     pure (DEF x . ctx, hole)
 ++
  do One e <- [lhs]
     (ctx, hole) <- allX xx e
     pure (One . ctx, hole)
 ++
  do All e <- [lhs]
     (ctx, hole) <- allX xx e
     pure (All . ctx, hole)
 -- XXX Split

--------------------------------------------------------------------------------

{-
before :: Rule a -> Rule a -> Rule a
before r1 r2 s lhs =
  case r1 s lhs of
    [] -> r2 s lhs
    cs -> cs

rulesAll :: ERule
rulesAll s | tfAlias s = (rulesAlias `before` rulesPLDI') s
           | otherwise = rulesPLDI' s
-}

allRules :: ERule
allRules =
     rulesPrimOps
  <> rulesUnification
  <> rulesApplication
  <> rulesFail
  <> rulesSpeculation
  <> rulesNormalization

rulesUnification :: ERule
-- Has DEREF-H but not DREF-S
rulesUnification =
     rulesDerefH
  <> rulesUnificationNoOcc

rulesSpeculation :: ERule
rulesSpeculation =
     rulesChoice
  <> rulesOne
  <> rulesAll
  <> rulesSplit

--------------------------------------------------------------------------------

{-
rulesAlias :: ERule
rulesAlias _ lhs =
  "ALIAS-SUBST" `name`
  do DEF x e <- [lhs]
     (ctx, y) <- aliasX x e
     guard (x /= y)
     -- traceM ("alias-1 " ++ show ((x, y), ctx (VAR y), underDefs (subst [(x, Var y)]) (ctx (VAR y))))
     pure $ underDefs (subst [(x, Var y)]) (ctx (VAR y))

  ++
  "ALIAS-REFL" `name`
  do VAR x :=: VAR x' <- [lhs]
     guard (x == x')
     pure (NOTFCN :@: Var x)


aliasX :: Ident -> Expr -> [(EContext, Ident)]
aliasX x lhs =
  do DEF y e <- [lhs]
     guard (x /= y)
     (ctx, z) <- aliasX x e
     pure (DEF y . ctx, z)
  ++
  aliasX' x lhs

aliasX' :: Ident -> Expr -> [(EContext, Ident)]
aliasX' x lhs =
  do (VAR y :=: VAR z) :>: e <- [lhs]
     guard (x == y)
     pure ((:>: e), z)
  ++
  do (VAR z :=: VAR y) :>: e <- [lhs]
     guard (x == y)
     pure ((:>: e), z)
  ++
  do (e1 :>: e2) <- [lhs]
     (ctx, z) <- aliasX' x e2
     pure ((e1 :>:) . ctx, z)

underDefs :: (Expr -> Expr) -> Expr -> Expr
underDefs f (DEF x e) = DEF x (underDefs f e)
underDefs f e = f e
-}

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps _ lhs =
  "APP-ADD" `name`
  do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1+k2))
 ++
  "APP-SUB" `name`
  do Op Sub :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1-k2))
 ++
  "APP-MUL" `name`
  do Op Mul :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1*k2))
 ++
  "APP-DIV" `name`
  do Op Div :@: Arr [Int k1, Int k2] <- [lhs]
     if k2 /= 0
       then pure (Int (k1 `div` k2))
       else pure Fail
 ++
  "APP-NEG" `name`
  do Op Neg :@: Int k <- [lhs]
     pure (Int k)
 ++
  "APP-PLUS" `name`
  do Op Plus :@: Int k <- [lhs]
     pure (Int k)
 ++
  "APP-GT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 > k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-GE" `name`
  do Op Ge :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 >= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-LT" `name`
  do Op Lt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 < k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-LE" `name`
  do Op Le :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 <= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-NE" `name`
  do Op Ne :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 /= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure (Arr [])
       _     -> pure Fail
 ++
  "APP-MAPAP" `name`
  do Op MapAp :@: Arr vs <- [lhs]
     pure (mapAp vs)
 ++
  "APP-CONS" `name`
  do Op Cons :@: Arr [v, Arr vs] <- [lhs]
     pure (Arr (v:vs))

-- Turn array{f1, ... fn} into array{f1(), ... fn()}
mapAp :: [Expr] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> Var x :=: (v :@: Arr [])) xs vs ++ [Arr $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr DEF e vs

seqs :: [Expr] -> Expr
seqs = foldr1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do LAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = DEF y ((Var y :=: Val v) :>: b)
     if x `notElem` freeV then
       pure (beta x e)
      else do
       -- The x has to be renamed to avoid capture
       let freeE = free e
           x' = identNotIn (freeV ++ freeE)
           e' = subst [(x, Var x')] e
       pure (beta x' e')
 ++
  "APP-TUP0" `name`
  do Arr [] :@: _ <- [lhs]
     pure Fail
 ++
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     let x = identNotIn (free lhs)
         xe = Var x
         e = foldr1 (:|:) [ (xe :=: Int i) :>: vi | (i, vi) <- [0..] `zip` vs ]
     pure (DEF x ((xe :=: v) :>: e))

--------------------------------------------------------------------------------

-- There 4 kinds of values: k, op, tuple, lambda
rulesUnificationNoOcc :: ERule
rulesUnificationNoOcc _ lhs =
--
-- Equal values
-- x=x, k=k
  "U-SCALAR" `name`
  do (SCL s1 :=: SCL s2) :>: e <- [lhs]
     guard (s1 == s2)
     pure e
 ++
-- tuple=tuple
  "U-TUP" `name`
  do (Arr ss :=: Arr ss') :>: e <- [lhs]
     guard (length ss == length ss')
     pure (foldr (:>:) e (zipWith (:=:) ss ss'))
{-
 ++
  "U-FAIL-OP-OP" `name`
  do OP{} :=: OP{} <- [lhs]
     pure Fail
-}
 ++
  "U-FAIL-LAM-LAM" `name`
  do LAM{} :=: LAM{} <- [lhs]
     pure Fail
--
-- Unequal values
 ++
  "U-FAIL" `name`
  do HNF h1 :=: HNF h2 <- [lhs]
     -- Arrays and up here, make sure we don't flag them as unequal when they may not be.
     guard (case (h1, h2) of (Arr ss1, Arr ss2) -> length ss1 /= length ss2; _ -> True)
     guard (h1 /= h2)
     pure Fail
{-
 ++
  "U-FAIL-K-K" `name`
  do INT k1 :=: INT k2 <- [lhs]
     guard(k1 /= k2)
     pure Fail
 ++
  "U-FAIL-K-H" `name`
  do INT{} :=: HVAL{} <- [lhs]
     pure Fail
 ++
  "U-FAIL-OP-V" `name`
  do OP{} :=: EHNF{} <- [lhs]
     pure Fail
 ++
  "U-FAIL-T-K" `name`
  do ARR ss :=: INT{} <- [lhs]
     pure Fail
 ++
  "U-FAIL-T-O" `name`
  do ARR ss :=: OP{} <- [lhs]
     pure Fail
 ++
  "U-FAIL-T-T" `name`
  do ARR ss :=: ARR ss' <- [lhs]
     guard (length ss /= length ss')
     pure Fail
 ++
  "U-FAIL-T-L" `name`
  do ARR{} :=: LAM{} <- [lhs]
     pure Fail
 ++
  "U-FAIL-L-V" `name`
  do LAM{} :=: EHNF{} <- [lhs]
     pure Fail
-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))
 ++
  "CHOOSE-ASSOC" `name`
  do (sx, e) <- scopeX lhs
     (e1 :|: e2) :|: e3 <- [e]
     pure (sx (e1 :|: (e2 :|: e3)))
 ++
  "CHOOSE-R" `name`
  do (sx, fe) <- scopeX lhs
     Fail :|: e <- [fe]
     pure (sx e)
 ++
  "CHOOSE-L" `name`
  do (sx, ef) <- scopeX lhs
     e :|: Fail <- [ef]
     pure (sx e)

-- Put v into ctx, alpha-converting binders in ctx
-- when necessary to avoid capture.
--     (ctx $HOLE)  [v/$HOLE]
-- where the [v/$HOLE] is capture avoiding substitution
plug :: VContext -> Value -> Expr
plug ctx v = subst [(hole,v)] (ctx (Var hole))
  where
   hole    = ident "$HOLE$"

rulesGarbageCollection :: ERule
rulesGarbageCollection _ lhs =
{-
-- Not like the paper
  "ELIM-DEF-DEAD" `name`
  do e@Def{} <- [lhs]
     elimDead e
 ++
-}
  "ELIM-DEF" `name`
  do ee@Def{} <- [lhs]
     (xs, _, e) <- wfResE ee
     guard (not (null xs))
     guard (null (intersect xs (free e)))
     pure e

{-
-- ELIM-DEF together with the structural SWAP rules
-- is able to remove all unused bindings.
-- Without the structural rules this doesn't happen.
-- So we deal with them separately.
elimDead :: Expr -> [Expr]
elimDead ee =
  let
    getXs rs (DEF x e) = getXs (x:rs) e
    getXs rs e = (reverse rs, e)
    getBs bs ((VAR x :=: v@Val{}) :>: e) = getBs ((x, v):bs) e
    getBs bs e = (reverse bs, e)
    -- xs are the initial defined variables
    (xs, e') = getXs [] ee
    -- bs are the bindings
    (bs, e'') = getBs [] e'
  in
    simpleCst xs bs e''

existBind :: [Ident] -> [(Ident, Expr)] -> Expr -> Expr
existBind xs bs ee = mkRes xs (map (\ (x, v) -> VAR x :=: v) bs) ee

-- Remove bindings of the form 'x=e' where x occurs nowhere else.
-- Also remove unused existentials.
simpleCst :: [Ident] -> [(Ident, Expr)] -> Expr -> [Expr]
simpleCst xs bs e =
  let evs = free e
      ts = filter isTriv xs
      isTriv x | x `elem` evs = False
               | otherwise =
        case partition ((== x) . fst) bs of
          ([_], obs) -> x `notElem` (free (map snd obs) ++ map fst obs) -- single binding, no other occurrences
          _ -> False
      xs' = filter  (`notElem` ts) xs
      bs' = filter ((`notElem` ts) . fst) bs
      avs = free (bs', e)
      xs'' = filter (`elem` avs) xs'
  in  if xs'' /= xs then
        [existBind xs'' bs' e]
      else
        []
-}

{- | Application Contexts

   (paper) A ::= □ | ∃x.A | □ = h  e | □ = k;e | c;A | A e | e A | v = A ; e
              | A + e | e + A | all{A} | one{A}

   (TRS)   A ::= □ v | □ = h | □ = k | ∃x.A | A; e | e;A | A e | e A | v = A
              | A + e | e + A | all{A} | one{A}

-}

{-
derefB :: Expr -> Ident -> [VContext]
derefB lhs xx =
   do VAR x <- [lhs]
      guard (x == xx)
      pure Val
   ++
   do (e1 :>: e2) <- [lhs]
      ctx <- derefB e2 xx
      pure ((e1 :>:) . ctx)
   ++
   do DEF x e <- [lhs]
      guard (x /= xx)
      ctx <- derefB e xx
      pure (Def . Bind x . ctx)
-}

derefA :: TRSFlags -> Expr -> Ident -> [VContext]
derefA = derefA' False

-- Used in consuming positions.
-- Does the same as derefA in current rules.
derefB :: TRSFlags -> Expr -> Ident -> [VContext]
derefB = derefA' False
{-x
derefB s | tfUnifyEq s = derefA' True s
         | otherwise   = derefA' False s
-}

derefA' :: Bool -> TRSFlags -> Expr -> Ident -> [VContext]
derefA' b s lhs xx =
   do (Var x :@: v) <- [lhs]
      guard (x == xx)
      pure (:@: v)
   ++
   do (Var x :=: e) <- [lhs]
      guard (x == xx)
      guard (b || case e of HNF{} -> True; _ -> False)
      pure ( (:=: e) . Val)
   ++
   do (e :=: Var x) <- [lhs]
      guard (x == xx)
      guard b
      pure ( (e :=:) . Val)
   ++
   do (v@Val{} :=: e) <- [lhs]
      ctx <- derefA' b s e xx
      pure ( (v :=:) . ctx)
   ++
   do DEF x e <- [lhs]
      guard (x /= xx)
      ctx <- derefA' b s e xx
      pure (Def . Bind x . ctx)
   ++
   do (e1 :>: e2) <- [lhs]
      ctx <- derefA' b s e1 xx
      pure ((:>: e2) . ctx)
   ++
   do (e1 :>: e2) <- [lhs]
      ctx <- derefA' b s e2 xx
      pure ((e1 :>:) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx <- derefA' b s e1 xx
      pure ((:|: e2) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx <- derefA' b s e2 xx
      pure ((e1 :|:) . ctx)
   ++
   do (One e) <- [lhs]
      ctx <- derefB s e xx
      pure (One . ctx)
   ++
   do (All e) <- [lhs]
      ctx <- derefB s e xx
      pure (All . ctx)
   -- NOTE: not in paper
   ++
   do v@Op{} :@: Var x <- [lhs]
      guard (x == xx)
      pure (v :@:)
   ++
   do Op Cons :@: Arr [v, Var x] <- [lhs]
      guard (x == xx)
      pure (\ a -> Op Cons :@: Arr [v, a])
   ++
   do Split e f g <- [lhs]
      ctx <- derefB s e xx
      pure ((\ e' -> Split e' f g) . ctx)

{- | Expression Contexts `E` ----------------------------------------------
   V ::= □ | ⟨s1, · · · , □, · · · , sn⟩ | 𝜆x. E
   C ::= V = v | v = V | V = e | v = E
   E ::= 𝑉 | ∃x.E | C;e | c;E | E e | e E
       | E + e | e + E | all{E} | one{E}
-}
type VContext = Value -> Expr

derefE :: Expr -> Ident -> [VContext]
derefE lhs xx =
   do Val v <- [lhs]
      ctx <- derefV v xx
      pure (Val . ctx)
   ++
   do DEF x e <- [lhs]
      guard (x /= xx)
      ctx       <- derefE e xx
      pure (Def . Bind x . ctx)
   ++
   do (v :=: e) <- [lhs]
      ctx    <- derefE v xx
      pure ((:=: e) . ctx)
   ++
   do (v :=: e) <- [lhs]
      ctx    <- derefE e xx
      pure ((v :=:) . ctx)
   ++
   do (c :>: e) <- [lhs]
      ctx  <- derefC c xx
      pure ((:>: e) . ctx)
   ++
   do (c :>: e) <- [lhs]
      ctx  <- derefE e xx
      pure ((c :>:) . ctx)
   ++
   do (v1 :@: v2) <- [lhs]
      ctx    <- derefV v1 xx
      pure ((:@: v2) . ctx)
   ++
   do (v1 :@: v2) <- [lhs]
      ctx    <- derefV v2 xx
      pure ((v1 :@:) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx    <- derefE e1 xx
      pure ((:|: e2) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx    <- derefE e2 xx
      pure ((e1 :|:) . ctx)
   ++
   do (One e) <- [lhs]
      ctx <- derefE e xx
      pure (One . ctx)
   ++
   do (All e) <- [lhs]
      ctx <- derefE e xx
      pure (All . ctx)
   ++
   do Split e f g <- [lhs]
      ctx <- derefE e xx
      pure ((\ e' -> Split e' f g) . ctx)
   ++
   do Split e f g <- [lhs]
      ctx <- derefV f xx
      pure ((\ f' -> Split e f' g) . ctx)
   ++
   do Split e f g <- [lhs]
      ctx <- derefV g xx
      pure (Split e f . ctx)

-- (paper) C ::= V = v | v = V | V = e | v = E
-- (TRS)   C ::= E = e | e = E

derefC :: Expr -> Ident -> [Value -> Expr]
derefC lhs xx = derefE lhs xx

-- (paper) V ::= □ | 𝜆x. E | ⟨s1, · · · , □, · · · , sn⟩
-- (TRS)   V ::= □ |
derefV :: Value -> Ident -> [Value -> Value]
derefV lhs xx =
  do Var x <- [lhs]
     guard (x == xx)
     pure id
  ++
  do LAM v e  <- [lhs]
     guard (v /= xx)
     ctx <- derefE e xx
     pure (LAM v . ctx)
  ++
  do Arr vs <- [lhs]
     (i, v) <- zip [0..] vs
     ctx <- derefV v xx
     pure ((\ x -> Arr (update i x vs)) . ctx)

{-
  x = s; E [x] ===> x = s; E[s]
-}

-- Make all substitutions that involve variable free arrays.
finalSubst :: Expr -> Expr
finalSubst ee | [(_, cs, vv)] <- wfRes ee = Val $ inline cs vv
              | otherwise = --(if ee /= Fail then trace ("finalSubst: not WF\n" ++ show ee) else id)
                            ee
  where
    inline :: [(Ident, Value)] -> Value -> Value
    inline bs v | isGnd bs v = v
                | otherwise = inline bs (inl v)
      where
--        inl (Var x) | Just v@VHNF{} <- lookup x bs = v  -- Only inline arrays, scalars should not happen
--                    | otherwise = error $ "finalSubst: not an array " ++ show (ee, x, lookup x bs)
        inl (Var x) = fromMaybe (Var x) $ lookup x bs
        inl e@Int{} = e
        inl e@Op{} = e
        inl (Arr vs) = Arr (map inl vs)
        inl (LAM x e) = LAM x (LAM (Name "_") e :@: Var (Name "[...]")) -- XXX
        inl _ = undefined
    isGnd :: [(Ident, a)] -> Value -> Bool
    isGnd _ Int{} = True
    isGnd bs (Arr vs) = all (isGnd bs) vs
    isGnd _ Op{} = True
    isGnd _ Lam{} = True
    isGnd bs (Var x) = isNothing (lookup x bs)
    isGnd _ _ = False

----------------------

isS :: Value -> Bool
isS Int{} = True
isS Var{} = True
isS Op{} = True
isS _ = False

rulesDerefH :: ERule
rulesDerefH ss lhs =
  "DEREF-H" `name`
  do xh@(Var x :=: HVAL h) :>: e <- [lhs]
--     traceM $ "DEREF-H " ++ show (x, h, e, length (derefA e x))
     ctx <- derefA ss e x
     pure (xh :>: plug ctx h)

rulesDerefS :: ERule
rulesDerefS _ss lhs =
  "DEREF-S" `name`
  do xs@(Var x :=: SCL s) :>: e <- [lhs]
     guard (Var x /= s)
--     traceM $ "DEREF-S " ++ show (x, s, e, length (derefE e x))
     ctx <- derefE e x  -- handles necessary alpha-conversion
     pure (xs :>: plug ctx s)

{-
 ++
  "DEREF-K" `name`
  do xh@(VAR x :=: HVAL h) :>: e <- [lhs]
     ctx <- derefB e x
     pure (xh :>: plug ctx (HNF h))
-}

isVar :: Expr -> Bool
isVar Var{} = True
isVar _ = False

rulesS :: ERule
rulesS _ lhs =
  -- DEREF-K is like DEREF-S but for constants only
  "DEREF-K" `name`
  do xs@(Var x :=: SCL s) :>: e <- [lhs]
     guard (not (isVar s))
--     traceM $ "DEREF-K " ++ show (x, s, e, length (derefE e x))
     ctx <- derefE e x  -- handles necessary alpha-conversion
     pure (xs :>: plug ctx s)
 ++
  "SWAP-S" `name`
  do DEF x ex <- [lhs]
     (ctx1, DEF y ey) <- allX x ex
     guard (x /= y)
     (ctx2, Var x' :=: Var y') <- allX y ey
     guard (x == x' && y == y')
     traceM $ "SWAP-S " ++ show (x, y)
     pure (DEF x (ctx1 (DEF y (ctx2 (Var y :=: Var x)))))
 ++
  "SUBST-S" `name`
  do DEF x ex <- [lhs]
     (ctx1, DEF y ey) <- allX x ex
     guard (x /= y)
     (ctx2, (Var y' :=: Var x') :>: e) <- allX y ey
     guard (x == x' && y == y')
     ctx <- derefE e y
     traceM $ "SUBST-S " ++ show (x, y)
     pure (DEF x (ctx1 (DEF y (ctx2 ((Var y :=: Var x) :>: plug ctx (Var x))))))

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL-SEQL" `name`
  do Fail :>: _ <- [lhs]
     pure Fail
 ++
  "FAIL-SEQR" `name`
  do _ :>: Fail <- [lhs]
     pure Fail
 ++
  -- NOTE: not in paper
  "FAIL-UNIFY" `name`
  do _ :=: Fail <- [lhs]
     pure Fail
{-
 ++
  -- Not needed when we have GC
  "FAIL-DEF" `name`
  do DEF _ Fail <- [lhs]
     pure Fail
-}

rulesOne :: ERule
rulesOne _ lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (e :|: _) <- [lhs]
     _ <- wfRes e
     pure e
 ++
  "ONE-VAL" `name`
  do One e <- [lhs]
     _ <- wfRes e
     pure e

rulesAll :: ERule
rulesAll _ lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All xs <- [lhs]
     let choiceRes e | [r] <- wfRes e = [[r]]
         choiceRes (e :|: es) | [r] <- wfRes e = [ r : rs | rs <- choiceRes es ]
         choiceRes _ = []
     rs <- choiceRes xs
     let (is, es, vs) = mkRess rs
     pure (mkRes is es (Arr vs))
{-
 ++
  "ALL-VAL" `name`
  do All e <- [lhs]
     (is, es, v) <- wfRes e
     pure (mkRes is es (Arr [v]))
-}

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f _g <- [lhs]
     pure (f :@: Arr [])
 ++
  "SPLIT-CHOICE" `name`
  do Split (e :|: ee) _f g <- [lhs]
     _ <- wfRes e
     spl g e ee
 ++
  "SPLIT-VAL" `name`
  do Split e _f g <- [lhs]
     _ <- wfRes e
     spl g e Fail
 where
   spl g e ee =
     let x:y:h:_ = identsNotIn (free lhs)
         ve = Var y :=: e
         gv = Var h :=: (g :@: Var y)
         hlam = Var h :@: LAM x ee
     in  pure (DEF h (DEF y (ve :>: gv :>: hlam)))

type BindV = (Ident, Value)

wfRes :: Expr -> [([Ident], [BindV], Value)]
wfRes e = do
  --traceM ("wfRes " ++ show (e, wfResE e))
  (is, es, Val v) <- wfResE e
  pure (is, es, v)

-- Returns a non-empty list WF decompositions of the expression.
-- The most eager (i.e., most binders and bindings) is first in the list.
wfResE :: Expr -> [([Ident], [BindV], Expr)]
wfResE = wf []
  where
    -- WF-DEF
    wf g e@(DEF x e1) = do
      (xs, cs, e2) <- wf (x:g) e1
      guard (x `notElem` xs)
      pure (x:xs, cs, e2)
     ++ pure ([], [], e)  -- including this is the right thing, but exceedingly slow
    -- WF-EQ
    wf g e@((Var x :=: Val v) :>: e1) = do
      guard (v /= Var x)                                -- eliminate x=x before WFF
      guard (isS v `implies` (x `notElem` free e1))     -- subst scalars before WFF
      guard (x `elem` g)
      (xs, cs, e2) <- wf (delete x g) e1
      guard (null (intersect (free v) xs))
      pure (xs, (x, v):cs, e2)
     ++ pure ([], [], e)  -- including this is the right thing, but exceedingly slow
    -- WF-EXP
    -- This judgement makes WF non-deterministic.
    -- With USE_CORRECT_WF we explore all possibilites,
    -- without it we eagerly consume DEF and :=:.
    wf _g e =
      pure ([], [], e)

mkRes :: [Ident] -> [Expr] -> Expr -> Expr
mkRes is es r = foldr (\ i e -> DEF i e) r' is
  where r' = foldr (:>:) r es

mkRess :: [([Ident], [BindV], Value)] -> ([Ident], [Expr], [Value])
mkRess as = loop [] [] [] as
  where
    fvs = free as
    loop ris res rvs [] = (reverse ris, reverse res, reverse rvs)
    loop ris res rvs ((is, cs, v) : xs) = loop (is' ++ ris) (es' ++ res) (v' : rvs) xs
      where is' = take (length is) $ identsNotIn (ris ++ fvs)
            s = zipWith (\ i i' -> (i, Var i')) is is'
            es' = map (subst s) es
            v' = subst s v
            es = [Var x :=: Val vv | (x, vv) <- cs]

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "NORM-VAL" `name`
  do Val _ :>: e <- [lhs]
     pure e
 ++
  "NORM-SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "NORM-SEQ-SWAP" `name`
  do e1 :>: (xv@(Var _x :=: Val v) :>: e2) <- [lhs]
     let ok | SCL{} <- v = case e1 of Var _ :=: SCL{} -> False; _ -> True
            | otherwise  = case e1 of Var _ :=: Val _ -> False; _ -> True
--     traceM $ "NORM " ++ show (x, v, e1)
     guard ok
     pure (xv :>: (e1 :>: e2))
{-
 ++
  "NORM-EQ" `name`
  do e :>: c@(VAR x :=: Val{}) <- [lhs]
     pure (e :>: (c :>: VAR x))
 ++
  "NORM-DEF-EQ" `name`
  do DEF y c@(VAR x :=: Val{}) <- [lhs]
     pure (DEF y (c :>: VAR x))
-}
 ++
  "NORM-SWAP-EQ" `name`
  do h@HNF{} :=: x@Var{} <- [lhs]
     pure (x :=: h)
 ++
  "NORM-SEQ-DEFR" `name`
  do DEF x e1 :>: e2 <- [lhs]
     let (nx, ne1) =
           if x `notElem` free e2 then
             (x, e1)
           else
             let x' = identNotIn (free [e1, e2])
             in  (x', subst [(x, Var x')] e1)
     pure (DEF nx (ne1 :>: e2))
 ++
  "NORM-SEQ-DEFL" `name`
  do e1 :>: DEF x e2 <- [lhs]
     let (nx, ne2) =
           if x `notElem` free e1 then
             (x, e2)
           else
             let x' = identNotIn (free [e1, e2])
             in  (x', subst [(x, Var x')] e2)
     pure (DEF nx (e1 :>: ne2))
 ++
  "NORM-DEFR" `name`
  do (v1 :=: DEF x e2) :>: e3 <- [lhs]
     let (nx, ne2) =
           if x `notElem` free (v1, e3) then
             (x, e2)
           else
             let x' = identNotIn (free [v1, e2, e3])
             in  (x', subst [(x, Var x')] e2)
     pure (DEF nx ((v1 :=: ne2) :>: e3))
 ++
  "NORM-SEQR" `name`
  do (v@Val{} :=: (e1 :>: e2)) :>: e3 <- [lhs]
     pure (e1 :>: (v :=: e2) :>: e3)

------

rulesStructural :: ERule
rulesStructural _ lhs =
  do Def (Bind x (Def (Bind y e))) <- [lhs]
     pure ("SWAP-C", DEF y (DEF x e))
 ++
  do (Var x1 :=: Val v1) :>: ((Var x2 :=: Val v2) :>: e) <- [lhs]
     pure ("SWAP-D", (Var x2 :=: Val v2) :>: ((Var x1 :=: Val v1) :>: e))

