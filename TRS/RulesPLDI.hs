{-# OPTIONS_GHC -Wno-unused-matches -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
module RulesPLDI(rulesPLDI, dsFreshFP, finalSubst, canon) where

import TRS
import Bind
import TRSCore
import Control.Monad( guard )
import Data.List --( sort, find, union, (\\), delete, intersect )
import Data.Maybe
--import Data.Functor.Classes (Show1(liftShowList))
--import Debug.Trace

-- Use ue in unification rules
-- #define USE_UE 1
-- Use the DEREF-K
-- #define USE_DEREF_K 1
-- Use the correct definition of WF
#define USE_CORRECT_WF 1
-- Use ELIM-DEF-DEAD, a weak substitute for structural rules
#define USE_ELIM_DEF_DEAD NO_STRUCT_RULES

implies :: Bool -> Bool -> Bool
b1 `implies` b2 = b1 <= b2

update :: Int -> a -> [a] -> [a]
update _ _ [] = undefined
update 0 v (_:vs) = v:vs
update i v (v':vs) = v' : update (i-1) v vs

--------------------------------------------------------------------------------
-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val v)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (DEF _ e) = isChoiceFree e  -- NOTE: new
isChoiceFree (HNF (Op op) :@: _) = isChoiceFreeOp op  -- NOTE: not in POPL submission
--isChoiceFree (Split _ (VLAM _ f) (VLAM _ (LAM _ g))) = isChoiceFree f && isChoiceFree g
isChoiceFree (Split _ _ _) = True
isChoiceFree Wrong     = True
isChoiceFree _         = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False -- May generate choices from the embedded functions.
isChoiceFreeOp _ = True

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

-- scope contexts

-- choice contexts

choiceX, choiceX1 :: Expr -> [(Context, Expr)]
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
scopeX :: Expr -> [(Context, Expr)]
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

--------------------------------------------------------------------------------

type ERule = Rule Expr

--------------------------------------------------------------------------------

rulesPLDI :: ERule
rulesPLDI =
     rulesPrimOps                      -- standard POPL rules
  <> rulesUnificationFP
  <> rulesApplication
  <> rulesGarbageCollection
  <> rulesFailFP
  <> rulesSpeculation
  <> rulesNormalization

rulesUnificationFP :: ERule
rulesUnificationFP =
     rulesDerefFP
  <> rulesUnificationNoOcc

rulesSpeculation :: ERule
rulesSpeculation =
     rulesChoice
  <> rulesOneFP
  <> rulesAllFP
  <> rulesSplit

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps _ lhs =
  "APP-ADD" `name`
  do ADD :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1+k2))
 ++
  "APP-SUB" `name`
  do SUB :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1-k2))
 ++
  "APP-MUL" `name`
  do MUL :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1*k2))
 ++
  "APP-DIV" `name`
  do DIV :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k2 /= 0
       then pure (INT (k1 `div` k2))
       else pure Fail
 ++
  "APP-NEG" `name`
  do NEG :@: VINT k <- [lhs]
     pure (INT k)
 ++
  "APP-PLUS" `name`
  do PLUS :@: VINT k <- [lhs]
     pure (INT k)
 ++
  "APP-GT" `name`
  do GRT :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 > k2
       then pure (INT k1)
       else pure Fail
 ++
  "APP-GE" `name`
  do GRE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 >= k2
       then pure (INT k1)
       else pure Fail
 ++
  "APP-LT" `name`
  do LST :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 < k2
       then pure (INT k1)
       else pure Fail
 ++
  "APP-LE" `name`
  do LSE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 <= k2
       then pure (INT k1)
       else pure Fail
 ++
  "APP-NE" `name`
  do NEQ :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 /= k2
       then pure (INT k1)
       else pure Fail
 ++
  "APP-ISINT" `name`
  do IsINT :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure (ARR [])
       _     -> pure Fail
 ++
  "APP-MAPAP" `name`
  do MAPAP :@: VARR vs <- [lhs]
     pure (mapAp vs)
 ++
  "APP-CONS" `name`
  do CONS :@: VARR [v, VARR vs] <- [lhs]
     pure (ARR (v:vs))

-- Turn array{f1, ... fn} into array{f1(), ... fn()}
mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> VAR x :=: (v :@: unit)) xs vs ++ [ARR $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr (\ x e -> DEF x e) e vs

unit :: Value
unit = VARR []

seqs :: [Expr] -> Expr
seqs = foldr1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do VLAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = DEF y ((VAR y :=: Val v) :>: b)
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
  do VARR [] :@: _ <- [lhs]
     pure Fail
 ++
  "APP-TUP" `name`
  do VARR vs@(_:_) :@: v <- [lhs]
     let x = identNotIn (free lhs)
         xe = VAR x
         e = foldr1 (:|:) [ (xe :=: INT i) :>: Val vi | (i, vi) <- [0..] `zip` vs ]
     pure (DEF x ((xe :=: Val v) :>: e))

--------------------------------------------------------------------------------

-- There 4 kinds of values: k, op, tuple, lambda
rulesUnificationNoOcc :: ERule
rulesUnificationNoOcc _ lhs =
--
-- Equal values
-- x=x, k=k
  "U-SCALAR" `name`
#if USE_UE
  do (SCL s1 :=: SCL s2) :>: e <- [lhs]
     guard (s1 == s2)
     pure e
 ++
-- tuple=tuple
  "U-TUP" `name`
  do (ARR ss :=: ARR ss') :>: e <- [lhs]
     guard (length ss == length ss')
     pure (foldr (:>:) e [ Val s :=: Val s' | (s,s') <- ss `zip` ss' ])
#else
  do v@(SCL s1) :=: SCL s2 <- [lhs]
     guard (s1 == s2)
     pure v
 ++
-- tuple=tuple
  "U-TUP" `name`
  do v@(ARR ss) :=: ARR ss' <- [lhs]
     guard (length ss == length ss')
     pure (foldr (:>:) v [ Val s :=: Val s' | (s,s') <- ss `zip` ss' ])
#endif
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
  do EHNF h1 :=: EHNF h2 <- [lhs]
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
  do (sx, e) <- scopeX lhs
     Fail :|: e <- [e]
     pure (sx e)
 ++
  "CHOOSE-L" `name`
  do (sx, e) <- scopeX lhs
     e :|: Fail <- [e]
     pure (sx e)

-- Put v into ctx, alpha-converting binders in ctx
-- when necessary to avoid capture.
plug :: VContext -> Value -> Expr
plug ctx v = subst [(hole,v)] (ctx (Var hole))
  where
   hole    = ident "$HOLE$"

rulesGarbageCollection :: ERule
rulesGarbageCollection _ lhs =
#if USE_ELIM_DEF_DEAD
  "ELIM-DEF-DEAD" `name`
  do e@Def{} <- [lhs]
     elimDead e
 ++
#endif
  "ELIM-DEF" `name`
  do ee@Def{} <- [lhs]
     r@(xs, _, e) <- wfResE ee
     guard (not (null xs))
     guard (null (intersect xs (free e)))
     pure e

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

{- | Application Contexts

   (paper) A ::= □ | ∃x.A | □ = h  e | □ = k;e | c;A | A e | e A | v = A ; e
              | A + e | e + A | all{A} | one{A}

   (TRS)   A ::= □ v | □ = h | □ = k | ∃x.A | A; e | e;A | A e | e A | v = A
              | A + e | e + A | all{A} | one{A}

-}

#if USE_DEREF_K
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
#endif

derefA :: Expr -> Ident -> [VContext]
derefA lhs xx =
   do (Var x :@: v) <- [lhs]
      guard (x == xx)
      pure (:@: v)
   ++
   do (VAR x :=: v@(Val HNF{})) <- [lhs]
      guard (x == xx)
      pure ( (:=: v) . Val)
   ++
   do (v@Val{} :=: e) <- [lhs]
      ctx <- derefA e xx
      pure ( (v :=:) . ctx)
   ++
   do DEF x e <- [lhs]
      guard (x /= xx)
      ctx <- derefA e xx
      pure (Def . Bind x . ctx)
   ++
   do (e1 :>: e2) <- [lhs]
      ctx <- derefA e1 xx
      pure ((:>: e2) . ctx)
   ++
   do (e1 :>: e2) <- [lhs]
      ctx <- derefA e2 xx
      pure ((e1 :>:) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx <- derefA e1 xx
      pure ((:|: e2) . ctx)
   ++
   do (e1 :|: e2) <- [lhs]
      ctx <- derefA e2 xx
      pure ((e1 :|:) . ctx)
   ++
   do (One e) <- [lhs]
      ctx <- derefA e xx
      pure (One . ctx)
   ++
   do (All e) <- [lhs]
      ctx <- derefA e xx
      pure (All . ctx)
   -- NOTE: not in paper
   ++
   do v@VOP{} :@: Var x <- [lhs]
      guard (x == xx)
      pure (v :@:)
   ++
   do VOP Cons :@: VARR [v, Var x] <- [lhs]
      guard (x == xx)
      pure (\ a -> VOP Cons :@: VARR [v, a])
   ++
   do Split e f g <- [lhs]
      ctx <- derefA e xx
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
  do VLAM v e  <- [lhs]
     guard (v /= xx)
     ctx <- derefE e xx
     pure (VLAM v . ctx)
  ++
  do VARR vs <- [lhs]
     (i, v) <- zip [0..] vs
     ctx <- derefV v xx
     pure ((\v -> VARR (update i v vs)) . ctx)

{-
  x = s; E [x] ===> x = s; E[s]
-}

isVal :: Expr -> Bool
isVal (Val _) = True
isVal _       = False

dsFreshFP :: Expr -> Expr
dsFreshFP = ds'
  where
    ds (Val v)      = Val (dsv v)
    ds (ex :=: ex') = dsEqu ex ex'
    ds (ex :>: ex') = ds ex :>: ds' ex'
    ds (ex :|: ex') = ds' ex :|: ds' ex'
    ds (vx :@: vx') = dsv vx :@: dsv vx'
    ds (DEF x e)    = DEF x (ds' e)
    ds (One ex)     = One (ds' ex)
    ds (All ex)     = All (ds' ex)
    ds (Split e f g)= Split (ds' e) (dsv f) (dsv g)
    ds e = e

    -- Make sure :=: not last
    ds' e =
      case ds e of
        c@(v :=: _) -> c :>: v
        e' -> e'

    dsEqu e1 e2
      | isVal e1  = e1 :=: e2'
--x      | isVal e2  = e2 :=: e1'
      | otherwise = DEF x ((VAR x :=: e1' :>: VAR x) :>: (VAR x :=: e2' :>: VAR x))
      where
        x   = identNotIn (free [e1', e2'])
        e1' = ds' e1
        e2' = ds' e2

    dsv (VLAM x e) = VLAM x (ds' e)
    dsv v = v

-- Make all substitutions that involve variable free arrays.
finalSubst :: Expr -> Expr
finalSubst ee | [(_, cs, vv)] <- wfRes ee = Val $ inline cs vv
              | otherwise = --(if ee /= Fail then trace ("finalSubst: not WF\n" ++ show ee) else id)
                            ee
  where
    inline :: [(Ident, Value)] -> Value -> Value
    inline bs v | isGnd v = v
                | otherwise = inline bs (inl v)
      where
        inl (Var x) | Just v@VHNF{} <- lookup x bs = v  -- Only inline arrays, scalars should not happen
                    | otherwise = error $ "finalSubst: not an array " ++ show (ee, x, lookup x bs)
        inl e@VINT{} = e
        inl e@VOP{} = e
        inl (VARR vs) = VARR (map inl vs)
        inl (VLAM x e) = VLAM x (VLAM (Name "_") e :@: Var (Name "[...]")) -- XXX
        inl _ = undefined
    isGnd :: Value -> Bool
    isGnd VINT{} = True
    isGnd (VARR vs) = all isGnd vs
    isGnd VOP{} = True
    isGnd VLAM{} = True
    isGnd _ = False

-- Make a WF value canonical, i.e., order the quantifiers
-- and bindings in a predictable order.
-- If the given value is not WF with the expression being a value then there are no promises.
canon :: Expr -> Expr
canon (One e) = One (canon e)
canon (All e) = All (canon e)
canon (e1 :|: e2) = canon e1 :|: canon e2
canon (e1 :>: e2) = canon e1 :>: canon e2
-- The cases above is just a vague attempt to canonicalize stuck results that are not values.
canon ee = order . head . wfResE $ ee  -- relies of wfResE returning the most eager result first
  where
    order :: ([Ident], [BindV], Expr) -> Expr
    order ([], [], r) = r
    order (is, cs, r) =
      let g = [(x, free e) | (x, e) <- cs ]  -- dependency graph
          -- Do a breadth first search visiting all used variables.
          bfs done [] = done
          bfs done (x : xs) | x `elem` done = bfs done xs
                            | otherwise  = bfs (x : done) (xs ++ fromMaybe [] (lookup x g))
          ys = bfs [] (free r)                                          -- Canonical variable order
          is' = filter (`elem` is) ys                                   -- Order existentials  
          cs' = [(VAR y :=: Val v) | y <- ys, Just v <- [lookup y cs] ] -- Order binders
          r' = r -- XXX WRONG canon r  -- In case it's not a value
      in  mkRes is' cs' r'

----------------------

isS :: Value -> Bool
isS VINT{} = True
isS Var{} = True
isS VOP{} = True
isS _ = False

rulesDerefFP :: ERule
rulesDerefFP _ lhs =
  "DEREF-S" `name`
  do xs@(VAR x :=: Val s) :>: e <- [lhs]
     guard (VAR x /= Val s)
     guard (isS s)
--     traceM $ "DEREF-S " ++ show (x, s, e, length (derefE e x))
     ctx <- derefE e x
     pure (xs :>: plug ctx s)
 ++
  "DEREF-H" `name`
  do xh@(VAR x :=: HVAL h) :>: e <- [lhs]
--     traceM $ "DEREF-H " ++ show (x, h, e, length (derefA e x))
     ctx <- derefA e x
     pure (xh :>: plug ctx (HNF h))
#if USE_DEREF_K
 ++
  "DEREF-K" `name`
  do xh@(VAR x :=: HVAL h) :>: e <- [lhs]
     ctx <- derefB e x
     pure (xh :>: plug ctx (HNF h))
#endif

rulesFailFP :: ERule
rulesFailFP _ lhs =
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
 ++
  -- Not needed when we have GC
  "FAIL-DEF" `name`
  do DEF _ Fail <- [lhs]
     pure Fail

rulesOneFP :: ERule
rulesOneFP _ lhs =
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

rulesAllFP :: ERule
rulesAllFP _ lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (ARR [])
 ++
  "ALL-CHOICE" `name`
  do All es <- [lhs]
     let choiceRes e | [r] <- wfRes e = [[r]]
         choiceRes (e :|: es) | [r] <- wfRes e = [ r : rs | rs <- choiceRes es ]
         choiceRes _ = []
     rs <- choiceRes es
     let (is, es, vs) = mkRess rs
     pure (mkRes is es (ARR vs))
{-
 ++
  "ALL-VAL" `name`
  do All e <- [lhs]
     (is, es, v) <- wfRes e
     pure (mkRes is es (ARR [v]))
-}

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f g <- [lhs]
     pure (f :@: VARR [])
 ++
  "SPLIT-CHOICE" `name`
  do Split (e :|: ee) f g <- [lhs]
     _ <- wfRes e
     spl g e ee
 ++
  "SPLIT-VAL" `name`
  do Split e f g <- [lhs]
     _ <- wfRes e
     spl g e Fail
 where
   spl g e ee =
     let x:y:h:_ = identsNotIn (free lhs)
         ve = VAR y :=: e
         gv = VAR h :=: (g :@: Var y)
         hlam = Var h :@: VLAM x ee
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
#if USE_CORRECT_WF
     ++ pure ([], [], e)  -- including this is the right thing, but exceedingly slow
#endif
    -- WF-EQ
    wf g e@((VAR x :=: Val v) :>: e1) = do
      guard (v /= Var x)                                -- eliminate x=x before WFF
      guard (isS v `implies` (x `notElem` free e1))     -- subst scalars before WFF
      guard (x `elem` g)
      (xs, cs, e2) <- wf (delete x g) e1
      guard (null (intersect (free v) xs))
      pure (xs, (x, v):cs, e2)
#if USE_CORRECT_WF
     ++ pure ([], [], e)  -- including this is the right thing, but exceedingly slow
#endif
    -- WF-EXP
    -- This judgement makes WF non-deterministic.
    -- With USE_CORRECT_WF we explore all possibilites,
    -- without it we eagerly consume DEF and :=:.
    wf g e =
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
            es = [VAR x :=: Val v | (x, v) <- cs]

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
  do e1 :>: (xv@(VAR x :=: Val v) :>: e2) <- [lhs]
     let valid | isS v     = case e1 of VAR _ :=: Val s | isS s -> False; _ -> True
               | otherwise = case e1 of VAR _ :=: Val _         -> False; _ -> True
--     traceM $ "NORM " ++ show (x, v, e1)
     guard valid
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
  do h@(Val HNF{}) :=: x@VAR{} <- [lhs]
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
