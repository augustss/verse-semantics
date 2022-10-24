{-# OPTIONS_GHC -Wno-unused-matches -Wno-missing-signatures -Wno-name-shadowing -Wno-orphans -Wno-type-defaults -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module RulesPLDI where

import TRS
import Bind
import TRSCore
import Control.Monad( guard )
import Data.List --( sort, find, union, (\\), delete, intersect )
import Data.Maybe( maybeToList )
--import Data.Functor.Classes (Show1(liftShowList))
import Debug.Trace

anySame :: (Eq a) => [a] -> Bool
anySame [] = False
anySame [_] = False
anySame (x:xs) = x `elem` xs || anySame xs

--------------------------------------------------------------------------------
-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val v)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Def (Bind _ e)) = isChoiceFree e  -- NOTE: new
isChoiceFree (HNF (Op op) :@: _) = isChoiceFreeOp op  -- NOTE: not in POPL submission
isChoiceFree (Split _ (VLAM _ f) (VLAM _ (LAM _ g))) = isChoiceFree f && isChoiceFree g
isChoiceFree Wrong     = True
isChoiceFree _         = False
-- KC: what about @?

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp _ = True

isVar :: Expr -> Bool
isVar (VAR _) = True
isVar _       = False

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

-- scope contexts

-- X ::= * | X = e | e = X | X;e | e;X

execX, execX1 :: Expr -> [(Context, Expr)]
-- X context
execX lhs = execX1 lhs ++ [(id,lhs)]
-- X context, X /= hole
execX1 lhs =
  do x :=: e <- [lhs]
     (ctx, hole) <- execX x
     pure ((:=: e) . ctx, hole)
 ++
  do e :=: x <- [lhs]
     (ctx, hole) <- execX x
     pure ((e :=:) . ctx, hole)
 ++
  do x :>: e <- [lhs]
     (ctx, hole) <- execX x
     pure ((:>: e) . ctx, hole)
 ++
  do e :>: x <- [lhs]
     (ctx, hole) <- execX x
     pure ((e :>:) . ctx, hole)

-- X context, or exist x . X
defX :: Expr -> [(Context, Expr)]
defX lhs =
  do execX lhs
 ++
  do Def (Bind x dx) <- [lhs]
     (ctx, hole) <- defX dx
     return (Def . Bind x . ctx, hole)

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
  do Def (Bind x cx) <- [lhs]
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

-- value contexts
-- V context
valueX, valueX1 :: Value -> [(Value->Value, Value)]
valueX lhs = valueX1 lhs ++ [(id, lhs)]

valueX1 lhs =
  do VARR vs <- [lhs]
     i <- [0..length vs-1]
     pure (\v -> VARR (take i vs ++ [v] ++ drop (i+1) vs), vs!!i)

--------------------------------------------------------------------------------

type ERule = Rule Expr

rulesPOPL :: ERule
rulesPOPL = rulesPrimOps
        +++ rulesApplication
        +++ rulesUnification
        +++ rulesUnificationVariables
        +++ rulesSequencing
        +++ rulesChoice
        +++ rulesOne
        +++ rulesAll
        +++ rulesFail
        +++ rulesSplit

--------------------------------------------------------------------------------

{-
rulesFRESH :: ERule
rulesFRESH = rulesPrimOps
         +++ rulesApplication
         +++ rulesUnificationNoOcc
         +++ rulesSubstitution
         +++ rulesStructural
         +++ rulesSequencing
         +++ rulesGarbageCollection
         +++ rulesChoice
         +++ rulesOne
         +++ rulesAll
         +++ rulesFail
         +++ rulesSplit
-}

--------------------------------------------------------------------------------

rulesFRESH_POPL :: ERule
rulesFRESH_POPL =
      rulesPrimOps                      -- standard POPL rules
  +++ rulesUnificationFP
  +++ rulesApplication
  +++ rulesGarbageCollection
  +++ rulesFailFP
  +++ rulesSpeculation
  +++ rulesNormalization

rulesUnificationFP :: ERule
rulesUnificationFP =
      rulesDerefFP
  +++ rulesUnificationNoOcc

rulesSpeculation :: ERule
rulesSpeculation =
      rulesChoice
  +++ rulesOneFP
  +++ rulesAllFP

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps lhs =
  "P-ADD" `name`
  do ADD :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1+k2))
 ++
  "P-SUB" `name`
  do SUB :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1-k2))
 ++
  "P-MUL" `name`
  do MUL :@: VARR [VINT k1, VINT k2] <- [lhs]
     pure (INT (k1*k2))
 ++
  "P-DIV" `name`
  do DIV :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k2 /= 0
       then pure (INT (k1 `div` k2))
       else pure Fail
 ++
  "P-NEG" `name`
  do NEG :@: VINT k <- [lhs]
     pure (INT k)
 ++
  "P-PLUS" `name`
  do PLUS :@: VINT k <- [lhs]
     pure (INT k)
 ++
  "P-GRT" `name`
  do GRT :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 > k2
       then pure (INT k1)
       else pure Fail
 ++
  "P-GRE" `name`
  do GRE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 >= k2
       then pure (INT k1)
       else pure Fail
 ++
  "P-LST" `name`
  do LST :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 < k2
       then pure (INT k1)
       else pure Fail
 ++
  "P-LSE" `name`
  do LSE :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 <= k2
       then pure (INT k1)
       else pure Fail
 ++
  "P-NEQ" `name`
  do NEQ :@: VARR [VINT k1, VINT k2] <- [lhs]
     if k1 /= k2
       then pure (INT k1)
       else pure Fail
 ++
  "P-IsINT" `name`
  do IsINT :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure (ARR [])
       _     -> pure Fail
 ++
  "P-MAPAP" `name`
  do MAPAP :@: VARR vs <- [lhs]
     pure (mapAp vs)
 ++
  "P-CONS" `name`
  do CONS :@: VARR [v, VARR vs] <- [lhs]
     pure (ARR (v:vs))

-- Turn array{f1, ... fn} into array{f1(), ... fn()}
mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> VAR x :=: (v :@: unit)) xs vs ++ [ARR $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr (\ x e -> Def (Bind x e)) e vs

unit :: Value
unit = VARR []

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication lhs =
  "APP-BETA" `name`
  do VLAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = Def (Bind y ((VAR y :=: Val v) :>: b))
     if x `notElem` freeV then
       pure (beta x e)
      else do
       -- The x has to be renamed to avoid capture
       let freeE = free e
           x' = identNotIn (freeV ++ freeE)
           e' = subst [(x, Var x')] e
       pure (beta x' e')
 ++
  "APP-TUP" `name`
  do VARR vs :@: v <- [lhs]
     pure (foldr (:|:) Fail [ (Val v :=: INT i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])

--------------------------------------------------------------------------------
rulesUnification :: ERule
rulesUnification = rulesUnificationNoOcc
               +++ rulesUnificationOcc

rulesUnificationNoOcc :: ERule
rulesUnificationNoOcc lhs =
  "ULIT" `name`
  do INT k1 :=: INT k2 <- [lhs]
     if k1 == k2
       then pure (INT k1)
       else pure Fail
 ++
  "UTUP" `name`
  do ARR vs :=: ARR vs' <- [lhs]
     if length vs == length vs'
       then pure (foldr (:>:) (ARR vs) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
       else pure Fail
 ++
  "UX1" `name`
  do INT k :=: ARR vs <- [lhs]
     pure Fail
 ++
  "UX2" `name`
  do ARR vs :=: INT k <- [lhs]
     pure Fail
 ++
  "UX3" `name`
  do Val (VLAM _ _) :=: Val (HNF _) <- [lhs]
     pure Fail
 ++
  "UX4" `name`
  do Val (HNF _) :=: Val (VLAM _ _) <- [lhs]
     pure Fail
 ++
  "UX5" `name`
  do Val (HNF (Op _)) :=: Val (HNF _) <- [lhs]
     pure Fail
 ++
  "UX6" `name`
  do Val (HNF _) :=: Val (HNF (Op _)) <- [lhs]
     pure Fail

rulesUnificationOcc :: ERule
rulesUnificationOcc lhs =
   "UX-OCCURS" `name`
   do VAR x :=: Val v <- [lhs]
      (_, Var x') <- valueX1 v
      guard (x == x')
      pure Fail

--------------------------------------------------------------------------------

rulesUnificationVariables :: ERule
rulesUnificationVariables lhs =
  "SUBST" `name`
  do (ctx, VAR x :=: Val v) <- execX lhs
     let freeX = free (ctx blob)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx (VAR x0 :=: Val v)))
 ++
  "SUBST-REC" `name`
  do VAR x :=: Val v <- [lhs]
     (ctx, VLAM y e) <- valueX v
     guard (x `elem` free e)
     pure (VAR x :=: Val (ctx (VLAM y (Def (Bind x (lhs :>: e))))))
 ++
  "DEF-ELIML" `name`
  do Def (Bind x a) <- [lhs]
     (ctx, VAR x' :=: Val v) <- defX a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  "DEF-ELIMR" `name`
  do Def (Bind x a) <- [lhs]
     (ctx, Val v :=: VAR x') <- defX a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  "SWAP" `name`
  do Val (HNF hnf) :=: VAR x <- [lhs]
     pure (VAR x :=: Val (HNF hnf))
 ++
  "DEF-FLOAT" `name`
  do (ctx, Def (Bind x e)) <- execX1 lhs
     let freeX = free (ctx blob)
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (Def (Bind x' (ctx (subst [(x,Var x')] e))))
       else pure (Def (Bind x (ctx e)))
 where
  blob = Fail -- just something to plug the hole in the context so we can look at it

--------------------------------------------------------------------------------

rulesSequencing :: ERule
rulesSequencing lhs =
  "SEQ" `name`
  do Val v :>: e <- [lhs]
     pure e
 ++
  "UNIFY-SEQL" `name`
  do (e1 :>: e2) :=: e3 <- [lhs]
     pure (e1 :>: (e2 :=: e3))
 ++
  "UNIFY-SEQR" `name`
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (Val v :=: e2))
 ++
  "UNIFY-UNIFYR" `name`
  do (e1 :=: e2) :=: e3 <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((VAR x :=: e1) :>: (VAR x :=: e2) :>: (VAR x :=: e3))))
 ++
  "UNIFY-UNIFYR" `name`
  do e1 :=: (e2 :=: e3) <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Def (Bind x ((VAR x :=: e1) :>: (VAR x :=: e2) :>: (VAR x :=: e3) :>: VAR x)))
{-
  -- for FRESH
 ++ "CONJ-CST-DEFR" `name` -- e1 = (ex y. e2) --> ex y. e1 = e2
  do (e1 :=: Def (Bind y e2)) <- [lhs]
     let y' = identNotIn (free e2 ++ free e2)
     if y `elem` free e1
       then pure (Def (Bind y' (e1 :=: subst [(y,Var y')] e2)))
       else pure (Def (Bind y (e1 :=: e2)))
-- ++ "CONJ-SEQ-ASSOC" `name`
--  do (e1 :>: e2) :>: e3 <- [lhs]
--     pure (e1 :>: (e2 :>: e3))
-}

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail lhs =
  "FAIL-DEF" `name`
  do Def (Bind x Fail) <- [lhs]
     pure Fail
 ++
  "FAIL" `name`
  do (cx, Fail) <- execX1 lhs
     pure Fail

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice lhs =
  "FAIL-L" `name`
  do (sx, e) <- scopeX lhs
     Fail :|: e <- [e]
     pure (sx e)
 ++
  "FAIL-R" `name`
  do (sx, e) <- scopeX lhs
     e :|: Fail <- [e]
     pure (sx e)
 ++
  "CHOOSE-ASSOC" `name`
  do (sx, e) <- scopeX lhs
     (e1 :|: e2) :|: e3 <- [e]
     pure (sx (e1 :|: (e2 :|: e3)))
 ++
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))

--------------------------------------------------------------------------------

rulesOne :: ERule
rulesOne lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (Val v :|: e) <- [lhs]
     pure (Val v)
 ++
  "ONE-VAL" `name`
  do One (Val v) <- [lhs]
     pure (Val v)

rulesAll :: ERule
rulesAll lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (ARR [])
 ++
  "ALL-CHOICE" `name`
  do All es@(_ :|: _) <- [lhs]
     let choiceVals (Val v) = [[v]]
         choiceVals (Val v :|: es) = [ v : vs | vs <- choiceVals es ]
         choiceVals _ = []
     vs <- choiceVals es
     pure (ARR vs)
 ++
  "ALL-VAL" `name`
  do All (Val v) <- [lhs]
     pure (ARR [v])

rulesSplit :: ERule
rulesSplit lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f g <- [lhs]
     pure (f :@: VARR [])
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = VAR h :=: (g :@: v)
         hlam = Var h :@: VLAM x e
     pure (Def (Bind h (gv :>: hlam)))
 ++
  "SPLIT-VAL" `name`
  do Split (Val v) f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = VAR h :=: (g :@: v)
         hlam = Var h :@: VLAM x Fail
     pure (Def (Bind h (gv :>: hlam)))

--------------------------------------------------------------------------------

(=~=) :: Expr -> Expr -> Bool
a =~= b = norm a == norm b

norm :: Expr -> Expr
norm (Val (Var x) :=: Val (Var y)) | y > x = Val (Var y) :=: Val (Var x)
norm (a :>: b) = norm a :>: norm b
norm (a :=: b) = norm a :=: norm b
norm (a :|: b) = norm a :|: norm b
norm a@(Def _) = normDef [] a
norm (One a)   = One (norm a)
norm (All a)   = All (norm a)
norm e         = e

normDef :: [Ident] -> Expr -> Expr
normDef xs (Def (Bind x a)) = normDef (x:xs) a
normDef xs a                = defs (sort ys) (norm (subst sub a))
 where
  vs  = filter (`elem` xs) (free a)
  ys  = [ ident ("x" ++ show (index x vs)) | x <- xs ]
  sub = [ (x, Var y) | (x,y) <- xs `zip` ys ]

  index x zs = head $ [ i | (z,i) <- zs `zip` [1..], z == x ] ++ [0]

  defs []     a = a
  defs (x:xs) a = Def (Bind x (defs xs a))


--------------------------------------------------------------------------------
-- Fresh Rules
--------------------------------------------------------------------------------

{-
rulesStructural :: ERule
rulesStructural lhs =
  "SWAP-E-1" `name`
  do (VAR y :=: VAR x) <- [lhs]
     guard (x < y)
     pure (VAR x :=: VAR y)
  ++
  "SWAP-E-2" `name`
  do (HVAL h :=: VAR x) <- [lhs]
     pure (VAR x :=: HVAL h)
  ++
  "SWAP-C"  `name`
  do (e1 :>: ((VAR x :=: HVAL v) :>: e3)) <- [lhs]
     guard (not (valueConstraint e1))
     pure ((VAR x :=: HVAL v) :>: (e1 :>: e3))
  ++
  "CONJ-SEMI" `name`
  do ((e1 :>: e2) :>: e3) <- [lhs]
     pure (e2 :>: (e1 :>: e3))

valueConstraint :: Expr -> Bool
valueConstraint (VAR _ :=: HVAL _) = True
valueConstraint _                  = False

rulesSubstitution :: ERule
rulesSubstitution lhs =
 "DEREF-S-K" `name`
  do (VAR x :=: INT k :>: e) <- [lhs]
     eCtx <- derefE e x
     pure ((VAR x :=: INT k) :>: plug eCtx (VINT k))
  ++
  "DEREF-S-V" `name`
  do (VAR x :=: VAR y :>: e) <- [lhs]
     eCtx <- derefE e x
     guard (x < y)
     pure ((VAR x :=: VAR y) :>: plug eCtx (Var y))
  ++
  "DEREF-H" `name`
  do (VAR x :=: HVAL h :>: e) <- [lhs]
     aCtx <- derefA e x
     pure ((VAR x :=: HVAL h) :>: plug aCtx (HNF h))
-}

plug :: VContext -> Value -> Expr
plug ctx v = subst [(hole,v)] (ctx (Var hole))
  where
   hole    = ident "$HOLE$"

rulesGarbageCollection :: ERule
rulesGarbageCollection lhs =
   "ELIM-DEF" `name`
   do Def (Bind x e) <- [lhs]
      guard (x `notElem` free e)
      pure e
   ++
   "ELIM-CST" `name`
   do Def _ <- [lhs]
--      traceM $ show (lhs, elimCst lhs)
      elimCst lhs

-- This is exactly as in the paper, it is wrong.
-- It does not allow the bindings to be a permutation of the defs.
elimCst_1 :: Expr -> [Expr]
elimCst_1 e =
  let
    getXs rs (Def (Bind x e)) = getXs (x:rs) e
    getXs rs e = (reverse rs, e)
    (xs, e') = getXs [] e
    vs = free e'
    checkEs [] r = [r]
    checkEs (x:xs) ((VAR x' :=: HVAL _) :>: r) | x == x', x `notElem` vs = checkEs xs r
    checkEs _ _ = []
  in
    checkEs xs e'

-- As in the paper, but
--  * allow permutations
--  * eliminate v (hot h)
-- Still wrong, since it is too picky
elimCst_2 :: Expr -> [Expr]
elimCst_2 e =
  let
    getXs rs (Def (Bind x e)) = getXs (x:rs) e
    getXs rs e = (rs, e)
    (xs, e') = getXs [] e
    checkEs [] r | null (intersect xs (free r)) = [r]
    checkEs is ((VAR x :=: Val _) :>: r) |
      x `elem` is = checkEs (delete x is) r
    checkEs _ _ = []
  in
--    trace ("elimCst: " ++ show (e, xs, e')) $
    checkEs xs e'

-- In a "def x1,x2,... in y1=e1; ...; e"
-- find all variables y_i where y_i is in xs,
-- and y_i is not reachable from e.
-- The y_i must also be unique.
elimCst :: Expr -> [Expr]
elimCst ee =
  let
    getXs rs (Def (Bind x e)) = getXs (x:rs) e
    getXs rs e = (reverse rs, e)
    getBs bs ((VAR x :=: v@Val{}) :>: e) = getBs ((x, v):bs) e
    getBs bs e = (reverse bs, e)
    -- xs are the initial defined variables
    (xs, e') = getXs [] ee
    -- bs are the bindings
    (bs, e'') = getBs [] e'
    -- deps is the dependency graph from the bindings
    deps = [(x, free e) | (x, e) <- bs]
    -- unr are the identifiers not reachable from e''
    unr = dfs deps (free e'')
    dfs g [] = map fst g   -- anything remaining is unreachable
    dfs g (i:is) =
      let vs = [ v | (i', vs) <- g, i == i', v <- vs ]
          g' = filter ((/= i) . fst) g
      in  dfs g' (vs ++ is) -- continue with more reachable
    -- del are the variables to delete, must be unreachable and defined here
    del = intersect xs unr
    xs' = filter (`notElem` del) xs
    bs' = filter ((`notElem` del) . fst) bs
    res = foldr (\ x e -> Def (Bind x e)) b xs'
      where b = foldr (\ (x, v) e -> (VAR x :=: v) :>: e) e'' bs'
  in
--    if length xs == 6 && not (null del) then error (show (xs, unr, free e'', e'', deps)) else
    if null del || anySame del then
      []
    else
      [res]

{-
      (xs, cs, e) <- gcDefs lhs
      let (n, e') = gc xs cs e
      guard (n < length xs)
      pure e'

gcDefs :: Expr -> [([Ident], [(Ident, HNF)], Expr)]
gcDefs e = do
   do let (xs, e')  = goD e
      let (cs, e'') = goC e'
      guard (not (null xs))
      guard (not (null cs))
      pure (xs, cs, e'')
   where
      goD (Def (Bind x e)) = (x:xs, e') where (xs, e') = goD e
      goD e                = ([], e)
      goC ((VAR x :=: HVAL h) :>: e) = ((x,h) : cs, e') where (cs, e') = goC e
      goC e                          = ([], e)

gc :: [Ident] -> [(Ident, HNF)] -> Expr -> (Int, Expr)
gc xs cs e = go (free e) cs e
   where
      go :: [Ident] -> [(Ident, HNF)] -> Expr -> (Int, Expr)
      go live cands e = case find (\(xi,_) -> xi `elem` live) cands of
         Nothing       -> (length live, defs [x | x <- xs, x `elem` live] e)
         Just (xi, hi) -> go (live `union` free hi) (cands \\ [(xi, hi)]) (VAR xi :=: HVAL hi :>: e)
-}

{- | Application Contexts

   (paper) A ::= □ | ∃x.A | □ = h  e | □ = k;e | c;A | A e | e A | v = A ; e
              | A + e | e + A | all{A} | one{A}

   (TRS)   A ::= □ v | □ = h | □ = k | ∃x.A | A; e | e;A | A e | e A | v = A
              | A + e | e + A | all{A} | one{A}

-}

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
   do Def (Bind x e) <- [lhs]
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
   do Def (Bind x e) <- [lhs]
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

-- (paper) C ::= V = v | v = V | V = e | v = E
-- (TRS)   C ::= E = e | e = E

derefC :: Expr -> Ident -> [Value -> Expr]
derefC lhs xx = derefE lhs xx
{-
   do (Val v :=: e) <- [lhs]
      ctx   <- derefV v xx
      pure ((:=: e) . Val . ctx)
   ++
   do (v :=: e) <- [lhs]
      ctx   <- derefE e xx
      pure ((v :=:). ctx)
   ++
   -- (TRS) to allow (e1; e2)
   do e <- [lhs]
      derefE e xx
-}
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
     (i, x) <- [ (i, x) | i <- [0..length vs-1], Var x <- [vs !! i]]
     guard (x == xx)
     pure (\v -> VARR (take i vs ++ [v] ++ drop (i+1) vs))

{-
  x = s; E [x] ===> x = s; E[s]
-}

-- TODO: replace with `FLAT-EQ` from Simon-Lennart doc: e1 = e2 ==> ex x. x = e1 ; x = e2; x

dsFresh :: Expr -> Expr
dsFresh e = trace ("TRACE: ds : " ++ show e') $ e'
  where e' = dsFresh' e

dsFresh' :: Expr -> Expr
dsFresh' = go
  where
   go (ex :=: ex') = dsEq ex ex'
   go (ex :>: ex') = go ex :>: go ex' -- dsSeq ex ex'
   go (ex :|: ex') = go ex :|: go ex'
   go (Def bi)     = dsBind bi
   go (One ex)     = One (go ex)
   go (All ex)     = All (go ex)
   go e            = e

-- | `e1 == e2` ===> `ex x . x == e1; x == e2; x`
dsEq :: Expr -> Expr -> Expr
dsEq e1 e2
  | isVal e1  = (e1 :=: e2') :>: e1
  | isVal e2  = (e2 :=: e1') :>: e2
  | otherwise = Def (Bind x ((VAR x :=: e1') :>: (VAR x :=: e2') :>: VAR x))
  where
    x   = identNotIn (free [e1', e2'])
    e1' = dsFresh' e1
    e2' = dsFresh' e2

isVal :: Expr -> Bool
isVal (Val _) = True
isVal _       = False

isCst :: Expr -> Bool
isCst (_ :=: _) = True
isCst _         = False

-- | `e1; e2` ===> `ex x . x == e1; e2`
dsSeq :: Expr -> Expr -> Expr
dsSeq e1 e2
  | isCst e1' = e1' :>: e2'
  | otherwise = Def (Bind x ((VAR x :=: e1') :>: e2'))
  where
     x   = identNotIn (free [e1', e2'])
     e1' = dsFresh' e1
     e2' = dsFresh' e2

dsBind :: Bind Expr -> Expr
dsBind (Bind x e) = Def (Bind x (dsFresh' e))

freshIdent :: Expr -> Ident
freshIdent e = identNotIn (free e)

dsFreshFP :: Expr -> Expr
dsFreshFP = ds
  where
    ds (ex :=: ex') = dsEqu ex ex'
    ds (ex :>: ex') = ds ex :>: val (ds ex') -- dsSeq ex ex'
    ds (ex :|: ex') = ds ex :|: ds ex'
    ds (Def (Bind x e)) = Def (Bind x (val (ds e)))
    ds (One ex)     = One (ds ex)
    ds (All ex)     = All (ds ex)
    ds e            = e

    dsEqu e1 e2
      | isVal e1  = e1 :=: e2'
      | isVal e2  = e2 :=: e1'
      | otherwise = Def (Bind x ((VAR x :=: e1') :>: (VAR x :=: e2')))
      where
        x   = identNotIn (free [e1', e2'])
        e1' = ds e1
        e2' = ds e2

    val (e@(x :=: _)) = e :>: x
    val e = e

-- Make all substitutions that involve variable free arrays.
finalSubst :: Expr -> Expr
finalSubst ee =
  case findArr ee of
    Nothing -> ee
    Just (x, a) -> finalSubst $ subst [(x, a)] $ dropDef x $ dropBind x ee
  where
    dropDef x (Def (Bind x' e)) | x == x' = e
                                | otherwise = Def (Bind x' (dropDef x e))
    dropDef x e = error $ "dropDef: " ++ show (x, e)
    dropBind x (Def (Bind i e)) = Def (Bind i (dropBind x e))
    dropBind x (b@(VAR x' :=: _) :>: r) | x == x' = r
                                        | otherwise = b :>: dropBind x r
    dropBind x e = error $ "dropBind: " ++ show (x, e)
    findArr (Def (Bind _ e)) = findArr e
    findArr ((VAR x :=: Val a) :>: r) | isGnd a = Just (x, a)
                                      | otherwise = findArr r
    findArr _ = Nothing
    isGnd VINT{} = True
    isGnd (VARR vs) = all isGnd vs
    isGnd VOP{} = True
    isGnd _ = False

----------------------

isS :: Value -> Bool
isS VINT{} = True
isS Var{} = True
isS _ = False

isK :: Value -> Bool
isK VINT{} = True
isK _ = False

rulesDerefFP :: ERule
rulesDerefFP lhs =
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

rulesFailFP :: ERule
rulesFailFP lhs =
  "FAIL-CST" `name`
  do Fail :>: _ <- [lhs]
     pure Fail
 ++
  "FAIL-EXP" `name`
  do _ :>: Fail <- [lhs]
     pure Fail
 ++
  -- NOTE: not in paper
  "FAIL-UNIFY" `name`
  do _ :=: Fail <- [lhs]
     pure Fail
 ++
  "FAIL-DEF" `name`
  do Def (Bind x Fail) <- [lhs]
     pure Fail

rulesOneFP :: ERule
rulesOneFP lhs =
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
rulesAllFP lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (ARR [])
 ++
  "ALL-CHOICE" `name`
  do All es@(_ :|: _) <- [lhs]
     let choiceRes e | [r] <- wfRes e = [[r]]
         choiceRes (e :|: es) | [r] <- wfRes e = [ r : rs | rs <- choiceRes es ]
         choiceRes _ = []
     rs <- choiceRes es
     let (is, es, vs) = mkRess rs
     pure (mkRes is es (ARR vs))
 ++
  "ALL-VAL" `name`
  do All e <- [lhs]
     (is, es, v) <- wfRes e
     pure (mkRes is es (ARR [v]))

-- 0 or 1 result
wfRes :: Expr -> [([Ident], [Expr], Value)]
wfRes = maybeToList . wf []
  where wf _ (Val v) = pure ([], [], v)
        wf g (Def (Bind x r)) = do
          (xs, es, v) <- wf (x:g) r
          pure (x:xs, es, v)
        wf g (c@(VAR x :=: e@HVAL{}) :>: r) = do
          (xs, es, v) <- wf (delete x g) r
          pure (xs, c:es, v)
        wf _ _ = Nothing

mkRes :: [Ident] -> [Expr] -> Expr -> Expr
mkRes is es r = foldr (\ i e -> Def (Bind i e)) r' is
  where r' = foldr (:>:) r es

-- XXX This is wrong, it captures variables
mkRess :: [([Ident], [Expr], Value)] -> ([Ident], [Expr], [Value])
mkRess as = loop [] [] [] as
  where
    fvs = free [ (es, v) | (_, es, v) <- as ]
    loop ris res rvs [] = (reverse ris, reverse res, reverse rvs)
    loop ris res rvs ((is, es, v) : xs) = loop (is' ++ ris) (es' ++ res) (v' : rvs) xs
      where is' = take (length is) $ identsNotIn (ris ++ fvs)
            s = zipWith (\ i i' -> (i, Var i')) is is'
            es' = map (subst s) es
            v' = subst s v
{-
mkRess :: [([Ident], [Expr])] -> Expr -> Expr
mkRess = uncurry mkRes . combine [] []
  where
    combine :: [Ident] -> [Expr] -> [([Ident], [Expr])] -> ([Ident], [Expr])
    combine ris res [] = (ris, res)
    combine ris res ((is, es) : isess) = combine (is' ++ ris) (es' ++ res) isess
      where (is', es') = (is, es) -- XXX wrong
-}

rulesNormalization :: ERule
rulesNormalization lhs =
  "NORM-SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  -- NOTE: not like the paper, added e2
  "NORM-SEQ-SWAP" `name`
  do e1 :>: (xv@(VAR x :=: Val v) :>: e2) <- [lhs]
     let valid --x | isK v     = case e1 of VAR _ :=: Val s | isK s -> False; _ -> True  -- NOTE: not in paper
               | isS v     = case e1 of VAR _ :=: Val s | isS s -> False; _ -> True
               | otherwise = case e1 of VAR _ :=: Val _         -> False; _ -> True
--     traceM $ "NORM " ++ show (x, v, e1)
     guard valid
     pure (xv :>: (e1 :>: e2))
 ++
  "NORM-SWAP-EQ" `name`
  do h@(Val HNF{}) :=: x@VAR{} <- [lhs]
     pure (x :=: h)
 ++
  "NORM-VAL" `name`
  do Val _ :>: e <- [lhs]
     pure e
 ++
  "NORM-SEQ-DEFR" `name`
  do Def (Bind x e1) :>: e2 <- [lhs]
     let (nx, ne1) =
           if x `notElem` free e2 then
             (x, e1)
           else
             let x' = identNotIn (free [e1, e2])
             in  (x', subst [(x, Var x')] e1)
     pure (Def (Bind nx (ne1 :>: e2)))
 ++
  "NORM-SEQ-DEFL" `name`
  do e1 :>: Def (Bind x e2) <- [lhs]
     let (nx, ne2) =
           if x `notElem` free e1 then
             (x, e2)
           else
             let x' = identNotIn (free [e1, e2])
             in  (x', subst [(x, Var x')] e2)
     pure (Def (Bind nx (e1 :>: ne2)))
 ++
  "NORM-DEFL" `name`
  do e1 :=: Def (Bind x e2) <- [lhs]
     let (nx, ne2) =
           if x `notElem` free e1 then
             (x, e2)
           else
             let x' = identNotIn (free [e1, e2])
             in  (x', subst [(x, Var x')] e2)
     pure (Def (Bind nx (e1 :=: ne2)))
 ++
  "NORM-SEQR" `name`
  do v@Val{} :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (v :=: e2))
 ++
  "NORM-UNIFYR" `name`
  do v@Val{} :=: (e1 :=: e2) <- [lhs]
     pure ((v :=: e1) :>: (v :=: e2))
