{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.TRS2024(allSystemsTRS2024) where
import Control.Monad( guard )

--import Epic.Print(prettyShow)
--import qualified Epic.SIntMap as IM
import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
--import Debug.Trace

--------------------------------------------------------------------------------

allSystemsTRS2024 :: [TRSystem Expr]
allSystemsTRS2024 = [ systemTRS2024 ]

systemTRS2024 :: TRSystem Expr
systemTRS2024 = TRSystem
  { sname               = "TRS2024"
  , description         = "TRS2024, as specified in our internal document"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check valid . anf)
  , postProcess         = const id
  , rules               = allRules
  , rules2              = \ _ _ -> []
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = const valid
  , sortRewrites        = id
  }

-- Check that an expression is valid as defined by the grammar
valid :: Expr -> Bool
valid = expr
  where
    -- left out for now: assume, check, verify, havoc, olam
    expr (v  :=: e)       = value v && expr e
    expr (e1 :>: e2)      = expr e1 && expr e2
    expr (e1 :|: e2)      = expr e1 && expr e2
    expr Fail             = True
    expr (Exi (Bind _ e)) = expr e
    expr (v1 :@: v2)      = value v1 && value v2
    expr (One e)          = expr e
    expr (All e)          = expr e
    expr (Uni (Bind _ e)) = expr e
    expr v                = value v
    
    value (Var _) = True
    value r       = hnf r
    
    hnf (Int _)          = True
    hnf (Op _)           = True
    hnf (Arr vs)         = all value vs
    hnf (Lam (Bind _ e)) = expr e
    hnf _                = False    

-- Make the expression obey the grammar; i.e. valid (anf e) == True
anf :: Expr -> Expr
anf = expr
  where
    expr (v' :=: e)       = makeValue v' (\v -> v :=: expr e)
    expr (e1 :>: e2)      = expr e1 :>: expr e2
    expr (e1 :|: e2)      = expr e1 :|: expr e2
    expr Fail             = Fail
    expr (Exi (Bind x e)) = Exi (Bind x (expr e))
    expr (v1' :@: v2')    = makeValues [v1',v2'] (\[v1,v2] -> v1 :@: v2)
    expr (One e)          = One (expr e)
    expr (All e)          = All (expr e)
    expr (Uni (Bind x e)) = Uni (Bind x (expr e))
    expr (Arr vs')        = makeValues vs' (\vs -> Arr vs)
    expr (Lam (Bind x e)) = Lam (Bind x (expr e))
    expr (Int k)          = Int k
    expr (Op op)          = Op op
    expr _                = Int 1 -- what to do here??

    value v = valid (v :=: Int 0)

    makeValue v f
      | value v   = f v
      | otherwise = Exi (Bind x ((Var x :=: expr v) :>: f (Var x)))
     where
      x:_ = identsNotIn (free (f v))
    
    makeValues []       f = f []
    makeValues (v':vs') f = makeValue v' (\v -> makeValues vs' (\vs -> f (v:vs)))
    
--------------------------------------------------------------------------------

-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (a :=: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Op op :@: _) = isChoiceFreeOp op -- && not (isStoreOp op)
isChoiceFree Split{}   = True  -- XXX is it?
isChoiceFree Wrong{}   = True
isChoiceFree (EXI _ e) = isChoiceFree e
isChoiceFree _         = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _ = True

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
 ++
  do Store h e <- [lhs]
     (ctx, hole) <- execX e
     pure (Store h . ctx, hole)

-- X context, or exist x . defX
defX :: Ident -> Expr -> [(Context, Expr)]
defX xx lhs =
  do execX lhs
 ++
  do Exi (Bind x dx) <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- defX xx dx
     return (Exi . Bind x . ctx, hole)

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
  do Exi (Bind x cx) <- [lhs]
     (ctx, hole) <- choiceX cx
     pure (Exi . Bind x . ctx, hole) -- hopefully this is sound!
{-
 ++
  do Store h e <- [lhs]
     (ctx, hole) <- choiceX e
     pure (Store h . ctx, hole)
-}

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

allRules :: ERule
allRules =  rulesPrimOps
         <> rulesApplication
         <> rulesUnificationNoOcc
         <> rulesUnificationVariables
         <> rulesSequencing
         <> rulesChoice
         <> rulesOne
         <> rulesAll
         <> rulesFail
         <> rulesSplit

--------------------------------------------------------------------------------

rulesPrimOps :: ERule
rulesPrimOps _ lhs =
  "P-ADD" `name`
  do Op Add :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1+k2))
 ++
  "P-SUB" `name`
  do Op Sub :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1-k2))
 ++
  "P-MUL" `name`
  do Op Mul :@: Arr [Int k1, Int k2] <- [lhs]
     pure (Int (k1*k2))
 ++
  "P-DIV" `name`
  do Op Div :@: Arr [Int k1, Int k2] <- [lhs]
     if k2 /= 0
       then pure (Int (k1 `div` k2))
       else pure Fail
 ++
  "P-NEG" `name`
  do Op Neg :@: Int k <- [lhs]
     pure (Int k)
 ++
  "P-PLUS" `name`
  do Op Plus :@: Int k <- [lhs]
     pure (Int k)
 ++
  "P-GRT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 > k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-GRE" `name`
  do Op Ge :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 >= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-LST" `name`
  do Op Lt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 < k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-LSE" `name`
  do Op Le :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 <= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-NEQ" `name`
  do Op Ne :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 /= k2
       then pure (Int k1)
       else pure Fail
 ++
  "P-IsInt" `name`
  do Op IsInt :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure hnf -- (Arr [])
       _     -> pure Fail
 ++
  "P-MAPAP" `name`
  do Op MapAp :@: Arr vs <- [lhs]
     pure (mapAp vs)
 ++
  "P-CONS" `name`
  do Op Cons :@: Arr [v, Arr vs] <- [lhs]
     pure (Arr (v:vs))

-- Turn array{f1, ... fn} into array{f1(), ... fn()}
mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> Var x :=: (v :@: unit)) xs vs ++ [Arr $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs = exis

unit :: Value
unit = Arr []

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do LAM x e :@: v <- [lhs]
     let freeV = free v
         beta y b = EXI y ((Var y :=: Val v) :>: b)
     -- A small shortcut for dummy variables.
     if x == Name "_" then
       pure e
      else if x `notElem` freeV then
       pure (beta x e)
      else do
       -- The x has to be renamed to avoid capture
       let freeE = free e
           x' = identNotIn (freeV ++ freeE)
           e' = subst [(x, Var x')] e
       pure (beta x' e')
 ++
  "APP-TUP" `name`
  do Arr vs :@: v <- [lhs]
     if null vs then
       pure Fail
      else
       pure (foldr1 (:|:) [ (Val v :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])

--------------------------------------------------------------------------------
--rulesUnification :: ERule
--rulesUnification = rulesUnificationNoOcc
--                <> rulesUnificationOcc

rulesUnificationNoOcc :: ERule
rulesUnificationNoOcc _ lhs =
  "ULIT" `name`
  do Int k1 :=: Int k2 <- [lhs]
     if k1 == k2
       then pure (Int k1)
       else pure Fail
 ++
  "UREF" `name`
  do Ref k1 :=: Ref k2 <- [lhs]
     if k1 == k2
       then pure (Ref k1)
       else pure Fail
 ++
  "UTUP" `name`
  do Arr vs :=: Arr vs' <- [lhs]
     if length vs == length vs'
       then pure (foldr (:>:) (Arr vs) [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
       else pure Fail
 ++
  "UX-LAM" `name`
  do Lam{} :=: Lam{} <- [lhs]
     pure Fail
 ++
  "UX-OP" `name`
  do Op{} :=: Op{} <- [lhs]
     pure Fail
 ++
  "UX" `name`
  do HNF e1 :=: HNF e2 <- [lhs]
     -- Avoid the cases handled above, and fail for any unequal hnfs
     guard (case (e1,e2) of (Int{},Int{}) -> False
                            (Ref{},Ref{}) -> False
                            (Arr{},Arr{}) -> False
                            (Lam{},Lam{}) -> False
                            (Op{}, Op{})  -> False
                            _             -> True)
     guard (e1 /= e2)
     pure Fail
{-
 ++
  "UX1" `name`
  do Int _k :=: Arr _vs <- [lhs]
     pure Fail
 ++
  "UX2" `name`
  do Arr _vs :=: Int _k <- [lhs]
     pure Fail
 ++
  "UX3" `name`
  do Val (LAM _ _) :=: Val (HNF _) <- [lhs]
     pure Fail
 ++
  "UX4" `name`
  do Val (HNF _) :=: Val (LAM _ _) <- [lhs]
     pure Fail
 ++
  "UX5" `name`
  do Val (HNF (Op _)) :=: Val (HNF _) <- [lhs]
{-
     if h1 == h2 then  -- To make it compatible with the PLDI rules
       pure (Val h1)
      else
-}
     pure Fail
 ++
  "UX6" `name`
  do Val (HNF _) :=: Val (HNF (Op _)) <- [lhs]
     pure Fail
-}

--------------------------------------------------------------------------------

rulesUnificationVariables :: ERule
rulesUnificationVariables env lhs =
  rulesUnificationVariablesNoR env lhs
 ++
  "DEF-ELIMR" `name`
  do Exi (Bind x a) <- [lhs]
     (ctx, Val v :=: Var x') <- defX x a
     guard (x == x')
     let freeX = free (ctx Fail)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
  

rulesUnificationVariablesNoR :: ERule
rulesUnificationVariablesNoR _ lhs =
  "SUBST" `name`
  do (ctx, Var x :=: Val v) <- execX lhs
     let freeX = free (ctx blob)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx (Var x0 :=: Val v)))
 ++
  "DEF-ELIML" `name`
  do Exi (Bind x a) <- [lhs]
     (ctx, Var x' :=: Val v) <- defX x a
     guard (x == x')
     let freeX = free (ctx blob)
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))
 ++
  "SWAP" `name`
  do Val (HNF hnf) :=: Var x <- [lhs]
     pure (Var x :=: Val hnf)
 ++
  "DEF-FLOAT" `name`
  do (ctx, Exi (Bind x e)) <- execX1 lhs
     let freeX = free (ctx blob)
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (Exi (Bind x' (ctx (subst [(x,Var x')] e))))
       else pure (Exi (Bind x (ctx e)))
 where
  blob = Fail -- just something to plug the hole in the context so we can look at it

--------------------------------------------------------------------------------

rulesSequencing :: ERule
rulesSequencing _ lhs =
  "SEQ" `name`
  do Val _v :>: e <- [lhs]
     pure e
 ++
  "SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "UNIFY-SEQL" `name`
  do (e1 :>: e2) :=: e3 <- [lhs]
     pure (e1 :>: (e2 :=: e3))
 ++
  "UNIFY-SEQR" `name`
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (Val v :=: e2))
 ++
  "UNIFY-UNIFYL" `name`
  do (e1 :=: e2) :=: e3 <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Exi (Bind x ((Var x :=: e1) :>: (Var x :=: e2) :>: (Var x :=: e3))))
 ++
  "UNIFY-UNIFYR" `name`
  do e1 :=: (e2 :=: e3) <- [lhs]
     let x = identNotIn (free [e1,e2,e3])
     pure (Exi (Bind x ((Var x :=: e1) :>: (Var x :=: e2) :>: (Var x :=: e3) :>: Var x)))
{-
  -- for FRESH
  -- XXX is this needed
 ++ "CONJ-CST-DEFR" `name` -- e1 = (ex y. e2) --> ex y. e1 = e2
  do (e1 :=: Exi (Bind y e2)) <- [lhs]
     let y' = identNotIn (free e1 ++ free e2)
     if y `elem` free e1
       then pure (Exi (Bind y' (e1 :=: subst [(y,Var y')] e2)))
       else pure (Exi (Bind y (e1 :=: e2)))
-}
-- ++ "CONJ-SEQ-ASSOC" `name`
--  do (e1 :>: e2) :>: e3 <- [lhs]
--     pure (e1 :>: (e2 :>: e3))

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL-DEF" `name`
  do Exi (Bind _x Fail) <- [lhs]
     pure Fail
 ++
  "FAIL" `name`
  do (_cx, Fail) <- execX1 lhs
     pure Fail

_rulesMoreFail :: ERule
_rulesMoreFail _ lhs =
  "FAIL-LAM" `name`
  do Lam (Bind _x Fail) <- [lhs]
     pure Fail

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))

--------------------------------------------------------------------------------

rulesOne :: ERule
rulesOne _ lhs =
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (Val v :|: _e) <- [lhs]
     pure (Val v)
 ++
  "ONE-VAL" `name`
  do One (Val v) <- [lhs]
     pure (Val v)

rulesAll :: ERule
rulesAll _ lhs =
  "ALL-FAIL" `name`
  do All Fail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All ves@(_ :|: _) <- [lhs]
     let choiceVals (Val v) = [[v]]
         choiceVals (Val v :|: es) = [ v : vs | vs <- choiceVals es ]
         choiceVals _ = []
     vs <- choiceVals ves
     pure (Arr vs)
 ++
  "ALL-VAL" `name`
  do All (Val v) <- [lhs]
     pure (Arr [v])

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f _g <- [lhs]
     pure (f :@: Arr [])
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x e
     pure (Exi (Bind h (gv :>: hlam)))
 ++
  "SPLIT-VAL" `name`
  do Split (Val v) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x Fail
     pure (Exi (Bind h (gv :>: hlam)))

--------------------------------------------------------------------------------

rulesStructural :: ERule
rulesStructural _ lhs =
  "EXI-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     pure (EXI y (EXI x e))
 <>
  "VAR-SWAP" `name`
  do (ctx, Var x :=: Var y) <- execX lhs
     let y0 = identNotIn (free (ctx Fail, y, x))
         sub = [(y, Var x), (y0, Var y)]
     pure (subst sub (ctx (Var y0 :=: Var x)))
{- These 2 rules makes it very slow
 <>
  "UNIFY-MOVE" `name`
  do (ctx, e@(Val _v1 :=: Val v2)) <- execX lhs
     pure (e :>: ctx v2)
 <>
  "SEQ" `name`
  do Val _v :>: e <- [lhs]
     pure e
-}

 -- NEW RULE
 <>
  "EU-SWAP" `name`
  do e1 :>: (e2 :>: e3) <- [lhs]
     guard (isChoiceFree e1 || isChoiceFree e2)
     pure $ e2 :>: (e1 :>: e3)
{-
 -- NEW RULE
 -- Needed for \x.(<> = x); <>
 --  Maybe better: x=v --> x=v; v
 <>
  "UNIFY-RES" `name`
  do (e1 :=: Val e2) :>: e3 <- [lhs]
     guard (e2 == e3)
     pure (e1 :=: e2)
-}
{-
 -- NEW RULE
 <>
  "UNIFY-SWAP" `name`
  do (e1 :=: e2) <- [lhs]
     guard (isChoiceFree e1 || isChoiceFree e2)
     pure (e2 :=: e1)
-}

-----------------------------------------------------------------------------------

