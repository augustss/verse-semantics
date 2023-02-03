{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.ICFP(allSystemsICFP, isRecursive) where
import Control.Monad( guard )
import Data.List

--import qualified Epic.SIntMap as IM
import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
--import Debug.Trace

isRecursive :: Expr -> Bool
isRecursive = not . null . step rulesSubstRec defaultTRSFlags

--------------------------------------------------------------------------------

allSystemsICFP :: [TRSystem Expr]
allSystemsICFP = [ systemICFP, systemICFPR, systemICFPV ]

systemICFP :: TRSystem Expr
systemICFP = TRSystem
  { sname = "ICFP"
  , description = "ICFP, from verse-icfp23/rewrites.ltx"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check valid . anf)
  , postProcess         = const id
  , rules               = allRules
  , rules2              = \ _ _ -> []
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = const valid
  }

systemICFPR :: TRSystem Expr
systemICFPR = s
  { sname = "ICFPR"
  , description = description s ++ " + SUBST-REC"
  , rules = rules s <> rulesSubstRec
  }
  where s = systemICFP

systemICFPV :: TRSystem Expr
systemICFPV = s
  { sname = "ICFPV"
  , description = description s ++ " - EXI-SWAP + EXI-VAR-SWAP"
  , confluenceRules = (confluenceRules s -= "EXI-SWAP") <> rulesExiVarSwap
  }
  where s = systemICFP

-- Check that an expression is in the subset defined by the ICFP (PLDI) grammar.
valid :: Expr -> Bool
valid = expr
  where
    expr e@Val{} = value e
    expr (LAM _ e) = expr e
    expr (_ :=: _) = False
    expr (e1 :>: e2) = expru e1 && expr e2
    expr (e1 :|: e2) = expr e1 && expr e2
    expr (e1 :@: e2) = value e1 && value e2
    expr (EXI _ e) = expr e
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
    hnf (Arr vs) = all value vs
    hnf (LAM _ e) = expr e
    hnf _ = False

-- Make the expression obey the ICFP (PLDI) grammar,
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
    expr (LAM i e) = LAM i (expr e)
    expr e@(_ :=: _) =
      case expru e of
        -- Bare unifications not allowed as an expression
        eu@(v :=: _) -> eu :>: v
        eu -> eu
    expr (_ :~: _) = error "anf: impossible"
    expr (e1 :>: e2) = expru e1 :>: expr e2
    expr (e1 :|: e2) = expr e1 :|: expr e2
    expr (e1 :@: e2) =
      let i1:i2:_ = identsNotIn (free (e1 :@: e2))
          (ds1, v1) = value i1 e1
          (ds2, v2) = value i2 e2
          ds = ds1 ++ ds2
      in  binds ds (v1 :@: v2)
    expr (EXI i e) = EXI i (expr e)
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
    expr _ = undefined

    -- Expression or unification
    expru (e1 :=: e2) =
      case (expr e1, expr e2) of
        (e1'@Val{}, e2') -> e1' :=: e2'
        (e1', e2') -> EXI x $ (Var x :=: e1') :>: (Var x :=: e2') :>: Var x
          where x = identNotIn (free (e1',  e2'))
    expru e = expr e

    value _ e@Var{} = ([], e)
    value _ e@Int{} = ([], e)
    value _ e@Op{}  = ([], e)
    value _ (LAM x e) = ([], LAM x (expr e))
    value _ (Arr es) = arr es
    value i e = ([(i, expr e)], Var i)

    arr es =
      let is = identsNotIn $ free es
          (dss, vs) = unzip $ zipWith value is es
          ds = concat dss
      in  (ds, Arr vs)

    binds :: [(Ident, Expr)] -> Expr -> Expr
    binds [] b = b
    binds ((i,e):ds) b = EXI i $ (Var i :=: e) :>: binds ds b

--------------------------------------------------------------------------------

type Context = Expr -> Expr

-- scope contexts

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

scopeX :: Expr -> [(Context, Expr)]
scopeX lhs =
  do One hole <- [lhs]
     choices One hole
 ++
  do All hole <- [lhs]
     choices All hole
 ++
  do Split hole f g <- [lhs]
     choices (\ e -> Split e f g) hole
 where
  choices ctx e =
    (ctx,e) : case e of
                e1 :|: e2 -> choices (ctx . (e1 :|:)) e2
                          ++ choices (ctx . (:|: e2)) e1
                _         -> []

choiceX, choiceX1 :: Expr -> [(Context, Expr)]
-- CX context
choiceX lhs = choiceX1 lhs ++ [(id,lhs)]
-- CX context, CX /= hole
choiceX1 lhs =
  do Val v :=: cx <- [lhs]
     (ctx, hole) <- choiceX cx
     pure ((v :=:) . ctx, hole)
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
  do EXI x cx <- [lhs]
     (ctx, hole) <- choiceX cx
     pure (EXI x . ctx, hole)

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (Val _ :=: b) = isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Op op :@: _) = isChoiceFreeOp op && not (isStoreOp op)
isChoiceFree Split{}   = True  -- XXX This isn't true!!
isChoiceFree Wrong     = True
isChoiceFree (EXI _ e) = isChoiceFree e
isChoiceFree _         = False
-- KC: what about @?

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _ = True

isStoreOp :: Op -> Bool
isStoreOp Alloc = True -- Don't mess with memory ops
isStoreOp Read = True -- Don't mess with memory ops
isStoreOp Write = True -- Don't mess with memory ops
isStoreOp AddTo = True
isStoreOp _ = False

valueX, valueX1 :: Value -> [(Value->Value, Value)]
valueX lhs = valueX1 lhs ++ [(id, lhs)]

valueX1 lhs =
  do Arr vs <- [lhs]
     i <- [0..length vs-1]
     let ctx1 = \ v -> Arr (take i vs ++ [v] ++ drop (i+1) vs)
         v1 = vs!!i
     (ctx2, v2) <- valueX v1
     pure (ctx1 . ctx2, v2)

-- X context, or exist x . defX
defX :: Ident -> Expr -> [(Context, Expr)]
defX xx lhs =
  do execX lhs
 ++
  do EXI x dx <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- defX xx dx
     return (EXI x . ctx, hole)

--------------------------------------------------------------------------------

allRules :: ERule
allRules =  rulesApplication
         <> rulesUnification
         <> rulesElimination
         <> rulesNormalization
         <> rulesSpeculation
         <> rulesFail
         -- SPLIT rules only trigger in case of a SPLIT
         <> rulesSplit

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
  "APP-GRT" `name`
  do Op Gt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 > k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-GRE" `name`
  do Op Ge :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 >= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-LST" `name`
  do Op Lt :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 < k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-LSE" `name`
  do Op Le :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 <= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-NEQ" `name`
  do Op Ne :@: Arr [Int k1, Int k2] <- [lhs]
     if k1 /= k2
       then pure (Int k1)
       else pure Fail
 ++
  "APP-ISINT" `name`
  do Op IsInt :@: (HNF hnf) <- [lhs]
     case hnf of
       Int _ -> pure hnf -- (Arr [])
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
mapAp :: [Value] -> Expr
mapAp vs =
  let xs = take (length vs) $ identsNotIn $ free vs
  in  defs xs $ seqs $ zipWith (\ x v -> Var x :=: (v :@: unit)) xs vs ++ [Arr $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr EXI e vs

unit :: Value
unit = Arr []

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication env lhs =
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
 <>
  "APP-TUP-0" `name`
  do Arr [] :@: _ <- [lhs]
     pure Fail
 <>
  "APP-TUP" `name`
  do Arr vs@(_:_) :@: v <- [lhs]
     let x = identNotIn (free (vs, v))
         vx = Var x
     pure (EXI x ((vx :=: v) :>: (foldr1 (:|:) [ (vx :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])))

 <>
  rulesPrimOps env lhs

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification env lhs =
  "U-LIT" `name`
  do Int k1 :=: Int k2 <- [lhs]
     guard (k1 == k2)
     pure unit
 ++
  "U-REF" `name`
  do Ref k1 :=: Ref k2 <- [lhs]
     guard(k1 == k2)
     pure unit
 ++
  "U-TUP" `name`
  do Arr vs :=: Arr vs' <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) unit [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-FAIL" `name`
  do HNF e1 :=: HNF e2 <- [lhs]
     -- Avoid the cases handled above
     guard (case (e1,e2) of (Int k1,Int k2) -> k1 /= k2
                            (Ref k1,Ref k2) -> k1 /= k2
                            (Arr a1,Arr a2) -> length a1 /= length a2
                            _               -> True)
     pure Fail
 ++
   "U-OCCURS" `name`
   do Var x :=: Val v <- [lhs]
      (_, Var x') <- valueX1 v
      guard (x == x')
      pure Fail
 ++
  "SUBST" `name`
  do (ctx, (Var x :=: Val v) :>: e) <- execX lhs
     let freeX = free (ctx Fail, e)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     pure (subst sub (ctx ((Var x0 :=: Val v) :>: e)))
 ++
  "HNF-SWAP" `name`
  do Val (HNF hnf) :=: Var x <- [lhs]
     pure (Var x :=: Val hnf)
 ++
  "VAR-SWAP" `name`
  do Var y :=: Var x <- [lhs]
     guard (lessThan env x y)
     pure (Var x :=: Var y)

rulesSubstRec :: ERule
rulesSubstRec _ lhs =
  "SUBST-REC" `name`
  do Var x :=: Val v <- [lhs]
     (ctx, LAM y e) <- valueX v
     guard (x `elem` free (LAM y e))
     pure (Var x :=: Val (ctx (LAM y (Exi (Bind x (lhs :>: e))))))

-- Order variables by binding depth, innermost is smaller.
-- Use name comparison for unbound variables.
lessThan :: TRSFlags -> Ident -> Ident -> Bool
lessThan env x y =
  case (elemIndex x (boundVars env), elemIndex y (boundVars env)) of
    (Nothing, Nothing) -> x < y   -- Both unbound, use names
    (Just _,  Nothing) -> True    -- Bound is smaller than unbound
    (Nothing, Just _ ) -> False
    (Just i,  Just j ) -> i < j   -- Use binding depth

--------------------------------------------------------------------------------

rulesElimination :: ERule
rulesElimination _ lhs =
  "EXI-ELIM" `name`
  do EXI x e <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "EXI-ELIML" `name`
  do EXI x a <- [lhs]
     (ctx, (Var x' :=: Val v) :>: e) <- defX x a
     guard (x == x')
     guard (x `notElem` free (ctx (v :>: e)))
     pure (ctx e)

--------------------------------------------------------------------------------

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "NORM-EXI" `name`
  do (ctx, EXI x e) <- execX1 lhs
     let freeX = free (ctx Fail)
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (EXI x' (ctx (subst [(x,Var x')] e)))
       else pure (EXI x (ctx e))
 ++
  "NORM-VAL" `name`
  do Val _ :>: e <- [lhs]
     pure e
 ++
  "NORM-SEQ" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "NORM-SEQR" `name`
  do Val v :=: (e1 :>: e2) <- [lhs]
     pure (e1 :>: (Val v :=: e2))

--------------------------------------------------------------------------------

rulesSpeculation :: ERule
rulesSpeculation _ lhs =
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))
 ++
  "CHOOSE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
  "FAIL-L" `name`
  do Fail :|: e <- [lhs]
     pure e
 ++
  "FAIL-R" `name`
  do e :|: Fail <- [lhs]
     pure e
 ++
  "ONE-FAIL" `name`
  do One Fail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (Val v :|: _e) <- [lhs]
     pure (Val v)
 ++
  "ONE-VALue" `name`
  do One (Val v) <- [lhs]
     pure (Val v)
 ++
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
  "ALL-VALue" `name`
  do All (Val v) <- [lhs]
     pure (Arr [v])

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL" `name`
  do (_cx, Fail) <- execX1 lhs
     pure Fail

--------------------------------------------------------------------------------

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail f _g <- [lhs]
     pure (f :@: unit)
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x e
     pure (EXI h (gv :>: hlam))
 ++
  "SPLIT-VALUE" `name`
  do Split (Val v) _f g <- [lhs]
     let x:h:_ = identsNotIn (free lhs)
         gv = Var h :=: (g :@: v)
         hlam = Var h :@: LAM x Fail
     pure (EXI h (gv :>: hlam))

--------------------------------------------------------------------------------

rulesStructural :: ERule
rulesStructural _ lhs =
  "EXI-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     pure (EXI y (EXI x e))
 <>
  "VAL-SWAP" `name`
  do e1 :>: (e2@(_ :=: Val _) :>: e3) <- [lhs]
     pure $ e2 :>: (e1 :>: e3)

rulesExiVarSwap :: ERule
rulesExiVarSwap _ lhs =
  "EXI-VAR-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     let e' = substExp (Var y :=: Var x) (Var x :=: Var y) e
     pure (EXI y (EXI x e'))
