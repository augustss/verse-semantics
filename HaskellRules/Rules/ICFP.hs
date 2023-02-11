{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.ICFP(allSystemsICFP, isRecursive) where
import Control.Monad( guard )
import Data.List
import Data.Maybe

import Epic.Uniplate(universe)
import qualified Epic.SIntMap as IM
import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
--import Debug.Trace (traceShow, trace)
--import Debug.Trace

isRecursive :: Expr -> Bool
isRecursive = not . null . step rulesSubstRec defaultTRSFlags

--------------------------------------------------------------------------------

allSystemsICFP :: [TRSystem Expr]
allSystemsICFP = [ systemICFP, systemICFPR,
                   systemICFPA, systemICFPC, systemICFPD, systemICFPF, systemICFPG,
                   systemICFPH, systemICFPI, systemICFPJ, systemICFPK,
                   systemICFPS
                 ]

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
  { sname = "RICFP"
  , description = description s ++ " + SUBST-REC"
  , rules = rules s <> rulesSubstRec
  }
  where s = systemICFP

systemICFPA :: TRSystem Expr
systemICFPA = s
  { sname = "ICFPA"
  , description = description s ++ ", plan A: - EXI-SWAP + EXI-VAR-SWAP"
  , confluenceRules = (confluenceRules s -= "EXI-SWAP") <> rulesExiVarSwap
  }
  where s = systemICFP

systemICFPC :: TRSystem Expr
systemICFPC = s
  { sname = "ICFPC"
  , description = description s ++ ", plan C: + EXI-SWAP-ND"
  , confluenceRules = confluenceRules s <> rulesVarSwapND
  }
  where s = systemICFP

systemICFPD :: TRSystem Expr
systemICFPD = s
  { sname = "ICFPD"
  , description = description s ++ ", plan D: - VAR-SWAP + EXI-ELIMV + VAR-SWAP-SUBST"
  , rules = (rules s -= "VAR-SWAP") <> rulesExiElimV
  , confluenceRules = confluenceRules s <> rulesVarSwapSubst
  }
  where s = systemICFP

systemICFPF :: TRSystem Expr
systemICFPF = s
  { sname = "ICFPF"
  , description = description s ++ ", - NORM-EXI - NORM-SEQR - EXI-SWAP"
  , rules = rules s -= "NORM-EXI" -= "NORM-SEQR"
  , confluenceRules = confluenceRules s -= "EXI-SWAP"
  }
  where s = systemICFP

systemICFPG :: TRSystem Expr
systemICFPG = s
  { sname = "ICFPG"
  , description = description s ++ ", - NORM-EXI - EXI-SWAP + NORM-EXI-{R,L,E}"
  , rules = (rules s -= "NORM-EXI") <> rulesNormExiCanon
  , confluenceRules = confluenceRules s -= "EXI-SWAP"
  }
  where s = systemICFP

systemICFPH :: TRSystem Expr
systemICFPH = s
  { sname = "ICFPH"
  , description = description s ++ ", Plan H"
  , rules = (rules s -= "NORM-EXI") <> rulesNormExiLR
  , confluenceRules = confluenceRules s -= "EXI-SWAP"
  }
  where s = systemICFP

systemICFPI :: TRSystem Expr
systemICFPI = s
  { sname = "ICFPI"
  , description = description s ++ ", Plan I"
  , rules = (rules s -= "VAR-SWAP") <> rulesPlanI<> rulesExiElimV
--  , confluenceRules = confluenceRules s -= "VAR-SWAP-SUBST"
  }
  where s = systemICFP

systemICFPJ :: TRSystem Expr
systemICFPJ = s
  { sname = "ICFPJ"
  , description = description s ++ ", Plan J"
  , rules = (rules s -= "VAR-SWAP") <> rulesPlanJ
  , confluenceRules = confluenceRules s -= "VAR-SWAP-SUBST"
  , rulesHaveStructural = True
  }
  where s = systemICFP

systemICFPK :: TRSystem Expr
systemICFPK = s
  { sname = "ICFPK"
  , description = description s ++ ", Plan K"
  , rules = (rules s -= "EXI-ELIMV" -= "EXI-ELIML") <> rulesValSwapK <> rulesExiElimL
  , confluenceRules = \ _ _ -> []
  }
  where s = systemICFPJ

systemICFPS :: TRSystem Expr
systemICFPS = s
  { sname = "ICFPS"
  , description = description s ++ ", store"
  , rules = rules s <> rulesStore
  , preProcess = \ e -> addStore . preProcess s e
  , postProcess = const dropStore
  }
  where s = systemICFPK

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
    expr e = error $ "anf: " ++ show e

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

instance Free Context where
  -- Get free variables that are not in the hole.
  free ctx = free (ctx Fail)

-- scope contexts

execX, execX1 :: Expr -> [(Context, Expr)]
-- X context
execX lhs = execX1 lhs ++ [(id,lhs)]
-- X context, X /= hole
execX1 lhs =
  do (v :=: x) :>: e <- [lhs]
     (ctx, hole) <- execX x
     pure (\ a -> (v :=: ctx a) :>: e, hole)
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
                Store h e1 -> choices (ctx . Store h) e1
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
     guard (isEffFree ce)
     (ctx, hole) <- choiceX cx
     pure ((ce :>:) . ctx, hole)
 ++
  do EXI x cx <- [lhs]
     (ctx, hole) <- choiceX cx
     pure (EXI x . ctx, hole)

isEffFree :: Expr -> Bool
isEffFree e = isChoiceFree e && isStoreFree e

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (Val _ :=: b) = isChoiceFree b
isChoiceFree (a :>: b) = isChoiceFree a && isChoiceFree b
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Op op :@: _) = isChoiceFreeOp op
isChoiceFree Split{}   = True  -- XXX This isn't true!!
isChoiceFree Wrong     = True
isChoiceFree (EXI _ e) = isChoiceFree e
isChoiceFree _         = False
-- KC: what about @?

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _ = True

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
     let freeX = free (ctx, e)
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

rulesPlanJ :: ERule
rulesPlanJ env lhs =
  "VAR-SWAP-J" `name`
  do Var y :=: Var x <- [lhs]
     guard ({- myTraceShow ("lessThan: " ++ show (x, y)) -} (lessThan env x y))
     pure (Var x :=: Var y)
  ++
  "EXI-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     pure (EXI y (EXI x e))


--myTraceShow :: Show a => String -> a -> a
--myTraceShow msg x = trace ("TRACE: " ++ msg ++ show x) x

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
     let freeX = free ctx
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

rulesNormExiCanon :: ERule
rulesNormExiCanon _ lhs =
  "NORM-EXI-L" `name`
  do xe :>: EXI x e <- [lhs]
     guard (isExistsFree xe)
     let (x', e') = alphaExi (free xe) x e
     pure (EXI x' (xe :>: e'))
 <>
  "NORM-EXI-R" `name`
  do EXI x e1 :>: e2 <- [lhs]
     let (x', e1') = alphaExi (free e2) x e1
     pure (EXI x' (e1' :>: e2))
 <>
  "NORM-EXI-E" `name`
  do v :=: EXI y e <- [lhs]
     let (y', e') = alphaExi (free v) y e
     pure (EXI y' ((v :=: e') :>: v))

alphaExi :: [Ident] -> Ident -> Expr -> (Ident, Expr)
alphaExi is x e | x `notElem` is = (x, e)
                | otherwise =
  let x' = identNotIn (is ++ free e)
  in  (x', subst [(x, Var x')] e)

isExistsFree :: Expr -> Bool
isExistsFree (e1 :>: e2) = isExistsFree e1 && isExistsFree e2
isExistsFree (e1 :=: e2) = isExistsFree e1 && isExistsFree e2
isExistsFree (_ :@: _) = False
isExistsFree EXI{} = False
isExistsFree _ = True

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
  "ONE-VALUE" `name`
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
  "ALL-VALUE" `name`
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

rulesVarSwapND :: ERule
rulesVarSwapND _ lhs =
  "VAR-SWAP-ND" `name`
  do x@Var{} :=: y@Var{} <- [lhs]
     pure (y :=: x)

rulesExiElimV :: ERule
rulesExiElimV _ lhs =
  "EXI-ELIMV" `name`
  do EXI x a <- [lhs]
     (ctx, (Var z :=: Var x') :>: e) <- defX x a
     guard (x == x' && not(isUV x))
     guard (x /= z)
     guard (z `notElem` defVars ctx)
     pure (subst [(x, Var z)] (ctx e))

-- Get initially quantified variables from a defX context
defVars :: Context -> [Ident]
defVars ctx = loop (ctx Fail)
  where loop (EXI x e) = x : loop e
        loop _ = []

rulesVarSwapSubst :: ERule
rulesVarSwapSubst _ lhs =
  "VAR-SWAP-SUBST" `name`
  do (ctx, Var x :=: Var y) <- execX lhs
     let y0 = identNotIn (free (ctx, y, x))
         sub = [(y, Var x), (y0, Var y)]
     pure (subst sub (ctx (Var y0 :=: Var x)))

rulesNormExiLR :: ERule
rulesNormExiLR _ lhs =
  "NORM-EXI-2" `name`
  do EXI x e1 :>: e2 <- [lhs]
     let (x', e1') = alphaExi (free e2) x e1
     pure (EXI x' (e1' :>: e2))
 <>
  "NORM-EXI-3" `name`
  do (x :=: EXI y e1) :>: e2 <- [lhs]
     let (y', e1') = alphaExi (free (x, e2)) y e1
     pure (EXI y' ((x :=: e1') :>: e2))

{-
rulesPlanI :: ERule
rulesPlanI env lhs =
  "VAR-SWAP-FF" `name`
  do EXI y a <- [lhs]
     (ctx, (Var x :=: Var y') :>: e) <- defX x a
     guard (y == y')
     guard (x /= y)
     guard (x `elem` flexVars env)
     pure (subst [(y, Var x)] (ctx e))
-}

rulesPlanI :: ERule
rulesPlanI env lhs =
  "VAR-SWAP-RR" `name`
  do (Var a :=: Var b) :>: e <- [lhs]
     guard (a /= b)
     let fs = flexVars env
     guard (a `notElem` fs && b `notElem` fs)
     -- let x = identNotIn (free (a, b, e))
     let x = uvIdentNotIn (free (a, b, e))
     pure (EXI x (Var a :=: Var x :>: Var b :=: Var x :>: e))
 <>
  "NORM-SWAP-FF" `name`
  do EXI x a <- [lhs]
     (ctx, (Var z :=: Var x') :>: e) <- defX x a
     guard (x == x')
     guard (x /= z)
     guard (z `notElem` defVars ctx)
     guard (z `elem` flexVars env)
     pure (subst [(x, Var z)] (ctx e))

rulesValSwapK :: ERule
rulesValSwapK env lhs =
  "VAL-SWAP-K" `name`
  do e1 :>: (e2 :>: e3) <- [lhs]
--     traceM $ show (e1, e2, _ltExpr _env e2 e1, boundVars _env)
     guard $
       -- First, order by choice-free-ness;
       -- choice free goes first
       case (isEffFree e1, isEffFree e2) of
         (False, False) -> False  -- cannot change order of choices
         (False, True)  -> True   -- put ce before e
         (True, False)  -> False  -- ce is already first
         (True, True)   ->
           -- Next, order so equations go before expressions.
           -- (This is an arbitrary choice)
           case (isEqn e1, isEqn e2) of
             (False, False) -> ltExpr env e2 e1  -- use ordering
             (False, True)  -> True              -- need to swap
             (True, False)  -> False             -- already in correct order
             (True, True)   -> ltExpr env e2 e1  -- use ordering
     pure $ e2 :>: (e1 :>: e3)
{-
  "VAL-SWAPL" `name`
  do e1 :>: (e2@(Var{} :=: Val{}) :>: e3) <- [lhs]
     guard (ltExpr env e2 e1)
     pure $ e2 :>: (e1 :>: e3)
 <>
  "VAL-SWAPR" `name`
  do e1@(Var{} :=: Val{}) :>: (e2 :>: e3) <- [lhs]
     guard (ltExpr env e2 e1)
     pure $ e2 :>: (e1 :>: e3)
-}
  
-- Compare two expression using lessThan for identifiers
ltExpr :: TRSFlags -> Expr -> Expr -> Bool
ltExpr env e1 e2 = comp vs vs e1 e2 == LT
  where
    vs = boundVars env

isEqn :: Expr -> Bool
isEqn (_ :=: _) = True
isEqn _ = False

rulesExiElimL :: ERule
rulesExiElimL _ lhs =
  "EXI-ELIML" `name`
  do EXI x a <- [lhs]
     (ctx, (Var x' :=: Val v) :>: e) <- execX a
     guard (x == x')
     guard (x `notElem` free (ctx (v :>: e)))
     pure (ctx e)

----------------------

storeEmpty :: Heap
storeEmpty = IM.empty

storeAlloc :: Heap -> Value -> (Heap, Ptr)
storeAlloc h v =
  let p | IM.null h = Ptr 0
        | otherwise = fst $ IM.findMax h
      h' = IM.insert p v h
  in  (h', p)

storeRead :: Heap -> Ptr -> Value
storeRead h p = fromMaybe (error $ "storeRead: " ++ show p) $ IM.lookup p h

storeWrite :: Heap -> Ptr -> Value -> Heap
storeWrite h p v = IM.insert p v h

addStore :: Expr -> Expr
addStore e = Store storeEmpty e

dropStore :: Expr -> Expr
dropStore (Store _ e) | hasNoStoreOps e = e
dropStore e = e

hasNoStoreOps :: Expr -> Bool
hasNoStoreOps e = null [ () | Op o <- universe e, isStoreOp o ]

isNonStore :: Expr -> Bool
isNonStore Store{} = False
isNonStore Fail = False
isNonStore (EXI _ e) = isNonStore e
isNonStore e = not (isResult e)

isResult :: Expr -> Bool
isResult (v :|: _) = isVal v
isResult v = isVal v

isStoreFree :: Expr -> Bool
isStoreFree Val{}   = True
isStoreFree (Val{} :=: b) = isStoreFree b
isStoreFree (a :>: b) = isStoreFree a && isStoreFree b
isStoreFree (One e)   = isStoreFree e
isStoreFree (All e)   = isStoreFree e
isStoreFree (Op op :@: _) = not (isStoreOp op)
isStoreFree (Split e _ _) = isStoreFree e
isStoreFree Wrong     = True
isStoreFree (EXI _ e) = isStoreFree e
isStoreFree _         = False

isStoreOp :: Op -> Bool
isStoreOp Alloc = True
isStoreOp Read = True
isStoreOp Write = True
isStoreOp AddTo = True
isStoreOp _ = False

storeX, storeX1 :: Expr -> [(Context, Expr)]
-- S context
storeX lhs = storeX1 lhs ++ [(id,lhs)]
-- S context, S /= hole
storeX1 One{} = error "storeX: one"
storeX1 All{} = error "storeX: all"
storeX1 lhs =
  do Val v :=: sx <- [lhs]
     (ctx, hole) <- storeX sx
     pure ((v :=:) . ctx, hole)
 ++
  do sx :>: e <- [lhs]
     (ctx, hole) <- storeX sx
     pure ((:>: e) . ctx, hole)
 ++
  do se :>: sx <- [lhs]
     guard (isStoreFree se)
     (ctx, hole) <- storeX sx
     pure ((se :>:) . ctx, hole)
{-
 ++
  do Exi (Bind x sx) <- [lhs]
     (ctx, hole) <- storeX sx
     pure (Exi . Bind x . ctx, hole)
-}

rulesStore :: ERule
rulesStore _ lhs =
  "REF-ALLOC" `name`
  do Store h e <- [lhs]
     (ctx, Op Alloc :@: Val v) <- storeX e
     let (h', p) = storeAlloc h v
     pure (Store h' (ctx (Ref p)))
 ++
  "REF-READ" `name`
  do Store h e <- [lhs]
     (ctx, Op Read :@: Ref p) <- storeX e
     let v = storeRead h p
     pure (Store h (ctx v))
 ++
  "REF-WRITE" `name`
  do Store h e <- [lhs]
     (ctx, Op Write :@: Arr [Ref p, Val v]) <- storeX e
     let h' = storeWrite h p v
     pure (Store h' (ctx (Arr [])))
 ++
  "ST-SPLIT-DUP" `name`
  do Store h e <- [lhs]
     (ctx, Split oe f g) <- storeX e
     guard (isNonStore oe)
     pure (Store h (ctx (Split (Store h oe) f g)))
 ++
  "ST-CHOICE-DUP" `name`
  do Store h ee <- [lhs]
     (ctx, oe :|: e) <- storeX ee
     guard (isChoiceFree oe)
     guard (isNonStore oe)
     --traceM $ "ST-CHOICE-DUP " ++ show oe
     pure (Store h (ctx (Store h oe :|: e)))
 ++
  "ST-SPLIT" `name`
  do Store _ e <- [lhs]
     (ctx, Split (Store h w) f g) <- storeX e
     guard (isResult w)
     pure (Store h (ctx (Split w f g)))
 ++
  "ST-CHOICE" `name`
  do Store _ ee <- [lhs]
     (ctx, Store h w :|: e) <- storeX ee
     guard (isResult w)
     pure (Store h (ctx (w :|: e)))
{-
 ++
  "ST-FAIL" `name`
  do Store _ Fail <- [lhs]
     pure Fail
-}
 ++
  "REF-ADDTO" `name`
  do Store h e <- [lhs]
     (ctx, Op AddTo :@: Arr [Ref p, Int i]) <- storeX e
     Int j <- [storeRead h p]
     let h' = storeWrite h p v
         v = Int (j + i)
     pure (Store h' (ctx v))

