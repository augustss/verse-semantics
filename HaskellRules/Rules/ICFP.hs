{-# OPTIONS_GHC -Wno-incomplete-uni-patterns -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Rules.ICFP(allSystemsICFP, isRecursive, anf, anfK, execX, ltExpr) where
import Control.Monad( guard )
import Data.List
import Data.Maybe

import Epic.Print hiding ((<>))
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
allSystemsICFP = [ systemICFP,
                   systemICFPC,
                   systemICFPSX,
                   systemICFPSXC,
                   systemICFPK,
                   systemICFPE,
                   systemICFPF,
                   systemICFPJ,
                   systemICFPR,
                   systemICFPP,
                   systemICFPS,
                   systemICFPBX,
                   systemICFPBXP,
                   systemICFPBXS
                 ]

systemICFP :: TRSystem Expr
systemICFP = TRSystem
  { sname               = "ICFP"
  , description         = "ICFP from verse-icfp23/rewrites.ltx"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check valid . anf)
  , postProcess         = const id
  , rules               = (allRules -= "SEQ-SWAP") <> rulesSimonSwap
  , rules2              = noRules
  , rulesHaveStructural = True
  , confluenceRules     = noRules
  , validExpr           = const valid
  }

systemICFPC :: TRSystem Expr
systemICFPC = systemICFP
  { sname               = "ICFPC"
  , description         = "ICFP with and Simon's extra choice rule"
  , rules               = rules systemICFP <> rulesValEqualsChoice
  }

systemICFPSXC :: TRSystem Expr
systemICFPSXC = systemICFP
  { sname               = "ICFPSXC"
  , description         = "ICFP with a simplified substitution rule and context, and Simon's choice rule"
  , rules               = (rules systemICFP -= "SUBST" -= "EQN-ELIM") <> rulesSubstX <> rulesValEqualsChoice
  }

systemICFPSX :: TRSystem Expr
systemICFPSX = systemICFP
  { sname               = "ICFPSX"
  , description         = "ICFP with a simplified substitution rule and context"
  , rules               = (rules systemICFP -= "SUBST" -= "EQN-ELIM") <> rulesSubstX
  }

systemICFPK :: TRSystem Expr
systemICFPK = systemICFP
  { sname               = "ICFPK"
  , description         = "ICFP simplified by removing 'eq'"
  , preProcess          = const (check validK . anfK)
  , rules               = rules systemICFP -= "SEQ-ASSOC" -= "VAL-ELIM"
  , validExpr           = const validK
  }

systemICFPBX :: TRSystem Expr
systemICFPBX = s
  { sname               = "ICFPBX"
  , description         = "ICFP with BX for SUBST and EXI-FLOAT, no EXI-SWAP, SEQ-SWAP1"
  , rules               = (allRules -= "EXI-SWAP" -= "EXI-FLOAT" -= "SUBST" -= "SEQ-SWAP") <> rulesExiFloatBX <> rulesSeqSwap1 <> rulesSubstBX
  -- , rules               = (rules s -= "EXI-SWAP" -= "EXI-FLOAT") <> rulesExiFloatBX
  }
  where s = systemICFP

systemICFPBXS :: TRSystem Expr
systemICFPBXS = s
  { sname               = "ICFPBXS"
  , description         = description s ++ ", BX for SUBST"
  , rules               = (rules s -= "SUBST") <> rulesSubstBX
  }
  where s = systemICFPBX

systemICFPBXP :: TRSystem Expr
systemICFPBXP = s
  { sname               = "ICFPBXP"
  , description         = description s ++ ", Simon's SEQ-SWAP and SUBST"
  , rules               = rules s -= "SUBST" -= "SEQ-SWAP" -= "SEQ-SWAP-ORD" <> rulesSimonSwap <> rulesSimonSubst
  }
  where s = systemICFPBX

-- A system without the structural rules.
-- Without EXI-SWAP we need a more powerful EXI-ELIML.
-- These rules are useful for regression tests, since the ICFP rules are very slow.
systemICFPE :: TRSystem Expr
systemICFPE = s
  { sname = "ICFPE"
  , description = description s ++ " - EXI-SWAP - SEQ-SWAP"
  , rules = rules s -= "EXI-SWAP" -= "SEQ-SWAP" -= "EXI-ELIML" <> ruleElimL
  , rulesHaveStructural = False
  }
  where s = systemICFP

systemICFPP :: TRSystem Expr
systemICFPP = s
  { sname = "ICFPP"
  , description = description s ++ ", Simon's SUBST"
  , rules = rules s -= "SUBST" <> rulesSimonSubst
  }
  where s = systemICFPJ

systemICFPJ :: TRSystem Expr
systemICFPJ = s
  { sname = "ICFPJ"
  , description = description s ++ ", Simon's SEQ-SWAP"
  , rules = rules s -= "SEQ-SWAP" -= "SEQ-SWAP-ORD" <> rulesSimonSwap
  }
  where s = systemICFP

systemICFPF :: TRSystem Expr
systemICFPF = s
  { sname               = "ICFPF"
  , description         = description s ++ ", - FAIL-ELIM + FAIL-ELIM1 + FAIL-ELIM2"
  , rules               = (rules s -= "FAIL-ELIM") <> rulesFailElim12
  }
  where s = systemICFP

systemICFPR :: TRSystem Expr
systemICFPR = s
  { sname = "RICFP"
  , description = description s ++ " + SUBST-REC"
  , rules = rules s <> rulesSubstRec
  , rulesHaveStructural = False
  }
  where s = systemICFPE

systemICFPS :: TRSystem Expr
systemICFPS = s
  { sname = "ICFPS"
  , description = description s ++ ", store"
  , rules = rules s <> rulesStore
  , preProcess = \ e -> addStore . preProcess s e
  , postProcess = const dropStore
  }
  where s = systemICFPE

-- Check that an expression is in the subset defined by the ICFP (PLDI) grammar.
valid, validK :: Expr -> Bool
valid  = valid' False
validK = valid' True

valid' :: Bool -> Expr -> Bool
valid' onlyEq = expr
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
    expr (Assume e) = expr e
    expr (Assert e) = expr e
    expr (Verify e) = expr e
    expr Fail = True
    expr Wrong{} = True
    expr (Split e (LAM _ e1) (LAM _ (LAM _ (LAM _ e2)))) =
      expr e && expr e1 && expr e2
    expr (Split e (LAM _ e1) Var{}) =
      expr e && expr e1
    expr e@Split{} = error $ "malformed split: " ++ prettyShow e
    expr (If e1 e2 e3) = expr e1 && expr e2 && expr e3
    expr e = error $ "valid: unexpected " ++ show e
    expru (v :=: e) = value v && expr e
    expru e = not onlyEq && expr e
    value Var{} = True
    value e = hnf e
    hnf Int{} = True
    hnf Op{}  = True
    hnf (Arr vs) = all value vs
    hnf (LAM _ e) = expr e
    hnf _ = False

-- Make the expression obey the ICFP (PLDI) grammar,
-- i.e., valid (anf e) == True
anf, anfK :: Expr -> Expr
anf  = anf' False
anfK = anf' True

anf' :: Bool -> Expr -> Expr
anf' onlyEq = expr
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
        e'@(v :=: _) -> e' :>: v
        e'           -> e'
    expr (_ :~: _) = error "anf: impossible"
    expr (e1 :>: e2) =
      case (expru e1, expr e2) of
        (e1', e2')
          | not onlyEq || isEq e1' -> e1' :>: e2'
          | otherwise              -> EXI x $ (Var x :=: e1') :>: e2'
         where x = identNotIn (free (e1',e2'))
               isEq (_ :=: _) = True
               isEq _         = False
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
    expr (Assume e) = Assume $ expr e
    expr (Assert e) = Assert $ expr e
    expr (Verify e) = Verify $ expr e
    expr e@Fail = e
    expr e@Wrong{} = e
    expr (Split e e1 e2) =
      let i1:i2:_ = identsNotIn (free (Split e e1 e2))
          (ds1, v1) = value i1 e1
          (ds2, v2) = value i2 e2
          ds = ds1 ++ ds2
      in  binds ds (Split (expr e) v1 v2)
    expr (If e1 e2 e3) = If (expr e1) (expr e2) (expr e3)
    expr e = error $ "anf: " ++ prettyShow e

    expru (e1 :=: e2) =
      case (expr e1, expr e2) of
        (v@Val{}, e2') -> v :=: e2'
        (e1',     e2') -> EXI x $ (Var x :=: e1') :>: (Var x :=: e2') :>: Var x
          where x = identNotIn (free (e1',e2'))
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
{-
 ++
  do EXI y x <- [lhs]
     (ctx, hole) <- execX x
     pure (EXI y . ctx, hole)
-}
 ++
  do Store h e <- [lhs]
     (ctx, hole) <- execX e
     pure (Store h . ctx, hole)
 ++ -- extra rule for verifier
  do (Assume x) :>: e <- [lhs]
     (ctx, hole) <- execX x
     pure ( \ a -> Assume (ctx a) :>: e, hole)

substX :: Expr -> [(Context, Expr)]
-- X context
substX lhs =
  [(id,lhs)]
 ++
  do (v :=: e) :>: ex <- [lhs]
     (ctx, hole) <- substX ex
     pure (((v :=: e) :>:) . ctx, hole)

-- X context additionally descending under EXI and returning boundVars at hole
execBX, execBX1 :: Expr -> [(Context, [Ident], Expr)]
-- X context
execBX lhs = execBX1 lhs ++ [(id, [], lhs)]
-- X context, X /= hole
execBX1 lhs =
  do (v :=: x) :>: e <- [lhs]
     (ctx, bs, hole) <- execBX x
     pure (\ a -> (v :=: ctx a) :>: e, bs, hole)
 ++
  do x :>: e <- [lhs]
     (ctx, bs, hole) <- execBX x
     pure ((:>: e) . ctx, bs, hole)
 ++
  do e :>: x <- [lhs]
     (ctx, bs, hole) <- execBX x
     pure ((e :>:) . ctx, bs, hole)
 ++
  do Store h e <- [lhs]
     (ctx, bs, hole) <- execBX e
     pure (Store h . ctx, bs, hole)
 ++
  do EXI x e <- [lhs]
     (ctx, bs, hole) <- execBX e
     pure (EXI x . ctx, x:bs, hole)



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
 ++
  do Assert hole <- [lhs]
     choices Assert hole

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
isChoiceFree (Split _ (LAM _ f) (LAM _ (LAM _ (LAM _ g)))) = isChoiceFree f && isChoiceFree g
isChoiceFree (Split _ (LAM _ f) (Var _)) = isChoiceFree f
isChoiceFree e@Split{} = error $ "bad split: " ++ prettyShow e
isChoiceFree Wrong{}   = True
isChoiceFree (EXI _ e) = isChoiceFree e  -- necessary when using split
isChoiceFree _         = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp DotDot = False
isChoiceFreeOp Append = False   -- An approximation
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

--------------------------------------------------------------------------------

allRules :: ERule
allRules =  rulesApplication
         <> rulesUnification
         <> rulesElimination
         <> rulesNormalization
         <> rulesChoice
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
 ++
  "APP-DOTDOT" `name`
  do Op DotDot :@: Arr [Int lo, Int hi] <- [lhs]
     pure (foldr (:|:) Fail (map Int [lo .. hi]))

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
rulesApplication env lhs =
  "APP-BETA" `name`
  do LAM x e :@: Val v <- [lhs]
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
  do (Int k1 :=: Int k2) :>: e <- [lhs]
     guard (k1 == k2)
     pure e
 ++
  "U-REF" `name`
  do (Ref k1 :=: Ref k2) :>: e <- [lhs]
     guard(k1 == k2)
     pure e
 ++
  "U-TUP" `name`
  do (Arr vs :=: Arr vs') :>: e <- [lhs]
     guard (length vs == length vs')
     pure (foldr (:>:) e [ Val v :=: Val v' | (v,v') <- vs `zip` vs' ])
 ++
  "U-FAIL" `name`
  do (HNF e1 :=: HNF e2) :>: _ <- [lhs]
     -- Avoid the cases handled above
     guard (case (e1,e2) of (Int k1,Int k2) -> k1 /= k2
                            (Ref k1,Ref k2) -> k1 /= k2
                            (Arr a1,Arr a2) -> length a1 /= length a2
                            _               -> True)
     pure Fail
 ++
   "U-OCCURS" `name`
   do (Var x :=: Val v) :>: _ <- [lhs]
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
     -- guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (subst sub (ctx ((Var x0 :=: Val v) :>: e)))
 ++
  "HNF-SWAP" `name`
  do (hnf@HNF{} :=: x@Var{}) :>: e <- [lhs]
     pure ((x :=: hnf) :>: e)
 ++
  "VAR-SWAP" `name`
  do y@Var{} :=: x@Var{} <- [lhs]
     guard (ltExpr env x y)
     pure (x :=: y)

rulesSubstX :: ERule
rulesSubstX env lhs =
  "SUBST-X" `name`
  do (ctx, (Var x :=: Val v) :>: e) <- substX lhs
     let freeX = free (ctx, e)
         freeV = free v
     let x0    = identNotIn (freeX ++ freeV) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (subst sub (ctx ((Var x0 :=: Val v) :>: e)))
 ++
 "EQN-ELIM-S" `name`
  do EXI x a <- [lhs]
     (ctx, (Var x' :=: Val v) :>: e) <- substX a
     guard (x == x')
     guard (x `notElem` free (ctx (v :>: e)))
     pure (ctx e)

rulesSubstBX :: ERule
rulesSubstBX env lhs =
  "SUBST-BX" `name`
  do (ctx, xBoundVars, (Var x :=: Val v) :>: e) <- execBX lhs
     let freeX = free (ctx, e)
         freeV = free v
     -- let allBoundVars = {- boundVars env ++ -} xBoundVars
     let x0    = identNotIn (freeX ++ freeV ++ xBoundVars) -- replacing x temporarily
         sub   = [(x, v),(x0, Var x)]
     guard (x `elem` freeX)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     guard (all (`notElem` xBoundVars) (x:freeV))          -- NEW PRECONDITION
     pure (subst sub (ctx ((Var x0 :=: Val v) :>: e)))


-- rulesSeqSwap1 is the same as rulesSimonSwap
--   e1; x=v; e3   -->   x=v; e1; e3    if not (eq is (y=v2) and y<x)
rulesSeqSwap1 :: ERule
rulesSeqSwap1 env lhs =
  "SEQ-SWAP1" `name`
  do e1 :>: (e2@(Var x :=: Val _) :>: e3) <- [lhs]
     guard (case e1 of Var y :=: Val _ -> not (ltExpr env (Var y) (Var x)); _ -> True)
     pure $ e2 :>: (e1 :>: e3)

rulesSimonSwap :: ERule
rulesSimonSwap env lhs =
  "SEQ-SWAP-SIMON" `name`
  do e1 :>: (e2@(Var x :=: Val _) :>: e3) <- [lhs]
     guard (case e1 of Var y :=: Val _ -> not (ltExpr env (Var y) (Var x)) && x /= y; _ -> True)

{-
     -- This side condition is not confluent, see tricky:QC11
     guard (ltExpr env e2 e1)
     -- This side condition has the same problem
     guard $
       -- First, order by choice-free-ness;
       -- choice free goes first
       case isEffFree e1 of
         False  -> True   -- put ce before e
         True   ->
           -- Next, order so equations go before expressions.
           -- (This is an arbitrary choice)
           case isEqn e1 of
             False  -> True              -- need to swap
             True   -> ltExpr env e2 e1  -- use ordering
-}
     pure $ e2 :>: (e1 :>: e3)

rulesSimonSubst :: ERule
rulesSimonSubst env lhs =
  "SUBST-SIMON" `name`
  do eq@(Var x :=: Val v) :>: e <- [lhs]
     let freeV = free v
         freeE = free e
         sub   = [(x, v)]
     guard (x `elem` freeE)
     guard (x `notElem` freeV)
     guard (case v of Var y -> ltExpr env (Var x) (Var y); _ -> True)
     pure (eq :>: subst sub e)

_rulesSeqSwapOrd :: ERule
_rulesSeqSwapOrd env lhs =
  "SEQ-SWAP-ORD" `name`
  do e1 :>: (e2 :>: e3) <- [lhs]
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

_rulesVarSwapSubst :: ERule
_rulesVarSwapSubst env lhs =
  "VAR-SWAP-SUBST" `name`
  do (ctx, Var x :=: Var y) <- execX lhs
     guard (ltExpr env (Var y) (Var x))
     let y0 = identNotIn (free (ctx Fail, y, x))
         sub = [(y, Var x), (y0, Var y)]
     pure (subst sub (ctx (Var y0 :=: Var x)))

isEqn :: Expr -> Bool
isEqn (_ :=: _) = True
isEqn _ = False

-- Compare two expression using lessThan for identifiers
ltExpr :: TRSFlags -> Expr -> Expr -> Bool
ltExpr env e1 e2 = comp vs vs e1 e2 == LT
  where
    vs = boundVars env

rulesSubstRec :: ERule
rulesSubstRec _ lhs =
  "SUBST-REC" `name`
  do Var x :=: Val v <- [lhs]
     (ctx, LAM y e) <- valueX v
     guard (x `elem` free (LAM y e))
     pure (Var x :=: Val (ctx (LAM y (Exi (Bind x (lhs :>: e))))))

--------------------------------------------------------------------------------

rulesElimination :: ERule
rulesElimination _ lhs =
  "VAL-ELIM" `name`
  do Val _ :>: e <- [lhs]
     pure e
 ++
  "EXI-ELIM" `name`
  do EXI x e <- [lhs]
     guard (x `notElem` free e)
     pure e
 ++
  "EQN-ELIM" `name`
  do EXI x a <- [lhs]
     (ctx, (Var x' :=: Val v) :>: e) <- execX a
     guard (x == x')
     guard (x `notElem` free (ctx (v :>: e)))
     pure (ctx e)
 ++
  "FAIL-ELIM" `name`
  do (_cx, Fail) <- execX1 lhs
     pure Fail

rulesFailElim12 :: ERule
rulesFailElim12 _ lhs =
  "FAIL-ELIM1" `name`
  do Fail :>: _ <- [lhs]
     pure Fail
 <>
  "FAIL-ELIM2" `name`
  do (_ :=: Fail) :>: _ <- [lhs]
     pure Fail

ruleElimL :: ERule
ruleElimL _ lhs =
  "EXI-ELIML-OLD" `name`
  do EXI x a <- [lhs]
     (ctx, Var x' :=: Val v) <- defX x a
     guard (x == x')
     let freeX = free ctx
         freeV = free v
     guard (x `notElem` freeX)
     guard (x `notElem` freeV)
     pure (ctx (Val v))

-- X context, or exist x . defX
defX :: Ident -> Expr -> [(Context, Expr)]
defX xx lhs =
  do execX lhs
 ++
  do Exi (Bind x dx) <- [lhs]
     guard (x /= xx)
     (ctx, hole) <- defX xx dx
     return (Exi . Bind x . ctx, hole)

--------------------------------------------------------------------------------

rulesNormalization :: ERule
rulesNormalization _ lhs =
  "EXI-FLOAT" `name`
  do (ctx, EXI x e) <- execX1 lhs  -- Note: Store not allowed in ctx
     guard (hasStore (ctx Fail) <= isChoiceFree e)  -- <= is implication for booleans
     let freeX = free ctx
         x'    = identNotIn (freeX ++ free e)
     if x `elem` freeX
       then pure (EXI x' (ctx (subst [(x,Var x')] e)))
       else pure (EXI x (ctx e))
 ++
  "SEQ-ASSOC" `name`
  do (e1 :>: e2) :>: e3 <- [lhs]
     pure (e1 :>: (e2 :>: e3))
 ++
  "EQN-FLOAT" `name`
  do (Val v :=: (eq :>: e1)) :>: e2 <- [lhs]
     pure (eq :>: ((Val v :=: e1) :>: e2))
 ++
  "EXI-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     pure (EXI y (EXI x e))
 ++
  "SEQ-SWAP" `name`
  do e1 :>: (e2 :>: e3) <- [lhs]
     -- Don't reorder effects
     guard (isEffFree e1 || isEffFree e2)
     pure $ e2 :>: (e1 :>: e3)

rulesExiFloatBX :: ERule
rulesExiFloatBX _ lhs =
  "EXI-FLOAT-BX" `name`
  do (ctx, bVars, EXI x e) <- execBX1 lhs  -- Note: Store not allowed in ctx
     guard (hasStore (ctx Fail) <= isChoiceFree e)  -- <= is implication for booleans
     guard (x `notElem` bVars)
     let freeX = free ctx
         x'    = identNotIn (freeX ++ free e ++ bVars)
     if x `elem` freeX
       then pure (EXI x' (ctx (subst [(x,Var x')] e)))
       else pure (EXI x (ctx e))

--------------------------------------------------------------------------------

rulesChoice :: ERule
rulesChoice _ lhs =
  "CHOOSE" `name`
  do (sx, e)         <- scopeX lhs
     (cx, e1 :|: e2) <- choiceX1 e
     pure (sx (cx e1 :|: cx e2))
 ++
  "CHOOSE-ASSOC" `name`
  do (e1 :|: e2) :|: e3 <- [lhs]
     pure (e1 :|: (e2 :|: e3))
 ++
  "CHOOSE-R" `name`
  do Fail :|: e <- [lhs]
     pure e
 ++
  "CHOOSE-L" `name`
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

rulesValEqualsChoice :: ERule
rulesValEqualsChoice _ lhs =
  "VAL-EQU-CHOICE" `name`
  do Val v :=: (e1 :|: e2) <- [lhs]
     pure (((v :=: e1) :>: v) :|: ((v :=: e2) :>: v))

--------------------------------------------------------------------------------

rulesSplit :: ERule
rulesSplit _ lhs =
  "SPLIT-FAIL" `name`
  do Split Fail (LAM x f) _g <- [lhs]
     pure (subst [(x, unit)] f)
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) _f g@(LAM x (LAM k (LAM h b))) <- [lhs]
     pure $ doSplit lhs v e g x k h b
 ++
  "SPLIT-VALUE" `name`
  do Split (Val v) _f g@(LAM x (LAM k (LAM h b))) <- [lhs]
     pure $ doSplit lhs v Fail g x k h b

doSplit :: Expr -> Expr -> Expr -> Expr -> Ident -> Ident -> Ident -> Expr -> Expr
doSplit lhs v e g x k h b =
  let u = identNotIn (free lhs)
  in  subst [(x, v), (k, LAM u e), (h, g)] b

{-
  "SPLIT-FAIL" `name`
  do Split Fail f _g <- [lhs]
     pure (f :@: unit)
 ++
  "SPLIT-CHOICE" `name`
  do Split (Val v :|: e) _f g <- [lhs]
     pure $ doSplit lhs v e g
 ++
  "SPLIT-VALUE" `name`
  do Split (Val v) _f g <- [lhs]
     pure $ doSplit lhs v Fail g

doSplit :: Expr -> Value -> Expr -> Value -> Expr
doSplit lhs v e g =
--  trace ("doSplit " ++ prettyShow (lhs, v, e, g)) $
  let x:h:t:_ = identsNotIn (free lhs)
      gv = Var h :=: (g :@: v)
      hlam = Var t :=: (Var h :@: LAM x e)
      res = Var t :@: g
  in  EXI h (EXI t (gv :>: hlam :>: res))
-}

--------------------------------------------------------------------------------

storeEmpty :: Heap
storeEmpty = IM.empty

storeAlloc :: Heap -> Value -> (Heap, Ptr)
storeAlloc h v =
  let p | IM.null h = Ptr 0
        | otherwise = succ (fst (IM.findMax h))
      h' = IM.insert p v h
  in  (h', p)

storeRead :: Heap -> Ptr -> Value
storeRead h p = fromMaybe (error $ "storeRead: " ++ show p) $ IM.lookup p h

storeWrite :: Heap -> Ptr -> Value -> Heap
storeWrite h p v = IM.insert p v h

addStore :: Expr -> Expr
addStore e = Store storeEmpty e

-- If there are no store operations, drop the store
-- and any existentials that are no longer needed.
dropStore :: Expr -> Expr
dropStore ee | hasStoreOps ee = ee
            | otherwise = drops ee
  where drops (Store _ e) = e
        drops (EXI x e) | x `elem` free e' = EXI x e'
                        | otherwise = e'
          where e' = drops e
        drops e = e

hasStoreOps :: Expr -> Bool
hasStoreOps e = not $ null [ () | Op o <- universe e, isStoreOp o ]

hasStore :: Expr -> Bool
hasStore e = not $ null [ () | Store{} <- universe e ]

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
isStoreFree (_ :=: b) = isStoreFree b
isStoreFree (a :>: b) = isStoreFree a && isStoreFree b
isStoreFree (a :|: b) = isStoreFree a && isStoreFree b
isStoreFree (Op op :@: _) = not (isStoreOp op)
isStoreFree (One e)   = isStoreFree e
isStoreFree (All e)   = isStoreFree e
isStoreFree (Split e (LAM _ f) (LAM _ (LAM _ (LAM _ g)))) = isStoreFree e && isStoreFree f && isStoreFree g
isStoreFree (Split (Var _ :@: Arr []) (LAM _ f) (Var _)) = isStoreFree f
isStoreFree e@Split{} = error $ "bad split: " ++ prettyShow e
isStoreFree Wrong{}   = True
isStoreFree (EXI _ e) = isStoreFree e
isStoreFree _         = False

isStoreOp :: Op -> Bool
isStoreOp Alloc = True
isStoreOp Read = True
isStoreOp Write = True
isStoreOp AddTo = True
isStoreOp _ = False

storeX, storeX1 :: Expr -> [(Context, [Ident], Expr)]
-- S context
storeX lhs = storeX1 lhs ++ [(id, [], lhs)]
-- S context, S /= hole
storeX1 One{} = error "storeX: one"
storeX1 All{} = error "storeX: all"
storeX1 lhs =
  do Val v :=: sx <- [lhs]
     (ctx, is, hole) <- storeX sx
     pure ((v :=:) . ctx, is, hole)
 ++
  do sx :>: e <- [lhs]
     (ctx, is, hole) <- storeX sx
     pure ((:>: e) . ctx, is, hole)
 ++
  do se :>: sx <- [lhs]
     guard (isEffFree se)
     (ctx, is, hole) <- storeX sx
     pure ((se :>:) . ctx, is, hole)
 ++
  do EXI x sx <- [lhs]
     (ctx, is, hole) <- storeX sx
     pure (EXI x . ctx, x:is, hole)

rulesStore :: ERule
rulesStore _ lhs =
  "REF-ALLOC" `name`
  do Store h e <- [lhs]
     (ctx, is, Op Alloc :@: Val v) <- storeX e
     guard (null (intersect is (free v)))
     let (h', p) = storeAlloc h v
     pure (Store h' (ctx (Ref p)))
 ++
  "REF-READ" `name`
  do Store h e <- [lhs]
     (ctx, is, Op Read :@: Ref p) <- storeX e
     let v = storeRead h p
         ctx' = ctxAlpha v is ctx
     pure (Store h (ctx' v))
 ++
  "REF-WRITE" `name`
  do Store h e <- [lhs]
     (ctx, is, Op Write :@: Arr [Ref p, Val v]) <- storeX e
     guard (null (intersect is (free v)))
     let h' = storeWrite h p v
     pure (Store h' (ctx (Arr [])))
 ++
  "ST-SPLIT-DUP" `name`
  do Store h e <- [lhs]
     (ctx, is, Split oe f g) <- storeX e
     guard (not (isResult oe) && oe /= Fail)
     guard (isNonStore oe)
     let (ctx', oe', f', g') = ctxAlpha h is (ctx, oe, f, g)
     pure (Store h (ctx' (Split (Store h oe') f' g')))
 ++
  "ST-CHOICE-DUP" `name`
  do Store h (oe :|: e) <- [lhs]
     guard (not (isResult oe) && oe /= Fail)
     guard (isNonStore oe)
     pure (Store h (Store h oe :|: e))
 ++
  "ST-SPLIT" `name`
  do Store _ e <- [lhs]
     (ctx, is, Split (Store h w) f g) <- storeX e
     guard (null (intersect is (free h)))
     guard (isResult w)
     pure (Store h (ctx (Split w f g)))
 ++
  "ST-CHOICE" `name`
  do Store _ ee <- [lhs]
     (ctx, is, Store h w :|: e) <- storeX ee
     guard (null (intersect is (free h)))
     guard (isResult w)
     pure (Store h (ctx (w :|: e)))
 ++
  "REF-ADDTO" `name`
  do Store h e <- [lhs]
     (ctx, _, Op AddTo :@: Arr [Ref p, Int i]) <- storeX e
     Int j <- [storeRead h p]
     let h' = storeWrite h p v
         v = Int (j + i)  -- No free vars
     pure (Store h' (ctx v))
{-
 ++
  "ST-FAIL" `name`
  do Store _ Fail <- [lhs]
     pure Fail
-}

ctxAlpha :: (Free a) => a -> [Ident] -> b -> b
ctxAlpha e is ctx | null (intersect (free e) is) = ctx
                  | otherwise = error "unimplemented"
