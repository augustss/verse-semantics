{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{- # LANGUAGE FlexibleInstances # -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Rules.Block(allSystemsBlock) where
import Control.Monad.State.Strict
import Data.List(delete)
import Epic.List(pick, pickLR)
import TRS.Bind
import TRS.System
import TRS.TRS
import Rules.Core
--import Data.Functor.Classes (Show1(liftShowList))
--import Debug.Trace

--------------------------------------------------------------------------------

allSystemsBlock :: [TRSystem Expr]
allSystemsBlock = [ systemBlock ]

systemBlock :: TRSystem Expr
systemBlock = TRSystem
  { sname               = "Block"
  , description         = "Block"
  , ruleEnv             = defaultTRSFlags
  , preProcess          = const (check valid . anf)
  , postProcess         = const id
  , rules               = allRules
  , rules2              = rulesOcc
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = const valid
  }

{-
v ::= x | hnf
hnf ::= k | <v,...> | \x.b | op
b ::= fail | (b || b) | exist x*; (v=e)*; v
e ::= v | v(v) | b | one{b} | all{b} | split{b,v,v}
-}

pattern BFail :: Expr
pattern BFail = BlockC Fail

pattern BVal :: Value -> Expr
pattern BVal v = BlockC (Val v)

-- Check that an expression is in the subset defined by the Block grammar.
valid :: Expr -> Bool
valid = block
  where
    expr e@Val{} = value e
    expr (e1 :|: e2) = block e1 && block e2
    expr (e1 :@: e2) = value e1 && value e2
    expr (One e) = block e
    expr (All e) = block e
    expr Fail = True
    expr Wrong = True
    expr (Split e v1 v2) = block e && value v1 && value v2
--    expr (BlockC b) = block b
    expr _ = False
    value Var{} = True
    value e = hnf e
    hnf Int{} = True
    hnf Op{}  = True
    hnf (Arr vs) = all value vs
    hnf (LAM _ e) = block e
    hnf _ = False
    block (BlockC b) = block' b
    block _ = False
    block' (EXI _ b) = block' b
    block' b = block'' b
    block'' ((v :=: e) :>: b) = value v && expr e && block'' b
    block'' Fail = True  -- Block [] [] Fail is ok
    block'' b = value b
--    block'' b = expr b

type A a = State (([Ident], [Eqn]), [Ident]) a

-- Make the expression obey the Block grammar,
-- i.e., valid (anf e) == True
anf :: Expr -> Expr
anf ee = foo $ evalState (block ee) (undefined, allVars ee)
  where
    foo xx = --trace (show ee) $ trace (show xx)
             xx
    expr :: Expr -> A Expr
    expr e@Var{} = pure e
    expr e@Int{} = pure e
    expr e@Op{}  = pure e
    expr (Arr es) = Arr <$> mapM value es
    expr (LAM x e) = LAM x <$> blockV [x] e
    expr (e1 :=: e2) = do
      v <- value e1
      e <- expr e2
      addEqn (v, e)
      pure v
    expr (e1 :>: e2) = addExpr e1 *> expr e2
    expr (e1 :|: e2) = (:|:) <$> block e1 <*> block e2
    expr (e1 :@: e2) = (:@:) <$> value e1 <*> value e2
    expr (EXI i e) = do s <- addExist i; expr (subst s e)
    expr (One e) = One <$> block e
    expr (All e) = All <$> block e
    expr e@Fail = pure e
    expr e@Wrong = pure e
    expr (Split e e1 e2) = Split <$> expr e <*> value e1 <*> value e2
    expr e = error $ "anf: impossible: " ++ show e

    value :: Expr -> A Value
    value e@Var{} = pure e
    value e@Int{} = pure e
    value e@Op{}  = pure e
    value (Arr es) = Arr <$> mapM value es
    value (LAM x e) = LAM x <$> blockV [x] e
    -- Handle some constructors specially to avoid (harmless) nonsense bindings
    value (e1 :>: e2) = addExpr e1 *> value e2
    value (EXI i e) = do s <- addExist i; value (subst s e)
    value e = Var <$> addExpr e

    block = blockV []

    -- Avoid the variable names vs
    blockV :: [Ident] -> Expr -> A Expr
    blockV vs e = do
      (old, is) <- get
      put ((vs, []), is)
      v <- value e
--      v <- expr e
      ((xs, bs), is') <- get
      put (old, is')
      pure $ Block (drop (length vs) $ reverse xs) (reverse bs) v

    addEqn :: Eqn -> A ()
    addEqn q = do
      ((xs, qs), is) <- get
      put ((xs, q:qs), is)

    addExist :: Ident -> A (Subst Expr)
    addExist x = do
      ((is, qs), vs) <- get
      if x `elem` is then do
        let y = identNotIn vs
        put ((y:is, qs), y : vs)
        pure [(x, Var y)]
       else do
        put ((x:is, qs), vs)
        pure []

    addExpr :: Expr -> A Ident
--    addExpr e | trace ("addExpr " ++ show e) False = undefined
    addExpr (Var x :=: e) = do
      e' <- expr e
      addEqn (Var x, e')
      pure x
    addExpr e = do
      e' <- expr e
      ((is, qs), vs) <- get
      let y = identNotIn vs
      put ((y:is, (Var y, e') : qs), y : vs)
      pure y
      
--------------------------------------------------------------------------------

-- sub-categories of expressions

isChoiceFree :: Expr -> Bool
isChoiceFree (Val _)   = True
isChoiceFree (One _)   = True
isChoiceFree (All _)   = True
isChoiceFree (Op op :@: _) = isChoiceFreeOp op
--isChoiceFree Split{}   = True  -- XXX is it?
isChoiceFree _         = False

isChoiceFreeOp :: Op -> Bool
isChoiceFreeOp MapAp = False
isChoiceFreeOp _ = True

--------------------------------------------------------------------------------
-- contexts

type Context = Expr -> Expr

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
         <> rulesUnification
         <> rulesSubst
         <> rulesDefElim
         <> rulesBlock
         <> rulesChoice
         <> rulesOne
         <> rulesAll
         <> rulesFail
--         <> rulesSplit

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
       Int _ -> pure (Arr [])
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
  in  Block xs (zipWith (\ x v -> (Var x, v :@: unit)) xs vs) (Arr $ map Var xs)

unit :: Value
unit = Arr []

-- Make bad uses of primitives go to FAIL
_rulesBadFail :: ERule
_rulesBadFail _ lhs =
  "OP-FAIL" `name`
  do Op op :@: HNF e <- [lhs]
     guard (not (opArgTC op e))
     pure Fail
 <>
  "AP-FAIL" `name`
  do HNF f :@: _ <- [lhs]
     guard (not (validFcn f))
     pure Fail

-- Check that an Op has a legal argument.
-- The argument is assumed to be in HNF.
-- If the argument is an array, the elements are not in HNF
-- so they need extra tests.
opArgTC :: Op -> Value -> Bool
opArgTC op =
  case op of
    Neg -> int                    -- Must be Int
    Plus -> int                   -- Must be Int
    IsInt -> any_                 -- Any type is allowed
    MapAp -> arr lam              -- Used internally: takes an array of thunks
    Cons -> pair any_ (arr any_)  -- Must be anything, array
    _ -> pair (hnf int) (hnf int) -- Must be Int, Int
  where int Int{} = True          
        int _ = False
        any_ _ = True
        arr p (Arr vs) = all p vs
        arr _ _ = False
        lam Lam{} = True
        lam _ = False
        hnf p (HNF e) = p e  -- Check the predicate for HNF
        hnf _ _ = True       -- Assume OK if it's not HNF
        pair t1 t2 (Arr [e1, e2]) = t1 e1 && t2 e2
        pair _ _ _ = False

validFcn :: Expr -> Bool
validFcn Op{} = True
validFcn Arr{} = True
validFcn Lam{} = True
validFcn _ = False

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do LAM x (Block xs bs e) :@: v <- [lhs]
     let (xs'@(x' : _), bs', e') = alphaBlk (free v) (x:xs, bs, e)  -- Avoid capturing in v
     pure $ Block xs' ((Var x', v) : bs') e'
 ++
  "APP-TUP" `name`
  do Arr vs :@: v <- [lhs]
     if null vs then
       pure Fail
      else
       pure (foldr1 (:|:) [ Block [] [(Val v, Int i)] (Val vi) | (i,vi) <- [0..] `zip` vs ])

type Blk = ([Ident], [Eqn], Expr)

alphaBlk :: [Ident] -> Blk -> Blk
alphaBlk vs b@(xs, bs, e) =
  let is = identsNotIn (vs ++ free b)
      xs' = zipWith (\ x i -> if x `elem` vs then i else x) xs is
      sub = [ (x, Var x') | (x, x') <- zip xs xs', x /= x' ]
  in  --trace ("alphaBlk " ++ show sub) $
      (xs', subst sub bs, subst sub e)

rulesBlock :: ERule
rulesBlock _ lhs =
  "MERGE-BLK" `name`
  do Block oxs obs oe <- [lhs]
     (lbs, (ox, Block xs bs e), rbs) <- pickLR obs
     let (nxs, nbs, ne) = alphaBlk (oxs ++ free (lbs, ox, rbs)) (xs, bs, e)
     pure $ Block (oxs ++ nxs) (lbs ++ nbs ++ [(ox, ne)] ++ rbs) oe

--------------------------------------------------------------------------------

rulesSubst :: ERule
rulesSubst _ lhs =
  "SUBST" `name`
  do Block xs bs e <- [lhs]
     (lbs, b@(Var x, Val v), rbs) <- pickLR bs
     let freeB = free (lbs, rbs, e)
         freeV = free v
     guard (x `elem` freeB)
     guard (x `notElem` freeV)
     let x0    = identNotIn (freeB ++ freeV) -- replacing x temporarily
         sub   = [(x, v), (x0, Var x)]
         (lbs', rbs', e') = subst sub (lbs, rbs, e)
     pure (Block xs (lbs' ++ [b] ++ rbs') e')

--------------------------------------------------------------------------------

rulesDefElim :: ERule
rulesDefElim _ lhs =
  "DEF-ELIML" `name`
  do Block xs bs e <- [lhs]
     ((Var x, Val v), bs') <- pick bs
     guard (x `elem` xs)
     guard (x `notElem` free (bs', e, v))
     pure (Block (delete x xs) bs' e)
 ++
  "DEF-ELIMR" `name`
  do Block xs bs e <- [lhs]
     ((Val v, Var x), bs') <- pick bs
     guard (x `elem` xs)
     guard (x `notElem` free (bs', e, v))
     pure (Block (delete x xs) bs' e)
 ++
  "SWAP" `name`
  do Val (HNF hnf) :=: Var x <- [lhs]
     pure (Var x :=: Val hnf)
 ++
  "DEF-ELIM" `name`
  do Block xs bs e <- [lhs]
     (x, xs') <- pick xs
     guard (x `notElem` free (bs, e))
     pure (Block xs' bs e)
 ++
  "DEF-ELIMV" `name`
  do Block xs bs e <- [lhs]
     ((Var y, Var x), bs') <- pick bs
     guard (x `elem` xs)
     guard (x /= y)
     let sub = [(x, Var y)]
     pure $ Block (delete x xs) (subst sub bs') (subst sub e)

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification _ lhs =
  "ULIT" `name`
  do Block xs bs e <- [lhs]
     ((Int k1, Int k2), bs') <- pick bs
     if k1 == k2
       then pure (Block xs bs' e)
       else pure BFail
 ++
  "UTUP" `name`
  do Block xs bs e <- [lhs]
     (lbs, (Arr vs, Arr vs'), rbs) <- pickLR bs
     if length vs == length vs'
       then pure (Block xs (lbs ++ [ (v, v') | (v,v') <- vs `zip` vs' ] ++ rbs) e)
       else pure BFail
 ++
  "UX-LAM" `name`
  do Block _ bs _ <- [lhs]
     ((Lam{}, Lam{}), _) <- pick bs
     pure BFail
 ++
{-
  "UX-OP" `name`
  do Block _ bs _ <- [lhs]
     ((Op{}, Op{}), _) <- pick bs
     pure BFail
 ++
-}
  "UX-OP" `name`
  do Block xs bs e <- [lhs]
     ((Op o1, Op o2), bs') <- pick bs
     if o1 == o2
       then pure (Block xs bs' e)
       else pure BFail
 ++
  "UX" `name`
  do Block _ bs _ <- [lhs]
     ((HNF e1, HNF e2), _) <- pick bs
     -- Avoid the cases handled above, and fail for any unequal hnfs
     guard (case (e1,e2) of (Int{},Int{}) -> False
                            (Arr{},Arr{}) -> False
                            (Lam{},Lam{}) -> False
                            (Op{}, Op{})  -> False
                            _             -> True)
     guard (e1 /= e2)
     pure BFail

--------------------------------------------------------------------------------

rulesFail :: ERule
rulesFail _ lhs =
  "FAIL" `name`
  do Block _ bs _ <- [lhs]
     ((_, Fail), _) <- pick bs
     pure BFail

rulesOne :: ERule
rulesOne _ lhs =
  "ONE-FAIL" `name`
  do One BFail <- [lhs]
     pure Fail
 ++
  "ONE-CHOICE" `name`
  do One (BVal v :|: _e) <- [lhs]
     pure (Val v)
 ++
  "ONE-VAL" `name`
  do One (BVal v) <- [lhs]
     pure (Val v)

rulesAll :: ERule
rulesAll _ lhs =
  "ALL-FAIL" `name`
  do All BFail <- [lhs]
     pure (Arr [])
 ++
  "ALL-CHOICE" `name`
  do All ves@(_ :|: _) <- [lhs]
     let choiceVals (BVal v) = [[v]]
         choiceVals (BVal v :|: es) = [ v : vs | vs <- choiceVals es ]
         choiceVals _ = []
     vs <- choiceVals ves
     pure (Arr vs)
 ++
  "ALL-VAL" `name`
  do All (BVal v) <- [lhs]
     pure (Arr [v])

rulesChoice :: ERule
rulesChoice _ lhs =
  "FAIL-L" `name`
  do (sx, fe) <- scopeX lhs
     BFail :|: e <- [fe]
     pure (sx e)
 ++
  "FAIL-R" `name`
  do (sx, ef) <- scopeX lhs
     e :|: BFail <- [ef]
     pure (sx e)
 ++
  "ASSOC-CHOICE" `name`
  do (sx, e) <- scopeX lhs
     (e1 :|: e2) :|: e3 <- [e]
     pure (sx (e1 :|: (e2 :|: e3)))
 ++
  "CHOOSE" `name`
  do (sx, Block xs bs v) <- scopeX lhs
     (lbs, (x, b1 :|: b2), rbs) <- pickLR bs
     guard (all (isChoiceFree . snd) lbs)
     pure (sx (Block xs (lbs ++ [(x,b1)] ++ rbs) v :|: Block xs (lbs ++ [(x,b2)] ++ rbs) v))

--------------------------------------------------------------------------------

rulesOcc :: ERule
rulesOcc _ lhs =
   "UX-OCCURS" `name`
   do Var x :=: Val v <- [lhs]
      guard (Var x /= v)
      guard (x `elem` free v)
      pure (Var x :=: Fail)

-- Ban recursion.
-- Doesn't really work
_rulesOcc :: ERule
_rulesOcc _ lhs =
   "UX-OCCURS" `name`
   do Var x :=: Val v <- [lhs]
      guard (Var x /= v)
      guard (x `elem` free v)
      pure (Var x :=: addFailV x v)

addFailV :: Ident -> Value -> Expr
addFailV x e@(Var x') | x == x' = Fail
                      | otherwise = e
addFailV x f@(LAM y e) | x /= y = LAM y $ addFailE x e
                       | otherwise = f
addFailV x (Arr vs) | any (== Fail) vs' = Fail
                    | otherwise = Arr vs'
                    where vs' = map (addFailV x) vs
addFailV _ e@(Int _) = e
addFailV _ e@(Op _) = e
addFailV _ e = error $ "impossible: " ++ show e

addFailE :: Ident -> Expr -> Expr
addFailE x (Val v) = addFailV x v
addFailE x (v1 :@: v2) | v1' == Fail || v2' == Fail = Fail
                       | otherwise = v1' :@: v2'
                       where v1' = addFailV x v1; v2' = addFailV x v2
addFailE x (e1 :>: e2) = addFailE x e1 :>: addFailE x e2
addFailE x (v :=: e) | v' == Fail = Fail
                     | otherwise = v' :=: addFailE x e
                     where v' = addFailV x v
addFailE x (e1 :|: e2) = addFailE x e1 :|: addFailE x e2
addFailE _ Fail = Fail
addFailE x (BlockC e) = BlockC (addFailE x e)
addFailE x (EXI y e) | x == y = EXI y e
                     | otherwise = EXI x (addFailE x e)
addFailE x (One e) = One (addFailE x e)
addFailE x (All e) = All (addFailE x e)
addFailE _ e = error $ "impossible: " ++ show e

--------------------------------------------------------------------------------

rulesStructural :: ERule
rulesStructural _ lhs =
  "EXI-SWAP" `name`
  do EXI x (EXI y e) <- [lhs]
     pure (EXI y (EXI x e))
 <>
  "UNIFY-SWAP" `name`
  do u1@(_ :=: e1) :>: (u2@(_ :=: e2) :>: r) <- [lhs]
     guard (isChoiceFree e1 || isChoiceFree e2)
     pure $ u2 :>: (u1 :>: r)
 <>
  "VAR-SWAP" `name`
  do Block xs bs v <- [lhs]
     (lbs, (Var x, Var y), rbs) <- pickLR bs
     let (lbs', rbs', v') = subst [(y, Var x), (x, Var y)] (lbs, rbs, v)
     pure $ Block xs (lbs' ++ [(Var y, Var x)] ++ rbs') v'
{-
  do Var x :=: Var y <- [lhs]
     guard (x /= y)
     pure $ Var y :=: Var x
-}
{-
  do (ctx, Var x :=: Var y) <- execX lhs
     let y0 = identNotIn (free (ctx Fail, y, x))
         sub = [(y, Var x), (y0, Var y)]
     pure (subst sub (ctx (Var y0 :=: Var x)))
-}
