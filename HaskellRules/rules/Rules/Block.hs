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
import Control.Monad( guard )
--import Data.Functor.Classes (Show1(liftShowList))
import Debug.Trace

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
  , rulesHaveStructural = False
  , confluenceRules     = rulesStructural
  , validExpr           = const valid
  }

{-
v ::= x | hnf
hnf ::= k | <v,...> | \x.b | op
b ::= (exist x)*; (v=e)*; e        -- Maybe the last e should be v
e ::= v | v(v) | fail | (b || b) | one{b} | all{b} | split{b,v,v}
-}

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
    block'' b = value b
--    block'' b = expr b

type A a = State (([Ident], [Eqn]), [Ident]) a

-- Make the expression obey the Block grammar,
-- i.e., valid (anf e) == True
anf :: Expr -> Expr
anf ee = foo $ evalState (block ee) (undefined, allVars ee)
  where
    foo xx = trace (show ee) $ trace (show xx) xx
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
    addExpr e | trace ("addExpr " ++ show e) False = undefined
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
-- contexts

type Context = Expr -> Expr

--------------------------------------------------------------------------------

allRules :: ERule
allRules =  rulesPrimOps
         <> rulesApplication
         <> rulesSubst
         <> rulesDefElim
         <> rulesUnification

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
  in  defs xs $ seqs $ zipWith (\ x v -> Var x :=: (v :@: unit)) xs vs ++ [Arr $ map Var xs]

defs :: [Ident] -> Expr -> Expr
defs vs e = foldr (\ x -> Exi . Bind x) e vs

unit :: Value
unit = Arr []

seqs :: [Expr] -> Expr
seqs = foldl1 (:>:)

--------------------------------------------------------------------------------

rulesApplication :: ERule
rulesApplication _ lhs =
  "APP-BETA" `name`
  do Block oxs obs oe <- [lhs]
     (lbs, (Var ox, LAM x b@BlockC{} :@: v), rbs) <- pickLR obs
     let freeV = free v
     let Block xs qs e =
           if x `notElem` freeV then
             b
           else
             -- The x has to be renamed to avoid capture
             let freeE = free b
                 x' = identNotIn (freeV ++ freeE)
             in  subst [(x, Var x')] b
     let fb = (x:xs, (Var x, v):qs, e)
     pure $ mergeBlock oxs (lbs, rbs) ox fb oe
 ++
  "APP-TUP" `name`
  do Arr vs :@: v <- [lhs]
     if null vs then
       pure Fail
      else
       pure (foldr1 (:|:) [ (Val v :=: Int i) :>: Val vi | (i,vi) <- [0..] `zip` vs ])

type Blk = ([Ident], [Eqn], Expr)

mergeBlock :: [Ident] -> ([Eqn], [Eqn]) -> Ident -> Blk -> Expr -> Expr
mergeBlock oxs (lbs, rbs) ox b oe =
  let (nxs, nbs, ne) = alphaBlk oxs b
  in  Block (oxs ++ nxs) (lbs ++ nbs ++ [(Var ox, ne)] ++ rbs) oe

alphaBlk :: [Ident] -> Blk -> Blk
alphaBlk vs b@(xs, bs, e) =
  let is = identsNotIn (vs ++ free b)
      xs' = zipWith (\ x i -> if x `elem` vs then i else x) xs is
      sub = [ (x, Var x') | (x, x') <- zip xs xs', x /= x' ]
  in  (xs', subst sub bs, subst sub e)

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

--------------------------------------------------------------------------------

rulesUnification :: ERule
rulesUnification _ lhs =
  "ULIT" `name`
  do Block xs bs e <- [lhs]
     ((Int k1, Int k2), bs') <- pick bs
     if k1 == k2
       then pure (Block xs bs' e)
       else pure Fail
 ++
  "UTUP" `name`
  do Block xs bs e <- [lhs]
     (lbs, (Arr vs, Arr vs'), rbs) <- pickLR bs
     if length vs == length vs'
       then pure (Block xs (lbs ++ [ (v, v') | (v,v') <- vs `zip` vs' ] ++ rbs) e)
       else pure Fail
 ++
  "UX-LAM" `name`
  do Block _ bs _ <- [lhs]
     ((Lam{}, Lam{}), _) <- pick bs
     pure Fail
 ++
  "UX-OP" `name`
  do Block _ bs _ <- [lhs]
     ((Op{}, Op{}), _) <- pick bs
     pure Fail
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
     pure Fail

--------------------------------------------------------------------------------

rulesStructural :: ERule
rulesStructural _ _ = []
