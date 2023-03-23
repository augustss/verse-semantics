{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module FrontEnd.EvalBlock(runBlock) where
import Prelude hiding ((<>))
import Control.Monad.State.Strict
import Data.Data(Data)
import Data.List
--import qualified Data.Map as M
import Epic.Print
import Epic.Uniplate
import Rules.Core
import TRS.Bind(Ident(Name), Subst, identNotIn, free)
--import GHC.Stack
import Debug.Trace

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = null (xs \\ ys)

--------------------

--
-- TODO
--  * propagate allowed effects
--  * check if an effect is allowed
--  * limit effects for lambda bodies

doTrace :: Bool
doTrace = False

dtrace :: String -> a -> a
dtrace s a | doTrace = trace s a
           | otherwise = a

runBlock :: TRSFlags -> Expr -> Expr
runBlock flg e =
  let b = coreToChoice e
      b' = evalChoiceFull topEffects b
      b'' = if tfUnderLambda flg then evalUnderLambda b' else b'
  in
      dtrace ("core: " ++ prettyShow e) $
      dtrace ("input: " ++ prettyShow b) $
      dtrace ("output: " ++ prettyShow b') $
      choiceToCore b''

evalChoiceFull :: AllowedEffects -> BChoice -> BChoice
evalChoiceFull aeffs c =
  case evalChoice aeffs [] c of
    BCFork c1 c2 -> BCFork c1 (evalChoiceFull aeffs c2)
    c' -> c'

evalUnderLambda :: BChoice -> BChoice
evalUnderLambda = transformBi ev
  where ev :: BHNF -> BHNF
        ev v@BInt{} = v
        ev (BArr vs) = BArr (map (transformBi ev) vs)
        ev (BHLam i b) = BHLam i $ transformBi ev $ evalB b
        evalB b =
          case evalBlock topEffects [] b of
            BCBlk b' -> b'
            BCFail -> BlockFail
            BCWrong s -> BlockWrong s
            BCFork c1 c2 -> blkChoice (BCFork c1 (evalChoiceFull lambdaEffs c2))
        lambdaEffs = [Efails, Eiterates, Ewrong]

---------------------------------------------

newtype BIdent = BIdent String
  deriving (Show, Eq, Ord, Data)

data BBlock = BBlock
  { --limitEffs :: !Effects                   -- limit effects to these
--  , needEffs  :: !Effects                   -- must have these effects
--  , vars      :: !(M.Map BIdent EffectType) -- existentially bound variables with effects
    vars      :: ![BIdent]
  , binds     :: [BEqn]                    -- variable bindings
  , result    :: !BValue                    -- final value of the block
  }
  deriving (Show, Eq, Data)

pattern BlockFail :: BBlock
pattern BlockFail = BBlock{ vars = [], binds = [(BDummy, BFail)], result = BDummy }

pattern BlockWrong :: String -> BBlock
pattern BlockWrong s = BBlock{ vars = [], binds = [(BDummy, BWrong s)], result = BDummy }

pattern BlockValue :: [BIdent] -> BValue -> BBlock
pattern BlockValue vs v = BBlock { vars = vs, binds = [], result = v }

pattern BlockExpr :: BExpr -> BBlock
pattern BlockExpr e <- (getBlockExpr -> Just e)

getBlockExpr :: BBlock -> Maybe BExpr
getBlockExpr BBlock { vars = [i], binds = [(BVar i', e)], result = BVar i'' }
  | i == i', i == i'' = Just e
getBlockExpr _ = Nothing

pattern BDummy :: BValue
pattern BDummy = BVar (BIdent "_")

type BEqn = (BValue, BExpr)
type BEqnV = (BIdent, BValue)

data BExpr
  = BPrimOp Op [BValue]
  | BApply BValue BValue
  | BSplit BChoice BValue BValue
  | BChoice BChoice
  | BVal BValue
  deriving (Show, Eq, Data)

pattern BEBlk :: BBlock -> BExpr
pattern BEBlk b = BChoice (BCBlk b)

data BChoice
  = BCFork BChoice BChoice
  | BCBlk BBlock
  | BCFail
  | BCWrong String
  deriving (Show, Eq, Data)

--pattern BEVar :: BIdent -> BExpr
--pattern BEVar i = BVal (BVar i)

pattern BFail :: BExpr
pattern BFail = BChoice BCFail

pattern BWrong :: String -> BExpr
pattern BWrong s = BChoice (BCWrong s)

data BValue
  = BVar BIdent
  | BHNF BHNF
  | BRec BHNF   -- The BHNF must be a BHLam i (BlockValue [] (BHNF h))
  deriving (Show, Eq, Data)

pattern BMu :: BIdent -> BHNF -> BValue
pattern BMu i h = BRec (BHLam i (BlockValue [] (BHNF h)))

isBVar :: BValue -> Bool
isBVar BVar{} = True
isBVar _ = False

data BHNF
  = BInt Integer
  | BArr [BValue]
  | BHLam BIdent BBlock  -- invariant: the bound BIdent will not be among the vars in the block
  deriving (Show, Eq, Data)

pattern BVInt :: Integer -> BValue
pattern BVInt i = BHNF (BInt i)

pattern BVArr :: [BValue] -> BValue
pattern BVArr vs = BHNF (BArr vs)

pattern BVLam :: BIdent -> BBlock -> BValue
pattern BVLam i b = BHNF (BHLam i b)

pattern BXLam :: BIdent -> BValue -> BValue
pattern BXLam i v = BVLam i (BlockValue [] v)

data Effect
  = Ediverges
  | Ewrong
  | Efails
  | Eiterates  -- multi-valued, possibly 0 
  | Eallocates | Ereads | Ewrites
  | Ethrows
  | Einteracts
  ---- 
  | Ecovariant
  deriving (Eq, Ord, Show, Enum, Bounded)

type Effects = [Effect]

allEffects :: Effects
allEffects = [minBound .. maxBound]

{-
data EffectType = ETNone | ETUnknown | ETArrow EffectType Effects EffectType
  deriving (Eq, Ord, Show)
-}

--type BSubst = [(BIdent, BValue)]

type BlockedEffects = Effects
type AllowedEffects = Effects

instance Pretty BIdent where
  pPrintPrec _ _ (BIdent s) = text s

instance Pretty BBlock where
  pPrintPrec _ _ BlockFail = text "fail"
  pPrintPrec l p BBlock{vars=[], binds=[], result=v} = pPrintPrec l p v
  pPrintPrec l p b =
    maybeParens (p > 0) $ sep $ pVars ++ pBinds ++ pResult
      where pVars | null (vars b) = []
                  | otherwise = [text "exists" <+> hsep (map (pPrintPrec l 10) (vars b)) <+> text "."]
            pBinds = map (\ (v,e) -> pPrintPrec l 1 v <+> text "=" <+> pPrintPrec l 1 e <> text ";") (binds b)
            pResult = [pPrintPrec l 0 (result b)]

instance Pretty BExpr where
  pPrintPrec l _ (BPrimOp op vs) = pPrintPrec l 10 op <> parens (hsep (punctuate (text ",") (map (pPrintPrec l 0) vs)))
  pPrintPrec l _ (BApply f a) = pPrintPrec l 10 f <> parens (pPrintPrec l 0 a)
  pPrintPrec l _ (BSplit b f g) = text "split" <> parens (pPrintPrec l 0 b) <> braces (pPrintPrec l 1 f <> text ";" <+> pPrintPrec l 1 g)
  pPrintPrec l p (BChoice c) = pPrintPrec l p c
  pPrintPrec l p (BVal v) = pPrintPrec l p v

instance Pretty BChoice where
  pPrintPrec l p (BCFork e1 e2) = maybeParens (p > 1) $ pPrintPrec l 2 e1 <+> text "|" <+> pPrintPrec l 2 e2
  pPrintPrec l p (BCBlk b) = pPrintPrec l p b
  pPrintPrec _ _ BCFail = text "fail"
  pPrintPrec _ _ (BCWrong s) = text ("WRONG(" ++ s ++ ")")

instance Pretty BValue where
  pPrintPrec l p (BVar v) = pPrintPrec l p v
  pPrintPrec l p (BHNF h) = pPrintPrec l p h
  pPrintPrec l p (BRec h) = parens $ text "rec" <+> pPrintPrec l p h

instance Pretty BHNF where
  pPrintPrec l p (BInt i) = pPrintPrec l p i
  pPrintPrec l _ (BArr [v]) = parens (pPrintPrec l 0 v <> text ",")
  pPrintPrec l _ (BArr vs) = parens $ fsep (punctuate comma (map (pPrintPrec l 0) vs))
  pPrintPrec l p (BHLam x b) = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 x <> text "." <+> pPrintPrec l 0 b

instance Pretty Effect where
  pPrintPrec _ _ e = text $ tail $ show e

{-
instance Pretty EffectType where
  pPrintPrec _ _ ETNone = text "none"
  pPrintPrec _ _ ETUnknown = text "?"
  pPrintPrec l p (ETArrow a rs b) = maybeParens (p>0) $ pPrintPrec l 1 a <> text "-" <> t <> pPrintPrec l 1 b
    where t | [] <- rs = text ">"
            | [Ediverges,Eallocates,Ereads,Ewrites] <- rs = text "-<heap>"
            | otherwise = text "-<" <> commaSep l rs <> text ">"
-}

-----------------------------------

bIdentsNotIn :: [BIdent] -> [BIdent]
bIdentsNotIn is = [ BIdent ("$" ++ show i) | i <- [1::Integer ..] ] \\ is

bIdentNotIn :: [BIdent] -> BIdent
bIdentNotIn = head . bIdentsNotIn

unionMap :: (Eq b) => (a -> [b]) -> [a] -> [b]
unionMap f = foldr union [] . map f

bsubst :: (Bound a) => [BEqnV] -> a -> a
bsubst [] a = a
bsubst s a = bsubst' s a

class Bound a where
  allBVars :: a -> [BIdent]
  freeBVars :: a -> [BIdent]
  bsubst' :: [BEqnV] -> a -> a

instance (Bound a) => Bound [a] where
  allBVars = unionMap allBVars
  freeBVars = unionMap freeBVars
  bsubst' s = map (bsubst' s)

instance (Bound a, Bound b) => Bound (a, b) where
  allBVars (a, b) = union (allBVars a) (allBVars b)
  freeBVars (a, b) = union (freeBVars a) (freeBVars b)
  bsubst' s (a, b) = (bsubst' s a, bsubst' s b)

instance (Bound a, Bound b, Bound c) => Bound (a, b, c) where
  allBVars (a, b, c) = union (allBVars a) $ union (allBVars b) (allBVars c)
  freeBVars (a, b, c) = union (freeBVars a) $ union (freeBVars b) (freeBVars c)
  bsubst' s (a, b, c) = (bsubst' s a, bsubst' s b, bsubst' s c)

instance Bound BBlock where
  allBVars b =
    (vars b) `union`
    allBVars (binds b) `union`
    allBVars (result b)
  freeBVars b =
    (freeBVars (binds b) `union` freeBVars (result b)) \\ vars b
  bsubst' s b
    | null s' = b
    | otherwise = (freshenBlock bnd b){
        binds = bsubst s' (binds b),
        result = bsubst s' (result b) }
    where
      bnd = freeBVars (map snd s')
      s' = filter ((`notElem` vars b) . fst) s

-- Change the initial existentials so they don't clash with vs.
freshenBlock :: [BIdent] -> BBlock -> BBlock
freshenBlock vs b | null bad = b
                  | otherwise =
                    b{ vars = map subIdent (vars b)
                     , binds = bsubst sub (binds b)
                     , result = bsubst sub (result b) }
  where
    bad = intersect (vars b) vs
    xs = bIdentsNotIn (vs `union` allBVars b)
    sub = zip bad (map BVar xs)
    subIdent i = maybe i unVar $ lookup i sub
      where unVar (BVar x) = x
            unVar _ = undefined

instance Bound BExpr where
  allBVars (BPrimOp _ vs) = unionMap allBVars vs
  allBVars (BApply v1 v2) = allBVars v1 `union` allBVars v2
  allBVars (BSplit b v1 v2) = allBVars b `union` allBVars v1 `union` allBVars v2
  allBVars (BChoice c) = allBVars c
  allBVars (BVal v) = allBVars v
  freeBVars (BPrimOp _ vs) = unionMap freeBVars vs
  freeBVars (BApply v1 v2) = freeBVars v1 `union` freeBVars v2
  freeBVars (BSplit b v1 v2) = freeBVars b `union` freeBVars v1 `union` freeBVars v2
  freeBVars (BChoice c) = freeBVars c
  freeBVars (BVal v) = freeBVars v
  bsubst' s (BPrimOp op vs) = BPrimOp op (map (bsubst' s) vs)
  bsubst' s (BApply v1 v2) = BApply (bsubst' s v1) (bsubst' s v2)
  bsubst' s (BSplit b v1 v2) = BSplit (bsubst' s b) (bsubst' s v1) (bsubst' s v2)
  bsubst' s (BChoice c) = BChoice (bsubst' s c)
  bsubst' s (BVal v) = BVal (bsubst' s v)

instance Bound BChoice where
  allBVars (BCFork b1 b2) = allBVars b1 `union` allBVars b2
  allBVars (BCBlk b) = allBVars b
  allBVars BCFail = []
  allBVars (BCWrong _) = []
  freeBVars (BCFork b1 b2) = freeBVars b1 `union` freeBVars b2
  freeBVars (BCBlk b) = freeBVars b
  freeBVars BCFail = []
  freeBVars (BCWrong _) = []
  bsubst' s (BCFork b1 b2) = BCFork (bsubst' s b1) (bsubst' s b2)
  bsubst' s (BCBlk b) = BCBlk (bsubst' s b)
  bsubst' _ BCFail = BCFail
  bsubst' _ e@(BCWrong _) = e

instance Bound BValue where
  allBVars (BVar i) = [i]
  allBVars (BHNF h) = allBVars h
  allBVars (BRec h) = allBVars h
  freeBVars (BVar i) = [i]
  freeBVars (BHNF h) = freeBVars h
  freeBVars (BRec h) = freeBVars h
  bsubst' s v@(BVar i) | Just e <- lookup i s = e
                       | otherwise = v
  bsubst' s (BHNF h) = BHNF (bsubst' s h)
  bsubst' s (BRec h) = BRec (bsubst' s h)
  
instance Bound BHNF where
  allBVars (BInt _) = []
  allBVars (BArr vs) = unionMap allBVars vs
  allBVars (BHLam i b) = [i] `union` allBVars b
  freeBVars (BInt _) = []
  freeBVars (BArr vs) = unionMap freeBVars vs
  freeBVars (BHLam i b) = freeBVars b \\ [i]
  bsubst' _ h@(BInt _) = h
  bsubst' s (BArr vs) = BArr (map (bsubst' s) vs)
  bsubst' s h@(BHLam i b)
    | null s' = h
    | otherwise = BHLam i' (bsubst s' b')
    where
      s' = filter ((/= i) . fst) s
      (i', b') = freshenLambda (freeBVars (map snd s')) (i, b)

freshenLambda :: [BIdent] -> (BIdent, BBlock) -> (BIdent, BBlock)
freshenLambda bnd (i, b) | i `notElem` bnd = (i, b)
                         | otherwise =
  let x = bIdentNotIn (bnd ++ allBVars b)
  in  (x, bsubst' [(i, BVar x)] b)

---------------------------------------------

choiceToCore :: BChoice -> Expr
choiceToCore BCFail = Fail
choiceToCore (BCWrong s) = Wrong s
choiceToCore (BCBlk b) = blockToCore b
choiceToCore (BCFork c1 c2) = choiceToCore c1 :|: choiceToCore c2

blockToCore :: BBlock -> Expr
blockToCore (BBlock [v] [(BVar v', e)] (BVar v'')) | v == v' && v == v'' = exprToCore e
blockToCore b = foldr EXI bs (map bIdentToIdent (vars b))
  where bs = foldr eqn (valueToCore (result b)) (binds b)
        eqn (v, e) r = (valueToCore v :=: exprToCore e) :>: r

valueToCore :: BValue -> Expr
valueToCore (BVar v) = Var (bIdentToIdent v)
valueToCore (BHNF h) = hnfToCore h
valueToCore (BRec h) = mu (hnfToCore h)
  where mu (LAM i e) = EXI i $ Var i :=: e
        mu _ = undefined

bIdentToIdent :: BIdent -> Ident
bIdentToIdent (BIdent s) = Name s

hnfToCore :: BHNF -> Expr
hnfToCore (BInt i) = Int i
hnfToCore (BArr vs) = Arr (map valueToCore vs)
hnfToCore (BHLam i e) = lam (bIdentToIdent i) (blockToCore e)
  where lam ii (f :@: Var i') | ii == i', ii `notElem` free f = f
        lam ii ee = LAM ii ee

exprToCore :: BExpr -> Expr
exprToCore (BPrimOp o [v]) = Op o :@: valueToCore v
exprToCore (BPrimOp o vs) = Op o :@: Arr (map valueToCore vs)
exprToCore (BApply f a) = valueToCore f :@: valueToCore a
exprToCore (BSplit e f g) = Split (choiceToCore e) (valueToCore f) (valueToCore g)
exprToCore (BChoice c) = choiceToCore c
exprToCore (BVal v) = valueToCore v

-----------------------------------

-- ((current existentials, current equations), all used variables)
type A a = State (([Ident], [BEqn]), [Ident]) a

coreToChoice :: Expr -> BChoice
coreToChoice e = evalState (cChoice e) (undefined, allVars e)

addEqn :: BEqn -> A ()
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

addExpr :: Expr -> A BIdent
addExpr (Var x) = pure $ cIdent x
addExpr (Var x :=: e) = do
  e' <- cExpr e
  let bx = cIdent x
  addEqn (BVar bx, e')
  pure bx
addExpr e = do
  e' <- cExpr e
  ((is, qs), vs) <- get
  let y = identNotIn vs
      by = cIdent y
  put ((y:is, (BVar by, e') : qs), y : vs)
  pure by

cIdent :: Ident -> BIdent
cIdent = BIdent . prettyShow

cChoice :: Expr -> A BChoice
cChoice (e1 :|: e2) = BCFork <$> cChoice e1 <*> cChoice e2
cChoice Fail = pure BCFail
cChoice (Wrong s) = pure $ BCWrong s
cChoice e = BCBlk <$> cBlock e

cBlock :: Expr -> A BBlock
cBlock = cBlockV []

-- Avoid the variable names vs
cBlockV :: [Ident] -> Expr -> A BBlock
cBlockV vs e = do
  (old, is) <- get
  put ((vs, []), is)
  v <- cValue e
  ((xs, bs), is') <- get
  put (old, is')
  pure $ BBlock (map cIdent $ drop (length vs) $ reverse xs) (reverse bs) v

cValue :: Expr -> A BValue
cValue (Var i) = pure $ BVar (cIdent i)
cValue (Int i) = pure $ BHNF $ BInt i
cValue (Op o) = cValue $ LAM i (Op o :@: Var i) where i = Name "a"
cValue (Arr es) = BHNF . BArr <$> mapM cValue es
cValue (LAM x e) = BHNF . BHLam (cIdent x) <$> cBlockV [x] e
cValue (e1 :>: e2) = addExpr e1 *> cValue e2
cValue (EXI i e) = do s <- addExist i; cValue (subst s e)
cValue e = BVar <$> addExpr e

cExpr :: Expr -> A BExpr
cExpr e@Var{} = BVal <$> cValue e
cExpr e@Int{} = BVal <$> cValue e
cExpr e@Op{}  = BVal <$> cValue e
cExpr e@Arr{} = BVal <$> cValue e
cExpr e@LAM{} = BVal <$> cValue e
cExpr (e1 :=: e2) = do
  v <- cValue e1
  e <- cExpr e2
  addEqn (v, e)
  pure $ BVal v
cExpr (e1 :>: e2) = addExpr e1 *> cExpr e2
cExpr e@(_ :|: _) = BChoice <$> cChoice e
cExpr (Op o :@: Arr es) = BPrimOp o <$> mapM cValue es
cExpr (Op o :@: e) = BPrimOp o . (:[]) <$> cValue e
cExpr (e1 :@: e2) = BApply <$> cValue e1 <*> cValue e2
cExpr (EXI i e) = do s <- addExist i; cExpr (subst s e)
cExpr Fail = pure BFail
cExpr (Wrong s) = pure $ BWrong s
cExpr (Split e e1 e2) = BSplit <$> cChoice e <*> cValue e1 <*> cValue e2
cExpr (One e) = cExpr $ Split e (LAM u Fail) (LAM v $ LAM u $ LAM u $ Var v)
  where u = Name "_"; v = Name "v"
cExpr (All e) =
  let u = Name "_"; v = Name "v"; r = Name "r"; h = Name "h"
      f = LAM u $ Arr []
      g = LAM v $ LAM r $ LAM h $
            let x = Split (Var r :@: Arr []) f (Var h)
            in  Op Cons :@: Arr [Var v, x]
  in  cExpr $ Split e f g
cExpr e = error $ "cExpr: impossible: " ++ prettyShow e
{-
cExpr (One e) = One <$> cBlock e
cExpr (All e) = All <$> cBlock e
-}

primOpEffs :: Op -> Effects
primOpEffs o | o `elem` [Gt, Ge, Lt, Le, Ne, IsInt] = [Efails]
primOpEffs MapAp = allEffects  -- XXX can do better?
primOpEffs Alloc = [Eallocates]
primOpEffs Read  = [Ereads]
primOpEffs Write = [Ewrites]
primOpEffs AddTo = [Ereads, Ewrites]
primOpEffs DotDot = [Eiterates]
primOpEffs Print = [Einteracts]
primOpEffs _ = []

-- XXX needs an environment
funcEffs :: BValue -> Effects
funcEffs _ = allEffects

-- XXX needs an environment
exprEffs :: BExpr -> Effects
exprEffs (BPrimOp op _) = primOpEffs op
exprEffs (BApply f _) = funcEffs f
-- Assume BSplit is in a form generated by the desugaring
exprEffs (BSplit c (BVLam _ f) (BXLam _ (BXLam _ (BVLam _ g)))) =
  (choiceEffs c \\ [Eiterates, Efails]) `union` blockEffs f `union` blockEffs g
exprEffs (BSplit (BCBlk (BlockExpr (BApply (BVar _) (BVArr []))))
                 (BVLam _ f) (BVar _)) = blockEffs f
exprEffs e@BSplit{} = error $ "exprEffs: " ++ show e
exprEffs (BChoice c) = choiceEffs c
exprEffs (BVal _) = []

choiceEffs :: BChoice -> Effects
choiceEffs (BCFork c1 c2) = [Eiterates] `union` choiceEffs c1 `union` choiceEffs c2
choiceEffs (BCBlk b) = blockEffs b
choiceEffs BCFail = [Efails]
choiceEffs (BCWrong _) = [Ewrong]

blockEffs :: BBlock -> Effects
blockEffs b = unionMap (exprEffs . snd) (binds b)

-- Effects that should not be blocked in the domain of an if/for
domEffects :: Effects
domEffects = [Efails, Eiterates] ++ heapEffects

-- Memory effects
heapEffects :: Effects
heapEffects = [Eallocates, Ereads, Ewrites]

-- Effects allowed on the top level
topEffects :: AllowedEffects
topEffects = [Einteracts, Ewrong, Ediverges] ++ heapEffects
             ++ [Efails, Eiterates]   -- These are not really allowed in MaxVerse

notBlocked :: Effect -> BlockedEffects -> Bool
notBlocked = all . effCommutes

-- If the returned choice is
--   * BCBlk it has been evaluated as far as possible
--   * BCFork the first fork is a BCBlk evaluated as far as possible
evalChoice :: AllowedEffects -> BlockedEffects -> BChoice -> BChoice
evalChoice _ _ c | dtrace ("evalChoice: " ++ prettyShow c) False = undefined
evalChoice aeffs beffs (BCFork c1 c2) =
  case evalChoice aeffs beffs c1 of
    BCFail -> evalChoice aeffs beffs c2
    c@BCWrong{} -> c
    c@BCBlk{} -> BCFork c c2
    BCFork d1 d2 -> BCFork d1 (BCFork d2 c2)
evalChoice aeffs beffs (BCBlk b) = evalBlock aeffs beffs b
evalChoice _ _ BCFail = BCFail
evalChoice _ _ c@(BCWrong _) = c

-- If the returned choice is
--   * BCBlk it has been evaluated as far as possible
--   * BCFork the first fork is a BCBlk evaluated as far as possible
evalBlock :: AllowedEffects -> BlockedEffects -> BBlock -> BChoice
evalBlock _ _ b | dtrace ("evalBlock: " ++ prettyShow b) False = undefined
evalBlock aeffs beffs b =
  let c = evalBlock' aeffs beffs b
  in  dtrace ("evalBlock returns: " ++ prettyShow c) $
      checkPostCond aeffs beffs c

checkPostCond :: AllowedEffects -> BlockedEffects -> BChoice -> BChoice
checkPostCond aeffs beffs c@(BCBlk b) | evalBlock' aeffs beffs b == c = c
                                      | otherwise = error $ "checkPostCond: " ++ prettyShow c
checkPostCond aeffs beffs c@(BCFork c1@(BCBlk b) _) | evalBlock' aeffs beffs b == c1 = c
checkPostCond _ _ c@BCFork{} = error $ "checkPostCond: " ++ prettyShow c
checkPostCond _ _ c = c

evalBlock' :: AllowedEffects -> BlockedEffects -> BBlock -> BChoice
evalBlock' _ _ BBlock{ binds = (_, BFail) : _ } = BCFail             -- XXX check effs?
evalBlock' _ _ BBlock{ binds = (_, BWrong s) : _ } = BCWrong s       -- XXX check effs?
evalBlock' aeffs bbeffs ablk = sweep bbeffs [] (vars ablk) (binds ablk) (result ablk)
  where
    notAllowed es = not (subset es aeffs)

    startSweep :: [BIdent] -> [BEqn] -> BValue -> BChoice
    startSweep = sweep bbeffs []

    sweep :: BlockedEffects -> [BEqn] -> [BIdent] -> [BEqn] -> BValue -> BChoice
    sweep beffs done bvars bbinds _bresult | dtrace ("sweep: " ++ prettyShow (beffs, bvars, done, bbinds)) False = undefined
    sweep _     done bvars     [] bresult =
      -- End of binds reached, no further progress possible
      BCBlk BBlock{ vars = bvars `intersect` freeBVars (done, bresult), binds = reverse done, result = bresult }
    sweep beffs done bvars bbinds@(eqn@(val, expr) : bs) bresult =
      let
        -- Swap so local binding is first
        unify :: BValue -> BValue -> BChoice
        unify (BVar x) (BVar y) | x `notElem` bvars && y `elem` bvars =
          unify (BVar y) (BVar x)
        unify (BVar x) v =
          let sub a = bsubst [(x, v)] a
              ueqn = (BVar x, BVal v)
          in
          if x `elem` freeBVars v then
            -- Recursive
            case v of
              BVar _ ->
                -- Leave x=x alone
                -- (The variable must be x, since x is among the free variables of v)
                suspend ueqn
              -- Change recursion to mu
              BHNF h -> succeeds [(BVar x, BVal $ BMu x h)]
              -- XXX what is this supposed to do?
              BRec _ -> undefined
          else if x `elem` bvars then
            -- Locally bound, get rid of the variable entirely.
            -- And restart sweep
            startSweep (bvars \\ [x]) (sub (reverse done ++ bs)) (sub bresult)
          else if x `elem` freeBVars ((done, bs), bresult) then
            -- x occurs, so substitute.
            -- Bound outside, keep equation.
            -- And restart sweep
            startSweep bvars (sub (reverse done) ++ [ueqn] ++ sub bs) (sub bresult)
          else
            -- x does not occur, just keep equation.
            suspend ueqn
        -- Swap so variable is first (v cannot be a variable, that's handled above)
        unify v (BVar x) = unify (BVar x) v
        -- Equal integers are ok
        unify (BVInt i) (BVInt j) | i == j = succeeds []
        -- Replace equal length arrays with new equations
        unify (BVArr vs) (BVArr ws) | length vs == length ws = succeeds $ zipWith (\ v w -> (v, BVal w)) vs ws
        unify _x@(BVLam _ _) _y@(BVLam _ _) =
          -- According to the ICFP paper this fails.  Being WRONG would be better
          fails
          -- wrongs $ "unify lambda: " ++ prettyShow (_x, _y)
        unify BRec{} _ = undefined -- XXX dunno
        unify _ BRec{} = undefined -- XXX dunno
        unify _ _ = fails -- anything else fails

        -- Fail if it is allowed, otherwise suspend
        fails | notBlocked Efails beffs = BCFail
              | otherwise = suspend (BDummy, BFail)
        -- Generate WRONG if allowed, otherwise suspend
        wrongs s | notBlocked Ewrong beffs = BCWrong s
                 | otherwise = suspend (BDummy, BWrong s)

        -- Put es on the unprocessed bindings and continue the sweep
        succeeds :: [BEqn] -> BChoice
        succeeds = succeeds' []
        succeeds' is es = sweep beffs done (is ++ bvars) (es ++ bs) bresult

        -- Put eqn on the done list, block its effects, and continue the sweep
        suspend :: BEqn -> BChoice
        suspend ve@(_, e) = sweep (exprEffs e `union` beffs) (ve : done) bvars bs bresult
      in
        let blk = BBlock{ vars = bvars, binds = undefined, result = bresult }
            allvars = bvars ++ allBVars (done, bbinds, bresult)
        in
        -- Examine the expression and evaluate if possible.
        case expr of
          BPrimOp op vs | notAllowed (primOpEffs op) -> wrongs "effect not allowed"
                        | Just e <- evalPrimOp op vs -> succeeds [(val, e)]
                        | otherwise -> suspend eqn
          BApply f a | BVLam i b <- f ->
                         -- Bind the argument and insert the lambda body
                         let (i', b') = freshenLambda allvars (i, b)
                         in  succeeds' [i'] [(BVar i', BVal a), (val, BEBlk b')]
                     | BVArr vs <- f ->
                       let e = BChoice $ choices [ BBlock { vars = [], binds = [(a, BVal $ BVInt i)], result = v }
                                                 | (i, v) <- zip [0..] vs ]
                       in  succeeds [(val, e)]
                     | BMu i h <- f -> succeeds [(val, BApply (bsubst [(i, f)] (BHNF h)) a)]
                     | BRec _  <- f -> undefined
                     | otherwise -> suspend eqn           -- not a hnf yet
          BSplit c f g ->
            case evalChoice (aeffs `intersect` domEffects) (beffs \\ domEffects) c of  -- XXX need to propagate blocked effects
              BCFail -> succeeds [(val, BApply f (BVArr []))]
              BCWrong s -> wrongs s
              BCBlk b@BlockValue{} -> callG b BCFail
              BCFork (BCBlk b@BlockValue{}) r -> callG b r
              c' -> suspend (val, BSplit c' f g)
            where callG b r =
                    let (vb: a1: a2: a3: dummy: _) = bIdentsNotIn (vars blk ++ freeBVars (b, c))
                        b0 = (BVar vb, BChoice (BCBlk b))
                        b1 = (BVar a1, BApply g (BVar vb))
                        b2 = (BVar a2, BApply (BVar a1) (BVLam dummy (blkChoice r)))
                        b3 = (BVar a3, BApply (BVar a2) g)
                        bb = BBlock{ vars = [vb, a1, a2, a3], binds = [b0,b1,b2,b3], result = BVar a3 }
                    in  succeeds [(val, BEBlk bb)]
          BChoice BCFail -> fails
          BChoice (BCWrong s) -> wrongs s
          BChoice (BCBlk b) ->
            let rhs = freshenBlock allvars b
            in  succeeds' (vars rhs) (binds rhs ++ [(val, BVal $ result rhs)])
          BChoice (BCFork x1 x2) | notAllowed [Eiterates] -> wrongs "iterates not allowed"
                                 | notBlocked Eiterates beffs -> evalChoice aeffs bbeffs (BCFork c1 c2)
                                 | otherwise -> suspend eqn
            where
              c1 = BCBlk $ blk{ binds = rdone ++ [(val, BChoice x1)] ++ bs }
              c2 = BCBlk $ blk{ binds = rdone ++ [(val, BChoice x2)] ++ bs }
              rdone = reverse done
          BVal v -> unify val v

blkChoice :: BChoice -> BBlock
blkChoice (BCBlk b) = b
blkChoice c = BBlock { vars = [i], binds = [(BVar i, BChoice c)], result = BVar i }
  where i = bIdentNotIn (allBVars c)

choices :: [BBlock] -> BChoice
choices [] = BCFail
choices bs = foldr1 BCFork (map BCBlk bs)

evalPrimOp :: Op -> [BValue] -> Maybe BExpr
evalPrimOp op vs | Just cmp <- lookup op compareOps =
  case vs of
    [BVInt a, BVInt b] -> Just $ if cmp a b then BVal $ BVInt a else BFail
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)
evalPrimOp op vs | Just arith <- lookup op arithBinOps =
  case vs of
    [BVInt _, BVInt 0] | op == Div -> Just BFail
    [BVInt a, BVInt b] -> Just $ BVal $ BVInt $ a `arith` b
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)
evalPrimOp op vs | Just arith <- lookup op arithUnOps =
  case vs of
    [BVInt a] -> Just $ BVal $ BVInt $ arith a
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)
evalPrimOp op vs | op == IsInt =
  case vs of
    [a@(BVInt _)] -> Just $ BVal a
    [BHNF _] -> Just BFail
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)      
evalPrimOp op vs | op == Cons =
  case vs of
    [a, BVArr as] -> Just $ BVal $ BVArr (a : as)
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)      
evalPrimOp op vs | op == DotDot =
  case vs of
    [BVInt a, BVInt b] -> Just $ BChoice $ choices [ BlockValue [] (BVInt i) | i <- [a .. b] ]
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)
evalPrimOp op vs | op == Print =
  -- A temporary hack for printing.
  case vs of
    [BHNF h] ->
      trace ("Print: " ++ prettyShow h) $
      Just $ BVal $ BVArr []
    _ | any isBVar vs -> Nothing
      | otherwise -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op vs)      
  
evalPrimOp op vs = error $ "evalPrimOp: " ++ show (op, vs)

compareOps :: [(Op, Integer -> Integer -> Bool)]
compareOps = [(Gt, (>)), (Ge, (>=)), (Lt, (<)), (Le, (<=)), (Ne, (/=))]

arithBinOps :: [(Op, Integer -> Integer -> Integer)]
arithBinOps = [(Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div)]

arithUnOps :: [(Op, Integer -> Integer)]
arithUnOps = [(Neg, negate), (Plus, id)]

-- Do the two effects commute?
effCommutes :: Effect -> Effect -> Bool
effCommutes Ediverges e = e `elem` [Eallocates]  -- divergence does not commute with anything
effCommutes Ewrong e = e `elem` [Eallocates]   -- WRONG does not commute with anything
effCommutes Efails e = e `elem` [Efails, Eiterates, Eallocates, Ereads, Ewrites]
effCommutes Eiterates e = e `elem` [Efails, Eallocates]
effCommutes Eallocates _ = True
effCommutes Ereads e = e `elem` [Efails, Eallocates, Ereads]
effCommutes Ewrites e = e `elem` [Efails, Eallocates]
effCommutes Ethrows e = e `elem` [Eallocates]
effCommutes Einteracts e = e `elem` [Eallocates]
effCommutes Ecovariant _ = undefined
