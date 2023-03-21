{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
module FrontEnd.EvalBlock where
import Prelude hiding ((<>))
import Control.Monad.State.Strict
import Data.List
--import qualified Data.Map as M
import Epic.Print
import Rules.Core
import TRS.Bind(Ident(Name), Subst, identNotIn, free)
--import GHC.Stack
import Debug.Trace

doTrace :: Bool
doTrace = False

dtrace :: String -> a -> a
dtrace s a | doTrace = trace s a
           | otherwise = a

runBlock :: Expr -> Expr
runBlock e =
  let b = coreToChoice e
      b' = evalChoiceFull b
  in
      dtrace ("core: " ++ prettyShow e) $
      dtrace ("input: " ++ prettyShow b) $
      dtrace ("output: " ++ prettyShow b') $
      choiceToCore b'

evalChoiceFull :: BChoice -> BChoice
evalChoiceFull c =
  case evalChoice c of
    BCFork c1 c2 -> BCFork c1 (evalChoiceFull c2)
    c' -> c'

---------------------------------------------

newtype BIdent = BIdent String
  deriving (Show, Eq, Ord)

data BBlock = BBlock
  { --limitEffs :: !Effects                   -- limit effects to these
--  , needEffs  :: !Effects                   -- must have these effects
--  , vars      :: !(M.Map BIdent EffectType) -- existentially bound variables with effects
    vars      :: ![BIdent]
  , binds     :: ![BEqn]                    -- variable bindings
  , result    :: !BValue                    -- final value of the block
  }
  deriving (Show, Eq)

pattern BlockFail :: BBlock
pattern BlockFail = BBlock{ vars = [], binds = [(BDummy, BFail)], result = BDummy }

--pattern BlockWrong :: String -> BBlock
--pattern BlockWrong s = BBlock{ vars = [], binds = [(BDummy, BWrong s)], result = BDummy }

pattern BlockValue :: [BIdent] -> BValue -> BBlock
pattern BlockValue vs v = BBlock { vars = vs, binds = [], result = v }

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
  deriving (Show, Eq)

data BChoice
  = BCFork BChoice BChoice
  | BCBlk BBlock
  | BCFail
  | BCWrong String
  deriving (Show, Eq)

pattern BEVar :: BIdent -> BExpr
pattern BEVar i = BVal (BVar i)

pattern BFail :: BExpr
pattern BFail = BChoice BCFail

pattern BWrong :: String -> BExpr
pattern BWrong s = BChoice (BCWrong s)

data BValue
  = BVar BIdent
  | BHNF BHNF
  deriving (Show, Eq)

isBVar :: BValue -> Bool
isBVar BVar{} = True
isBVar _ = False

data BHNF
  = BInt Integer
  | BArr [BValue]
  | BHLam BIdent BBlock  -- invariant: the bound BIdent will not be among the vars in the block
  deriving (Show, Eq)

pattern BVInt :: Integer -> BValue
pattern BVInt i = BHNF (BInt i)

pattern BVArr :: [BValue] -> BValue
pattern BVArr vs = BHNF (BArr vs)

pattern BVLam :: BIdent -> BBlock -> BValue
pattern BVLam i b = BHNF (BHLam i b)

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

instance Pretty BHNF where
  pPrintPrec l p (BInt i) = pPrintPrec l p i
  pPrintPrec l _ (BArr [v]) = parens (pPrintPrec l 0 v <> text ",")
  pPrintPrec l _ (BArr vs) = parens $ fsep (punctuate comma (map (pPrintPrec l 0) vs))
  pPrintPrec l p (BHLam x b) = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 x <> text "." <> pPrintPrec l 0 b

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

instance (Bound a, Pretty a) => Bound [a] where
  allBVars = unionMap allBVars
  freeBVars = unionMap freeBVars
  bsubst' s = map (bsubst' s)

instance (Bound a, Bound b, Pretty a, Pretty b) => Bound (a, b) where
  allBVars (a, b) = union (allBVars a) (allBVars b)
  freeBVars (a, b) = union (freeBVars a) (freeBVars b)
  bsubst' s (a, b) = (bsubst' s a, bsubst' s b)

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
  freeBVars (BVar i) = [i]
  freeBVars (BHNF h) = freeBVars h
  bsubst' s v@(BVar i) | Just e <- lookup i s = e
                       | otherwise = v
  bsubst' s (BHNF h) = BHNF (bsubst' s h)
  
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
    | i `notElem` freeBVars vs = BHLam i (bsubst s' b)
    | otherwise = 
      let x = bIdentNotIn (allBVars (vs, h))
      in  BHLam x (bsubst' ((i, BVar x) : s') b)
    where
      s' = filter ((/= i) . fst) s
      vs = map snd s'

---------------------------------------------

choiceToCore :: BChoice -> Expr
choiceToCore BCFail = Fail
choiceToCore (BCWrong _) = Wrong
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

{-
bIdentsNotIn :: [BIdent] -> [BIdent]
bIdentsNotIn vs = [ BIdent ("$" ++ show i) | i <- [1::Integer ..] ] \\ vs

bIdentNotIn :: [BIdent] -> BIdent
bIdentNotIn = head . bIdentsNotIn
-}

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
cChoice Wrong = pure $ BCWrong "??"
cChoice e = BCBlk <$> cBlock e

-- A returned BCFork is always of the form
--  BCFork (BCBlk b) ...
bCFork :: BChoice -> BChoice -> BChoice
bCFork BCFail c = c
bCFork c@BCWrong{} _ = c
bCFork c1@BCBlk{} c2 = BCFork c1 c2
bCFork (BCFork c1 c2) c3 = bCFork c1 (BCFork c2 c3)

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
cExpr Wrong = pure $ BWrong "?"
cExpr (Split e e1 e2) = BSplit <$> cChoice e <*> cValue e1 <*> cValue e2
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
primOpEffs _ = []

-- XXX needs an environment
funcEffs :: BValue -> Effects
funcEffs _ = allEffects

-- XXX needs an environment
exprEffs :: BExpr -> Effects
exprEffs _ = allEffects

-- If the returned choice is
--   * BCBlk it has been evaluated as far as possible
--   * BCFork the first fork is a BCBlk evaluated as far as possible
evalChoice :: BChoice -> BChoice
evalChoice c | dtrace ("evalChoice: " ++ prettyShow c) False = undefined
evalChoice (BCFork c1 c2) =
  case evalChoice c1 of
    BCFail -> evalChoice c2
    c@BCWrong{} -> c
    c@BCBlk{} -> BCFork c c2
    BCFork d1 d2 -> evalChoice (BCFork d1 (BCFork d2 c2))
evalChoice (BCBlk b) = evalBlock b
evalChoice BCFail = BCFail
evalChoice c@(BCWrong _) = c

-- If the returned choice is
--   * BCBlk it has been evaluated as far as possible
--   * BCFork the first fork is a BCBlk evaluated as far as possible
evalBlock :: BBlock -> BChoice
evalBlock b | dtrace ("evalBlock: " ++ prettyShow b) False = undefined
evalBlock b =
  let c = evalBlock' b
  in  dtrace ("evalBlock returns: " ++ prettyShow c) $
      c

evalBlock' :: BBlock -> BChoice
evalBlock' BBlock{ binds = (_, BFail) : _ } = BCFail
evalBlock' BBlock{ binds = (_, BWrong s) : _ } = BCWrong s
evalBlock' b@BBlock{ binds = [] } = BCBlk b{ vars = intersect (freeBVars (result b)) (vars b) }
evalBlock' blk = loop [] [] (binds blk)
  where
    loop effs rs bs | dtrace ("loop: " ++ prettyShow (effs, vars blk, rs, bs)) False = undefined
    loop _ rs [] = BCBlk blk{ binds = reverse rs }   -- reached the end, no progress, so BCBlk is
    loop effs rs (eqn@(val, expr) : bs) =
      let
        unify (BVar x) (BVar y) | x `notElem` vars blk && y `elem` vars blk = unify (BVar y) (BVar x)
        unify (BVar x) v =
          if x `elem` freeBVars v then
            -- Recursive
            if v == BVar x then
              -- Leave x=x alone
              loop effs (eqn : rs) bs
            else
              error "*** recursion not implemented"
          else if x `elem` vars blk then
            evalBlock' blk{
              vars = vars blk \\ [x],
              binds = bsubst [(x, v)] (reverse rs ++ bs),
              result = bsubst [(x, v)] $ result blk }
          else if x `elem` freeBVars ((rs, bs), result blk) then
            evalBlock' blk{
              vars = vars blk,
              binds = bsubst [(x, v)] (reverse rs) ++ [eqn] ++ bsubst [(x, v)] bs,
              result = bsubst [(x, v)] $ result blk }
          else
            loop effs (eqn : rs) bs
        unify v (BVar x) = unify (BVar x) v
        unify (BVInt i) (BVInt j) | i == j = succeeds []
        unify (BVArr vs) (BVArr ws) | length vs == length ws = succeeds $ zipWith (\ v w -> (v, BVal w)) vs ws
        unify _x@(BVLam _ _) _y@(BVLam _ _) =
          -- According to the ICFP paper this fails.  Being WRONG would be better
          fails
          -- wrongs $ "unify lambda: " ++ prettyShow (_x, _y)
        unify _ _ = fails

        fails | all (effCommutes Efails) effs = BCFail
              | otherwise = loop ([Efails] `union` effs) ((BDummy, BFail) : rs) bs
        wrongs s | all (effCommutes Ewrong) effs = BCWrong s
                 | otherwise = loop ([Ewrong] `union` effs) ((BDummy, BWrong s) : rs) bs

        succeeds es = loop effs rs (es ++ bs)
      in
        case expr of
          BPrimOp op vs | Just e <- evalPrimOp op vs -> succeeds [(val, e)]
                        | otherwise -> loop (primOpEffs op `union` effs) (eqn : rs) bs
          -- XXX evalBlock' is overkill.  Could keep vars in the loop and extend it.
          BApply f a | BVLam i b <- f ->
                       if i == BIdent "_" then
                         evalBlock' $ insertBlock blk (reverse rs) Nothing (val, b) bs
                       else
                         let b' = b{ vars = i : vars b }
                         in  evalBlock' $ insertBlock blk (reverse rs) (Just a) (val, b') bs
                     | BVArr vs <- f ->
                       let e = BChoice $ choices [ BBlock { vars = [], binds = [(a, BVal $ BVInt i)], result = v }
                                                 | (i, v) <- zip [0..] vs ]
                       in  loop effs rs ((val, e) : bs)
                     | otherwise -> loop (funcEffs f `union` effs) (eqn : rs) bs
          BSplit c f g ->
            case evalChoice c of  -- XXX need to propagate blocked effects
              BCFail -> succeeds [(val, BApply f (BVArr []))]
              BCWrong s -> wrongs s
              BCBlk b@BlockValue{} -> callG b BCFail
              BCFork (BCBlk b@BlockValue{}) r -> callG b r
              c' -> loop (exprEffs e `union` effs) ((val, e) : rs) bs  where e = BSplit c' f g
            where callG b r =
                    let (vb: a1: a2: a3: dummy: _) = bIdentsNotIn (vars blk ++ freeBVars (b, c))
                        b0 = (BVar vb, BChoice (BCBlk b))
                        b1 = (BVar a1, BApply g (BVar vb))
                        b2 = (BVar a2, BApply (BVar a1) (BVLam dummy (blkChoice r)))
                        b3 = (BVar a3, BApply (BVar a2) g)
                        bb = BBlock{ vars = [vb, a1, a2, a3], binds = [b0,b1,b2,b3], result = BVar a3 }
                    in  evalBlock' $ insertBlock blk (reverse rs) Nothing (val, bb) bs
          BChoice BCFail -> fails
          BChoice (BCWrong s) -> wrongs s
          -- XXX evalBlock' is overkill.  Could keep vars in the loop and extend it.
          BChoice (BCBlk b) -> evalBlock' $ insertBlock blk (reverse rs) Nothing (val, b) bs
          BChoice (BCFork x1 x2) | all (effCommutes Eiterates) effs -> evalChoice (BCFork c1 c2)
                                 | otherwise -> loop ([Eiterates] `union` effs) (eqn : rs) bs
            where
              c1 = BCBlk $ blk{ binds = rrs ++ [(val, BChoice x1)] ++ bs }
              c2 = BCBlk $ blk{ binds = rrs ++ [(val, BChoice x2)] ++ bs }
              rrs = reverse rs
          BVal v -> unify val v

blkChoice :: BChoice -> BBlock
blkChoice (BCBlk b) = b
blkChoice c = BBlock { vars = [i], binds = [(BVar i, BChoice c)], result = BVar i }
  where i = bIdentNotIn (allBVars c)

-- Insert the block rhs into parent.  The equations are
-- before ++ [(lhs, rhs)] ++ after
-- XXX need to limit effects properly
insertBlock :: BBlock -> [BEqn] -> Maybe BValue -> (BValue, BBlock) -> [BEqn] -> BBlock
insertBlock parent before extra (lhs, rhs) after =
  let rhs' = freshenBlock (vars parent) rhs
      ex = maybe [] (\ v -> [(BVar (head (vars rhs')), BVal v)]) extra
  in  parent{ vars = vars rhs' ++ vars parent
            , binds = before ++ ex ++ binds rhs' ++ [(lhs, BVal $ result rhs')] ++ after
            }

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
