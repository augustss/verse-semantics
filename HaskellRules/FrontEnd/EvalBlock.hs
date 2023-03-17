{-# LANGUAGE PatternSynonyms #-}
module FrontEnd.EvalBlock where
import Prelude hiding ((<>))
import Control.Monad.State.Strict
import Data.List
--import qualified Data.Map as M
import Epic.Print
import Rules.Core
import TRS.Bind(Ident, Subst, identNotIn)

runBlock :: Expr -> Expr
runBlock e =
  let b = coreToBlock e
      b' = evalBlock b
  in  error $ prettyShow b'

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
pattern BlockFail = BBlock{ vars = [], binds = [(BVar (BIdent "_"), BFail)], result = BVar (BIdent "_") }

type BEqn = (BValue, BExpr)
type BEqnV = (BIdent, BValue)

data BExpr
  = BPrimOp Op [BValue]
  | BApply BValue BValue
  | BSplit BBlock BValue BValue
  | BChoice BBlock BBlock
  | BVal BValue
  | BFail
  | BWrong String
  deriving (Show, Eq)

pattern BEVar :: BIdent -> BExpr
pattern BEVar i = BVal (BVar i)

data BValue
  = BVar BIdent
  | BHNF BHNF
  deriving (Show, Eq)

data BHNF
  = BInt Integer
  | BArr [BValue]
  | BHLam BIdent BBlock
  deriving (Show, Eq)

{-
data Effect
  = Ediverges
  | Ecovariant
  | Esucceeds | Efails | Edecides | Eiterates
  | Eallocates | Ereads | Ewrites
  | Einteracts
  deriving (Eq, Ord, Show, Enum, Bounded)

type Effects = [Effect]

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
  pPrintPrec l p (BChoice e1 e2) = maybeParens (p > 1) $ pPrintPrec l 2 e1 <+> text "|" <+> pPrintPrec l 2 e2
  pPrintPrec l p (BVal v) = pPrintPrec l p v
  pPrintPrec _ _ BFail = text "fail"
  pPrintPrec _ _ (BWrong s) = text ("WRONG(" ++ s ++ ")")

instance Pretty BValue where
  pPrintPrec l p (BVar v) = pPrintPrec l p v
  pPrintPrec l p (BHNF h) = pPrintPrec l p h

instance Pretty BHNF where
  pPrintPrec l p (BInt i) = pPrintPrec l p i
  pPrintPrec l _ (BArr [v]) = parens (pPrintPrec l 0 v <> text ",")
  pPrintPrec l _ (BArr vs) = parens $ fsep (punctuate comma (map (pPrintPrec l 0) vs))
  pPrintPrec l p (BHLam x b) = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 x <> text "." <> pPrintPrec l 0 b

{-
instance Pretty Effect where
  pPrintPrec _ _ e = text $ tail $ show e

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
  bsubst' s = map (bsubst s)

instance (Bound a, Bound b) => Bound (a, b) where
  allBVars (a, b) = union (allBVars a) (allBVars b)
  freeBVars (a, b) = union (freeBVars a) (freeBVars b)
  bsubst' s (a, b) = (bsubst s a, bsubst s b)

instance Bound BBlock where
  allBVars b =
    (vars b) `union`
    allBVars (binds b) `union`
    allBVars (result b)
  freeBVars b =
    (freeBVars (binds b) `union` freeBVars (result b)) \\ vars b
  bsubst' s b
    | null s' = b
    | null bad = b{ binds = bsubst s' (binds b), result = bsubst s' (result b) }
    | otherwise =
      let xs = bIdentsNotIn (allBVars (vs, b))
          ss = zip bad (map BVar xs)
          sss = ss ++ s
          vars' = map f (vars b)
          f i = maybe i unVar $ lookup i ss
            where unVar (BVar x) = x
                  unVar _ = undefined
      in  b{ vars = vars', binds = bsubst sss (binds b), result = bsubst sss (result b) }
    where
      s' = filter ((`notElem` vars b) . fst) s
      bad = intersect (vars b) (freeBVars vs)
      vs = map snd s'

instance Bound BExpr where
  allBVars (BPrimOp _ vs) = unionMap allBVars vs
  allBVars (BApply v1 v2) = allBVars v1 `union` allBVars v2
  allBVars (BSplit b v1 v2) = allBVars b `union` allBVars v1 `union` allBVars v2
  allBVars (BChoice b1 b2) = allBVars b1 `union` allBVars b2
  allBVars (BVal v) = allBVars v
  allBVars BFail = []
  allBVars (BWrong _) = []
  freeBVars (BPrimOp _ vs) = unionMap freeBVars vs
  freeBVars (BApply v1 v2) = freeBVars v1 `union` freeBVars v2
  freeBVars (BSplit b v1 v2) = freeBVars b `union` freeBVars v1 `union` freeBVars v2
  freeBVars (BChoice b1 b2) = freeBVars b1 `union` freeBVars b2
  freeBVars (BVal v) = freeBVars v
  freeBVars BFail = []
  freeBVars (BWrong _) = []
  bsubst' s (BPrimOp op vs) = BPrimOp op (map (bsubst s) vs)
  bsubst' s (BApply v1 v2) = BApply (bsubst s v1) (bsubst s v2)
  bsubst' s (BSplit b v1 v2) = BSplit (bsubst s b) (bsubst s v1) (bsubst s v2)
  bsubst' s (BChoice b1 b2) = BChoice (bsubst s b1) (bsubst s b2)
  bsubst' s (BVal v) = BVal (bsubst s v)
  bsubst' _ BFail = BFail
  bsubst' _ e@(BWrong _) = e

instance Bound BValue where
  allBVars (BVar i) = [i]
  allBVars (BHNF h) = allBVars h
  freeBVars (BVar i) = [i]
  freeBVars (BHNF h) = freeBVars h
  bsubst' s v@(BVar i) | Just e <- lookup i s = e
                       | otherwise = v
  bsubst' s (BHNF h) = BHNF (bsubst s h)
  
instance Bound BHNF where
  allBVars (BInt _) = []
  allBVars (BArr vs) = unionMap allBVars vs
  allBVars (BHLam i b) = [i] `union` allBVars b
  freeBVars (BInt _) = []
  freeBVars (BArr vs) = unionMap freeBVars vs
  freeBVars (BHLam i b) = freeBVars b \\ [i]
  bsubst' _ h@(BInt _) = h
  bsubst' s (BArr vs) = BArr (map (bsubst s) vs)
  bsubst' s h@(BHLam i b)
    | null s' = h
    | i `notElem` freeBVars vs = BHLam i (bsubst s' b)
    | otherwise = 
      let x = bIdentNotIn (allBVars (vs, h))
      in  BHLam x (bsubst' ((i, BVar x) : s') b)
    where
      s' = filter ((/= i) . fst) s
      vs = map snd s'

-----------------------------------

-- ((current existentials, current equations), all used variables)
type A a = State (([Ident], [BEqn]), [Ident]) a

coreToBlock :: Expr -> BBlock
coreToBlock e = evalState (cBlock e) (undefined, allVars e)

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
cValue Op{}  = undefined
cValue (Arr es) = BHNF . BArr <$> mapM cValue es
cValue (LAM x e) = BHNF . BHLam (cIdent x) <$> cBlockV [x] e
cValue (e1 :>: e2) = addExpr e1 *> cValue e2
cValue (EXI i e) = do s <- addExist i; cValue (subst s e)
cValue e = BVar <$> addExpr e

cExpr :: Expr -> A BExpr
cExpr e@Var{} = BVal <$> cValue e
cExpr e@Int{} = BVal <$> cValue e
cExpr Op{}  = undefined
cExpr e@Arr{} = BVal <$> cValue e
cExpr e@LAM{} = BVal <$> cValue e
cExpr (e1 :=: e2) = do
  v <- cValue e1
  e <- cExpr e2
  addEqn (v, e)
  pure $ BVal v
cExpr (e1 :>: e2) = addExpr e1 *> cExpr e2
cExpr (e1 :|: e2) = BChoice <$> cBlock e1 <*> cBlock e2
cExpr (Op o :@: Arr es) = BPrimOp o <$> mapM cValue es
cExpr (Op o :@: e) = BPrimOp o . (:[]) <$> cValue e
cExpr (e1 :@: e2) = BApply <$> cValue e1 <*> cValue e2
cExpr (EXI i e) = do s <- addExist i; cExpr (subst s e)
cExpr Fail = pure BFail
cExpr Wrong = pure $ BWrong "?"
cExpr (Split e e1 e2) = BSplit <$> cBlock e <*> cValue e1 <*> cValue e2
cExpr e = error $ "cExpr: impossible: " ++ prettyShow e
{-
cExpr (One e) = One <$> cBlock e
cExpr (All e) = All <$> cBlock e

{-



exists f .
  f = fn($y1:any where
         exists g .
         g = (fn($y2:any where exists $r3 . $r3 =
                 fn($y5:any where exists x $z7 $r8 .
                   x = int[$y5];
                   $z7 = $y2[$y5];
                   $r8 = int[$r11]){$r8})
                <covariant>{$r3})
              [$y1])
      {g[0]}

f(g(x:A):B) := g[0]

f(ga:any) := let (g := (x:any) => B[ga[A[x]]]) in g[0]

-}

-}

data BlockRes = None | DoSubst BEqnV
  deriving (Show)

evalBlock :: BBlock -> BBlock
evalBlock BBlock{ binds = (_, BFail) : _ } = BlockFail
evalBlock b@BBlock{ binds = [] } = b
evalBlock b =
  case block b of
    None -> b
    DoSubst eqn -> evalBlock $ doSubst eqn b

-- Find an equation ready for substitution
block :: BBlock -> BlockRes
block b =
  case [ (i, v) | (BVar i, BVal v) <- binds b ] of
    [] -> None
    eqn : _ -> DoSubst eqn

doSubst :: BEqnV -> BBlock -> BBlock
doSubst eqn@(i, v) b =
  let bs = concatMap bind (binds b)
      bind eq | del, (BVar i', BVal v') <- eq, i == i' && v == v' = []
              | otherwise = [bsubst [eqn] eq]
      del = i `elem` vars b
      res = bsubst [eqn] (result b)
  in  b{vars = vars b \\ [i], binds = bs, result = res}
