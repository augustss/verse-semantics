{-# OPTIONS_GHC -Wall -Wno-incomplete-uni-patterns  #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#define EXT 0
module FrontEnd.EvalBlock(runBlock) where
import Prelude hiding ((<>))
import Control.Monad.State.Strict
import Data.Coerce
import Data.Data(Data)
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Ratio
import Data.Scientific
import Epic.Print
import Epic.Uniplate
import qualified Epic.SIntMap as IM
--import Rules.Core
--import TRS.Bind(Ident(Name), Subst, identNotIn, free)
--import GHC.Stack
import Debug.Trace
import System.IO
import System.IO.Unsafe(unsafePerformIO)
import Rules.Core(TRSFlags, RuleEnv(..))
import FrontEnd.Expr(Expr(..), Ident(..), Lit(..), noLoc, seqE)
import FrontEnd.Desugar(getFree, substMany, getAllVars)

-- TODO:
--  * Write down how evaluation works.
--  * Equip lambda with function effects
--    - What checks are needed for higher order function passing?
--  * Properly track effects
--  * Check for effect violation

type Op = String
type Subst e = [(Ident, e)]

opIntAdd, opIntSub, opIntMul, opIntDiv, opIntNeg, opIntPlus, opIntGt, opIntGe, opIntLt, opIntLe, opIntNe :: Op
opRatAdd, opRatSub, opRatMul, opRatDiv, opRatNeg, opRatPlus, opRatGt, opRatGe, opRatLt, opRatLe, opRatNe :: Op
opF32Add, opF32Sub, opF32Mul, opF32Div, opF32Neg, opF32Plus, opF32Gt, opF32Ge, opF32Lt, opF32Le, opF32Ne :: Op
opF64Add, opF64Sub, opF64Mul, opF64Div, opF64Neg, opF64Plus, opF64Gt, opF64Ge, opF64Lt, opF64Le, opF64Ne :: Op
opIsInt, opIsRat, opIsArr, opIsF32, opIsF64, opIsChr, opIsStr, opIsFcn :: Op
opErr, opArrLen, opMapAp, opCons, opAlloc, opRead, opWrite, opAddTo, opDotDot,opPrint, opAppend :: Op

opIntGt    = "intGT$"
opIntGe    = "intGE$"
opIntLt    = "intLT$"
opIntLe    = "intLE$"
opIntNe    = "intNE$"
opIntAdd   = "intAdd$"
opIntSub   = "intSub$"
opIntMul   = "intMul$"
opIntDiv   = "intDiv$"
opIntNeg   = "intNeg$"
opIntPlus  = "intPlus$"

opRatGt    = "ratGT$"
opRatGe    = "ratGE$"
opRatLt    = "ratLT$"
opRatLe    = "ratLE$"
opRatNe    = "ratNE$"
opRatAdd   = "ratAdd$"
opRatSub   = "ratSub$"
opRatMul   = "ratMul$"
opRatDiv   = "ratDiv$"
opRatNeg   = "ratNeg$"
opRatPlus  = "ratPlus$"
opF32Gt    = "f32GT$"
opF32Ge    = "f32GE$"
opF32Lt    = "f32LT$"
opF32Le    = "f32LE$"
opF32Ne    = "f32NE$"
opF32Add   = "f32Add$"
opF32Sub   = "f32Sub$"
opF32Mul   = "f32Mul$"
opF32Div   = "f32Div$"
opF32Neg   = "f32Neg$"
opF32Plus  = "f32Plus$"
opF64Gt    = "f64GT$"
opF64Ge    = "f64GE$"
opF64Lt    = "f64LT$"
opF64Le    = "f64LE$"
opF64Ne    = "f64NE$"
opF64Add   = "f64Add$"
opF64Sub   = "f64Sub$"
opF64Mul   = "f64Mul$"
opF64Div   = "f64Div$"
opF64Neg   = "f64Neg$"
opF64Plus  = "f64Plus$"

opIsInt = "isInt$"
opIsRat = "isRat$"
opIsF32 = "isF32$"
opIsF64 = "isF64$"
opIsChr = "isChr$"
opIsStr = "isStr$"
opIsFcn = "isFcn$"
opIsArr = "isArr$"
opArrLen= "arrLen$"
opMapAp = "mapAp$"
opCons  = "cons$"
opAlloc = "alloc$"
opRead  = "read$"
opWrite = "write$"
opAddTo = "in'+='"
opDotDot= "in'..'"
opPrint = "print$"
opAppend= "append$"
opErr   = "err$"

pattern One :: Expr -> Expr
pattern One x <- Macro1 (Ident _ "one") [] x
pattern All :: Expr -> Expr
pattern All x <- Macro1 (Ident _ "all") [] x

identNotIn :: [Ident] -> Ident
identNotIn vs = head $ [Ident noLoc ("$q" ++ show i) | i <- [1::Int ..]] \\ vs

-----------------------------------------------

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = null (xs \\ ys)

--------------------

--
-- TODO
--  * propagate allowed effects
--  * check if an effect is allowed
--  * limit effects for lambda bodies

doStep :: Bool
doStep = True

dtrace :: AllowedEffects -> String -> a -> a
dtrace effs s a | Etrace `notElem` effs = a
                | otherwise = unsafePerformIO $ do
                    putStrLn s
                    when doStep $ do
                      putStr "step> "
                      hFlush stdout
                      void getLine
                    pure a

runBlock :: TRSFlags -> Expr -> Expr
runBlock flg e =
  let b = coreToChoice e
      b' = evalChoiceFull emptyHeap effs b
      b'' = if tfUnderLambda flg then evalUnderLambda trcEff b' else b'
      trcEff = [Etrace | tfTrace flg]
      effs = trcEff ++ topEffects
  in
      dtrace effs ("core: " ++ prettyShow e) $
      dtrace effs ("input: " ++ prettyShow b) $
      dtrace effs ("output: " ++ prettyShow b') $
      dtrace effs ("output-lam: " ++ prettyShow b'') $
      dtrace effs ("output-core: " ++ show (choiceToCore b'')) $
      choiceToCore b''

evalChoiceFull :: BHeap -> AllowedEffects -> BChoice -> BChoice
evalChoiceFull h aeffs c =
  case evalChoice h aeffs [] c of
    BCFork (BCRBlk h' b) c2 ->
      case evalChoiceFull h' aeffs c2 of
        BCFail -> BCBlk $ dropUnusedEx b
        c2' -> BCFork (BCBlk $ dropUnusedEx b) c2'
    BCFork (BCBlk _b) _c2 -> undefined -- BCFork (BCBlk b) (evalChoiceFull h aeffs c2)  -- XXX
    BCRBlk _ b -> BCBlk $ dropUnusedEx b  -- throw away the heap
    c' -> c'

evalUnderLambda :: AllowedEffects -> BChoice -> BChoice
evalUnderLambda trcEffs = transformBi ev
  where ev :: BHNF -> BHNF
        ev v@BLit{} = v
        ev (BArr vs) = BArr (map (transformBi ev) vs)
        ev (BHLam i b) = BHLam i $ transformBi ev $ evalB b
#if EXT
        ev (BExt a r x) = BExt (ev a) (transformBi ev r) (transformBi ev x)
#endif
        evalB b =
          case evalBlock dummyHeap (trcEffs ++ lambdaEffs) [] b of
            BCBlk b' -> b'
            BCFail -> BlockFail
            BCDomainFail -> BlockDomainFail
            BCWrong s -> BlockWrong s
            BCFork c1 c2 -> blkChoice (BCFork c1 (evalChoiceFull dummyHeap lambdaEffs c2))
            BCRBlk _ b' -> dropUnusedEx b'   -- XXX
        lambdaEffs = [Efails, Eiterates, Ewrong]

-- When throwing away the heap, we need to get rid of things only bound in the heap.
dropUnusedEx :: BBlock -> BBlock
dropUnusedEx b = b{ vars = vars b `intersect` freeBVars (binds b, result b) }

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
  deriving (Show, Eq, Ord, Data)

pattern BlockFail :: BBlock
pattern BlockFail = BBlock{ vars = [], binds = [(BAVar, BFail)], result = BAVar }

pattern BlockDomainFail :: BBlock
pattern BlockDomainFail = BBlock{ vars = [], binds = [(BAVar, BDomainFail)], result = BAVar }

pattern BlockWrong :: String -> BBlock
pattern BlockWrong s = BBlock{ vars = [], binds = [(BAVar, BWrong s)], result = BAVar }

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

pattern BAVar :: BValue
pattern BAVar = BVar (BIdent "$$")

type BEqn = (BValue, BExpr)
type BEqnV = (BIdent, BValue)

data BExpr
  = BPrimOp Op BValue
  | BApply BValue BValue
  | BSplit BChoice BValue BValue
  | BChoice BChoice
  | BVal BValue
  deriving (Show, Eq, Ord, Data)

pattern BEBlk :: BBlock -> BExpr
pattern BEBlk b = BChoice (BCBlk b)

data BChoice
  = BCFork BChoice BChoice
  | BCBlk BBlock
  | BCFail
  | BCDomainFail
  | BCWrong String
  | BCRBlk BHeap BBlock    -- only in results
  deriving (Show, Eq, Ord, Data)

--pattern BEVar :: BIdent -> BExpr
--pattern BEVar i = BVal (BVar i)

pattern BFail :: BExpr
pattern BFail = BChoice BCFail

pattern BDomainFail :: BExpr
pattern BDomainFail = BChoice BCDomainFail

pattern BWrong :: String -> BExpr
pattern BWrong s = BChoice (BCWrong s)

data BValue
  = BVar BIdent
  | BHNF BHNF
  deriving (Show, Eq, Ord, Data)

isBVar :: BValue -> Bool
isBVar BVar{} = True
isBVar _ = False

data BLiteral
  = BRat Rational
  | BF32 Float
  | BF64 Double
  | BStr String
  | BChr Char
  | BRef BPtr
  deriving (Show, Eq, Ord, Data)

pattern BInt :: Integer -> BLiteral
pattern BInt i <- (getInt -> Just i)
  where BInt i = BRat (fromInteger i)

getInt :: BLiteral -> Maybe Integer
getInt (BRat r) | denominator r == 1 = Just (numerator r)
getInt _ = Nothing

data BHNF
  = BLit BLiteral
  | BArr [BValue]
  | BHLam BIdent BBlock  -- invariant: the bound BIdent will not be among the vars in the block
#if EXT
  | BExt BHNF BValue BValue
#endif
  deriving (Show, Eq, Ord, Data)

pattern BVInt :: Integer -> BValue
pattern BVInt i = BVLit (BInt i)

pattern BVRat :: Rational -> BValue
pattern BVRat i = BVLit (BRat i)

pattern BVF32 :: Float -> BValue
pattern BVF32 i = BVLit (BF32 i)

pattern BVF64 :: Double -> BValue
pattern BVF64 i = BVLit (BF64 i)

pattern BVLit :: BLiteral -> BValue
pattern BVLit x = BHNF (BLit x)

pattern BVRef :: BPtr -> BValue
pattern BVRef i = BHNF (BLit (BRef i))

pattern BVArr :: [BValue] -> BValue
pattern BVArr vs = BHNF (BArr vs)

pattern BVLam :: BIdent -> BBlock -> BValue
pattern BVLam i b = BHNF (BHLam i b)

#if EXT
pattern BVExt :: BHNF -> BValue -> BValue -> BValue
pattern BVExt a r x = BHNF (BExt a r x)
#endif

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
  | Einvariant
  ---- 
  | Etrace         -- not really an effect, used for debugging
  deriving (Eq, Ord, Show, Enum, Bounded)

type Effects = [Effect]

allEffects :: Effects
allEffects = [minBound .. maxBound]

data BHeap = BHeap (IM.SIntMap BPtr BValue) | BNoHeap
  deriving (Show, Eq, Ord, Data)

newtype BPtr = BPtr Int
  deriving (Show, Eq, Ord, Data, Enum)

emptyHeap :: BHeap
emptyHeap = BHeap IM.empty

-- Use this heap when no memory operations should happen
dummyHeap :: BHeap
dummyHeap = BNoHeap

heapAlloc :: BHeap -> BValue -> (BHeap, BPtr)
heapAlloc BNoHeap _ = error "heapAlloc: NoHeap"
heapAlloc (BHeap h) v =
  let p | IM.null h = BPtr 0
        | otherwise = succ (fst (IM.findMax h))
      h' = IM.insert p v h
  in  (BHeap h', p)

heapRead :: BHeap -> BPtr -> BValue
heapRead BNoHeap _ = error "heapRead: NoHeap"
heapRead (BHeap h) p = fromMaybe (error $ "heapRead: " ++ show p) $ IM.lookup p h

heapWrite :: BHeap -> BPtr -> BValue -> BHeap
heapWrite BNoHeap _ _ = error "heapWrite: NoHeap"
heapWrite (BHeap h) p v = BHeap $ IM.insert p v h

-----------------------------------------

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
  pPrintPrec l _ (BPrimOp op v) = pPrintPrec l 10 op <> parens (pPrintPrec l 0 v)
  pPrintPrec l _ (BApply f a) = pPrintPrec l 10 f <> parens (pPrintPrec l 0 a)
  pPrintPrec l _ (BSplit b f g) = text "split" <> parens (pPrintPrec l 0 b) <> braces (pPrintPrec l 1 f <> text ";" <+> pPrintPrec l 1 g)
  pPrintPrec l p (BChoice c) = pPrintPrec l p c
  pPrintPrec l p (BVal v) = pPrintPrec l p v

instance Pretty BChoice where
  pPrintPrec l p (BCFork e1 e2) = maybeParens (p > 1) $ pPrintPrec l 2 e1 <+> text "|" <+> pPrintPrec l 2 e2
  pPrintPrec l p (BCBlk b) = pPrintPrec l p b
  pPrintPrec _ _ BCFail = text "fail"
  pPrintPrec _ _ BCDomainFail = text "domainfail"
  pPrintPrec _ _ (BCWrong s) = text ("WRONG(" ++ s ++ ")")
  pPrintPrec l _ (BCRBlk h b) = text "store" <+> parens (pPrintPrec l 0 h) <+> braces (pPrintPrec l 0 b)

instance Pretty BValue where
  pPrintPrec l p (BVar v) = pPrintPrec l p v
  pPrintPrec l p (BHNF h) = pPrintPrec l p h

instance Pretty BHNF where
  pPrintPrec l p (BLit x) = pPrintPrec l p x
  pPrintPrec l _ (BArr [v]) = parens (pPrintPrec l 0 v <> text ",")
  pPrintPrec l _ (BArr vs) = parens $ fsep (punctuate comma (map (pPrintPrec l 0) vs))
  pPrintPrec l p (BHLam x b) = maybeParens (p > 0) $ text "\\" <> pPrintPrec l 0 x <> text "." <+> pPrintPrec l 0 b
#if EXT
  pPrintPrec l _ (BExt a r x) = parens $
    text "\\ arg . IF arg=" <> pPrintPrec l 0 a <+> text "THEN" <+> pPrintPrec l 0 r <+> text "ELSE" <+> pPrintPrec l 0 x
#endif

instance Pretty Effect where
  pPrintPrec _ _ e = text $ tail $ show e

instance Pretty BLiteral where
  pPrintPrec l p (BRat r) = if denominator r == 1
                            then pPrintPrec l p (numerator r)
                            else text $ "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"
  pPrintPrec l p (BF32 f) = pPrintPrec l p f <> text "f32"
  pPrintPrec l p (BF64 f) = pPrintPrec l p f <> text "f64"
  pPrintPrec l p (BStr s) = pPrintPrec l p s
  pPrintPrec l p (BChr c) = pPrintPrec l p c
  pPrintPrec l p (BRef r) = pPrintPrec l p r

{-
instance Pretty EffectType where
  pPrintPrec _ _ ETNone = text "none"
  pPrintPrec _ _ ETUnknown = text "?"
  pPrintPrec l p (ETArrow a rs b) = maybeParens (p>0) $ pPrintPrec l 1 a <> text "-" <> t <> pPrintPrec l 1 b
    where t | [] <- rs = text ">"
            | [Ediverges,Eallocates,Ereads,Ewrites] <- rs = text "-<heap>"
            | otherwise = text "-<" <> commaSep l rs <> text ">"
-}

instance Pretty BHeap where
  pPrintPrec l p (BHeap m) = pPrintPrec l p (IM.toList m)
  pPrintPrec _ _ BNoHeap = text "NoHeap"

instance Pretty BPtr where
  pPrintPrec _ _ (BPtr i) = text ("r" ++ show i)

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
    | otherwise = b'{
        binds = bsubst s' (binds b'),
        result = bsubst s' (result b') }
    where
      bnd = freeBVars (map snd s')
      s' = filter ((`notElem` vars b) . fst) s
      b' = freshenBlock bnd b

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
  allBVars (BPrimOp _ v) = allBVars v
  allBVars (BApply v1 v2) = allBVars v1 `union` allBVars v2
  allBVars (BSplit b v1 v2) = allBVars b `union` allBVars v1 `union` allBVars v2
  allBVars (BChoice c) = allBVars c
  allBVars (BVal v) = allBVars v
  freeBVars (BPrimOp _ v) = freeBVars v
  freeBVars (BApply v1 v2) = freeBVars v1 `union` freeBVars v2
  freeBVars (BSplit b v1 v2) = freeBVars b `union` freeBVars v1 `union` freeBVars v2
  freeBVars (BChoice c) = freeBVars c
  freeBVars (BVal v) = freeBVars v
  bsubst' s (BPrimOp op v) = BPrimOp op (bsubst' s v)
  bsubst' s (BApply v1 v2) = BApply (bsubst' s v1) (bsubst' s v2)
  bsubst' s (BSplit b v1 v2) = BSplit (bsubst' s b) (bsubst' s v1) (bsubst' s v2)
  bsubst' s (BChoice c) = BChoice (bsubst' s c)
  bsubst' s (BVal v) = BVal (bsubst' s v)

instance Bound BChoice where
  allBVars (BCFork b1 b2) = allBVars b1 `union` allBVars b2
  allBVars (BCBlk b) = allBVars b
  allBVars BCFail = []
  allBVars BCDomainFail = []
  allBVars (BCWrong _) = []
  allBVars (BCRBlk h b) = allBVars h `union` allBVars b
  freeBVars (BCFork b1 b2) = freeBVars b1 `union` freeBVars b2
  freeBVars (BCBlk b) = freeBVars b
  freeBVars BCFail = []
  freeBVars BCDomainFail = []
  freeBVars (BCWrong _) = []
  freeBVars (BCRBlk h b) = freeBVars h `union` freeBVars b
  bsubst' s (BCFork b1 b2) = BCFork (bsubst' s b1) (bsubst' s b2)
  bsubst' s (BCBlk b) = BCBlk (bsubst' s b)
  bsubst' _ BCFail = BCFail
  bsubst' _ BCDomainFail = BCDomainFail
  bsubst' _ e@(BCWrong _) = e
  bsubst' s (BCRBlk h b) = BCRBlk (bsubst' s h) (bsubst' s b)

instance Bound BValue where
  allBVars (BVar i) = [i]
  allBVars (BHNF h) = allBVars h
  freeBVars (BVar i) = [i]
  freeBVars (BHNF h) = freeBVars h
  bsubst' s v@(BVar i) | Just e <- lookup i s = e
                       | otherwise = v
  bsubst' s (BHNF h) = BHNF (bsubst' s h)
  
instance Bound BHNF where
  allBVars (BLit _) = []
  allBVars (BArr vs) = unionMap allBVars vs
  allBVars (BHLam i b) = [i] `union` allBVars b
#if EXT
  allBVars (BExt a r x) = allBVars (a, r, x)
#endif
  freeBVars (BLit _) = []
  freeBVars (BArr vs) = unionMap freeBVars vs
  freeBVars (BHLam i b) = freeBVars b \\ [i]
#if EXT
  freeBVars (BExt a r x) = freeBVars (a, r, x)
#endif
  bsubst' _ h@(BLit _) = h
  bsubst' s (BArr vs) = BArr (map (bsubst' s) vs)
  bsubst' s h@(BHLam i b)
    | null s' = h
    | otherwise = BHLam i' (bsubst s' b')
    where
      s' = filter ((/= i) . fst) s
      (i', b') = freshenLambda (freeBVars (map snd s')) (i, b)
#if EXT
  bsubst' s (BExt a r x) = BExt (bsubst' s a) (bsubst' s r) (bsubst' s x)
#endif

freshenLambda :: [BIdent] -> (BIdent, BBlock) -> (BIdent, BBlock)
freshenLambda bnd (i, b) | i `notElem` bnd = (i, b)
                         | otherwise =
  let x = bIdentNotIn (bnd ++ allBVars b)
  in  (x, bsubst' [(i, BVar x)] b)

instance Bound BHeap where
  allBVars (BHeap h) = unionMap allBVars (IM.elems h)
  allBVars BNoHeap = []
  freeBVars (BHeap h) = unionMap freeBVars (IM.elems h)
  freeBVars BNoHeap = []
  bsubst' s (BHeap h) = BHeap $ IM.map (bsubst' s) h
  bsubst' _ BNoHeap = BNoHeap

---------------------------------------------

choiceToCore :: BChoice -> Expr
choiceToCore BCFail = Fail
choiceToCore BCDomainFail = DomainFail
choiceToCore (BCWrong s) = Wrong s
choiceToCore (BCBlk b) = blockToCore b
choiceToCore (BCFork c1 c2) = choiceToCore c1 `Choice` choiceToCore c2
choiceToCore (BCRBlk _ b) = blockToCore b
--choiceToCore (BCRBlk h b) = error $ "choiceToCore: " ++ prettyShow (h, b)

blockToCore :: BBlock -> Expr
blockToCore (BBlock [v] [(BVar v', e)] (BVar v'')) | v == v' && v == v'' = exprToCore e
blockToCore b = if null vs then bs else Exists vs bs
  where bs = foldr eqn (valueToCore (result b)) (binds b)
        eqn (v, e) r = seqE [valueToCore v `Unify` exprToCore e, r]
        vs = map bIdentToIdent (vars b)

valueToCore :: BValue -> Expr
valueToCore (BVar v) = Variable (bIdentToIdent v)
valueToCore (BHNF h) = hnfToCore h

bIdentToIdent :: BIdent -> Ident
bIdentToIdent (BIdent s) = Ident noLoc s

hnfToCore :: BHNF -> Expr
hnfToCore (BLit l) = Lit $ bLitToLit l
hnfToCore (BArr vs) = Array (map valueToCore vs)
hnfToCore (BHLam i e) = lam (bIdentToIdent i) (blockToCore e)
  where lam ii (f `ApplyD` Variable i') | ii == i', ii `notElem` getFree f = f
        lam ii ee = Lam ii ee
#if EXT
hnfToCore (BExt a r x) = Lam (Ident noLoc "_") (Variable (Ident noLoc $ "EXT" ++ prettyShow (a, r, x)))
#endif

bLitToLit :: BLiteral -> Lit
bLitToLit (BRat r) | denominator r == 1 = LitInt (numerator r)
                   | otherwise = LitRat (fromRational r) "r"
bLitToLit (BF32 f) = LitRat (fromRational $ toRational f) "f32"
bLitToLit (BF64 f) = LitRat (fromRational $ toRational f) "f64"
bLitToLit (BChr i) = LitChar i
bLitToLit (BStr i) = LitStr i
bLitToLit (BRef r) = LitPtr (coerce r)


exprToCore :: BExpr -> Expr
exprToCore (BPrimOp o v) = EPrim o `ApplyD` valueToCore v
exprToCore (BApply f a) = valueToCore f `ApplyD` valueToCore a
exprToCore (BSplit e f g) = Split (choiceToCore e) (valueToCore f) (valueToCore g)
exprToCore (BChoice c) = choiceToCore c
exprToCore (BVal v) = valueToCore v

-----------------------------------

-- ((current existentials, current equations), all used variables)
type A a = State (([Ident], [BEqn]), [Ident]) a

coreToChoice :: Expr -> BChoice
coreToChoice e = evalState (cChoice e) (undefined, getAllVars e)

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
    pure [(x, Variable y)]
   else do
    put ((x:is, qs), vs)
    pure []

addExpr :: Expr -> A BIdent
addExpr (Variable x) = pure $ cIdent x
addExpr (Variable x `Unify` e) = do
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
cChoice (e1 `Choice` e2) = BCFork <$> cChoice e1 <*> cChoice e2
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
cValue (Variable i) = pure $ BVar (cIdent i)
cValue (Lit l) = pure $ BHNF $ BLit $ litToBLit l
cValue (EPrim o) = cValue $ Lam i (EPrim o `ApplyD` Variable i) where i = Ident noLoc "a"
cValue (Array es) = BHNF . BArr <$> mapM cValue es
cValue (Lam x e) = BHNF . BHLam (cIdent x) <$> cBlockV [x] e
cValue (Seq []) = undefined
cValue (Seq [e]) = cValue e
cValue (Seq (e : es)) = addExpr e *> cValue (Seq es)
cValue (Exists is e) = do ss <- mapM addExist is; cValue (substMany(concat ss) e)
cValue e = BVar <$> addExpr e

litToBLit :: Lit -> BLiteral
litToBLit (LitInt i) = BRat $ fromInteger i
litToBLit (LitRat s "r") = BRat $ toRational s
litToBLit (LitRat s "f32") = BF32 $ toRealFloat s
litToBLit (LitRat s "f64") = BF64 $ toRealFloat s
litToBLit (LitChar c) = BChr c
litToBLit (LitStr s) = BStr s
litToBLit (LitPtr i) = BRef $ coerce i
litToBLit l = error $ "litToBLit: " ++ show l

cExpr :: Expr -> A BExpr
cExpr e@Variable{} = BVal <$> cValue e
cExpr e@Lit{} = BVal <$> cValue e
cExpr e@EPrim{}  = BVal <$> cValue e
cExpr e@Array{} = BVal <$> cValue e
cExpr e@Lam{} = BVal <$> cValue e
cExpr (e1 `Unify` e2) = do
  v <- cValue e1
  e <- cExpr e2
  addEqn (v, e)
  pure $ BVal v
cExpr (Seq []) = undefined
cExpr (Seq [e]) = cExpr e
cExpr (Seq (e : es)) = addExpr e *> cExpr (Seq es)
cExpr e@(_ `Choice` _) = BChoice <$> cChoice e
cExpr (EPrim o `ApplyD` e) = BPrimOp o <$> cValue e
cExpr (e1 `ApplyD` e2) = BApply <$> cValue e1 <*> cValue e2
cExpr (Exists is e) = do ss <- mapM addExist is; cExpr (substMany(concat ss) e)
cExpr Fail = pure BFail
cExpr (Wrong s) = pure $ BWrong s
cExpr (Split e e1 e2) = BSplit <$> cChoice e <*> cValue e1 <*> cValue e2
cExpr (One e) = cExpr $ Split e (Lam u Fail) (Lam v $ Lam u $ Lam u $ Variable v)
  where u = Ident noLoc "_"; v = Ident noLoc "v"
cExpr (All e) =
  let u = Ident noLoc "_"; v = Ident noLoc "v"; r = Ident noLoc "r"; h = Ident noLoc "h"
      f = Lam u $ Array []
      g = Lam v $ Lam r $ Lam h $
            let x = Split (Variable r `ApplyD` Array []) f (Variable h)
            in  EPrim opCons `ApplyD` Array [Variable v, x]
  in  cExpr $ Split e f g
cExpr DomainFail = pure BDomainFail
cExpr e = error $ "cExpr: impossible: " ++ prettyShow e ++ "\n" ++ show e
{-
cExpr (One e) = One <$> cBlock e
cExpr (All e) = All <$> cBlock e
-}

primOpEffs :: Op -> Effects
primOpEffs o = fromMaybe [] $ M.lookup o $ M.fromList [

  (opIntGt, [Efails]),
  (opIntGe, [Efails]),
  (opIntLt, [Efails]),
  (opIntLe, [Efails]),
  (opIntNe, [Efails]),

  (opRatGt, [Efails]),
  (opRatGe, [Efails]),
  (opRatLt, [Efails]),
  (opRatLe, [Efails]),
  (opRatNe, [Efails]),
  (opF32Gt, [Efails]),
  (opF32Ge, [Efails]),
  (opF32Lt, [Efails]),
  (opF32Le, [Efails]),
  (opF32Ne, [Efails]),
  (opF64Gt, [Efails]),
  (opF64Ge, [Efails]),
  (opF64Lt, [Efails]),
  (opF64Le, [Efails]),
  (opF64Ne, [Efails]),

  (opIsInt, [Efails]),
  (opIsRat, [Efails]),
  (opIsF32, [Efails]),
  (opIsF64, [Efails]),
  (opIsChr, [Efails]),
  (opIsStr, [Efails]),
  (opIsFcn, [Efails]),

  (opMapAp, allEffects),  -- XXX can do better?
  (opAlloc, [Eallocates]),
  (opRead,  [Ereads]),
  (opWrite, [Ewrites]),
  (opAddTo, [Ereads, Ewrites]),
  (opDotDot, [Eiterates]),
  (opPrint, [Einteracts])
  ]

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
choiceEffs BCDomainFail = []  -- ???
choiceEffs (BCWrong _) = [Ewrong]
choiceEffs (BCRBlk _ b) = blockEffs b

blockEffs :: BBlock -> Effects
blockEffs b = unionMap (exprEffs . snd) (binds b)

-- Effects that should not be blocked in the domain of an if/for
domEffects :: Effects
domEffects = [Efails, Eiterates] ++ heapEffects ++ [Etrace]

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
evalChoice :: BHeap -> AllowedEffects -> BlockedEffects -> BChoice -> BChoice
evalChoice _ aeffs _ c | dtrace aeffs ("evalChoice: " ++ takeWhile (/= ' ') (show c) ++ "\n" ++ prettyShow c) False = undefined
evalChoice heap aeffs beffs (BCFork c1 c2) =
  case evalChoice heap aeffs beffs c1 of
    BCFail -> evalChoice heap aeffs beffs c2
    c@BCWrong{} -> c
--    c@BCBlk{} -> undefined -- BCFork c c2  -- XXX
--    BCFork d1@(BCRBlk _ _) d2 -> BCFork d1 (BCFork d2 c2)
    BCFork d1@BCRBlk{} d2 -> BCFork d1 (BCFork d2 c2) -- XXX
    c@BCRBlk{} -> BCFork c c2
    _ -> error "impossible"
evalChoice heap aeffs beffs (BCBlk b) = evalBlock heap aeffs beffs b
evalChoice _ _ _ BCFail = BCFail
evalChoice _ _ _ BCDomainFail = BCDomainFail
evalChoice _ _ _ c@(BCWrong _) = c
evalChoice heap aeffs beffs (BCRBlk h b) | heap == h = evalBlock heap aeffs beffs b
evalChoice _ _ _ c@BCRBlk{} = error $ "evalChoice: " ++ prettyShow c

-- If the returned choice is
--   * BCBlk it has been evaluated as far as possible
--   * BCFork the first fork is a BCBlk evaluated as far as possible
evalBlock :: BHeap -> AllowedEffects -> BlockedEffects -> BBlock -> BChoice
evalBlock _ aeffs _ b | dtrace aeffs ("evalBlock: " ++ prettyShow b) False = undefined
evalBlock heap aeffs beffs b =
  let c = evalBlock' heap aeffs beffs b
  in  dtrace aeffs ("evalBlock returns: " ++ prettyShow c) $
      --checkPostCond aeffs beffs c
      c

{-
checkPostCond :: AllowedEffects -> BlockedEffects -> BChoice -> BChoice
checkPostCond aeffs beffs c@(BCBlk b) | evalBlock' dummyHeap aeffs beffs b == c = c
                                      | otherwise = error $ "checkPostCond: " ++ prettyShow c
checkPostCond aeffs beffs c@(BCFork c1@(BCBlk b) _) | evalBlock' dummyHeap aeffs beffs b == c1 = c
checkPostCond _ _ c@BCFork{} = error $ "checkPostCond: " ++ prettyShow c
checkPostCond _ _ c = c
-}

freshenHeap :: [BIdent] -> [BIdent] -> BHeap -> ([BIdent], BHeap)
freshenHeap used bnd h =
  let xs = intersect bnd (freeBVars h)
      is = take (length xs) $ bIdentsNotIn used
      s = zip xs $ map BVar is
  in  (is, bsubst s h)

evalBlock' :: BHeap -> AllowedEffects -> BlockedEffects -> BBlock -> BChoice
evalBlock' _ _ _ BBlock{ binds = (_, BFail) : _ } = BCFail             -- XXX check effs?
evalBlock' _ _ _ BBlock{ binds = (_, BWrong s) : _ } = BCWrong s       -- XXX check effs?
evalBlock' aheap aeffs bbeffs ablk = startSweep aheap (vars ablk) (binds ablk) (result ablk)
  where
    notAllowed es = not (subset es aeffs)

    startSweep :: BHeap -> [BIdent] -> [BEqn] -> BValue -> BChoice
    startSweep = sweep bbeffs []

    sweep :: BlockedEffects -> [BEqn] -> BHeap -> [BIdent] -> [BEqn] -> BValue -> BChoice
    sweep beffs done _ bvars bbinds _bresult
      | dtrace aeffs ("sweep: " ++ prettyShow (beffs, bvars, done, bbinds)) False = undefined
    sweep _     done h bvars     [] bresult =
      -- End of binds reached, no further progress possible
--      BCBlk BBlock{ vars = bvars `intersect` freeBVars (done, bresult), binds = reverse done, result = bresult }
      BCRBlk h BBlock{ vars = bvars `intersect` freeBVars (h, done, bresult), binds = reverse done, result = bresult }
    sweep beffs done heap bvars bbinds@(eqn@(val, expr) : bs) bresult =
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
              BHNF _ -> substRec x v
          else if x `elem` bvars then
            -- Locally bound, get rid of the variable entirely.
            -- And restart sweep
            startSweep heap (bvars \\ [x]) (sub (reverse done ++ bs)) (sub bresult)
          else if x `elem` freeBVars ((done, bs), bresult) then
            -- x occurs, so substitute.
            -- Bound outside, keep equation.
            -- And restart sweep
            startSweep heap bvars (sub (reverse done) ++ [ueqn] ++ sub bs) (sub bresult)
          else
            -- x does not occur, just keep equation.
            suspend ueqn
        -- Swap so variable is first (v cannot be a variable, that's handled above)
        unify v (BVar x) = unify (BVar x) v
        -- Equal integers are ok
        unify (BVLit i) (BVLit j) | i == j = succeeds []
        -- Replace equal length arrays with new equations
        unify (BVArr vs) (BVArr ws) | length vs == length ws = succeeds $ zipWith (\ v w -> (v, BVal w)) vs ws
        unify _x@(BVLam _ _) _y@(BVLam _ _) =
          -- According to the ICFP paper this is stuck.  Being WRONG would be better
          wrongs $ "unify lambda: " ++ prettyShow (_x, _y)
#if EXT
        unify (BVExt a1 r1 x1) (BVExt a2 r2 x2) | a1 == a2 = succeeds [(r1, BVal r2), (x1, BVal x2)]
        -- These are dubious
        unify (BVExt a r x) v = succeeds [(r, BApply v (BHNF a)), (x, BVal v)]
        unify v (BVExt a r x) = succeeds [(r, BApply v (BHNF a)), (x, BVal v)]
--        unify v1@BVExt{} v2 = error $ "unify BExt: " ++ prettyShow (v1, v2)
--        unify v1 v2@BVExt{} = error $ "unify BExt: " ++ prettyShow (v1, v2)
#endif
        unify _ _ = fails -- anything else fails

        -- Fail if it is allowed, otherwise suspend
        fails | notBlocked Efails beffs = BCFail
              | otherwise = suspend (BDummy, BFail)
        domainfails = BCDomainFail
        -- Generate WRONG if allowed, otherwise suspend
        wrongs s | notBlocked Ewrong beffs = BCWrong s
                 | otherwise = suspend (BDummy, BWrong s)

        -- Put es on the unprocessed bindings and continue the sweep
        succeeds :: [BEqn] -> BChoice
        succeeds = succeeds' heap
        succeeds' h es = succeeds'' h [] es
        succeeds'' h is es = sweep beffs done h (is ++ bvars) (es ++ bs) bresult

        -- Put eqn on the done list, block its effects, and continue the sweep
        suspend :: BEqn -> BChoice
        suspend ve@(_, e) = sweep (exprEffs e `union` beffs) (ve : done) heap bvars bs bresult

        substRec :: BIdent -> BValue -> BChoice
        substRec ax av = maybe fails (\ v' -> succeeds [(BVar ax, BVal v')]) $ sub ax av
          where sub :: BIdent -> BValue -> Maybe BValue
                sub x (BVar x') | x == x' = Nothing
                sub x (BVArr vs) = BVArr <$> mapM (sub x) vs
                sub x (BVLam y b) =
                  if x == y || x `elem` vars b then undefined else -- not implemented
                  pure $ BVLam y $ b{ vars = x:vars b, binds = (BVar x, BVal av) : binds b }
                sub _ v = pure v

        -- AAA, AAV, AVA, VAA, VVA
        -- AVV? VAV?
        -- VVV
        append (BVArr a) (BVArr b)        c  = unify (BVArr (a ++ b)) c
        append (BVArr a)        b  (BVArr c) | length a > length c = fails
                                             | otherwise =
          succeeds $ zipWith (\ v w -> (v, BVal w)) a c ++ [(b, BVal $ BVArr (drop (length a) c))]
        append        a  (BVArr b) (BVArr c) | length b > length c = fails
                                             | otherwise =
          let (c', c'') = splitAt (length c - length b) c in
          succeeds $ zipWith (\ v w -> (v, BVal w)) b c'' ++ [(a, BVal $ BVArr c')]
        append        a         b  (BVArr c) =
          bChoices [ succeeds [(a, BVal $ BVArr a'), (b, BVal $ BVArr b')] | n <- [0 .. length c], let (a', b') = splitAt n c ]
        append        a         b         c  | bad a || bad b || bad c = fails
                                               where bad BVArr{} = False; bad BVar{} = False; bad _ = True
        append _ _ _ = suspend eqn

      in
        let allvars = bvars ++ allBVars (done, bbinds, bresult)
            succBlock b =
              let rhs = freshenBlock allvars b
              in  succeeds'' heap (vars rhs) (binds rhs ++ [(val, BVal $ result rhs)])
        in
        -- Examine the expression and evaluate if possible.
        dtrace aeffs ("sweep expr=" ++ take 30 (show expr)) $
        case expr of
          BPrimOp ((== opAppend) -> True) (BVArr [a, b, c]) -> append a b c
          BPrimOp op v  | notAllowed (primOpEffs op) -> wrongs $ "effect not allowed: " ++ show (op, primOpEffs op)
                        | Just e <- evalPrimOp op v -> succeeds [(val, e)]
                        | Just (h, e) <- evalPrimHeapOp heap op v -> succeeds' h [(val, e)]
                        | otherwise -> suspend eqn
          BApply f a ->
            case f of
#if EXT
              BVar _ | BHNF h <- a ->
                    let x = bIdentNotIn (allvars ++ freeBVars (h, val))
                    in  succeeds'' heap [x] [(f, BVal $ BVExt h val (BVar x))]
#endif
              BVar _ -> suspend eqn           -- not a hnf yet
              BVLam i b ->
                -- Bind the argument and insert the lambda body
                let (i', b') = freshenLambda allvars (i, b)
                in  succeeds'' heap [i'] [(BVar i', BVal a), (val, BEBlk b')]
              BVArr vs ->
                let e = BChoice $ choices [ BBlock { vars = [], binds = [(a, BVal $ BVInt i)], result = v }
                                          | (i, v) <- zip [0..] vs ]
                in  succeeds [(val, e)]
#if EXT
              BVExt i o x | BHNF h <- a -> if i == h then unify val o else succeeds [(val, BApply x a)]
                          | otherwise -> suspend eqn
#endif
              BHNF _ -> wrongs $ "bad function " ++ prettyShow f
          BSplit c f g ->
            case evalChoice heap (aeffs `intersect` domEffects) (beffs \\ domEffects) c of
              BCFail -> succeeds [(val, BApply f (BVArr []))]
              BCWrong s -> wrongs s
--              BCBlk b@BlockValue{} -> callG dummyHeap b BCFail
--              BCFork (BCBlk b@BlockValue{}) r -> callG dummyHeap b r
              BCRBlk heap' b@BlockValue{} -> callG (freshenHeap allvars (vars b) heap') b BCFail
              BCFork (BCRBlk heap' b@BlockValue{}) r -> callG (freshenHeap allvars (vars b) heap') b r
--              _ -> suspend eqn  -- XXX Work done in c is lost.  Hard to do right with effects
              BCBlk{} -> error "impossible"
              BCFork BCBlk{} _ -> error "impossible"
              c' -> suspend (val, BSplit c' f g)
            where callG (is, h) b r =
                    dtrace aeffs ("callG " ++ prettyShow (is, h, b, r, val)) $
                    let (vb: a1: a2: a3: dummy: _) = bIdentsNotIn (is ++ allvars ++ freeBVars (b, c))
                        b0 = (BVar vb, BEBlk b)
                        b1 = (BVar a1, BApply g (BVar vb))
                        b2 = (BVar a2, BApply (BVar a1) (BVLam dummy (blkChoice r)))
                        b3 = (BVar a3, BApply (BVar a2) g)
                        bb = BBlock{ vars = [vb, a1, a2, a3], binds = [b0,b1,b2,b3], result = BVar a3 }
                    in  succeeds'' h is [(val, BEBlk bb)]
          BChoice BCFail -> fails
          BChoice BCDomainFail -> domainfails
          BChoice (BCWrong s) -> wrongs s
          BChoice (BCBlk b) -> succBlock b
          BChoice (BCFork x1 x2) | notAllowed [Eiterates] -> wrongs "iterates not allowed"
                                 | notBlocked Eiterates beffs -> evalChoice heap aeffs bbeffs (BCFork c1 c2)
                                 | otherwise -> suspend eqn
            where
              c1 = BCBlk $ BBlock{ vars = bvars, binds = rdone ++ [(val, BChoice x1)] ++ bs, result = bresult }
              c2 = BCBlk $ BBlock{ vars = bvars, binds = rdone ++ [(val, BChoice x2)] ++ bs, result = bresult }
              rdone = reverse done
          BChoice (BCRBlk heap' b) | heap == heap' -> succBlock b
          BChoice (BCRBlk _ _) -> error "impossible"
          BVal v -> unify val v

blkChoice :: BChoice -> BBlock
blkChoice (BCBlk b) = b
blkChoice c = BBlock { vars = [i], binds = [(BVar i, BChoice c)], result = BVar i }
  where i = bIdentNotIn (allBVars c)

choices :: [BBlock] -> BChoice
choices = bChoices . map BCBlk

bChoices :: [BChoice] -> BChoice
bChoices [] = BCFail
bChoices cs = foldr1 BCFork cs

evalPrimOp :: Op -> BValue -> Maybe BExpr
evalPrimOp "any$" v = Just $ BVal v
evalPrimOp _ (BVar _) = Nothing
-- int
evalPrimOp op v | Just cmp <- lookup op compareIntOps =
  case v of
    BVArr [BVInt a, BVInt b] -> Just $ if cmp a b then BVal $ BVInt a else BFail
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithBinIntOps =
  case v of
    BVArr [BVInt _, BVInt 0] | op == opIntDiv -> Just BFail
    BVArr [BVInt a, BVInt b] -> Just $ BVal $ BVInt $ a `arith` b
    BVArr vs  | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithUnIntOps =
  case v of
    BVInt a -> Just $ BVal $ BVInt $ arith a
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
-- rational
evalPrimOp op v | Just cmp <- lookup op compareRatOps =
  case v of
    BVArr [BVRat a, BVRat b] -> Just $ if cmp a b then BVal $ BVRat a else BFail
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithBinRatOps =
  case v of
    BVArr [BVRat _, BVRat 0] | op == opRatDiv -> Just BFail
    BVArr [BVRat a, BVRat b] -> Just $ BVal $ BVRat $ a `arith` b
    BVArr vs  | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithUnRatOps =
  case v of
    BVRat a -> Just $ BVal $ BVRat $ arith a
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
-- f32
evalPrimOp op v | Just cmp <- lookup op compareF32Ops =
  case v of
    BVArr [BVF32 a, BVF32 b] -> Just $ if cmp a b then BVal $ BVF32 a else BFail
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithBinF32Ops =
  case v of
    BVArr [BVF32 _, BVF32 0] | op == opF32Div -> Just BFail
    BVArr [BVF32 a, BVF32 b] -> Just $ BVal $ BVF32 $ a `arith` b
    BVArr vs  | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithUnF32Ops =
  case v of
    BVF32 a -> Just $ BVal $ BVF32 $ arith a
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
-- f64
evalPrimOp op v | Just cmp <- lookup op compareF64Ops =
  case v of
    BVArr [BVF64 a, BVF64 b] -> Just $ if cmp a b then BVal $ BVF64 a else BFail
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithBinF64Ops =
  case v of
    BVArr [BVF64 _, BVF64 0] | op == opF64Div -> Just BFail
    BVArr [BVF64 a, BVF64 b] -> Just $ BVal $ BVF64 $ a `arith` b
    BVArr vs  | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | Just arith <- lookup op arithUnF64Ops =
  case v of
    BVF64 a -> Just $ BVal $ BVF64 $ arith a
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)

evalPrimOp op v | op == opArrLen =
  case v of
    BHNF (BArr xs) -> Just $ BVal $ BVInt $ toInteger $ length xs
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsInt =
  case v of
    a@(BVInt _) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsRat =
  case v of
    a@(BVLit (BRat _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsF32 =
  case v of
    a@(BVLit (BF32 _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsF64 =
  case v of
    a@(BVLit (BF64 _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsChr =
  case v of
    a@(BVLit (BChr _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsStr =
  case v of
    a@(BVLit (BStr _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsFcn =
  case v of
    a@(BHNF (BHLam _ _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opIsArr =
  case v of
    a@(BHNF (BArr _)) -> Just $ BVal a
    BHNF _ -> Just BFail
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opCons =
  case v of
    BVArr [a, BVArr as] -> Just $ BVal $ BVArr (a : as)
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op v | op == opDotDot =
  case v of
    BVArr [BVInt a, BVInt b] -> Just $ BChoice $ choices [ BlockValue [] (BVInt i) | i <- [a .. b] ]
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)
evalPrimOp op v | op == opPrint =
  -- A temporary hack for printing.
  case v of
    BHNF h ->
      trace ("Print: " ++ prettyShow h) $
      Just $ BVal $ BVArr []
--    _ -> Just $ BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v)      
evalPrimOp op _ | op == opAppend = Nothing
evalPrimOp op _ | op == opErr = error "Err() called"
evalPrimOp op _ | op `elem` [opAlloc, opRead, opWrite, opAddTo] = Nothing
evalPrimOp op v = error $ "evalPrimOp: " ++ show (op, v)

evalPrimHeapOp :: BHeap -> Op -> BValue -> Maybe (BHeap, BExpr)
evalPrimHeapOp h op v | op == opAlloc =
  case v of
    vv -> Just (h', BVal $ BVRef r) where (h', r) = heapAlloc h vv
--    _ -> Just (h, BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v))
evalPrimHeapOp h op v | op == opRead =
  case v of
    BVar _ -> Nothing
    BVRef r -> Just (h, BVal $ heapRead h r)
    _ -> Just (h, BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v))
evalPrimHeapOp h op v | op == opWrite =
  case v of
    BVar _ -> Nothing
    BVArr [BVRef r, vv] -> Just (heapWrite h r vv, BVal $ BVArr [])
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just (h, BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v))
evalPrimHeapOp h op v | op == opAddTo =
  case v of
    BVar _ -> Nothing
    BVArr [BVRef r, BVInt i] | BVInt j <- heapRead h r, let vv = BVInt $ j + i -> Just (heapWrite h r vv, BVal vv)
    BVArr vs | any isBVar vs -> Nothing
    _ -> Just (h, BWrong $ "bad primop args: " ++ prettyShow (BPrimOp op v))
evalPrimHeapOp _ _ _ = Nothing

compareIntOps :: [(Op, Integer -> Integer -> Bool)]
compareIntOps = [(opIntGt, (>)), (opIntGe, (>=)), (opIntLt, (<)), (opIntLe, (<=)), (opIntNe, (/=))]

arithBinIntOps :: [(Op, Integer -> Integer -> Integer)]
arithBinIntOps = [(opIntAdd, (+)), (opIntSub, (-)), (opIntMul, (*)) {-, (opIntDiv, div)-}]

arithUnIntOps :: [(Op, Integer -> Integer)]
arithUnIntOps = [(opIntNeg, negate), (opIntPlus, id)]

compareRatOps :: [(Op, Rational -> Rational -> Bool)]
compareRatOps = [(opRatGt, (>)), (opRatGe, (>=)), (opRatLt, (<)), (opRatLe, (<=)), (opRatNe, (/=))]

arithBinRatOps :: [(Op, Rational -> Rational -> Rational)]
arithBinRatOps = [(opRatAdd, (+)), (opRatSub, (-)), (opRatMul, (*)), (opRatDiv, (/))]

arithUnRatOps :: [(Op, Rational -> Rational)]
arithUnRatOps = [(opRatNeg, negate), (opRatPlus, id)]

compareF32Ops :: [(Op, Float -> Float -> Bool)]
compareF32Ops = [(opF32Gt, (>)), (opF32Ge, (>=)), (opF32Lt, (<)), (opF32Le, (<=)), (opF32Ne, (/=))]

arithBinF32Ops :: [(Op, Float -> Float -> Float)]
arithBinF32Ops = [(opF32Add, (+)), (opF32Sub, (-)), (opF32Mul, (*)), (opF32Div, (/))]

arithUnF32Ops :: [(Op, Float -> Float)]
arithUnF32Ops = [(opF32Neg, negate), (opF32Plus, id)]

compareF64Ops :: [(Op, Double -> Double -> Bool)]
compareF64Ops = [(opF64Gt, (>)), (opF64Ge, (>=)), (opF64Lt, (<)), (opF64Le, (<=)), (opF64Ne, (/=))]

arithBinF64Ops :: [(Op, Double -> Double -> Double)]
arithBinF64Ops = [(opF64Add, (+)), (opF64Sub, (-)), (opF64Mul, (*)), (opF64Div, (/))]

arithUnF64Ops :: [(Op, Double -> Double)]
arithUnF64Ops = [(opF64Neg, negate), (opF64Plus, id)]

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
effCommutes Einvariant _ = undefined
effCommutes Etrace _ = True
