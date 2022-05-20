{-# LANGUAGE PatternSynonyms #-}
module Core(
  Core(..),
  pattern COne, pattern CAll, pattern CSucceeds, pattern CFail,
  pattern CVar, pattern VPrim,
  Value(..),
  HNF(..),
  compos, composOp,
  exprToCore,
  coreToRedex,
  fvs, fvsV,
  subst,
  alphaConvert, alphaConvertV, alphaConvertH,
  ) where
import Prelude hiding ((<>))
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List
import Data.Maybe

import Print
import Expr hiding (compos, composOp)
import Desugar(primOps, getVisible)
import Error
import SExp
--import Debug.Trace

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Value Value
  | CBar [Core]
  | CMacro Ident Core
  | CDef Heap Core
  | CWrong String
  deriving (Show, Eq)

type Heap = [Ident]

data Value = Var Ident | HNF HNF
  deriving (Show, Eq)

data HNF
  = HInt Integer
  | HRat Rational
  | HPrim String
  | HArray [Value]
  | HLam Ident Core
  deriving (Show, Eq)

{-
pattern CInt :: Integer -> Core
pattern CInt x = CValue (HNF (HInt x))
pattern CRat :: Rational -> Core
pattern CRat x = CValue (HNF (HRat x))
-}
pattern VPrim :: String -> Value
pattern VPrim s = HNF (HPrim s)
pattern CVar :: Ident -> Core
pattern CVar x = CValue (Var x)
pattern COne :: Core -> Core
pattern COne c <- CMacro (Ident _ "one") c
  where COne e = CMacro (Ident noLoc "one") e
pattern CAll :: Core -> Core
pattern CAll c <- CMacro (Ident _ "all") c
  where CAll e = CMacro (Ident noLoc "all") e
pattern CSucceeds :: Core -> Core
pattern CSucceeds c <- CMacro (Ident _ "succeeds") c
  where CSucceeds e = CMacro (Ident noLoc "succeeds") e
pattern CFail :: Core
pattern CFail = CBar []

vEmpty :: Value
vEmpty = HNF $ HArray []

type C = State Int

seqC :: [Core] -> Core
seqC acs =
  case concatMap flat acs of
    [] -> impossible acs
    [c] -> c
    cs -> CSeq cs
  where
    flat (CSeq cs) = concatMap flat cs
    flat c = [c]

newTmp :: C Ident
newTmp = do
  n <- get
  let i = Ident noLoc ("$c" ++ show n)
  put $! n+1
  pure i

exprToCore :: Expr -> Core
exprToCore = flip evalState 1 . coreD

core :: Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = seqC <$> mapM core es
core (ApplyS e1 e2) = CSucceeds <$> core (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> value e1 <*> value e2
core (Unify e1 e2) = cUnify <$> core e1 <*> core e2
core e@Typedef{} = val e
core e@Choice{} = CBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i e) = cUnify (CVar i) <$> core e
core e@AnyT = val e
core Fail = pure $ CBar []
core (For2 e1 e2) = do
  e2' <- thunk e2
--  traceM $ show (e2, e2', seqE [e1, e2'])
  CAll <$> coreD (seqE [e1, e2'])
core (If3 e1 e2 e3) = do
  e2' <- thunk e2
  e3' <- thunk e3
  l <- coreD (seqE [e1, e2'])
  r <- core e3'
  let fn = COne $ CBar [l, r]
  i <- newTmp
  pure $ CDef [i] $ seqC [cUnify (CVar i) fn, CApply (Var i) vEmpty]
core e@Function{} = val e
core e = impossible e

-- A small optimization to get smaller examples.
cUnify :: Core -> Core -> Core
cUnify e (CValue (VPrim ":any")) = e
cUnify e1 e2 = CUnify e1 e2

coreD :: Expr -> C Core
coreD e | null is = core e
        | otherwise = CDef is <$> core e
  where is = getVisible e

val :: Expr -> C Core
val e = CValue <$> value e

value :: Expr -> C Value
value (LitInt i) = pure (HNF $ HInt i)
value (LitRat i) = pure (HNF $ HRat i)
value (Variable i@(Ident _ s)) | i `elem` primOps = pure (HNF $ HPrim s)
                               | otherwise = pure (Var i)
value (Array es) = HNF . HArray <$> mapM value es
value (Typedef e) = do
  i <- newTmp
  HNF . HLam i <$> coreD (Unify (Variable i) e)
value (Function (Define x AnyT) fs b) = HNF . HLam x . attr <$> coreD b
  where attr ae = foldr CMacro ae fs
value AnyT = pure (HNF $ HPrim ":any")
value e = internalErrorMsg $ "value: not a value\n" ++ show e
  
--eVar :: String -> Expr
--eVar = Variable . Ident noLoc

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ Function (Define i AnyT) [] e

------

instance Pretty Core where
  pPrintPrec l p (CValue v) = pPrintPrec l p v
  pPrintPrec l p (CUnify c1 c2) = maybeParens (True || p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 0) cs
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar cs) = maybeParens (p > 7) $ fsep (punctuate (text " |") (map (pPrintPrec l 7) cs))
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "def" <+> commaSep l 0 is, text "in" <+> pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"

instance Pretty Value where
  pPrintPrec l p (Var i) = pPrintPrec l p i
  pPrintPrec l p (HNF v) = pPrintPrec l p v

instance Pretty HNF where
  pPrintPrec l p (HInt i) = pPrintPrec l p i
  pPrintPrec _ _ (HRat _) = undefined -- pPrintPrec l p r
  pPrintPrec _ _ (HPrim s) = text s
  pPrintPrec l _ (HArray [v]) = text "array" <> braces (pPrintPrec l 0 v)
  pPrintPrec l _ (HArray vs) = parens $ commaSep l 0 vs
  pPrintPrec l p (HLam i c) = maybeParens (p > 2) $ pPrintPrec l 0 i <+> text "=>" <+> pPrintPrec l 0 c

------

coreToRedex :: Core -> SExp
coreToRedex = undefined

------

compos :: (Applicative f) => (Core -> f Core) -> Core -> f Core
compos f (CValue v) = CValue <$> appV f v
compos f (CUnify e1 e2) = CUnify <$> f e1 <*> f e2
compos f (CSeq es) = CSeq <$> traverse f es
compos f (CApply e1 e2) = CApply <$> appV f e1 <*> appV f e2
compos f (CBar es) = CBar <$> traverse f es
compos f (CMacro i e) = CMacro i <$> f e
compos f (CDef h e) = CDef h <$> f e
compos _ e@CWrong{} = pure e

appV :: (Applicative f) => (Core -> f Core) -> Value -> f Value
appV _ v@Var{} = pure v
appV f (HNF v) = HNF <$> appH f v

appH :: (Applicative f) => (Core -> f Core) -> HNF -> f HNF
appH _ v@HInt{} = pure v
appH _ v@HRat{} = pure v
appH _ v@HPrim{} = pure v
appH f (HArray vs) = HArray <$> traverse (appV f) vs
appH f (HLam i e) = HLam i <$> f e

composOp :: (Core -> Core) -> Core -> Core
composOp f = runIdentity . compos (pure . f)

fvs :: Core -> [Ident]
fvs (CValue v) = fvsV v
fvs (CUnify e1 e2) = fvs e1 `union` fvs e2
fvs (CSeq es) = foldr union [] $ map fvs es
fvs (CApply e1 e2) = fvsV e1 `union` fvsV e2
fvs (CBar es) = foldr union [] $ map fvs es
fvs (CMacro _ e) = fvs e
fvs (CDef is e) = fvs e \\ is
fvs CWrong{} = []

fvsV :: Value -> [Ident]
fvsV (Var i) = [i]
fvsV (HNF h) = fvsH h

fvsH :: HNF -> [Ident]
fvsH (HArray vs) = foldr union [] $ map fvsV vs
fvsH (HLam i e) = fvs e \\ [i]
fvsH _ = []

------

-- Replace x by b in e
-- Do an occurs check and alpha-conversion when necessary
subst :: Ident -> Value -> Core -> Core
subst x b ae | x `elem` bs = impossible "subst occur check"
             | otherwise = sub ae
  where
    bs = fvsV b
    sub (CValue v) = CValue $ subV v
    sub (CUnify e1 e2) = CUnify (sub e1) (sub e2)
    sub (CSeq es) = CSeq $ map sub es
    sub (CApply e1 e2) = CApply (subV e1) (subV e2)
    sub (CBar es) = CBar $ map sub es
    sub (CMacro i e) = CMacro i $ sub e
    sub a@(CDef h e) | x `elem` h = a
                     | null (intersect bs h) = CDef h $ sub e
                     | otherwise = sub $ alphaConvert bs a
    sub e@CWrong{} = e

    subV v@(Var i) | i == x = b
                   | otherwise = v
    subV (HNF v) = HNF $ subH v

    subH (HArray vs) = HArray $ map subV vs
    subH a@(HLam i e) | x == i = a
                      | i `notElem` bs = HLam i $ sub e
                      | otherwise = subH $ alphaConvertH bs a
    subH v = v

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> Core -> Core
alphaConvert vs = alpha []
  where
    alpha m (CValue v) = CValue (alphaV m v)
    alpha m (CUnify e1 e2) = CUnify (alpha m e1) (alpha m e2)
    alpha m (CSeq es) = CSeq (map (alpha m) es)
    alpha m (CApply e1 e2) = CApply (alphaV m e1) (alphaV m e2)
    alpha m (CBar es) = CBar (map (alpha m) es)
    alpha m (CMacro i e) = CMacro i (alpha m e)
    alpha m (CDef h e) = CDef h' (alpha m' e)
      where h' = map fresh h
            m' = foldr add m $ zip h h'
    alpha _ e@CWrong{} = e

    alphaV m (Var i) = Var $ fromMaybe i $ lookup i m
    alphaV m (HNF h) = HNF (alphaH m h)

    alphaH m (HArray es) = HArray (map (alphaV m) es)
    alphaH m (HLam i e) = HLam i' $ alpha (add (i, i') m) e where i' = fresh i
    alphaH _ h = h

    add ii@(i, i') m | i == i' = m
                     | otherwise = ii : m

    fresh i@(Ident l s) | i `notElem` vs = i
                        | otherwise = fresh $ Ident l (s ++ "'")

alphaConvertH :: [Ident] -> HNF -> HNF
alphaConvertH vs h =
  case alphaConvert vs (CValue (HNF h)) of
    CValue (HNF h') -> h'
    _ -> impossible ()

alphaConvertV :: [Ident] -> Value -> Value
alphaConvertV vs v =
  case alphaConvert vs (CValue v) of
    CValue v' -> v'
    _ -> impossible ()
