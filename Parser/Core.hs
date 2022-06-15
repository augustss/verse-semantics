{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Core(
  Core(..),
  pattern COne, pattern CAll, pattern CSucceeds, pattern CFail,
  pattern CVar, pattern VPrim, pattern VArray, pattern CArray,
  pattern VLam, pattern CLam,
  Value(..),
  HNF(..),
  compos, composOp,
  exprToCore,
  coreToRedex,
  cSeq, cDef,
  isValue,
  fvs, fvsV, cfvs, cfvsV,
  subst,
  alphaConvert, alphaConvertV, alphaConvertH,
  composC, composV, composH,
  composOpC, composOpV, composOpH,
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

-- Use CSplit instead of COne/CAll
useSplit :: Bool
useSplit = True

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Value Value
  | CBar [Core]
  | CMacro Ident Core
  | CDef Heap Core
  | CWrong String
  | CSplit Core Value Value
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
pattern VArray :: [Value] -> Value
pattern VArray vs = HNF (HArray vs)
pattern CArray :: [Value] -> Core
pattern CArray vs = CValue (VArray vs)
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
pattern CDecides :: Core -> Core
pattern CDecides c <- CMacro (Ident _ "decides") c
  where CDecides e = CMacro (Ident noLoc "decides") e
pattern CFail :: Core
pattern CFail = CBar []
pattern VLam :: Ident -> Core -> Value
pattern VLam i e = HNF (HLam i e)
pattern CLam :: Ident -> Core -> Core
pattern CLam i e = CValue (VLam i e)


cSeq :: [Core] -> Core
cSeq [] = internalError
cSeq [e] = e
cSeq es = CSeq es

cDef :: [Ident] -> Core -> Core
cDef [] e = e
cDef is e = CDef is e

isValue :: Core -> Bool
isValue CValue{} = True
isValue _ = False

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
core (Variable (Ident l "wrong")) = pure $ CWrong $ "called: " ++ prettyShow l
core e@Variable{} = val e
core e@Array{} = val e
core (Seq es) = seqC <$> mapM core es
core (ApplyS e1 e2) = cSucceeds =<< core (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> value e1 <*> value e2
core (ApplyEff rs e) = coreEffs rs =<< core e
core (Unify e1 e2) = cUnify <$> core e1 <*> core e2
core e@Typedef{} = val e
core e@Choice{} = CBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i AnyT) = pure $ CVar i
core (Define i e) = cUnify (CVar i) <$> core e
core AnyT = undefined
core Fail = pure $ CBar []
core (For2 e1 e2) = do
  e2' <- thunk e2
--  traceM $ show (e2, e2', seqE [e1, e2'])
  ee <- coreD (seqE [e1, e2'])
  cAll ee
core (If3 e1 e2 e3) = do
  e2' <- thunk e2
  e3' <- thunk e3
  l <- coreD (seqE [e1, e2'])
  r <- core e3'
  fn <- cOne $ CBar [l, r]
  i <- newTmp
  pure $ CDef [i] $ seqC [cUnify (CVar i) fn, CApply (Var i) vEmpty]
core e@Function{} = val e
core e = impossible e

coreEffs :: [Ident] -> Core -> C Core
coreEffs [] e = pure e
coreEffs [Ident _ "decides"] e = cDecides e
coreEffs [Ident _ "succeeds"] e = cSucceeds e
coreEffs rs _ = unimplemented $ "effects: " ++ prettyShow rs

cOne :: Core -> C Core
cOne e | not useSplit = pure $ COne e
cOne e = do
  u1 <- newTmp
  u2 <- newTmp
  v <- newTmp
  pure $ CSplit e (VLam u1 CFail) (VLam v $ CLam u2 $ CVar v)

cAll :: Core -> C Core
cAll e | not useSplit = pure $ CAll e
cAll e = do
  f <- newTmp
  g <- newTmp
  u <- newTmp
  v <- newTmp
  r <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CDef [f, g] $
           CSeq [
             CUnify (CVar f) (CLam u $ CArray []),
             CUnify (CVar g) (CLam v $ CLam r $
                               CDef [x, y] $
                                 CSeq [
                                   CUnify (CVar x) (CSplit (CApply (Var r) (VArray [])) (Var f) (Var g)),
                                   CUnify (CVar y) (CApply (Var v) (VArray [])),
                                   CApply (VPrim "cons$") (VArray [Var y, Var x])
                                   ]),
             CSplit e (Var f) (Var g)
             ]
                                
cSucceeds :: Core -> C Core
cSucceeds e | not useSplit = pure $ CSucceeds e
cSucceeds e = do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (VLam u1 (CWrong "succeeds-fail"))
                (VLam x $ CLam y $ CSplit (CApply (Var y) (VArray []))
                                          (VLam u2 (CVar x))
                                          (VLam u3 $ CLam u4 $ CWrong "succeed-many")
                )

cDecides :: Core -> C Core
cDecides e | not useSplit = pure $ CDecides e
cDecides e = do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (VLam u1 CFail)
                (VLam x $ CLam y $ CSplit (CApply (Var y) (VArray []))
                                          (VLam u2 (CVar x))
                                          (VLam u3 $ CLam u4 $ CWrong "succeed-many")
                )

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
  HNF . HLam i <$> coreD (Seq [Unify (Variable i) e, Variable i])
value (Function [(Define x AnyT, fs)] b) = HNF . HLam x . attr <$> coreD b
  where attr ae = foldr CMacro ae fs
value AnyT = pure (HNF $ HPrim ":any")
value e = internalErrorMsg $ "value: not a value\n" ++ show e
  
--eVar :: String -> Expr
--eVar = Variable . Ident noLoc

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ Function [(Define i AnyT, [])] e

------

instance Pretty Core where
  pPrintPrec l p (CValue v) = pPrintPrec l p v
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 0) cs
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar cs) = maybeParens (p > 7) $ fsep (punctuate (text " |") (map (pPrintPrec l 7) cs))
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "def" <+> commaSep l 0 is <+> text "in", pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"
  pPrintPrec l _ (CSplit e f g) =
    text "split" <> braces (sep [pPrintPrec l 0 e <> text ",",
                                 pPrintPrec l 0 f <> text ",",
                                 pPrintPrec l 0 g <> text ","])

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
compos f (CSplit e n g) = CSplit <$> f e <*> appV f n <*> appV f g

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

composC :: (Applicative f) => (Core -> f Core) -> (Value -> f Value) -> (HNF -> f HNF) -> Core -> f Core
composC _  fv _  (CValue v) = CValue <$> fv v
composC fc _  _  (CUnify e1 e2) = CUnify <$> fc e1 <*> fc e2
composC fc _  _  (CSeq es) = CSeq <$> traverse fc es
composC _  fv _  (CApply e1 e2) = CApply <$> fv e1 <*> fv e2
composC fc _  _  (CBar es) = CBar <$> traverse fc es
composC fc _  _  (CMacro i e) = CMacro i <$> fc e
composC fc _  _  (CDef h e) = CDef h <$> fc e
composC _  _  _   e@CWrong{} = pure e
composC fc fv _  (CSplit e f g) = CSplit <$> fc e <*> fv f <*> fv g

composV :: (Applicative f) => (Core -> f Core) -> (Value -> f Value) -> (HNF -> f HNF) -> Value -> f Value
composV _  _  _  v@Var{} = pure v
composV _  _  fh (HNF v) = HNF <$> fh v

composH :: (Applicative f) => (Core -> f Core) -> (Value -> f Value) -> (HNF -> f HNF) -> HNF -> f HNF
composH _  _  _  v@HInt{} = pure v
composH _  _  _  v@HRat{} = pure v
composH _  _  _  v@HPrim{} = pure v
composH _  fv _  (HArray vs) = HArray <$> traverse fv vs
composH fc _  _  (HLam i e) = HLam i <$> fc e

composOpC :: (Core -> Core) -> (Value -> Value) -> (HNF -> HNF) -> Core -> Core
composOpC fc fv fh = runIdentity . composC (pure . fc) (pure . fv) (pure . fh)

composOpV :: (Core -> Core) -> (Value -> Value) -> (HNF -> HNF) -> Value -> Value
composOpV fc fv fh = runIdentity . composV (pure . fc) (pure . fv) (pure . fh)

composOpH :: (Core -> Core) -> (Value -> Value) -> (HNF -> HNF) -> HNF -> HNF
composOpH fc fv fh = runIdentity . composH (pure . fc) (pure . fv) (pure . fh)

-- Unique free variables
fvs :: Core -> [Ident]
fvs = nub . cfvs

fvsV :: Value -> [Ident]
fvsV = nub . cfvsV

-- Occurrences of free variables
cfvs :: Core -> [Ident]
cfvs (CValue v) = cfvsV v
cfvs (CUnify e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CSeq es) = concatMap cfvs es
cfvs (CApply e1 e2) = cfvsV e1 ++ cfvsV e2
cfvs (CBar es) = concatMap cfvs es
cfvs (CMacro _ e) = cfvs e
cfvs (CDef is e) = filter (`notElem` is) $ cfvs e
cfvs (CSplit e f g) = cfvs e ++ cfvsV f ++ cfvsV g
cfvs CWrong{} = []

cfvsV :: Value -> [Ident]
cfvsV (Var i) = [i]
cfvsV (HNF h) = cfvsH h

cfvsH :: HNF -> [Ident]
cfvsH (HArray vs) = concatMap cfvsV vs
cfvsH (HLam i e) = filter (/= i) $ cfvs e
cfvsH _ = []

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
    sub (CSplit e f g) = CSplit (sub e) (subV f) (subV g)

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
    alpha m (CSplit e f g) = CSplit (alpha m e) (alphaV m f) (alphaV m g)

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

