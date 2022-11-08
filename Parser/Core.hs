{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
module Core(
  Core(..),
  pattern CApplyVV, pattern CUnifyVE,
  pattern COne, pattern CAll, pattern CSucceeds,
  pattern CVar, pattern VPrim, pattern VArray, pattern CArray,
  pattern VLam, pattern CLam, pattern CInt, pattern VInt,
  Value(..),
  HNF(..),
  compos, composOp,
  exprToCore,
  coreToRedex,
  cSeq, cDef, cBar,
  isValue,
  fvs, fvsV, cfvs, cfvsV,
  subst, substV,
  alphaConvert, alphaConvertV, alphaConvertH,
  composC, composV, composH,
  composOpC, composOpV, composOpH,
  pCore, pCoreFile,
  ) where
import Prelude hiding ((<>))
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.List
import Data.Maybe
import GHC.Stack(HasCallStack)
import Text.Megaparsec(sepBy, sepBy1, many, eof, choice, some, optional, (<|>))
-- import Text.Megaparsec.Char(skip)

import Print
import Expr hiding (compos, composOp)
import Desugar(primOps, getVisible, covariantId)
import Error
    ( unimplemented, impossible, internalError, internalErrorMsg )
import Flags
import SExp
import Parse(P, pOp, pParens, skip, pLiteral, pIdent, pMacroName, pBraces, try, pKeyword)
import Desugar(simpleDesugar)
import Misc(pattern Snoc)
--import Debug.Trace

data Core
  = CValue Value
  | CUnify Core Core
  | CSeq [Core]
  | CApply Core Core
  | CBar Core Core
  | CFail
  | CMacro Ident Core
  | CDef Heap Core
  | CWrong String
  | CSplit Core Value Value
  | CLambda Ident [Ident] Bool Core Core
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

pattern CApplyVV :: Value -> Value -> Core
pattern CApplyVV v1 v2 = CApply (CValue v1) (CValue v2)
pattern CUnifyVE :: Value -> Core -> Core
pattern CUnifyVE v e = CUnify (CValue v) e
pattern CInt :: Integer -> Core
pattern CInt x = CValue (HNF (HInt x))
pattern VInt :: Integer -> Value
pattern VInt i = HNF (HInt i)
{-
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

cBar :: [Core] -> Core
cBar [] = CFail
cBar [e] = e
cBar (e:es) = CBar e $ cBar es

isValue :: Core -> Bool
isValue CValue{} = True
isValue _ = False

isValue' :: Core -> Bool
isValue' (CSeq [e]) = isValue' e
isValue' (CSeq (e:es)) = isValue' e && isValue' (CSeq es)
isValue' CValue{} = True
isValue' _ = False

vEmpty :: Value
vEmpty = HNF $ HArray []

type C = ReaderT Flags (State Int)

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

exprToCore :: Flags -> Expr -> Core
--exprToCore _ e | trace ("exprToCore: " ++ prettyShow e) False = undefined
exprToCore flg e = flip evalState 1 . flip runReaderT flg . coreD $ e

core :: HasCallStack => Expr -> C Core
core e@LitInt{} = val e
core e@LitRat{} = val e
core (Wrong s) = pure $ CWrong s
core e@Variable{} = val e
core e@Array{} = val e
core (Seq (Snoc es e)) = seqC <$> mapM core (Snoc (filter p es) e)
  where p (Define _ AnyT) = False
        p _ = True
core (ApplyS e1 e2) = cSucceeds =<< core (ApplyD e1 e2)
core (ApplyD e1 e2) = --CApplyVV <$> value e1 <*> value e2
                      CApply <$> core e1 <*> core e2
core (ApplyEff rs e) = coreEffs rs =<< coreD e
core (Unify e1 e2) = cUnify <$> core e1 <*> core e2
core e@Typedef{} = val e
core e@Choice{} = cBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i AnyT) = pure $ CVar i
core (Define i e) = cUnify (CVar i) <$> core e
core AnyT = undefined
core Fail = pure $ CFail
core (For2 e1 e2) = do
  useSplit <- asks fSplit
  if useSplit then
    forSplit e1 e2
   else do
    e2' <- thunk e2
--  traceM $ show (e2, e2', seqE [e1, e2'])
    ee <- coreD (seqE [e1, e2'])
    ea <- cAll ee
    xa <- newTmp
    pure $ CDef [xa] $ CSeq [cUnify (CVar xa) ea, CApplyVV (VPrim "mapAp$") (Var xa)]
core (If3 e1 e2 e3) = do
  c1 <- core e1
  if isValue' c1 then
    core e2
   else do
    e2' <- thunk e2
    e3' <- thunk e3
    l <- coreD (seqE [e1, e2'])
    r <- core e3'
    fn <- cOne $ CBar l r
    i <- newTmp
    pure $ CDef [i] $ seqC [cUnify (CVar i) fn, CApplyVV (Var i) vEmpty]
--core e@Function{} = val e
core (Do e) = coreD e
core (Macro1 (Ident _ "all") [] e) = cAll =<< coreD e
core (Macro1 (Ident _ "one") [] e) = cOne =<< coreD e
core (Macro1 (Ident _ "succeeds") [] e) = cSucceeds =<< coreD e
core (Macro1 (Ident _ "decides") [] e) = cDecides =<< coreD e
core (Lambda i [] (Array []) e) = val $ Function [(Define i AnyT, [])] e
core (Lambda i rs e1 e2) = do
--  traceM $ "Lambda:\n" ++ prettyShow eee ++ "\n+++++\n"
  timLam <- asks fTimLambda
  let covariant = covariantId `elem` rs || True -- XXX
  if timLam then do
    let is = getVisible e1
    e1' <- core e1
    e2' <- coreD e2
    pure $ CLambda i is covariant e1' e2'
  else
    val $ lamFunc covariant i e1 e2
core EmptyT = pure CFail
core e = impossible e

-- Is the expression non-failing and contains no binders
trivial :: Expr -> Bool
trivial Array{} = True
trivial (ApplyD (Variable (Ident _ "any")) _) = True
trivial _ = False

lamFunc :: Bool -> Ident -> Expr -> Expr -> Expr
lamFunc cov i e1 e2 =
  Function [(Define i AnyT, [])] $
    if trivial e1 then
      Seq [e1, e2]
    else
      If3 e1 e2 (if cov then Fail else Wrong "outside domain")

coreEffs :: [Ident] -> Core -> C Core
coreEffs [] e = pure e
coreEffs [Ident _ "decides"] e = cDecides e
coreEffs [Ident _ "succeeds"] e = cSucceeds e
coreEffs rs _ = unimplemented $ "effects: " ++ prettyShow rs

forSplit :: Expr -> Expr -> C Core
forSplit e1 e2 = do
  f <- newTmp
  g <- newTmp
  u <- newTmp
  x <- newTmp
  y <- newTmp
  a <- newTmp
  b <- newTmp
  let vs = getVisible e1
  e1' <- coreD (Seq [e1, Array $ map Variable vs])
  e2' <- coreD e2
  let fDef e = CDef [f] (cSeq [CUnify (CVar f) (CLam u $ CArray []), e])
      gDef e = CDef [g] (cSeq [CUnify (CVar g) $
                               CLam x $ CLam y $ CDef (a:b:vs) $ cSeq [
                                  CUnify (CVar x) (CArray $ map Var vs),
                                  CUnify (CVar a) e2',
                                  CUnify (CVar b) (CSplit (CApplyVV (Var y) (VArray [])) (Var f) (Var g)),
                                  CApplyVV (VPrim "cons$") (VArray [Var a, Var b])
                                  ],
                               e
                              ])

  pure $ fDef $ gDef $ CSplit e1' (Var f) (Var g)

cOne :: Core -> C Core
cOne e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ COne e
 else do
  u1 <- newTmp
  u2 <- newTmp
  v <- newTmp
  pure $ CSplit e (VLam u1 CFail) (VLam v $ CLam u2 $ CVar v)

cAll :: Core -> C Core
cAll e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ CAll e
 else do
  f <- newTmp
  g <- newTmp
  u <- newTmp
  v <- newTmp
  r <- newTmp
  x <- newTmp
  pure $ CDef [f, g] $
           CSeq [
             CUnify (CVar f) (CLam u $ CArray []),
             CUnify (CVar g) (CLam v $ CLam r $
                               CDef [x] $
                                 CSeq [
                                   CUnify (CVar x) (CSplit (CApplyVV (Var r) (VArray [])) (Var f) (Var g)),
                                   CApplyVV (VPrim "cons$") (VArray [Var v, Var x])
                                   ]),
             CSplit e (Var f) (Var g)
             ]

cSucceeds :: Core -> C Core
cSucceeds e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ CSucceeds e
 else do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (VLam u1 (CWrong "succeeds-fail"))
                (VLam x $ CLam y $ CSplit (CApplyVV (Var y) (VArray []))
                                          (VLam u2 (CVar x))
                                          (VLam u3 $ CLam u4 $ CWrong "succeed-many")
                )

cDecides :: Core -> C Core
cDecides e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ CDecides e
 else do
  u1 <- newTmp
  u2 <- newTmp
  u3 <- newTmp
  u4 <- newTmp
  x <- newTmp
  y <- newTmp
  pure $ CSplit e
                (VLam u1 CFail)
                (VLam x $ CLam y $ CSplit (CApplyVV (Var y) (VArray []))
                                          (VLam u2 (CVar x))
                                          (VLam u3 $ CLam u4 $ CWrong "succeed-many")
                )

-- A small optimization to get smaller examples.
cUnify :: Core -> Core -> Core
cUnify e (CValue (VPrim ":any")) = e
cUnify e1 e2 = CUnify e1 e2

coreD :: HasCallStack => Expr -> C Core
coreD e | null is = core e
        | otherwise = CDef is <$> core e
  where is = getVisible e

val :: HasCallStack => Expr -> C Core
val e = CValue <$> value e

value :: HasCallStack => Expr -> C Value
--value e | trace ("value:\n" ++ prettyShow e) False = undefined
value (LitInt i) = pure (HNF $ HInt i)
value (LitRat i _) = pure (HNF $ HRat $ toRational i)
value (Variable i@(Ident _ s)) | i `elem` primOps = pure (HNF $ HPrim s)
                               | otherwise = pure (Var i)
value (Array es) = HNF . HArray <$> mapM value es
{-
value (Typedef e) = do
  i <- newTmp
  HNF . HLam i <$> coreD (Seq [Unify (Variable i) e, Variable i])
-}
value (Function [(Define x AnyT, fs)] b) = HNF . HLam x . attr <$> coreD b
  where attr ae = foldr CMacro ae fs
value (Lambda i [] e1 e2) = value $ lamFunc True i e1 e2
value AnyT = pure (HNF $ HPrim ":any")
value e = internalErrorMsg $ "value: not a value\n" ++ show e

--eVar :: String -> Expr
--eVar = Variable . Ident noLoc

thunk :: Expr -> C Expr
thunk e = do
--  i <- newTmp
  i <- pure $ Ident noLoc "_"
  pure $ -- Function [(Define i AnyT, [])] e
         Lambda i [] (Array []) e

------

instance Pretty Core where
  pPrintPrec l p (CValue v) = pPrintPrec l p v
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 0) cs
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar c1 c2) = maybeParens (p > 7) $ pPrintPrec l 7 c1 <+> text "|" <+> pPrintPrec l 7 c2
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "def" <+> commaSep l 0 is <+> text "in", pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"
  pPrintPrec l _ (CSplit e f g) =
    text "split" <> braces (sep [pPrintPrec l 0 e <> text ",",
                                 pPrintPrec l 0 f <> text ",",
                                 pPrintPrec l 0 g <> text ","])
  pPrintPrec l _ (CLambda x ys cov e1 e2) =
    parens $ text "\\" <+> pPrintPrec l 0 x <> text "." <+> pPrintPrec l 0 (CDef ys e1) <+>
             (if cov then text "<covariant> " else text "") <>
             text "." <+> pPrintPrec l 0 e2

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
compos f (CApply e1 e2) = CApply <$> f e1 <*> f e2
compos f (CBar e1 e2) = CBar <$> f e1 <*> f e2
compos _ CFail = pure CFail
compos f (CMacro i e) = CMacro i <$> f e
compos f (CDef h e) = CDef h <$> f e
compos _ e@CWrong{} = pure e
compos f (CSplit e n g) = CSplit <$> f e <*> appV f n <*> appV f g
compos f (CLambda i is cov e1 e2) = CLambda i is cov <$> f e1 <*> f e2

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
composC fc _fv _  (CUnify e1 e2) = CUnify <$> fc e1 <*> fc e2
composC fc _  _  (CSeq es) = CSeq <$> traverse fc es
composC fc _  _  (CApply e1 e2) = CApply <$> fc e1 <*> fc e2
composC fc _  _  (CBar e1 e2) = CBar <$> fc e1 <*> fc e2
composC _  _  _  CFail = pure CFail
composC fc _  _  (CMacro i e) = CMacro i <$> fc e
composC fc _  _  (CDef h e) = CDef h <$> fc e
composC _  _  _   e@CWrong{} = pure e
composC fc fv _  (CSplit e f g) = CSplit <$> fc e <*> fv f <*> fv g
composC fc _  _  (CLambda i is cov e1 e2) = CLambda i is cov <$> fc e1 <*> fc e2

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
cfvs (CApply e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CBar e1 e2) = cfvs e1 ++ cfvs e2
cfvs CFail = []
cfvs (CMacro _ e) = cfvs e
cfvs (CDef is e) = filter (`notElem` is) $ cfvs e
cfvs (CSplit e f g) = cfvs e ++ cfvsV f ++ cfvsV g
cfvs CWrong{} = []
cfvs (CLambda i is _ e1 e2) = filter (`notElem` (i:is)) $ cfvs e1 ++ cfvs e2

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
    sub (CApply e1 e2) = CApply (sub e1) (sub e2)
    sub (CBar e1 e2) = CBar (sub e1) (sub e2)
    sub CFail = CFail
    sub (CMacro i e) = CMacro i $ sub e
    sub a@(CDef h e) | x `elem` h = a
                     | null (intersect bs h) = CDef h $ sub e
                     | otherwise = sub $ alphaConvert bs a
    sub e@CWrong{} = e
    sub (CSplit e f g) = CSplit (sub e) (subV f) (subV g)
    sub e@(CLambda i is cov e1 e2)
      | x `elem` (i:is) = e
      | null (intersect (i:is) bs) = CLambda i is cov (sub e1) (sub e2)
      | otherwise = sub $ alphaConvert bs e

    subV v@(Var i) | i == x = b
                   | otherwise = v
    subV (HNF v) = HNF $ subH v

    subH (HArray vs) = HArray $ map subV vs
    subH a@(HLam i e) | x == i = a
                      | i `notElem` bs = HLam i $ sub e
                      | otherwise = subH $ alphaConvertH bs a
    subH v = v

substV :: Ident -> Value -> Value -> Value
substV i x v = let CValue v' = subst i x (CValue v) in v'

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> Core -> Core
alphaConvert vs = alpha []
  where
    alpha m (CValue v) = CValue (alphaV m v)
    alpha m (CUnify e1 e2) = CUnify (alpha m e1) (alpha m e2)
    alpha m (CSeq es) = CSeq (map (alpha m) es)
    alpha m (CApply e1 e2) = CApply (alpha m e1) (alpha m e2)
    alpha m (CBar e1 e2) = CBar (alpha m e1) (alpha m e2)
    alpha _ CFail = CFail
    alpha m (CMacro i e) = CMacro i (alpha m e)
    alpha m (CDef h e) = CDef h' (alpha m' e)
      where h' = map fresh h
            m' = foldr add m $ zip h h'
    alpha _ e@CWrong{} = e
    alpha m (CSplit e f g) = CSplit (alpha m e) (alphaV m f) (alphaV m g)
    alpha m (CLambda i is cov e1 e2) = CLambda i' is' cov (alpha m' e1) (alpha m' e2)
      where i' = fresh i
            is' = map fresh is'
            m' = foldr add m (zip (i:is) (i':is'))

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

-----------------------------------

-- Parse Core
pCoreFile :: P Core
pCoreFile = skip *> pCore <* eof

pCore :: P Core
pCore = (exprToCore flg . simpleDesugar) <$> pSeq
  where flg = defaultFlags{ fSplit = False }

-- XXX pDef, pLam
-- XXX primops

pExists :: P Expr
pExists = exists <$> (pQuant *> some pIdent <* pOp ".") <*> pSeq
  where
    exists :: [Ident] -> Expr -> Expr
    exists is e = foldr (\ i r -> Do $ Seq [Define i AnyT, r]) e is
    pQuant = pKeyword "exists" <|> pKeyword "ex" <|> pKeyword "E"
      -- <|> void (pOp "∃")

pLam :: P Expr
pLam = lam <$> (pLambda *> some pIdent <* pOp ".") <*> pSeq
  where
    lam :: [Ident] -> Expr -> Expr
    lam is e = foldr (\ i r -> Lambda i [] (Array []) r) e is
    pLambda = pKeyword "lam" <|> pKeyword "lambda" <|> void (pOp "\\")
      -- <|> pKeyword "λ"

pSeq :: P Expr
pSeq = choice [ pLam, pExists, cons <$> pEqu <*> optional (pOp ";" *> pSeq) ]
  where
    cons e Nothing = e
    cons e (Just e') = seqE [e, e']

pEqu :: P Expr
pEqu = try (Define <$> (pIdent <* pOp ":=") <*> pChoice)
       <|>
       foldr1 Unify <$> sepBy1 pChoice (pOp "=")

pChoice :: P Expr
pChoice = foldr1 Choice <$> sepBy1 pApply (pOp "|")

pApply :: P Expr
pApply = do
  e1 <- pAtom
  let app f [] = f
      app f (a:as) = app (ApplyD f a) as
      pCall :: P Expr
      pCall = app e1 <$> many pTuple
      pBinOp = do i <- pOper; e2 <- pAtom; pure (ApplyD (Variable i) (Array [e1, e2]))
  pBinOp <|> pCall

pOper :: P Ident
pOper = choice $ map (\ o -> const (Ident noLoc ("in'" ++ o ++ "'")) <$> pOp o)
  [ ">", ">=", "<", "<=", "<>", "+", "-", "*", "/" ]

pTuple :: P Expr
pTuple = try (pParens (pure (Array [])))
         <|>
         pParens pComma

pComma :: P Expr
pComma = try (arr <$> pEqu <*> some (pOp "," *> pEqu))
         <|>
         pSeq
  where arr x xs = Array (x:xs)

pAtom :: P Expr
pAtom = choice [pTuple, pLiteral, Variable <$> pIdent, pMacro, pArray]

pArray :: P Expr
pArray = Array <$> (pKeyword "array" *> pBraces (sepBy pEqu (pOp ",")))

pMacro :: P Expr
pMacro = mac <$> pMacroName <*> pBraces pSeq
  where
    mac i@(Ident _ s) e | s `elem` macros = Macro1 i [] e
    mac i _ = error $ "Unknown macro " ++ prettyShow i
    macros = ["one", "all", "succeeds", "decides"]
