{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module FrontEnd.Core(
  Core(..),
  pattern HNF, pattern CValue,
  pattern COne, pattern CAll, pattern CSucceeds, pattern CDecides,
  Value,
  getValue, getHNF,
  compos, composOp,
  exprToCore,
  cSeq, cDef, cBar,
  isValue,
  fvs, cfvs,
  subst,
  alphaConvert,
  pCore, pCoreFile,
  Ident(..), noLoc,
  ) where
import Prelude hiding ((<>))
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Data(Data)
import Data.List
import Data.Maybe
import GHC.Stack(HasCallStack)
import Text.Megaparsec(sepBy, sepBy1, many, eof, choice, some, optional, (<|>))
-- import Text.Megaparsec.Char(skip)

import Epic.Print
import FrontEnd.Expr hiding (compos, composOp)
import FrontEnd.Desugar(primOps, getVisible, covariantId)
import FrontEnd.Error(unimplemented, impossible, internalError)
import FrontEnd.Flags
import FrontEnd.Parse(P, pOp, pParens, skip, pLiteral, pIdent, pMacroName, pBraces, try, pKeyword, lexeme, string)
import Epic.List(pattern Snoc)
--import Debug.Trace

data Core
  = CVar Ident
  | CInt Integer
  | CRat Rational
  | CPrim String
  | CArray [Value]
  | CLam Ident Core
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
  deriving (Show, Eq, Data)

type Value = Core

type Heap = [Ident]

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

pattern HNF :: Core -> Core
pattern HNF e <- (getHNF -> Just e)

pattern CValue :: Core -> Value
pattern CValue e <- (getValue -> Just e)
  where CValue e = e
--x          | isValue e = e
--x          | otherwise = error "pattern CValue"

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

getHNF :: Core -> Maybe Core
getHNF e@CInt{} = Just e
getHNF e@CRat{} = Just e
getHNF e@CArray{} = Just e
getHNF e@CPrim{} = Just e
getHNF e@CLam{} = Just e
getHNF _ = Nothing

getValue :: Core -> Maybe Core
getValue e@CVar{} = Just e
getValue e = getHNF e

isValue :: Core -> Bool
isValue e = isJust (getValue e)

isValue' :: Core -> Bool
isValue' (CSeq [e]) = isValue' e
isValue' (CSeq (e:es)) = isValue' e && isValue' (CSeq es)
isValue' e = isValue e

vEmpty :: Value
vEmpty = CArray []

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
core (LitInt i) = pure (CInt i)
core (LitRat i _) = pure (CRat $ toRational i)
core (Variable i@(Ident _ s)) | i `elem` primOps = pure (CPrim s)
                              | otherwise = pure (CVar i)
core (Array es) = CArray <$> mapM core es
{-
core (Typedef e) = do
  i <- newTmp
  HNF . HLam i <$> coreD (Seq [Unify (Variable i) e, Variable i])
-}
core (Function [(Define x AnyT, fs)] b) = CLam x . attr <$> coreD b
  where attr ae = foldr CMacro ae fs
--core (Lambda i [] e1 e2) = core $ lamFunc True i e1 e2
core AnyT = pure (CPrim ":any")
core (Wrong s) = pure $ CWrong s
core (Seq (Snoc es e)) = seqC <$> mapM core (Snoc (filter p es) e)
  where p (Define _ AnyT) = False
        p _ = True
core (ApplyS e1 e2) = cSucceeds =<< core (ApplyD e1 e2)
core (ApplyD e1 e2) = CApply <$> core e1 <*> core e2
core (ApplyEff rs e) = coreEffs rs =<< coreD e
core (Unify e1 e2) = cUnify <$> core e1 <*> core e2
core e@Typedef{} = core e
core e@Choice{} = cBar <$> mapM coreD (flat e)
  where flat (Choice e1 e2) = flat e1 ++ flat e2
        flat ee = [ee]
core (Define i AnyT) = pure $ CVar i
core (Define i e) = cUnify (CVar i) <$> core e
core Fail = pure $ CFail
core (For2 e1@(Define i e) e2@(Variable i')) | i == i' = do
  useSplit <- asks fSplit
  if useSplit then
    forSplit e1 e2
   else
    cAll =<< coreD e
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
    pure $ CDef [xa] $ CSeq [cUnify (CVar xa) ea, CApply (CPrim "mapAp$") (CVar xa)]
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
    pure $ CDef [i] $ seqC [cUnify (CVar i) fn, CApply (CVar i) vEmpty]
--core e@Function{} = val e
core (Do e) = coreD e
core (Macro1 (Ident _ "all") [] e) = cAll =<< coreD e
core (Macro1 (Ident _ "one") [] e) = cOne =<< coreD e
core (Macro1 (Ident _ "succeeds") [] e) = cSucceeds =<< coreD e
core (Macro1 (Ident _ "decides") [] e) = cDecides =<< coreD e
core (Lambda i [] (Array []) e) = core $ Function [(Define i AnyT, [])] e
core (Lambda i rs e1 e2) = do
  --traceM $ "Lambda:\n" ++ prettyShow eee ++ "\n+++++\n"
  timLam <- asks fTimLambda
  let covariant = covariantId `elem` rs || True -- XXX
  if timLam then do
    let is = getVisible e1
    e1' <- core e1
    e2' <- coreD e2
    pure $ CLambda i is covariant e1' e2'
  else
    core $ lamFunc covariant i e1 e2
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
    if e1 == Array [] then
      e2
    else if trivial e1 then
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
                                  CUnify (CVar x) (CArray $ map CVar vs),
                                  CUnify (CVar a) e2',
                                  CUnify (CVar b) (CSplit (CApply (CVar y) (CArray [])) (CVar f) (CVar g)),
                                  CApply (CPrim "cons$") (CArray [CVar a, CVar b])
                                  ],
                               e
                              ])

  pure $ fDef $ gDef $ CSplit e1' (CVar f) (CVar g)

cOne :: Core -> C Core
cOne e = do
 useSplit <- asks fSplit
 if not useSplit then pure $ COne e
 else do
  u1 <- newTmp
  u2 <- newTmp
  v <- newTmp
  pure $ CSplit e (CLam u1 CFail) (CLam v $ CLam u2 $ CVar v)

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
                                   CUnify (CVar x) (CSplit (CApply (CVar r) (CArray [])) (CVar f) (CVar g)),
                                   CApply (CPrim "cons$") (CArray [CVar v, CVar x])
                                   ]),
             CSplit e (CVar f) (CVar g)
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
                (CLam u1 (CWrong "succeeds-fail"))
                (CLam x $ CLam y $ CSplit (CApply (CVar y) (CArray []))
                                          (CLam u2 (CVar x))
                                          (CLam u3 $ CLam u4 $ CWrong "succeed-many")
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
                (CLam u1 CFail)
                (CLam x $ CLam y $ CSplit (CApply (CVar y) (CArray []))
                                          (CLam u2 (CVar x))
                                          (CLam u3 $ CLam u4 $ CWrong "succeed-many")
                )

-- A small optimization to get smaller examples.
cUnify :: Core -> Core -> Core
cUnify e (CPrim ":any") = e
cUnify e1 e2 = CUnify e1 e2

coreD :: HasCallStack => Expr -> C Core
coreD e | null is = core e
        | otherwise = CDef is <$> core e
  where is = getVisible e

{-
val :: HasCallStack => Expr -> C Core
val e = CValue <$> value e

value :: HasCallStack => Expr -> C Value
--value e | trace ("value:\n" ++ prettyShow e) False = undefined
value (LitInt i) = pure (HNF $ HInt i)
value (LitRat i _) = pure (HNF $ HRat $ toRational i)
value (Variable i@(Ident _ s)) | i `elem` primOps = pure (HNF $ HPrim s)
                               | otherwise = pure (Var i)
value (Array es) = CArray <$> mapM value es
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
-}

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
  pPrintPrec l p (CVar i) = pPrintPrec l p i
  pPrintPrec l p (CInt i) = pPrintPrec l p i
  pPrintPrec _ _ (CRat _) = undefined -- pPrintPrec l p r
  pPrintPrec _ _ (CPrim s) = text s
  pPrintPrec l _ (CArray [v]) = text "array" <> braces (pPrintPrec l 0 v)
  pPrintPrec l _ (CArray vs) = parens $ commaSep l vs
  pPrintPrec l p (CLam i c) = maybeParens (p > 2) $ pPrintPrec l 0 i <+> text "=>" <+> pPrintPrec l 0 c
  pPrintPrec l p (CUnify c1 c2) = maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "=" <+> pPrintPrec l 6 c2
  pPrintPrec l p (CSeq cs) = maybeParens (p > 0) $ vcat $ punctuate (text ";") $ map (pPrintPrec l 0) cs
  pPrintPrec l p (CApply (CVar (Ident _ "~")) (CArray [c1, c2])) =
                  maybeParens (p > 6) $ pPrintPrec l 6 c1 <+> text "~" <+> pPrintPrec l 6 c2
  pPrintPrec l _ (CApply c1 c2) = pPrintPrec l 10 c1 <> brackets (pPrintPrec l 0 c2)
  pPrintPrec _ _ CFail = text "fail"
  pPrintPrec l p (CBar c1 c2) = maybeParens (p > 7) $ pPrintPrec l 7 c1 <+> text "|" <+> pPrintPrec l 7 c2
  pPrintPrec l _ (CMacro (Ident _ s) e) = text s <> braces (pPrintPrec l 0 e)
  pPrintPrec l p (CDef is e) =
    maybeParens (p > 0) $ fsep [text "ex" <+> hsep (map (pPrintPrec l 0) is) <> text ".", pPrintPrec l 0 e]
  pPrintPrec _ _ (CWrong s) = text $ "wrong(" ++ show s ++ ")"
  pPrintPrec l _ (CSplit e f g) =
    text "split" <> braces (sep [pPrintPrec l 0 e <> text ",",
                                 pPrintPrec l 0 f <> text ",",
                                 pPrintPrec l 0 g <> text ","])
  pPrintPrec l _ (CLambda x ys cov e1 e2) =
    parens $ text "\\" <+> pPrintPrec l 0 x <> text "." <+> pPrintPrec l 0 (CDef ys e1) <+>
             (if cov then text "<covariant> " else text "") <>
             text "." <+> pPrintPrec l 0 e2

------

compos :: (Applicative f) => (Core -> f Core) -> Core -> f Core
compos _ v@CVar{} = pure v
compos _ v@CInt{} = pure v
compos _ v@CRat{} = pure v
compos _ v@CPrim{} = pure v
compos f (CArray vs) = CArray <$> traverse f vs
compos f (CLam i e) = CLam i <$> f e
compos f (CUnify e1 e2) = CUnify <$> f e1 <*> f e2
compos f (CSeq es) = CSeq <$> traverse f es
compos f (CApply e1 e2) = CApply <$> f e1 <*> f e2
compos f (CBar e1 e2) = CBar <$> f e1 <*> f e2
compos _ CFail = pure CFail
compos f (CMacro i e) = CMacro i <$> f e
compos f (CDef h e) = CDef h <$> f e
compos _ e@CWrong{} = pure e
compos f (CSplit e n g) = CSplit <$> f e <*> f n <*> f g
compos f (CLambda i is cov e1 e2) = CLambda i is cov <$> f e1 <*> f e2

composOp :: (Core -> Core) -> Core -> Core
composOp f = runIdentity . compos (pure . f)

-- Unique free variables
fvs :: Core -> [Ident]
fvs = nub . cfvs

-- Occurrences of free variables
cfvs :: Core -> [Ident]
cfvs (CVar v) = [v]
cfvs CInt{} = []
cfvs CRat{} = []
cfvs CPrim{} = []
cfvs (CArray vs) = concatMap cfvs vs
cfvs (CLam i e) = filter (/= i) $ cfvs e
cfvs (CUnify e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CSeq es) = concatMap cfvs es
cfvs (CApply e1 e2) = cfvs e1 ++ cfvs e2
cfvs (CBar e1 e2) = cfvs e1 ++ cfvs e2
cfvs CFail = []
cfvs (CMacro _ e) = cfvs e
cfvs (CDef is e) = filter (`notElem` is) $ cfvs e
cfvs (CSplit e f g) = cfvs e ++ cfvs f ++ cfvs g
cfvs CWrong{} = []
cfvs (CLambda i is _ e1 e2) = filter (`notElem` (i:is)) $ cfvs e1 ++ cfvs e2


------

-- Replace x by b in e
-- Do an occurs check and alpha-conversion when necessary
subst :: Ident -> Value -> Core -> Core
subst x b ae | x `elem` bs = impossible "subst occur check"
             | otherwise = sub ae
  where
    bs = fvs b
    sub v@(CVar i) | i == x = b
                  | otherwise = v
    sub e@CInt{} = e
    sub e@CRat{} = e
    sub e@CPrim{} = e
    sub (CArray vs) = CArray $ map sub vs
    sub a@(CLam i e) | x == i = a
                     | i `notElem` bs = CLam i $ sub e
                     | otherwise = sub $ alphaConvert bs a
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
    sub (CSplit e f g) = CSplit (sub e) (sub f) (sub g)
    sub e@(CLambda i is cov e1 e2)
      | x `elem` (i:is) = e
      | null (intersect (i:is) bs) = CLambda i is cov (sub e1) (sub e2)
      | otherwise = sub $ alphaConvert bs e

-- Alpha convert a term, avoiding vs as the names for bound
-- variables.
alphaConvert :: [Ident] -> Core -> Core
alphaConvert vs = alpha []
  where
    alpha m (CVar i) = CVar $ fromMaybe i $ lookup i m
    alpha _ e@CInt{} = e
    alpha _ e@CRat{} = e
    alpha _ e@CPrim{} = e
    alpha m (CArray es) = CArray (map (alpha m) es)
    alpha m (CLam i e) = CLam i' $ alpha (add (i, i') m) e where i' = fresh i
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
    alpha m (CSplit e f g) = CSplit (alpha m e) (alpha m f) (alpha m g)
    alpha m (CLambda i is cov e1 e2) = CLambda i' is' cov (alpha m' e1) (alpha m' e2)
      where i' = fresh i
            is' = map fresh is'
            m' = foldr add m (zip (i:is) (i':is'))

    add ii@(i, i') m | i == i' = m
                     | otherwise = ii : m

    fresh i@(Ident l s) | i `notElem` vs = i
                        | otherwise = fresh $ Ident l (s ++ "'")

-----------------------------------

-- Parse Core
pCoreFile :: P Core
pCoreFile = skip *> pCore <* eof

pCore :: P Core
pCore = exprToCore flg <$> pSeq
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
  [ ">=", "<=", "<>", "+", "-", "*", "/" ]

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
pAtom = choice [pTuple, pLiteral, pName, pMacro, pArray]

pName :: P Expr
pName = do
  i@(Ident l s) <- pIdent
  let ops = [ ("fail", Fail)
            , ("gt", vi "in'>'")
            , ("lt", vi "in'<'")
            , ("add", vi "in'+'")
            , ("isInt", vi "isInt$")
            ]
      vi = Variable . Ident l
  pure $ fromMaybe (Variable i) $ lookup s ops

pArray :: P Expr
pArray =
    Array <$> (pKeyword "array" *> pBraces (sepBy pSeq (pOp ",")))
  <|>
    pLT *> (Array <$> sepBy pSeq (pOp ",")) <* pGT
 where pLT = lexeme (string "<")
       pGT = lexeme (string ">")

pMacro :: P Expr
pMacro = mac <$> pMacroName <*> pBraces pSeq
  where
    mac i@(Ident _ s) e | s `elem` macros = Macro1 i [] e
    mac i _ = error $ "Unknown macro " ++ prettyShow i
    macros = ["one", "all", "succeeds", "decides"]
